# P2P 数据传输指南
# P2P Data Transfer Guide

## 概述 (Overview)

P2P数据传输模块是OmniAgent框架的核心特性，实现了**异构存储之间的无缝数据迁移和同步**。

The P2P Data Transfer module is a core feature of the OmniAgent framework, enabling **seamless data migration and synchronization between heterogeneous storage systems**.

### 核心能力 (Core Capabilities)

- ✅ **异构存储互通** - SQLite ⟷ Elasticsearch, File ⟷ MongoDB, Redis ⟷ H2
- ✅ **数据转换管道** - 自动格式转换，支持自定义Transformer
- ✅ **批量传输** - 高性能批处理，可配置批次大小
- ✅ **双向同步** - 4种同步策略 (SOURCE_WINS, TARGET_WINS, LATEST_WINS, MERGE)
- ✅ **统计监控** - 传输统计、成功率、失败记录

---

## 架构设计 (Architecture)

### 三层架构模型

```
┌───────────────────────────────────────────┐
│       P2PTransferBridge (Bridge)          │
│  - 编排层：管理源和目标服务的协调          │
│  - 支持自定义Transformer                  │
│  - 实现双向同步策略                        │
└─────────────┬─────────────────────────────┘
              │
              │ orchestrates
              ↓
┌───────────────────────────────────────────┐
│   P2PDataTransferService (Interface)      │
│  - 存储抽象层：统一的读写接口              │
│  - readFromSource / writeToTarget         │
│  - transformData / batchTransfer          │
└─────────────┬─────────────────────────────┘
              │
              │ implemented by
              ↓
┌──────────┬──────────┬──────────┬──────────┐
│ SQLite   │  Redis   │ MongoDB  │    ES    │
│ Starter  │ Starter  │ Starter  │ Starter  │
└──────────┴──────────┴──────────┴──────────┘
```

### 核心接口 (Core Interfaces)

#### 1. P2PDataTransferService

存储层抽象接口，所有存储实现必须实现此接口：

```java
public interface P2PDataTransferService {
    
    /**
     * 从源存储读取数据
     * @param query 查询条件 (type, limit, offset, filters...)
     * @return 数据记录列表
     */
    List<Map<String, Object>> readFromSource(Map<String, Object> query);
    
    /**
     * 写入数据到目标存储
     * @param data 数据记录列表
     * @return 成功写入的记录数
     */
    int writeToTarget(List<Map<String, Object>> data);
    
    /**
     * 转换数据格式
     * @param sourceData 源数据
     * @return 标准化后的数据
     */
    Map<String, Object> transformData(Map<String, Object> sourceData);
    
    /**
     * 批量传输（完整流程：读取→转换→写入）
     * @param sourceQuery 源查询条件
     * @param batchSize 批次大小
     * @return 传输结果统计
     */
    TransferResult batchTransfer(Map<String, Object> sourceQuery, int batchSize);
    
    /**
     * 获取传输统计信息
     * @return 统计数据（记录数、大小、元数据等）
     */
    Map<String, Object> getTransferStatistics();
}
```

#### 2. P2PTransferBridge

编排层接口，负责协调源和目标服务：

```java
public interface P2PTransferBridge {
    
    /**
     * 单向数据传输
     * @param sourceService 源服务
     * @param targetService 目标服务
     * @param sourceQuery 源查询条件
     * @param transformer 自定义转换函数（可选）
     * @param batchSize 批次大小
     * @return 传输结果统计
     */
    P2PDataTransferService.TransferResult transfer(
        P2PDataTransferService sourceService,
        P2PDataTransferService targetService,
        Map<String, Object> sourceQuery,
        Function<Map<String, Object>, Map<String, Object>> transformer,
        int batchSize
    );
    
    /**
     * 双向同步
     * @param service1 服务1
     * @param service2 服务2
     * @param strategy 同步策略
     * @return 同步结果统计
     */
    SyncResult bidirectionalSync(
        P2PDataTransferService service1,
        P2PDataTransferService service2,
        SyncStrategy strategy
    );
    
    /**
     * 同步策略枚举
     */
    enum SyncStrategy {
        SOURCE_WINS,    // 服务1覆盖服务2
        TARGET_WINS,    // 服务2覆盖服务1
        LATEST_WINS,    // 最新修改的数据获胜
        MERGE           // 合并双方差异
    }
}
```

---

## 使用示例 (Usage Examples)

### 示例1: SQLite → Elasticsearch 数据迁移

```java
@Autowired
P2PTransferBridge transferBridge;

@Autowired
@Qualifier("sqliteService")
P2PDataTransferService sqliteService;

@Autowired
@Qualifier("esService")
P2PDataTransferService esService;

public void migrateSQLiteToElasticsearch() {
    // 定义查询条件
    Map<String, Object> query = new HashMap<>();
    query.put("type", "knowledge");
    query.put("limit", 1000);
    
    // 自定义数据转换
    Function<Map, Map> transformer = sourceData -> {
        Map<String, Object> transformed = new HashMap<>(sourceData);
        // 添加ES所需的字段
        transformed.put("_index", "knowledge_base");
        transformed.put("_type", "_doc");
        transformed.put("transferred_at", System.currentTimeMillis());
        return transformed;
    };
    
    // 执行传输
    TransferResult result = transferBridge.transfer(
        sqliteService,
        esService,
        query,
        transformer,
        100  // 每批100条
    );
    
    log.info("Transfer completed: {} records, {} succeeded, {} failed",
        result.getTotalRecords(),
        result.getSuccessCount(),
        result.getFailureCount());
}
```

### 示例2: 文件 → MongoDB 数据导入

```java
@Autowired
@Qualifier("fileService")
P2PDataTransferService fileService;

@Autowired
@Qualifier("mongoService")
P2PDataTransferService mongoService;

public void importFilesToMongoDB() {
    Map<String, Object> query = new HashMap<>();
    query.put("directory", "./data/exports");
    query.put("pattern", "*.json");
    
    // 使用默认转换（standardize格式）
    TransferResult result = transferBridge.transfer(
        fileService,
        mongoService,
        query,
        null,  // 无自定义转换，使用默认
        50
    );
    
    System.out.println("Imported: " + result.getSuccessCount() + " documents");
}
```

### 示例3: Redis ⟷ H2 双向同步

```java
public void syncRedisAndH2() {
    // MERGE策略：合并双方差异
    SyncResult result = transferBridge.bidirectionalSync(
        redisService,
        h2Service,
        SyncStrategy.MERGE
    );
    
    log.info("Sync completed:");
    log.info("  Redis → H2: {} records", result.getService1ToService2Count());
    log.info("  H2 → Redis: {} records", result.getService2ToService1Count());
    log.info("  Duration: {}ms", result.getDurationMs());
}
```

---

## 存储实现 (Storage Implementations)

### 1. SQLite P2P Data Transfer Starter

#### 依赖配置 (Maven)

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-p2p-starter-sqlite</artifactId>
    <version>1.0.0</version>
</dependency>
```

#### 配置文件 (application.yml)

```yaml
omni-agent:
  p2p:
    sqlite:
      database-path: ./data/knowledge.db
      source-table: knowledge_items
      batch-size: 100
      auto-create-table: true
```

#### 功能特性

- ✅ 自动创建表结构（id, content, type, metadata, timestamps）
- ✅ 动态SQL查询（WHERE条件、LIMIT/OFFSET分页）
- ✅ INSERT OR REPLACE批量写入
- ✅ 数据标准化（添加source、timestamp等元数据）
- ✅ 统计信息查询（表大小、记录数、索引信息）

### 2. Redis P2P Starter

```yaml
spring:
  redis:
    host: localhost
    port: 6379
    password: ${REDIS_PASSWORD}
```

- ✅ RedisTemplate JSON序列化
- ✅ 按键前缀批量读取
- ✅ Pipeline批量写入优化

### 3. MongoDB P2P Starter

```yaml
spring:
  data:
    mongodb:
      uri: mongodb://localhost:27017/omni-agent
```

- ✅ MongoTemplate查询和写入
- ✅ 支持复杂查询条件（$gt, $lt, $regex等）
- ✅ BulkOperations批量优化

### 4. Elasticsearch P2P Starter

```yaml
spring:
  elasticsearch:
    uris: http://localhost:9200
    username: elastic
    password: changeme
```

- ✅ ElasticsearchOperations索引操作
- ✅ 动态索引映射
- ✅ Bulk API批量索引

---

## 高级特性 (Advanced Features)

### 1. 自定义数据转换

```java
// 示例：将SQLite数据转换为ES的嵌套对象结构
Function<Map, Map> customTransformer = sourceData -> {
    Map<String, Object> esDoc = new HashMap<>();
    
    // 提取和转换字段
    esDoc.put("id", sourceData.get("id"));
    esDoc.put("title", sourceData.get("content"));
    
    // 创建嵌套对象
    Map<String, Object> metadata = new HashMap<>();
    metadata.put("source_type", "sqlite");
    metadata.put("imported_at", Instant.now().toString());
    metadata.put("version", sourceData.get("version"));
    esDoc.put("metadata", metadata);
    
    // 标签数组转换
    String tagsJson = (String) sourceData.get("tags");
    esDoc.put("tags", Arrays.asList(tagsJson.split(",")));
    
    return esDoc;
};
```

### 2. 双向同步策略详解

| 策略          | 行为描述                                      | 适用场景               |
|---------------|----------------------------------------------|------------------------|
| SOURCE_WINS   | Service1 → Service2（单向覆盖）              | 主从复制，Service1为主 |
| TARGET_WINS   | Service2 → Service1（单向覆盖）              | 数据恢复，Service2为主 |
| LATEST_WINS   | 比较时间戳，最新数据双向覆盖                  | 时间戳可靠的实时同步   |
| MERGE         | 计算差集，双向补充缺失数据                    | 多端协作，数据合并     |

### 3. 错误处理和重试

```java
// DefaultP2PTransferBridge内部实现
try {
    List<Map<String, Object>> sourceData = sourceService.readFromSource(sourceQuery);
    List<Map<String, Object>> transformedData = sourceData.stream()
        .map(data -> {
            try {
                return transformer != null ? transformer.apply(data) : data;
            } catch (Exception e) {
                log.error("Transform failed for record {}: {}", data.get("id"), e.getMessage());
                return null;  // 标记为失败
            }
        })
        .filter(Objects::nonNull)
        .collect(Collectors.toList());
    
    // 批量写入目标
    int written = targetService.writeToTarget(transformedData);
    
} catch (Exception e) {
    log.error("Batch transfer failed: {}", e.getMessage());
    // 记录失败统计
}
```

---

## 性能优化 (Performance Tuning)

### 批次大小调优

| 数据量        | 推荐批次大小 | 说明                               |
|--------------|-------------|-------------------------------------|
| < 1万条      | 50 - 100    | 小数据集，快速完成                  |
| 1万 - 10万   | 100 - 500   | 平衡内存和网络开销                  |
| 10万 - 100万 | 500 - 1000  | 大数据集，批量优化                  |
| > 100万      | 1000 - 5000 | 超大数据集，配合JVM内存调优         |

### 并发传输

```java
@Async
public CompletableFuture<TransferResult> transferAsync(
    P2PDataTransferService source,
    P2PDataTransferService target,
    Map<String, Object> query
) {
    return CompletableFuture.supplyAsync(() -> 
        transferBridge.transfer(source, target, query, null, 1000)
    );
}

// 并发执行多个传输任务
List<CompletableFuture<TransferResult>> futures = Arrays.asList(
    transferAsync(sqliteService, esService, query1),
    transferAsync(fileService, mongoService, query2),
    transferAsync(redisService, h2Service, query3)
);

CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
```

---

## 监控和日志 (Monitoring & Logging)

### 传输统计示例

```java
Map<String, Object> stats = sqliteService.getTransferStatistics();

System.out.println("SQLite Statistics:");
System.out.println("  Total Records: " + stats.get("total_records"));
System.out.println("  Table Size: " + stats.get("table_size_mb") + " MB");
System.out.println("  Last Modified: " + stats.get("last_modified"));
```

### 日志配置

```yaml
logging:
  level:
    top.yumbo.ai.p2p: DEBUG
    top.yumbo.ai.p2p.core.DefaultP2PTransferBridge: INFO
```

**日志输出示例：**

```
[INFO ] Starting transfer from SQLite to Elasticsearch
[DEBUG] Reading source data: query={type=knowledge, limit=1000}
[DEBUG] Read 1000 records from source
[DEBUG] Transforming data with custom transformer
[DEBUG] Transformed 1000 records successfully
[DEBUG] Writing batch 1/10: 100 records
[DEBUG] Writing batch 2/10: 100 records
...
[INFO ] Transfer completed: 1000 total, 998 succeeded, 2 failed, duration=3245ms
```

---

## 常见问题 (FAQ)

### Q1: 如何处理大文件（GB级）传输？

A: 使用流式处理和分批传输：

```java
// 分批查询，避免内存溢出
int offset = 0;
int batchSize = 1000;
while (true) {
    Map<String, Object> query = new HashMap<>();
    query.put("limit", batchSize);
    query.put("offset", offset);
    
    TransferResult result = transferBridge.transfer(source, target, query, null, 100);
    
    if (result.getTotalRecords() < batchSize) break;
    offset += batchSize;
}
```

### Q2: 如何保证数据一致性？

A: 使用事务和校验：

```java
// 启用目标存储的事务支持
@Transactional
public void transferWithTransaction() {
    TransferResult result = transferBridge.transfer(...);
    
    // 校验数据完整性
    if (result.getFailureCount() > 0) {
        throw new RuntimeException("Transfer failed, rolling back");
    }
}
```

### Q3: 支持哪些数据格式？

A: 统一使用 `Map<String, Object>` 作为数据交换格式，支持：
- 基础类型：String, Integer, Long, Double, Boolean
- 复杂类型：Date, Timestamp, List, Map
- 二进制数据：byte[], Base64编码

### Q4: 如何扩展新的存储类型？

A: 实现 `P2PDataTransferService` 接口：

```java
@Service
public class MyCustomStorageService implements P2PDataTransferService {
    
    @Override
    public List<Map<String, Object>> readFromSource(Map<String, Object> query) {
        // 实现读取逻辑
    }
    
    @Override
    public int writeToTarget(List<Map<String, Object>> data) {
        // 实现写入逻辑
    }
    
    // ... 实现其他方法
}
```

---

## 路线图 (Roadmap)

### 已完成 ✅

- [x] P2P数据传输API设计
- [x] SQLite存储实现
- [x] Redis存储实现（基于旧API，待迁移）
- [x] MongoDB存储实现（基于旧API，待迁移）
- [x] Elasticsearch存储实现（基于旧API，待迁移）
- [x] DefaultP2PTransferBridge实现
- [x] 批量传输和统计功能

### 开发中 🚧

- [ ] File-based P2P Starter（CSV/JSON/XML）
- [ ] H2 P2P Data Transfer Starter
- [ ] 迁移现有P2P Starters到新API
- [ ] 增量同步（基于时间戳/版本号）
- [ ] 冲突解决策略扩展

### 未来计划 📋

- [ ] 实时流式传输（基于WebSocket/gRPC）
- [ ] 数据加密传输（AES-256）
- [ ] 数据压缩（Gzip/LZ4）
- [ ] 分布式传输协调（基于Zookeeper/Consul）
- [ ] 传输调度和任务队列
- [ ] Web UI管理界面

---

## 贡献指南 (Contributing)

欢迎贡献代码、文档或问题反馈！

1. Fork项目到您的GitHub账号
2. 创建功能分支：`git checkout -b feature/new-storage-impl`
3. 实现您的功能并编写测试
4. 提交变更：`git commit -m "Add new storage implementation"`
5. 推送到分支：`git push origin feature/new-storage-impl`
6. 创建Pull Request

---

## 许可证 (License)

Apache License 2.0 - 详见 [LICENSE.txt](../../LICENSE.txt)

---

## 联系方式 (Contact)

- **作者**: Jinhua Yu
- **邮箱**: 1015770492@qq.com
- **GitHub**: https://github.com/jinhua10/omni-agent
- **文档**: https://github.com/jinhua10/omni-agent/wiki

---

**最后更新**: 2025-12-15  
**版本**: 1.0.0
