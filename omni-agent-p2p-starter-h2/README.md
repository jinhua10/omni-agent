# OmniAgent P2P Starter - H2

H2数据库的P2P数据传输启动器，支持从H2数据库读取数据并传输到其他存储系统。

## 特性

- ✅ 支持H2数据库的数据读取和写入
- ✅ 支持三种数据库模式：内存(mem)、文件(file)、嵌入式(embedded)
- ✅ 自动创建数据表
- ✅ 批量数据传输
- ✅ 连接测试
- ✅ 统计信息查询
- ✅ Spring Boot自动配置

## 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-p2p-starter-h2</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 2. 配置文件

```yaml
omni-agent:
  p2p:
    h2:
      database-path: data/p2p-transfer  # H2数据库文件路径
      username: sa                       # 数据库用户名
      password:                          # 数据库密码
      mode: file                         # 数据库模式: mem/file/embedded
      source-table: knowledge_base       # 源表名称
      batch-size: 1000                   # 批次大小
      auto-create-table: true            # 是否自动创建表
```

### 3. 使用示例

#### 基本使用

```java
@SpringBootApplication
public class H2P2PExample {
    
    @Autowired
    private P2PDataTransferService h2Service;
    
    @Autowired
    private P2PTransferBridge transferBridge;
    
    public void transferData() {
        // 定义查询条件
        Map<String, Object> query = Map.of(
            "type", "document",
            "limit", 1000
        );
        
        // 读取数据
        List<Map<String, Object>> data = h2Service.readFromSource(query);
        System.out.println("读取到 " + data.size() + " 条记录");
        
        // 写入数据
        int count = h2Service.writeToTarget(data);
        System.out.println("写入 " + count + " 条记录");
    }
}
```

#### H2 → Elasticsearch 传输

```java
@Autowired
private H2P2PDataTransferService h2Service;

@Autowired
private ElasticsearchP2PDataTransferService esService;

@Autowired
private P2PTransferBridge transferBridge;

public void transferH2ToES() {
    Map<String, Object> query = Map.of(
        "category", "技术文档",
        "limit", 5000
    );
    
    TransferResult result = transferBridge.transfer(
        h2Service,
        esService,
        query,
        Function.identity(),
        100  // batchSize
    );
    
    System.out.println("传输完成: " + result.getTransferredCount());
}
```

#### 连接测试

```java
@Autowired
private P2PDataTransferService h2Service;

public void testConnection() {
    boolean connected = h2Service.testConnection();
    if (connected) {
        System.out.println("H2数据库连接成功");
    } else {
        System.out.println("H2数据库连接失败");
    }
}
```

#### 查询统计信息

```java
@Autowired
private P2PDataTransferService h2Service;

public void showStatistics() {
    Map<String, Object> stats = h2Service.getStatistics();
    System.out.println("总记录数: " + stats.get("total_records"));
    System.out.println("数据库模式: " + stats.get("database_mode"));
    System.out.println("数据库路径: " + stats.get("database_path"));
}
```

## 配置说明

### 数据库模式

H2支持三种数据库模式：

1. **内存模式 (mem)**：数据存储在内存中，速度最快，但重启后数据丢失
   ```yaml
   mode: mem
   database-path: testdb
   # 生成URL: jdbc:h2:mem:testdb
   ```

2. **文件模式 (file)**：数据持久化到文件，推荐用于生产环境
   ```yaml
   mode: file
   database-path: data/p2p-transfer
   # 生成URL: jdbc:h2:file:data/p2p-transfer
   ```

3. **嵌入式模式 (embedded)**：与文件模式相同，是file的别名
   ```yaml
   mode: embedded
   database-path: data/p2p-transfer
   # 生成URL: jdbc:h2:file:data/p2p-transfer
   ```

### 表结构

自动创建的表结构：

```sql
CREATE TABLE IF NOT EXISTS knowledge_base (
    id VARCHAR(255) PRIMARY KEY,
    content TEXT,
    type VARCHAR(100),
    metadata TEXT,
    created_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_time TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## H2 vs SQLite 对比

| 特性 | H2 | SQLite |
|------|-----|--------|
| 性能 | ⭐⭐⭐⭐⭐ 更快 | ⭐⭐⭐⭐ 快 |
| 内存模式 | ✅ 支持 | ❌ 不支持 |
| SQL标准 | ⭐⭐⭐⭐⭐ 完全兼容 | ⭐⭐⭐⭐ 良好 |
| 并发性 | ⭐⭐⭐⭐⭐ 更好 | ⭐⭐⭐ 一般 |
| 文件大小 | 较大 | 较小 |
| 适用场景 | 开发测试、高性能需求 | 生产环境、嵌入式 |

## 高级特性

### 1. 批量传输

```java
TransferResult result = h2Service.transfer(query, 500);
System.out.println("成功: " + result.getTransferredCount());
System.out.println("失败: " + result.getFailedCount());
System.out.println("耗时: " + result.getDuration() + "ms");
```

### 2. 条件查询

```java
Map<String, Object> query = Map.of(
    "type", "document",
    "category", "技术",
    "limit", 100,
    "offset", 0
);

List<Map<String, Object>> data = h2Service.readFromSource(query);
```

### 3. 数据转换

```java
Function<Map<String, Object>, Map<String, Object>> transformer = data -> {
    data.put("transferred_at", Instant.now());
    data.put("source", "h2");
    return data;
};

TransferResult result = transferBridge.transfer(
    h2Service,
    targetService,
    query,
    transformer,
    100
);
```

## 故障排除

### 问题1: 数据库文件锁定

**现象**: `Database may be already in use`

**解决方案**:
```yaml
# 添加以下参数到JDBC URL
database-path: data/p2p-transfer;AUTO_SERVER=TRUE
```

### 问题2: 内存不足

**现象**: `OutOfMemoryError` in mem mode

**解决方案**:
```yaml
# 切换到文件模式
mode: file
```

### 问题3: 表不存在

**现象**: `Table not found`

**解决方案**:
```yaml
# 启用自动创建表
auto-create-table: true
```

## 注意事项

1. **内存模式**：仅用于测试，不适合生产环境
2. **文件路径**：确保应用有读写权限
3. **并发访问**：使用`AUTO_SERVER=TRUE`参数支持多进程访问
4. **备份**：定期备份.mv.db文件
5. **性能优化**：适当调整batch-size参数

## 相关链接

- [H2 Database官网](https://www.h2database.com/)
- [H2 SQL语法](https://www.h2database.com/html/grammar.html)
- [Spring Boot JDBC](https://spring.io/guides/gs/relational-data-access/)

## 许可证

本项目采用 Apache 2.0 许可证。
