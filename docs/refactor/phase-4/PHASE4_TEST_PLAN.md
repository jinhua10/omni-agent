# 🧪 Phase 4 - 集成测试计划

> **阶段**: Phase 4 - Integration Testing  
> **创建时间**: 2025-12-15  
> **状态**: ⏳ 进行中

---

## 📋 测试概览

### 测试目标
验证 OmniAgent 四维可插拔架构的：
1. ✅ **功能完整性** - 所有功能正常工作
2. ✅ **可插拔性** - Starter 可以自由切换
3. ✅ **稳定性** - 系统运行稳定可靠
4. ✅ **性能** - 不同实现的性能对比

### 测试范围
```
四个维度 × 多种实现 = 大量组合
├── Persistence: 6种实现
├── Document Storage: 6种实现
├── RAG: 6种实现
└── AI: 2种实现

总组合数: 6 × 6 × 6 × 2 = 432 种可能组合
测试组合: 精选 10 种典型组合进行深度测试
```

---

## 🎯 测试策略

### 1. 单元测试（Unit Tests）
**目标**: 验证单个模块的功能正确性

#### 1.1 API 模块测试
```java
// Persistence API 测试
- QuestionClassifierPersistence 接口方法测试
- QuestionTypeConfig 模型测试

// Document Storage API 测试
- DocumentStorageService 接口方法测试
- Chunk、Image、PPLData 模型测试

// RAG API 测试
- RAGService 接口方法测试
- Document、Query、SearchResult 模型测试

// AI API 测试
- AIService、EmbeddingService 接口方法测试
- AIRequest、AIResponse、ChatMessage 模型测试
```

#### 1.2 Core 模块测试（使用 Mock）
```java
// HOPE 系统测试
- QuestionClassifier 测试（Mock Persistence）
- HOPEKnowledgeManager 测试
- 三层服务测试（高频、中频、低频）

// 文档处理测试
- DocumentChunkingService 测试（Mock Storage）
- ImageStorageService 测试
- PPLStorageService 测试

// 其他核心模块测试
- QueryService 测试（Mock RAG）
- RoleService 测试
- FeedbackService 测试
- EvolutionService 测试
```

#### 1.3 Starter 独立测试
```java
// 每个 Starter 的独立功能测试
- Memory Persistence 测试
- H2 Persistence 测试
- File Document Storage 测试
- Lucene RAG 测试
- Ollama AI 测试
... (共 22 个 Starter)
```

---

### 2. 集成测试（Integration Tests）
**目标**: 验证多个模块协同工作

#### 2.1 典型组合测试

##### 组合 1: 开发环境（最轻量）✅
```yaml
配置:
  persistence: memory
  document-storage: file
  rag: file (Lucene)
  ai: ollama (本地)

测试场景:
  - 问题分类和存储
  - 文档上传和切分
  - RAG 检索
  - AI 对话
  - 端到端流程
```

##### 组合 2: 单机应用（嵌入式数据库）✅
```yaml
配置:
  persistence: h2
  document-storage: file
  rag: h2
  ai: ollama

测试场景:
  - 数据持久化
  - 重启后数据恢复
  - 并发访问
  - 性能基准测试
```

##### 组合 3: 轻量级部署（SQLite）✅
```yaml
配置:
  persistence: sqlite
  document-storage: file
  rag: sqlite
  ai: ollama

测试场景:
  - 单文件数据库
  - 备份和迁移
  - FTS5 全文搜索
  - 查询性能
```

##### 组合 4: 高性能缓存（Redis）✅
```yaml
配置:
  persistence: redis
  document-storage: redis
  rag: redis
  ai: ollama

测试场景:
  - 高速读写
  - 向量相似度搜索
  - TTL 自动过期
  - 主从复制
```

##### 组合 5: 文档数据库（MongoDB）✅
```yaml
配置:
  persistence: mongodb
  document-storage: mongodb (GridFS)
  rag: mongodb
  ai: ollama

测试场景:
  - GridFS 大文件存储
  - 文档灵活查询
  - 向量搜索
  - 副本集
```

##### 组合 6: 生产级搜索（Elasticsearch）✅
```yaml
配置:
  persistence: elasticsearch
  document-storage: elasticsearch
  rag: elasticsearch
  ai: ollama

测试场景:
  - 全文检索
  - 向量搜索
  - 聚合统计
  - 分布式扩展
```

##### 组合 7: 云存储（S3 + ES）✅
```yaml
配置:
  persistence: elasticsearch
  document-storage: s3
  rag: elasticsearch
  ai: online-api (OpenAI)

测试场景:
  - 云端文件存储
  - 高可用性
  - 在线 AI 调用
  - 成本优化
```

##### 组合 8: 私有云（MinIO + MongoDB）✅
```yaml
配置:
  persistence: mongodb
  document-storage: minio
  rag: mongodb
  ai: ollama

测试场景:
  - 私有云部署
  - 数据自主可控
  - 对象存储
  - 安全性
```

##### 组合 9: 混合架构 1（Redis + MongoDB）✅
```yaml
配置:
  persistence: redis
  document-storage: mongodb
  rag: elasticsearch
  ai: online-api

测试场景:
  - 多技术栈整合
  - 热数据 Redis
  - 冷数据 MongoDB
  - 检索 ES
```

##### 组合 10: 混合架构 2（ES + S3）✅
```yaml
配置:
  persistence: elasticsearch
  document-storage: s3
  rag: redis
  ai: ollama

测试场景:
  - 大规模数据
  - 公有云 + 私有云
  - 成本效益平衡
  - 性能优化
```

---

### 3. 切换测试（Switching Tests）
**目标**: 验证 Starter 切换的便捷性

#### 3.1 Persistence 切换测试
```
测试流程:
1. 使用 Memory 启动 → 存储数据 → 验证
2. 切换到 H2 → 启动 → 存储数据 → 验证
3. 切换到 Redis → 启动 → 存储数据 → 验证
4. 切换到 Elasticsearch → 启动 → 存储数据 → 验证

验证点:
- 只修改 pom.xml 和配置文件
- 业务代码无需改动
- 功能完全一致
- 数据格式兼容
```

#### 3.2 Document Storage 切换测试
```
测试流程:
1. File → MongoDB → Redis → S3 → MinIO → Elasticsearch

验证点:
- 文件上传和下载
- 大文件处理
- 元数据管理
- 性能对比
```

#### 3.3 RAG 切换测试
```
测试流程:
1. Lucene → H2 → Redis → MongoDB → Elasticsearch

验证点:
- 文本搜索准确性
- 向量搜索相似度
- 混合检索效果
- 查询速度
```

#### 3.4 AI 切换测试
```
测试流程:
1. Ollama (本地) → Online API (OpenAI/Claude)

验证点:
- 文本生成质量
- 对话连贯性
- 流式响应
- Token 使用量
```

---

### 4. 性能测试（Performance Tests）
**目标**: 对比不同实现的性能

#### 4.1 吞吐量测试
```
测试指标:
- QPS (Queries Per Second)
- TPS (Transactions Per Second)
- 并发用户数
- 响应时间

测试场景:
- 写入密集型（持久化）
- 读取密集型（检索）
- 混合读写
```

#### 4.2 延迟测试
```
测试指标:
- P50 延迟
- P95 延迟
- P99 延迟
- 最大延迟

不同实现对比:
- Memory < Redis < H2 < MongoDB < Elasticsearch
```

#### 4.3 资源消耗测试
```
测试指标:
- CPU 使用率
- 内存占用
- 磁盘 I/O
- 网络带宽

成本分析:
- 本地部署成本
- 云服务成本
- 维护成本
```

---

## 🛠️ 测试工具和框架

### 测试框架
```xml
<!-- JUnit 5 -->
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <scope>test</scope>
</dependency>

<!-- Spring Boot Test -->
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-test</artifactId>
    <scope>test</scope>
</dependency>

<!-- Mockito -->
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <scope>test</scope>
</dependency>

<!-- Testcontainers (Docker 容器测试) -->
<dependency>
    <groupId>org.testcontainers</groupId>
    <artifactId>testcontainers</artifactId>
    <scope>test</scope>
</dependency>
```

### 性能测试工具
- **JMH** - Java Microbenchmark Harness（微基准测试）
- **Gatling** - 负载测试和性能测试
- **JMeter** - Apache 性能测试工具

### 测试容器
- **Testcontainers** - Docker 容器化测试环境
  - Redis Container
  - MongoDB Container
  - Elasticsearch Container
  - Ollama Container

---

## 📝 测试清单

### ✅ Phase 4.1 - 单元测试
- [ ] API 模块测试（4个模块）
- [ ] Core 模块测试（16个类）
- [ ] Starter 单独测试（22个 Starter）

### ✅ Phase 4.2 - 集成测试
- [ ] 组合 1: 开发环境（Memory + File + Lucene + Ollama）
- [ ] 组合 2: 单机应用（H2 + File + H2 + Ollama）
- [ ] 组合 3: 轻量级（SQLite + File + SQLite + Ollama）
- [ ] 组合 4: 高性能（Redis + Redis + Redis + Ollama）
- [ ] 组合 5: 文档库（MongoDB + MongoDB + MongoDB + Ollama）
- [ ] 组合 6: 搜索引擎（ES + ES + ES + Ollama）
- [ ] 组合 7: 云存储（ES + S3 + ES + OpenAI）
- [ ] 组合 8: 私有云（MongoDB + MinIO + MongoDB + Ollama）
- [ ] 组合 9: 混合 1（Redis + MongoDB + ES + OpenAI）
- [ ] 组合 10: 混合 2（ES + S3 + Redis + Ollama）

### ✅ Phase 4.3 - 切换测试
- [ ] Persistence 切换（6种实现）
- [ ] Document Storage 切换（6种实现）
- [ ] RAG 切换（6种实现）
- [ ] AI 切换（2种实现）

### ✅ Phase 4.4 - 性能测试
- [ ] 吞吐量测试（10个组合）
- [ ] 延迟测试（10个组合）
- [ ] 资源消耗测试（10个组合）
- [ ] 性能对比报告

---

## 📊 测试通过标准

### 功能测试标准
- ✅ 所有 API 方法正常工作
- ✅ 数据正确存储和检索
- ✅ 无数据丢失
- ✅ 错误处理正确

### 切换测试标准
- ✅ 只修改配置，无需改代码
- ✅ 功能完全一致
- ✅ 切换时间 < 5 分钟
- ✅ 零停机切换

### 性能测试标准
```
轻量级实现（Memory, File, SQLite）:
- QPS > 1000
- P95 延迟 < 100ms
- 内存 < 512MB

中等实现（H2, Redis）:
- QPS > 5000
- P95 延迟 < 50ms
- 内存 < 1GB

重量级实现（MongoDB, Elasticsearch）:
- QPS > 10000
- P95 延迟 < 30ms
- 支持水平扩展
```

---

## 🚀 测试执行计划

### Week 1: 单元测试
- Day 1-2: API 模块测试
- Day 3-4: Core 模块测试
- Day 5: Starter 测试

### Week 2: 集成测试
- Day 1-2: 组合 1-4 测试
- Day 3-4: 组合 5-8 测试
- Day 5: 组合 9-10 测试

### Week 3: 切换和性能测试
- Day 1-2: 切换测试
- Day 3-4: 性能测试
- Day 5: 测试报告

---

## 📄 输出文档

### 测试报告
1. **单元测试报告** - Unit_Test_Report.md
2. **集成测试报告** - Integration_Test_Report.md
3. **切换测试报告** - Switching_Test_Report.md
4. **性能测试报告** - Performance_Test_Report.md
5. **Phase 4 完成报告** - PHASE4_COMPLETE_REPORT.md

---

**计划版本**: v1.0  
**创建时间**: 2025-12-15  
**预计完成**: 2025-12-29 (2 周)  
**负责人**: OmniAgent Team

