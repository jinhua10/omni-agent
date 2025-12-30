# RAG 架构决策分析报告

> 日期：2025-12-27  
> 任务：对比知识网络重构方案与当前实现，做出最终架构决策

---

## 📊 现状分析

### 当前存在的两套 RAG API

#### 1. `top.yumbo.ai.omni.rag.RagService`（简化版）✅ 推荐

**包路径：** `omni-agent-rag-api/src/main/java/top/yumbo/ai/omni/rag/RagService.java`

**特点：**
```java
// 简洁、面向领域的接口
public interface RagService {
    List<Document> semanticSearch(String query, int maxResults);
    List<Document> vectorSearch(Vector vector, int maxResults);
    Vector embed(String text);
    void batchIndex(List<Document> documents);
    void delete(String id);
    String getDomainId();  // ⭐ 关键：支持多域
}
```

**优势：**
- ✅ 包路径规范（`top.yumbo.ai.omni.*`）
- ✅ 接口简洁（7个核心方法）
- ✅ **支持域ID概念**（`getDomainId()`）
- ✅ 与知识网络架构方案**完全契合**
- ✅ 文档模型完整（14个字段）

**劣势：**
- ⚠️ 当前只有 MockRagService 一个实现
- ⚠️ 缺少查询统计接口

#### 2. `top.yumbo.ai.rag.api.RAGService`（完整版）❌ 不推荐

**包路径：** `omni-agent-rag-api/src/main/java/top/yumbo/ai/rag/api/RAGService.java`

**特点：**
```java
// 功能丰富但复杂
public interface RAGService {
    String indexDocument(Document document);
    List<SearchResult> searchByText(String text, int topK);
    List<SearchResult> vectorSearch(float[] embedding, int topK);
    List<SearchResult> hybridSearch(...);
    List<SearchResult> semanticSearch(String text, int topK);
    boolean documentExists(String documentId);
    IndexStatistics getStatistics();
    // ... 20+ 方法
}
```

**优势：**
- ✅ 功能完整（20+ 方法）
- ✅ 已有多个实现（File, H2, SQLite, Redis, MongoDB, Elasticsearch）
- ✅ 支持混合检索
- ✅ 支持统计和健康检查

**劣势：**
- ❌ 包路径不规范（`top.yumbo.ai.rag.api`）
- ❌ **不支持域ID概念**（无法实现多域架构）
- ❌ 接口过于复杂
- ❌ 与知识网络架构方案**不匹配**

---

## 🎯 知识网络重构方案的核心要求

根据 `KNOWLEDGE_NETWORK_REFACTORING_PLAN.md`：

### 1. 多知识域架构

```
知识网络
├── Domain 1 (文档知识域)    ← 独立的 RAG 索引
├── Domain 2 (源码知识域)    ← 独立的 RAG 索引
└── Domain 3 (角色知识域)    ← 独立的 RAG 索引
```

**要求：**
- ✅ 每个域有独立的 RAG 服务实例
- ✅ RAG 服务需要知道自己属于哪个域
- ✅ 支持跨域查询

### 2. RAG 服务工厂

```java
@Component
public class RAGServiceFactory {
    private final Map<String, RAGService> domainRAGServices = new ConcurrentHashMap<>();
    
    public RAGService getOrCreateRAGService(String domainId, DomainConfig config) {
        return domainRAGServices.computeIfAbsent(domainId, id -> {
            return createRAGService(id, config);
        });
    }
}
```

**要求：**
- ✅ 为每个域创建独立的 RAG 实例
- ✅ RAG 实例需要携带域ID信息

### 3. 知识域服务

```java
public interface KnowledgeDomainService {
    KnowledgeDomain createDomain(CreateDomainRequest request);
    RAGService getDomainRAGService(String domainId);  // ⭐ 核心
    List<SearchResult> crossDomainSearch(String query, List<String> domainIds, int topK);
}
```

---

## 🔍 对比分析表

| 维度 | RagService（简化版） | RAGService（完整版） | 重构方案要求 |
|------|---------------------|---------------------|-------------|
| **包路径规范** | ✅ `top.yumbo.ai.omni.*` | ❌ `top.yumbo.ai.rag.api` | ✅ 规范 |
| **域ID支持** | ✅ `getDomainId()` | ❌ 无 | ✅ 必须 |
| **多域架构** | ✅ 完全支持 | ❌ 不支持 | ✅ 核心需求 |
| **接口复杂度** | ✅ 简洁（7方法） | ⚠️ 复杂（20+方法） | ✅ 简洁优先 |
| **实现数量** | ⚠️ 1个（Mock） | ✅ 6个 | - |
| **文档模型** | ✅ Document（14字段） | ⚠️ Document（旧包路径） | ✅ 完整 |
| **向量化集成** | ✅ `embed()` 方法 | ❌ 无 | ✅ 需要 |
| **批量索引** | ✅ `batchIndex()` | ✅ `indexDocuments()` | ✅ 需要 |
| **统计接口** | ❌ 无 | ✅ `getStatistics()` | ⚠️ 可选 |

---

## 💡 最终决策

### ⭐ 推荐方案：采用 `RagService`（简化版）+ 扩展

**理由：**

1. **完美契合知识网络架构**
   - 支持域ID概念（`getDomainId()`）
   - 可以轻松实现 RAGServiceFactory
   - 符合多域隔离的设计

2. **包路径规范**
   - 遵循 `top.yumbo.ai.omni.*` 规范
   - 便于后续维护

3. **接口简洁清晰**
   - 7个核心方法，职责明确
   - 易于实现和扩展

4. **文档模型完整**
   - 已统一到 `top.yumbo.ai.omni.rag.model.Document`
   - 14个字段，功能完整

### 🔧 实施策略

#### 阶段1：扩展 RagService 接口（立即执行）

```java
package top.yumbo.ai.omni.rag;

import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;
import top.yumbo.ai.omni.rag.model.IndexStatistics;

import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * RAG 服务接口（支持多域架构）
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
public interface RagService {

    // ========== 核心检索 ==========
    
    List<Document> semanticSearch(String query, int maxResults);
    
    List<Document> vectorSearch(Vector vector, int maxResults);
    
    // ========== 向量化 ==========
    
    Vector embed(String text);
    
    List<Vector> batchEmbed(List<String> texts);
    
    // ========== 文档索引 ==========
    
    void index(String id, Vector vector, Map<String, Object> metadata);
    
    void batchIndex(List<Document> documents);
    
    void delete(String id);
    
    // ========== 域管理（新增）==========
    
    /**
     * 获取域ID
     */
    String getDomainId();
    
    // ========== 文档管理（新增）==========
    
    /**
     * 获取文档
     */
    Optional<Document> getDocument(String documentId);
    
    /**
     * 检查文档是否存在
     */
    boolean documentExists(String documentId);
    
    /**
     * 获取文档总数
     */
    long getDocumentCount();
    
    // ========== 统计与健康（新增）==========
    
    /**
     * 获取索引统计信息
     */
    IndexStatistics getStatistics();
    
    /**
     * 健康检查
     */
    boolean isHealthy();
}
```

#### 阶段2：迁移现有实现（1-2周）

**迁移路径：**
```
旧实现 → 新实现
top.yumbo.ai.rag.file.LuceneRAGService 
  → top.yumbo.ai.omni.rag.impl.FileRagService

top.yumbo.ai.rag.mongodb.MongoDBRAGService
  → top.yumbo.ai.omni.rag.impl.MongoDBRagService

... 依此类推
```

**适配器模式（过渡期）：**
```java
@Component
public class RAGServiceAdapter implements RagService {
    
    @Autowired(required = false)
    private top.yumbo.ai.rag.api.RAGService oldRAGService;
    
    private final String domainId;
    
    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        // 调用旧接口，转换返回值
        List<SearchResult> results = oldRAGService.semanticSearch(query, maxResults);
        return convertToDocuments(results);
    }
    
    @Override
    public String getDomainId() {
        return this.domainId;
    }
    
    // ... 其他适配方法
}
```

#### 阶段3：实现 RAGServiceFactory（1周）

```java
package top.yumbo.ai.omni.core.rag;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.impl.*;
import top.yumbo.ai.omni.knowledge.registry.model.domain.KnowledgeDomain;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Slf4j
@Component
public class RAGServiceFactory {
    
    private final Map<String, RagService> domainRAGServices = new ConcurrentHashMap<>();
    
    /**
     * 获取或创建域的 RAG 服务
     */
    public RagService getOrCreateRAGService(String domainId, DomainConfig config) {
        return domainRAGServices.computeIfAbsent(domainId, id -> {
            log.info("为域 {} 创建 RAG 服务，后端：{}", domainId, config.getBackendType());
            return createRAGService(id, config);
        });
    }
    
    private RagService createRAGService(String domainId, DomainConfig config) {
        String indexPath = config.getRagIndexPath();
        
        return switch (config.getBackendType()) {
            case LUCENE -> new FileRagService(domainId, indexPath);
            case MONGODB -> new MongoDBRagService(domainId, config.getMongoConfig());
            case ELASTICSEARCH -> new ElasticsearchRagService(domainId, config.getEsConfig());
            case REDIS -> new RedisRagService(domainId, config.getRedisConfig());
            case H2 -> new H2RagService(domainId, config.getH2Config());
            case SQLITE -> new SQLiteRagService(domainId, config.getSqliteConfig());
            default -> throw new IllegalArgumentException("Unsupported backend: " + config.getBackendType());
        };
    }
    
    /**
     * 移除域的 RAG 服务
     */
    public void removeDomainRAGService(String domainId) {
        RagService removed = domainRAGServices.remove(domainId);
        if (removed != null) {
            log.info("已移除域 {} 的 RAG 服务", domainId);
        }
    }
    
    /**
     * 获取所有域的 RAG 服务数量
     */
    public int getDomainCount() {
        return domainRAGServices.size();
    }
}
```

#### 阶段4：更新 KnowledgeStorageService（已完成）

```java
@Service
public class KnowledgeStorageService {
    
    @Autowired(required = false)
    private RAGServiceFactory ragServiceFactory;  // ✅ 已实现
    
    private void indexToRAG(RefinedKnowledge knowledge, KnowledgeDomain domain) {
        // 1. 获取域的 RAG 服务
        RagService ragService = ragServiceFactory.getOrCreateRAGService(
            domain.getDomainId()  // ✅ 使用域ID
        );
        
        // 2. 转换并索引
        Document doc = convertToDocument(knowledge);
        ragService.batchIndex(List.of(doc));
    }
}
```

---

## 📋 迁移清单

### 立即执行（本周）

- [ ] **扩展 RagService 接口**
  - [ ] 添加 `getDocument()`, `documentExists()`, `getDocumentCount()`
  - [ ] 添加 `getStatistics()`, `isHealthy()`
  - [ ] 创建 `IndexStatistics` 模型

- [ ] **创建适配器**
  - [ ] `RAGServiceAdapter` - 适配旧接口到新接口
  - [ ] 保持向后兼容

- [ ] **更新文档**
  - [ ] 标记旧接口为 `@Deprecated`
  - [ ] 编写迁移指南

### 短期（1-2周）

- [ ] **迁移实现类**
  - [ ] FileRagService（Lucene）
  - [ ] MongoDBRagService
  - [ ] RedisRagService
  - [ ] H2RagService
  - [ ] SQLiteRagService
  - [ ] ElasticsearchRagService

- [ ] **完善 RAGServiceFactory**
  - [ ] 支持所有后端类型
  - [ ] 添加配置验证
  - [ ] 添加健康检查

### 中期（3-4周）

- [ ] **删除旧接口**
  - [ ] 删除 `top.yumbo.ai.rag.api.RAGService`
  - [ ] 删除旧的实现类
  - [ ] 清理依赖

- [ ] **实现知识域服务**
  - [ ] `KnowledgeDomainService`
  - [ ] `DomainRouter`（领域路由器）
  - [ ] 跨域查询功能

---

## 🎯 架构对齐检查

### ✅ 与重构方案的契合度

| 重构方案要求 | RagService | RAGService | 决策 |
|-------------|-----------|-----------|------|
| 多域隔离 | ✅ `getDomainId()` | ❌ 不支持 | ✅ RagService |
| RAG工厂 | ✅ 完美契合 | ❌ 需要大改 | ✅ RagService |
| 知识域服务 | ✅ 可直接使用 | ❌ 需要适配 | ✅ RagService |
| 角色知识库 | ✅ 支持 | ❌ 不支持 | ✅ RagService |
| 源码分析 | ✅ 支持 | ❌ 不支持 | ✅ RagService |
| 领域路由 | ✅ 支持 | ❌ 不支持 | ✅ RagService |

### ✅ 架构图对齐

```
知识网络管理器
    ↓
RAGServiceFactory
    ├─→ Domain 1 → RagService(domainId="domain-1") ✅
    ├─→ Domain 2 → RagService(domainId="domain-2") ✅
    └─→ Domain 3 → RagService(domainId="domain-3") ✅
```

**完美契合！** ✅

---

## 📊 风险评估

### 低风险 ✅

- RagService 接口设计合理
- 已有统一的 Document 模型
- 已有 RAGServiceFactory 基础实现
- MockRagService 可作为参考

### 中等风险 ⚠️

- 需要迁移 6 个 RAG 实现
- 可能影响现有的 Web 模块
- 需要编写适配器保持兼容

### 缓解措施

1. **渐进式迁移**
   - 先实现适配器
   - 逐步迁移实现类
   - 保持双接口并存

2. **充分测试**
   - 单元测试
   - 集成测试
   - 端到端测试

3. **详细文档**
   - 迁移指南
   - API 文档
   - 示例代码

---

## 🎯 最终决策声明

**决定：采用 `top.yumbo.ai.omni.rag.RagService` 作为唯一的 RAG 服务接口**

**理由：**
1. ✅ 完美支持知识网络多域架构
2. ✅ 包路径规范
3. ✅ 接口设计简洁
4. ✅ 已有统一的 Document 模型
5. ✅ 可扩展性强

**淘汰：`top.yumbo.ai.rag.api.RAGService`**

**理由：**
1. ❌ 不支持域ID概念
2. ❌ 包路径不规范
3. ❌ 无法实现知识网络架构
4. ❌ 接口过于复杂

**过渡期：** 使用适配器保持兼容（1-2个月）

**完成时间：** 预计 4-6 周完成完整迁移

---

**创建时间：** 2025-12-27  
**状态：** 🟢 决策已确定  
**下一步：** 开始执行迁移计划

