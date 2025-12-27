# Omni-Agent 知识网络架构 - 实施状态文档

> **文档创建时间：** 2025-12-27  
> **最后更新时间：** 2025-12-27  
> **状态：** Phase 1 & 2 已完成  
> **作者：** 系统架构设计

---

## 📊 当前实施状态

### ✅ 已完成的核心功能

| 模块 | 状态 | 说明 |
|------|------|------|
| **RAG 架构统一** | ✅ 100% | 统一到 `top.yumbo.ai.omni.rag.*` |
| **Document 模型** | ✅ 100% | 14个字段，完整的元数据支持 |
| **知识域模型** | ✅ 100% | `KnowledgeDomain` 实体完成 |
| **角色模型** | ✅ 100% | `KnowledgeRole` 实体完成 |
| **知识注册表** | ✅ 100% | 7种存储实现 |
| **RAG 服务** | ✅ 100% | FileRagService 基于 Lucene |
| **AI Embedding** | ✅ 100% | ONNX + Ollama + Online API |
| **模型动态管理** | ✅ 100% | EmbeddingModelRegistry |
| **RAG 重建** | ✅ 100% | 支持切换模型和重新分块 |
| **角色服务** | ✅ 100% | 完整的角色生命周期管理 |
| **角色学习** | ✅ 100% | 知识提取、提炼、存储 |
| **领域路由** | ✅ 100% | DomainRouter 智能路由 |

**总代码量：** ~5,800 行

---

## 🏗️ 核心架构

### 1. 知识域 (Knowledge Domain)

**实际实现位置：**
```
omni-agent-knowledge-registry-api/
└── src/main/java/top/yumbo/ai/omni/knowledge/registry/model/
    ├── KnowledgeDomain.java          # 知识域实体
    ├── DomainType.java               # 域类型枚举
    └── DomainStatus.java             # 域状态枚举
```

**数据模型：**
```java
@Data
@Builder
public class KnowledgeDomain implements Serializable {
    private String domainId;              // 域ID
    private String domainName;            // 域名称
    private DomainType domainType;        // 域类型
    private String description;           // 描述
    private String storagePath;           // 存储路径
    private String ragIndexPath;          // RAG索引路径
    private Map<String, Object> config;   // 配置信息
    private DomainStatus status;          // 状态
    private String linkedEntityId;        // 关联实体ID
    private LocalDateTime createdAt;      // 创建时间
    private LocalDateTime updatedAt;      // 更新时间
}

public enum DomainType {
    DOCUMENT,           // 文档知识域
    SOURCE_CODE,        // 源码知识域（预留）
    ROLE_KNOWLEDGE,     // 角色知识域
    API_DOCUMENTATION,  // API文档域（预留）
    MIXED               // 混合域
}

public enum DomainStatus {
    ACTIVE,      // 活跃
    INACTIVE,    // 非活跃
    ARCHIVED,    // 已归档
    ERROR        // 错误状态
}
```

---

### 2. 知识角色 (Knowledge Role)

**实际实现位置：**
```
omni-agent-knowledge-registry-api/
└── src/main/java/top/yumbo/ai/omni/knowledge/registry/model/
    ├── KnowledgeRole.java            # 角色实体
    └── RoleStatus.java               # 角色状态枚举
```

**数据模型：**
```java
@Data
@Builder
public class KnowledgeRole implements Serializable {
    private String roleId;                   // 角色ID
    private String roleName;                 // 角色名称
    private String description;              // 角色描述
    private String responsibilities;         // 角色职责
    private String knowledgeDomainId;        // 关联的知识域ID
    private List<String> sourceDomainIds;    // 学习源域列表
    private RoleStatus status;               // 角色状态
    private Integer learningProgress;        // 学习进度(0-100)
    private Map<String, Object> config;      // 配置信息
    private LocalDateTime createdAt;         // 创建时间
    private LocalDateTime updatedAt;         // 更新时间
    private LocalDateTime lastLearnedAt;     // 最后学习时间
}

public enum RoleStatus {
    ACTIVE,      // 活跃
    INACTIVE,    // 非活跃
    LEARNING,    // 学习中
    ERROR        // 错误
}
```

---

### 3. 知识注册表 (Knowledge Registry)

**接口定义：**
```
omni-agent-knowledge-registry-api/
└── src/main/java/top/yumbo/ai/omni/knowledge/registry/
    └── KnowledgeRegistry.java        # 核心接口
```

**核心方法：**
```java
public interface KnowledgeRegistry {
    // 域管理
    void saveDomain(KnowledgeDomain domain);
    KnowledgeDomain getDomain(String domainId);
    List<KnowledgeDomain> listDomains(DomainType type);
    void deleteDomain(String domainId);
    
    // 角色管理
    void saveRole(KnowledgeRole role);
    KnowledgeRole getRole(String roleId);
    List<KnowledgeRole> listRoles(RoleStatus status);
    void deleteRole(String roleId);
}
```

**已实现的存储后端：**
```
1. FileKnowledgeRegistry          # 文件存储（默认）
2. MemoryKnowledgeRegistry         # 内存存储
3. H2KnowledgeRegistry             # H2数据库
4. SQLiteKnowledgeRegistry         # SQLite
5. MongoDBKnowledgeRegistry        # MongoDB
6. RedisKnowledgeRegistry          # Redis
7. ElasticsearchKnowledgeRegistry  # Elasticsearch
```

---

### 4. RAG 服务架构

**核心接口：**
```java
// omni-agent-rag-api/src/main/java/top/yumbo/ai/omni/rag/
public interface RagService {
    // 索引操作
    void index(Document document);
    void batchIndex(List<Document> documents);
    void clearAll();
    
    // 检索操作
    List<Document> semanticSearch(String query, int maxResults);
    List<Document> vectorSearch(Vector vector, int maxResults);
    List<Document> textSearch(String query, int maxResults);
    
    // 向量化
    Vector embed(String text);
    List<Vector> batchEmbed(List<String> texts);
}
```

**已实现：**
```
omni-agent-rag-starter-file/
└── src/main/java/top/yumbo/ai/omni/rag/file/
    └── FileRagService.java           # 基于 Lucene 的实现
```

---

### 5. AI Embedding 集成

**支持的后端：**

#### ONNX（本地模型）
```yaml
omni-agent:
  ai:
    onnx:
      model-path: ./models/bge-base-zh-v1.5/model.onnx
      dimension: 768
```

#### Ollama（本地API）
```yaml
omni-agent:
  ai:
    ollama:
      base-url: http://localhost:11434
      default-model: qwen2.5:7b
      embedding-model: nomic-embed-text  # 768维
```

#### Online API（云端）
```yaml
omni-agent:
  ai:
    online:
      provider: openai
      api-key: ${OPENAI_API_KEY}
      embedding-model: text-embedding-3-small  # 1536维
```

**动态模型管理：**
```java
// 支持运行时切换模型，自动检测维度
EmbeddingModelRegistry.register("custom-model", 1024, "provider", "desc");
int dimension = embeddingService.getDimension(); // 自动获取
```

---

### 6. 角色学习框架

**核心服务：**
```
omni-agent-core/src/main/java/top/yumbo/ai/omni/core/role/
├── KnowledgeRoleService.java         # 角色管理服务
├── RoleLearningService.java          # 角色学习服务
├── KnowledgeExtractionService.java   # 知识提取
├── KnowledgeRefinementService.java   # 知识提炼
└── KnowledgeStorageService.java      # 知识存储
```

**学习流程：**
```
1. 从源域检索文档
   ↓
2. 知识提取（基于角色职责筛选）
   ↓
3. 知识提炼（使用AI总结精炼）
   ↓
4. 存储到角色专属知识域
   ↓
5. 更新学习进度
```

---

### 7. 领域路由器

**实现位置：**
```
omni-agent-core/src/main/java/top/yumbo/ai/omni/core/role/
└── DomainRouter.java                 # 领域路由服务
```

**核心功能：**
```java
public class DomainRouter {
    /**
     * 智能路由查询到最合适的域
     */
    public QueryRouteResult route(String question) {
        // 1. 分析问题意图
        // 2. 匹配领域关键词
        // 3. 计算置信度
        // 4. 返回路由结果
    }
}
```

---

## 📂 数据组织结构

### 当前使用的结构

```
data/
├── rag/
│   └── lucene/                       # FileRagService 的 Lucene 索引
│       └── default/                  # 默认域
│
├── storage/                          # DocumentStorageService
│   ├── documents/                    # 原始文档
│   ├── chunks/                       # 分块数据
│   └── extracted/                    # 提取的文本
│
└── knowledge-network/                # 知识网络数据（预留）
    ├── domains.json                  # 域列表（File实现）
    └── roles.json                    # 角色列表（File实现）
```

**注意：** 当前使用的文件结构已经可以正常工作，域的隔离通过 `domainId` 参数实现。

---

## 🔄 核心工作流程

### 1. 创建角色并学习知识

```java
// 1. 创建角色
CreateRoleRequest request = CreateRoleRequest.builder()
    .roleName("安全分析师")
    .responsibilities("分析代码安全漏洞")
    .build();
KnowledgeRole role = roleService.createRole(request);

// 2. 角色从源域学习
LearnFromDomainsRequest learnRequest = LearnFromDomainsRequest.builder()
    .sourceDomainIds(List.of("source-code-domain", "security-docs-domain"))
    .build();
roleService.learnFromDomains(role.getRoleId(), learnRequest);

// 3. 查询角色知识库
List<Document> results = ragServiceFactory
    .getOrCreateRAGService(role.getKnowledgeDomainId())
    .semanticSearch("SQL注入漏洞", 10);
```

### 2. 智能路由查询

```java
// 用户提问
String question = "这段Java代码有安全漏洞吗？";

// 路由到最合适的域
QueryRouteResult route = domainRouter.route(question);

// 使用路由结果查询
RagService ragService = ragServiceFactory.getOrCreateRAGService(
    route.getRecommendedDomainId()
);
List<Document> results = ragService.semanticSearch(question, 10);
```

### 3. 切换 Embedding 模型并重建索引

```java
// 场景：从本地模型升级到云端模型

// 步骤1：修改配置
// application.yml 中切换 embedding-model

// 步骤2：重建索引
RAGRebuildService rebuildService = ...;
RebuildResult result = rebuildService.rebuildFromStorage(
    "domain-id", 
    RebuildStrategy.USE_EXISTING_CHUNKS  // 只重新向量化
);

// 或者重新分块+向量化
RebuildResult result = rebuildService.rebuildFromStorage(
    "domain-id",
    RebuildStrategy.RECHUNK  // 重新分块并向量化
);
```

---

## 🎯 下一步计划

### 短期（可选）

1. **完善 Web UI**
   - 角色管理界面
   - 学习进度监控
   - 域管理界面

2. **性能优化**
   - 缓存机制
   - 批量处理优化
   - 并发控制

3. **监控和日志**
   - 学习进度追踪
   - 查询性能监控
   - 错误告警

### 中期（按需）

4. **源码分析功能**（如果需要）
   - Git 集成
   - 增量更新
   - 代码分析

5. **知识图谱**（如果需要）
   - 实体识别
   - 关系抽取
   - 图谱查询

6. **协作功能**（如果需要）
   - P2P 知识共享
   - 多用户协作
   - 权限管理

---

## 📚 参考文档

**已完成的详细文档：**
- [Phase 1 完成报告](../PHASE1_COMPLETE_REPORT.md)
- [Phase 2 完成报告](../PHASE2_FINAL_SUMMARY.md)
- [AI 模块优化报告](../AI_MODULE_OPTIMIZATION_COMPLETE.md)
- [Embedding 模型动态管理](../EMBEDDING_MODEL_DYNAMIC_MANAGEMENT.md)
- [RAG 重建能力](../RAG_REBUILD_CAPABILITY.md)
- [Document 模型统一](../DOCUMENT_MODEL_UNIFICATION.md)

**快速开始：**
- [API 使用示例](../API_USAGE_EXAMPLES.md)
- [配置指南](../README_RAG_REFACTOR.md)

---

## ✅ 总结

**当前状态：**
- ✅ 核心架构已完整实现
- ✅ 所有基础功能已可用
- ✅ 代码质量良好，文档完整

**可以直接使用的功能：**
1. 多域知识库管理
2. 角色创建和学习
3. 智能领域路由
4. AI Embedding（3种后端）
5. RAG 重建（切换模型/重新分块）
6. 7种存储后端

**未实现的功能（按需开发）：**
- 源码分析（如果不需要可以不做）
- 知识图谱（如果不需要可以不做）
- P2P 协作（如果不需要可以不做）

**建议：** 先在实际应用中验证现有功能，根据反馈决定是否需要开发未完成的功能。

---

**文档版本：** 2.0  
**状态：** 反映实际实现  
**最后更新：** 2025-12-27

