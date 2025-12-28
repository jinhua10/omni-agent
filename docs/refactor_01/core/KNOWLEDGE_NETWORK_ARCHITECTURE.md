# 知识网络架构设计文档

## 📋 概述

**知识网络**是 Omni-Agent 系统的增强层，独立于原有的文档处理流程，基于已提取的文本在后台构建知识图谱和关联网络。

## 🏗️ 架构定位

```
┌─────────────────────────────────────────────────────────────────┐
│                    Omni-Agent 系统架构                             │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │          原有架构（文档处理流程）                            │  │
│  ├───────────────────────────────────────────────────────────┤  │
│  │  1. 文档上传                                                │  │
│  │  2. 文本提取 (extracted text)                               │  │
│  │  3. PPL 分块                                                │  │
│  │  4. 向量化                                                  │  │
│  │  5. RAG 索引                                                │  │
│  └───────────────────────────────────────────────────────────┘  │
│                          ↓                                       │
│                   (extracted text)                               │
│                          ↓                                       │
│  ┌───────────────────────────────────────────────────────────┐  │
│  │        知识网络（增强层 - 独立后台服务）                     │  │
│  ├───────────────────────────────────────────────────────────┤  │
│  │  1. 监听已提取文本                                          │  │
│  │  2. 调用 AI 服务提取知识点                                  │  │
│  │  3. 构建知识图谱                                            │  │
│  │  4. 建立跨域关联                                            │  │
│  │  5. 持久化到知识域                                          │  │
│  └───────────────────────────────────────────────────────────┘  │
│                                                                   │
└─────────────────────────────────────────────────────────────────┘
```

## 🎯 设计原则

### 1. 非侵入性
- ✅ **不修改原有文档处理流程**
- ✅ **独立后台运行，异步执行**
- ✅ **原有功能不受影响**

### 2. 模块化
- ✅ **API 定义在 `omni-agent-knowledge-registry-api`**
- ✅ **实现在 `omni-agent-core` 或专门模块**
- ✅ **依赖已有的知识域架构**

### 3. 可选性
- ✅ **可以启用/禁用知识网络构建**
- ✅ **不影响基础 RAG 功能**
- ✅ **用户可按需使用**

## 📦 模块结构

### API 层 (`omni-agent-knowledge-registry-api`)

定义知识网络的接口和模型：

```
omni-agent-knowledge-registry-api/
└── src/main/java/top/yumbo/ai/omni/knowledge/registry/
    ├── KnowledgeRegistry.java              # 已有：知识注册表
    ├── model/
    │   ├── KnowledgeDomain.java            # 已有：知识域模型
    │   ├── DomainType.java                 # 已有：域类型
    │   └── DomainStatus.java               # 已有：域状态
    └── network/                             # ⭐ 新增：知识网络API
        ├── KnowledgeNetworkService.java    # 知识网络服务接口
        ├── KnowledgeBuildResult.java       # 构建结果模型
        ├── KnowledgeBuildStatus.java       # 构建状态枚举
        └── KnowledgeNetworkStatistics.java # 统计信息模型
```

### 实现层 (`omni-agent-core`)

具体实现知识网络构建逻辑：

```
omni-agent-core/
└── src/main/java/top/yumbo/ai/omni/core/knowledge/network/
    ├── KnowledgeNetworkManager.java     # 知识网络管理器（实现接口）
    └── KnowledgeNetworkBuilder.java     # 知识网络构建器（内部实现）
```

## 🔄 工作流程

### 1. 初始化阶段

```java
@EventListener(ApplicationReadyEvent.class)
public void onApplicationReady() {
    // 1. 扫描 data/storage/extracted 目录
    // 2. 发现已提取的文本文件
    // 3. 加入待处理队列
}
```

### 2. 后台处理

```java
@Scheduled(fixedDelay = 300000)  // 每5分钟
public void periodicCheck() {
    // 1. 检查新增的提取文本
    // 2. 触发知识网络构建
}
```

### 3. 知识构建

```java
public CompletableFuture<KnowledgeBuildResult> buildKnowledgeNetworkAsync(
        String documentId, 
        String domainId) {
    
    // 1. 读取 extracted text
    String text = documentStorage.getExtractedText(documentId);
    
    // 2. 调用 AI 服务提取知识
    List<Knowledge> knowledge = aiService.extractKnowledge(text);
    
    // 3. 存储到知识域
    knowledgeRegistry.saveBatch(knowledge, domainId);
    
    // 4. 返回构建结果
    return CompletableFuture.completedFuture(result);
}
```

## 🔌 接口定义

### KnowledgeNetworkService

```java
public interface KnowledgeNetworkService {
    
    // 异步构建知识网络
    CompletableFuture<KnowledgeBuildResult> buildKnowledgeNetworkAsync(
        String documentId, 
        String domainId
    );
    
    // 批量构建
    List<CompletableFuture<KnowledgeBuildResult>> batchBuildKnowledgeNetwork(
        List<String> documentIds, 
        String domainId
    );
    
    // 扫描并构建
    void scanAndBuildKnowledgeNetwork();
    
    // 手动触发
    CompletableFuture<KnowledgeBuildResult> triggerBuild(
        String documentId, 
        String domainId
    );
    
    // 查询状态
    KnowledgeBuildStatus getBuildStatus(String documentId);
    
    // 统计信息
    KnowledgeNetworkStatistics getStatistics();
    
    // 启用/禁用
    void setEnabled(boolean enabled);
}
```

## 💡 使用示例

### 1. 自动后台构建（默认）

```yaml
# application.yml
omni-agent:
  knowledge-network:
    enabled: true          # 启用知识网络
    auto-scan: true        # 自动扫描
    scan-interval: 300000  # 扫描间隔（毫秒）
```

### 2. 手动触发构建

```java
@Autowired
private KnowledgeNetworkService knowledgeNetworkService;

// 为单个文档构建知识网络
CompletableFuture<KnowledgeBuildResult> future = 
    knowledgeNetworkService.triggerBuild(documentId, domainId);

KnowledgeBuildResult result = future.get();
if (result.isSuccess()) {
    System.out.println("提取了 " + result.getKnowledgeCount() + " 条知识");
}
```

### 3. 查询状态

```java
// 查询构建状态
KnowledgeBuildStatus status = 
    knowledgeNetworkService.getBuildStatus(documentId);

// 获取统计信息
KnowledgeNetworkStatistics stats = 
    knowledgeNetworkService.getStatistics();

System.out.println("已处理: " + stats.getProcessedDocuments());
System.out.println("待处理: " + stats.getPendingTasks());
```

## 🎨 与原架构的关系

### 原有架构（不变）

| 阶段 | 模块 | 功能 |
|------|------|------|
| 1. 文档存储 | document-storage-api | 存储原始文档 |
| 2. 文本提取 | core | 提取文本内容 |
| 3. PPL 分块 | ppl-onnx | 智能分块 |
| 4. 向量化 | ai-starter-* | 文本向量化 |
| 5. RAG 索引 | rag-starter-* | 构建索引 |

### 知识网络（增强）

| 阶段 | 模块 | 功能 |
|------|------|------|
| 1. 监听文本 | knowledge-network | 监听 extracted text |
| 2. AI 分析 | knowledge-network | 调用 AI 提取知识 |
| 3. 知识存储 | knowledge-registry | 存储到知识域 |
| 4. 关联构建 | knowledge-network | 建立跨域关联 |

## ⚙️ 配置选项

```yaml
omni-agent:
  knowledge-network:
    # 是否启用知识网络
    enabled: true
    
    # 自动扫描设置
    auto-scan: true
    scan-interval: 300000  # 5分钟
    
    # AI 服务配置
    ai-service: "online-api"  # 或 ollama, onnx
    
    # 默认知识域
    default-domain-id: "default-docs-domain"
    
    # 批处理大小
    batch-size: 10
    
    # 异步线程池
    thread-pool-size: 5
```

## 🚀 实施计划

### Phase 1: API 定义（已完成）✅
- ✅ KnowledgeNetworkService 接口
- ✅ KnowledgeBuildResult 模型
- ✅ KnowledgeBuildStatus 枚举
- ✅ KnowledgeNetworkStatistics 模型

### Phase 2: 核心实现（进行中）
- ⏳ KnowledgeNetworkManager 实现
- ⏳ KnowledgeNetworkBuilder 实现
- ⏳ 修复依赖和编译错误

### Phase 3: 功能增强（TODO）
- ⬜ 知识图谱构建
- ⬜ 跨域关联分析
- ⬜ 知识推理引擎

### Phase 4: UI 集成（TODO）
- ⬜ 知识网络可视化
- ⬜ 构建进度显示
- ⬜ 手动触发按钮

## 📝 注意事项

1. **不替代原有RAG**
   - 知识网络是增强层，不是替代品
   - 原有 RAG 查询功能保持不变

2. **可选功能**
   - 用户可以选择不启用知识网络
   - 不启用时，系统功能完全正常

3. **资源消耗**
   - 知识网络构建需要调用 AI 服务
   - 建议配置资源限制和调度策略

4. **存储方式**
   - 支持多种存储后端（与现有一致）
   - file, mongodb, redis, elasticsearch等

## 🔗 相关文档

- [知识网络重构方案](KNOWLEDGE_NETWORK_REFACTORING_PLAN.md)
- [知识域架构设计](KNOWLEDGE_DOMAIN_ARCHITECTURE.md)
- [Phase 1 实施计划](refactor_01/PHASE1_FINAL_IMPLEMENTATION_PLAN.md)

---

**创建时间：** 2025-12-28  
**更新时间：** 2025-12-28  
**版本：** 1.0.0

