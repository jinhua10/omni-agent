# Omni-Agent Core 模块重构计划

**基于架构定位：** Core 核心层只保留**业务编排 + 领域服务协调**

**重构策略：** 不删除代码，归档到 `old` 包供 review

---

## 📋 当前 Core 模块结构分析

```
top.yumbo.ai.omni.core/
├── benchmark/          # 性能基准测试
├── chunking/           # 文档分块服务 ❌ (具体实现，应移到专门模块)
├── config/             # 配置类 ✅ (保留)
├── document/           # 文档处理器 ❌ (具体实现)
├── dto/                # 数据传输对象 ✅ (保留)
├── feedback/           # 反馈服务 ❌ (具体实现)
├── hope/               # HOPE 问答系统 ✅ (保留 - 业务编排)
├── image/              # 图像存储服务 ❌ (具体实现)
├── optimization/       # RAG 优化服务 ❌ (具体实现)
├── p2p/                # P2P 协作管理 ❌ (具体实现)
├── qa/                 # 问答服务 ✅ (保留 - 业务编排)
├── query/              # 查询服务 ❌ (部分具体实现)
├── router/             # 智能路由 ✅ (保留 - 领域服务协调)
├── service/            # 各种服务 ⚠️ (需细分)
├── util/               # 工具类 ❌ (应移到 common)
└── voting/             # 投票服务 ❌ (具体实现)
```

---

## 🎯 重构后的 Core 结构（目标）

### ✅ 保留（业务编排 + 领域服务协调）

```
top.yumbo.ai.omni.core/
├── orchestration/                  # 业务编排层 ⭐ 核心
│   ├── document/
│   │   └── DocumentProcessingOrchestrator.java    # 文档处理编排器
│   ├── knowledge/
│   │   └── KnowledgeEnhancementOrchestrator.java  # 知识增强编排器
│   └── qa/
│       └── IntelligentQAOrchestrator.java         # 智能问答编排器
│
├── coordinator/                    # 领域服务协调器 ⭐ 核心
│   ├── DomainCoordinator.java                     # 领域协调器
│   ├── ServiceCoordinator.java                    # 服务协调器
│   └── WorkflowCoordinator.java                   # 工作流协调器
│
├── router/                         # 智能路由 ✅ 保留
│   ├── DomainRouter.java                          # 领域路由器
│   ├── IntentAnalyzer.java                        # 意图分析器
│   └── QueryRouter.java                           # 查询路由器
│
├── service/                        # 领域服务接口
│   ├── domain/
│   │   └── KnowledgeDomainService.java            # 知识域管理服务
│   ├── role/
│   │   ├── RoleLearningService.java               # 角色学习服务
│   │   └── RoleManagementService.java             # 角色管理服务
│   └── query/
│       └── CrossDomainQueryService.java           # 跨域查询服务（编排）
│
├── hope/                           # HOPE 问答系统 ✅ 保留
│   ├── HOPEKnowledgeManager.java                  # HOPE 知识管理器
│   ├── QuestionClassifier.java                    # 问题分类器
│   └── layer/                                     # 三层架构
│
├── dto/                            # 数据传输对象 ✅ 保留
├── config/                         # 配置类 ✅ 保留
└── old/                            # 归档代码 📦 待 review
    ├── chunking/
    ├── document/
    ├── feedback/
    ├── image/
    ├── optimization/
    ├── p2p/
    ├── voting/
    ├── benchmark/
    └── util/
```

---

## 📊 代码分类表

### ✅ 保留（业务编排 + 领域服务协调）

| 目录/文件 | 职责 | 保留原因 |
|----------|------|---------|
| `hope/` | HOPE 问答系统 | 业务编排 - 协调多层知识检索 |
| `router/DomainRouter` | 领域路由器 | 领域协调 - 智能路由到不同域 |
| `service/domain/` | 知识域管理 | 领域服务协调 |
| `service/role/` | 角色服务 | 领域服务协调 |
| `service/query/CrossDomainQueryService` | 跨域查询 | 业务编排 - 协调多域查询 |
| `qa/IntelligentQAService` | 智能问答 | 业务编排 - 协调 RAG + AI |
| `dto/` | 数据传输对象 | 接口定义 |
| `config/` | 配置类 | 系统配置 |

### ❌ 归档到 `old/`（具体实现）

| 目录/文件 | 职责 | 归档原因 |
|----------|------|---------|
| `chunking/` | 文档分块服务 | 具体实现 - 应移到专门模块 |
| `document/` | 文档处理器 | 具体实现 - 文本提取、格式转换 |
| `image/` | 图像存储服务 | 具体实现 - 应移到 storage 模块 |
| `optimization/` | RAG 优化服务 | 具体实现 - 应移到 RAG 模块 |
| `p2p/` | P2P 协作管理 | 具体实现 - 应在 p2p-starter 中 |
| `feedback/` | 反馈服务 | 具体实现 - 应移到专门模块 |
| `voting/` | 投票服务 | 具体实现 - 应在 voting-starter 中 |
| `benchmark/` | 性能基准测试 | 测试工具 - 应移到 test 模块 |
| `util/` | 工具类 | 通用工具 - 应移到 common 模块 |
| `query/` (部分) | 具体查询实现 | 具体实现 - 保留编排，归档实现 |

---

## 🔄 迁移步骤

### Phase 1: 创建 old 目录（归档）

```bash
# 创建归档目录
mkdir -p omni-agent-core/src/main/java/top/yumbo/ai/omni/core/old
```

### Phase 2: 移动代码到 old（不删除）

按照上表将具体实现代码移动到 `old/` 目录：

```bash
# 移动具体实现代码到 old
mv omni-agent-core/src/main/java/top/yumbo/ai/omni/core/chunking \
   omni-agent-core/src/main/java/top/yumbo/ai/omni/core/old/

mv omni-agent-core/src/main/java/top/yumbo/ai/omni/core/document \
   omni-agent-core/src/main/java/top/yumbo/ai/omni/core/old/

mv omni-agent-core/src/main/java/top/yumbo/ai/omni/core/image \
   omni-agent-core/src/main/java/top/yumbo/ai/omni/core/old/

# ... 依此类推
```

### Phase 3: 创建新的编排层

创建新的业务编排类：

1. **DocumentProcessingOrchestrator** - 文档处理编排器
2. **KnowledgeEnhancementOrchestrator** - 知识增强编排器
3. **IntelligentQAOrchestrator** - 智能问答编排器

### Phase 4: 创建协调器层

创建领域服务协调器：

1. **DomainCoordinator** - 领域协调器
2. **ServiceCoordinator** - 服务协调器
3. **WorkflowCoordinator** - 工作流协调器

---

## 📝 编排器设计示例

### 1. 文档处理编排器

```java
@Service
public class DocumentProcessingOrchestrator {
    
    @Autowired
    private DocumentStorageService documentStorage;
    
    @Autowired
    private AIService aiService;  // 用于文本提取
    
    @Autowired
    private PPLService pplService;  // 用于分块
    
    @Autowired
    private RAGService ragService;  // 用于索引
    
    /**
     * 编排完整的文档处理流程
     */
    public ProcessResult processDocument(String documentId) {
        // 1. 存储文档（调用 API）
        // 2. 文本提取（调用 AI Service）
        // 3. PPL 分块（调用 PPL Service）
        // 4. 向量化（调用 AI Service）
        // 5. RAG 索引（调用 RAG Service）
        // 
        // ⭐ 只做编排，不做具体实现
    }
}
```

### 2. 知识增强编排器

```java
@Service
public class KnowledgeEnhancementOrchestrator {
    
    @Autowired
    private KnowledgeExtractionService extractionService;
    
    @Autowired
    private KnowledgeRefinementService refinementService;
    
    @Autowired
    private KnowledgeStorageService storageService;
    
    @Autowired
    private KnowledgeNetworkService networkService;
    
    /**
     * 编排知识增强流程（异步后台）
     */
    @Async
    public CompletableFuture<EnhancementResult> enhanceKnowledge(String documentId) {
        // 1. 提取知识（调用 API）
        // 2. AI 提炼（调用 API）
        // 3. 存储知识（调用 API）
        // 4. 构建关联（调用 API）
        //
        // ⭐ 只做编排，不做具体实现
    }
}
```

### 3. 智能问答编排器

```java
@Service
public class IntelligentQAOrchestrator {
    
    @Autowired
    private DomainRouter domainRouter;
    
    @Autowired
    private RAGService ragService;
    
    @Autowired
    private KnowledgeNetworkService knowledgeNetwork;
    
    @Autowired
    private AIService aiService;
    
    /**
     * 编排智能问答流程
     */
    public AnswerResult answer(String question) {
        // 1. 智能路由（领域识别）
        // 2. RAG 检索（调用 API）
        // 3. 知识增强（调用 API）
        // 4. AI 生成（调用 API）
        // 5. 答案优化（调用 API）
        //
        // ⭐ 只做编排，不做具体实现
    }
}
```

---

## 🎯 重构原则

### ✅ Core 应该做什么

1. **业务流程编排**
   - 定义处理流程
   - 协调各个服务
   - 处理异常和回滚

2. **领域服务协调**
   - 跨域查询协调
   - 多服务组合
   - 资源调度

3. **智能路由**
   - 意图识别
   - 领域路由
   - 负载均衡

### ❌ Core 不应该做什么

1. **具体实现**
   - 文本提取具体算法
   - 图像处理具体逻辑
   - 存储具体操作

2. **工具类**
   - 通用工具方法
   - 格式转换
   - 文件操作

3. **Starter 职责**
   - 自动配置
   - Bean 注册
   - 条件装配

---

## 📋 执行清单

- [ ] Phase 1: 创建 `old/` 归档目录
- [ ] Phase 2: 移动具体实现代码到 `old/`
  - [ ] chunking → old/chunking
  - [ ] document → old/document
  - [ ] image → old/image
  - [ ] optimization → old/optimization
  - [ ] p2p → old/p2p
  - [ ] feedback → old/feedback
  - [ ] voting → old/voting
  - [ ] benchmark → old/benchmark
  - [ ] util → old/util
- [ ] Phase 3: 创建新的编排层
  - [ ] DocumentProcessingOrchestrator
  - [ ] KnowledgeEnhancementOrchestrator
  - [ ] IntelligentQAOrchestrator
- [ ] Phase 4: 创建协调器层
  - [ ] DomainCoordinator
  - [ ] ServiceCoordinator
  - [ ] WorkflowCoordinator
- [ ] Phase 5: Review 归档代码
- [ ] Phase 6: 决定归档代码去向
  - [ ] 移到专门模块
  - [ ] 移到 common
  - [ ] 真正删除

---

## 🔍 Review 要点

在 review `old/` 目录下的代码时，需要决定：

1. **是否保留？**
   - 是否还需要这个功能？
   - 是否有替代方案？

2. **放到哪里？**
   - 移到 common 模块？
   - 创建新的专门模块？
   - 移到某个 starter？

3. **如何改造？**
   - 提取接口到 API 层？
   - 改为可插拔实现？
   - 简化为工具类？

---

**创建时间：** 2025-12-28  
**状态：** 待执行  
**下一步：** 开始执行 Phase 1

