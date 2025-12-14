# 🚀 Phase 2 启动：Core 层解耦

> **开始时间**: 2025-12-14 23:15  
> **阶段**: Phase 2 - Core 层解耦  
> **目标**: 改造 omni-agent-core，使其只依赖接口

---

## 🎯 Phase 2 目标

### 核心任务
1. 创建 omni-agent-core 模块 ✅
2. 删除所有持久化实现（impl 目录）
3. 改造 HOPE 系统使用接口注入
4. 改造其他核心模块（chunking、image、ppl 等）

### 关键原则
```java
// ❌ 错误：依赖具体实现
private ElasticsearchPersistence persistence;
private PersistenceManager manager;

// ✅ 正确：依赖接口
@Autowired
private QuestionClassifierPersistence persistence;

@Autowired
private DocumentStorageService storageService;
```

---

## ✅ 已完成

### 1. 创建 Core 模块基础结构 ✅
- [x] 创建 omni-agent-core 目录
- [x] 创建 pom.xml
- [x] 配置依赖（只依赖 4 个 API 模块）
- [x] 更新根 pom.xml

**pom.xml 依赖**:
```xml
<!-- 只依赖 API 接口，不依赖任何实现 -->
<dependency>
    <artifactId>omni-agent-persistence-api</artifactId>
</dependency>
<dependency>
    <artifactId>omni-agent-document-storage-api</artifactId>
</dependency>
<dependency>
    <artifactId>omni-agent-rag-api</artifactId>
</dependency>
<dependency>
    <artifactId>omni-agent-ai-api</artifactId>
</dependency>
```

---

## ⏳ 进行中

### 2. 改造 HOPE 系统
准备从 old/omni-agent-core 迁移和改造 HOPE 系统的核心类。

#### HOPE 系统结构分析
```
old/omni-agent-core/src/main/java/top/yumbo/ai/omni/core/hope/
├── HOPEConfig.java                  (配置类)
├── HOPEKnowledgeManager.java        (知识管理器) ← 需要改造
├── QuestionClassifier.java          (问题分类器) ← 需要改造
├── ResponseStrategy.java            (响应策略)
├── ResponseStrategyDecider.java     (策略决策器)
├── integration/                     (集成层)
├── layer/                           (分层服务) ← 需要改造
│   ├── HighFrequencyLayerService
│   ├── OrdinaryLayerService
│   └── PermanentLayerService
├── learning/                        (学习服务) ← 需要改造
└── model/                           (模型类)
```

#### 改造重点
1. **HOPEKnowledgeManager**:
   - 删除 PersistenceManager 依赖
   - 注入 QuestionClassifierPersistence 接口

2. **Layer Services**:
   - 使用 QuestionClassifierPersistence 接口
   - 删除对具体实现的引用

3. **QuestionClassifier**:
   - 使用接口而非具体实现

---

## 📋 Phase 2 任务清单

### Week 2 任务

#### 2.1 清理现有实现 ⏳
- [ ] 确认 old/core 中没有需要保留的 persistence/impl
- [ ] 确认 old/core 中没有 PersistenceFactory
- [ ] 确认 old/core 中没有 PersistenceManager

#### 2.2 改造 HOPE 系统 ⏳
- [ ] 创建新的 HOPEKnowledgeManager（使用接口）
- [ ] 创建新的 QuestionClassifier（使用接口）
- [ ] 改造 HighFrequencyLayerService
- [ ] 改造 OrdinaryLayerService
- [ ] 改造 PermanentLayerService
- [ ] 改造 QuestionClassifierLearningService

### Week 3 任务

#### 2.3 改造其他核心模块 ⏳
- [ ] 改造 chunking/ 模块（使用 DocumentStorageService）
- [ ] 改造 image/ 模块（使用 DocumentStorageService）
- [ ] 改造 ppl/ 模块（使用 DocumentStorageService）
- [ ] 改造 role/ 模块
- [ ] 改造 evolution/ 模块
- [ ] 改造 feedback/ 模块
- [ ] 改造 query/ 模块

---

## 🔧 改造示例

### 示例 1: HOPEKnowledgeManager 改造

**改造前（old）**:
```java
public class HOPEKnowledgeManager {
    private PersistenceManager persistenceManager;
    
    public void init() {
        // 运行时切换策略
        persistenceManager.switchStrategy(PersistenceStrategy.ELASTICSEARCH);
    }
    
    public void saveQuestionType(QuestionTypeConfig config) {
        persistenceManager.getCurrentPersistence().saveQuestionType(config);
    }
}
```

**改造后（new）**:
```java
@Service
public class HOPEKnowledgeManager {
    private final QuestionClassifierPersistence persistence;
    
    @Autowired
    public HOPEKnowledgeManager(QuestionClassifierPersistence persistence) {
        this.persistence = persistence;
        // Spring Boot 会根据用户选择的 Starter 自动注入实现
    }
    
    public void saveQuestionType(QuestionTypeConfig config) {
        // 直接使用接口，不关心具体实现
        persistence.saveQuestionType(config);
    }
}
```

### 示例 2: Chunking 改造

**改造前**:
```java
public class ChunkStorageService {
    private final String basePath = "./data/chunks";
    
    public void saveChunk(Chunk chunk) {
        // 硬编码本地文件存储
        File file = new File(basePath + "/" + chunk.getId());
        Files.write(file.toPath(), chunk.getContent());
    }
}
```

**改造后**:
```java
@Service
public class ChunkStorageService {
    private final DocumentStorageService storageService;
    
    @Autowired
    public ChunkStorageService(DocumentStorageService storageService) {
        this.storageService = storageService;
    }
    
    public void saveChunk(String documentId, Chunk chunk) {
        // 使用接口，可能是 File、MongoDB、S3...
        storageService.saveChunk(documentId, chunk);
    }
}
```

---

## 📊 进度追踪

| 模块 | 状态 | 进度 |
|------|------|------|
| Core 基础结构 | ✅ 完成 | 100% |
| HOPE 系统 | ⏳ 进行中 | 0% |
| Chunking | ⏳ 待开始 | 0% |
| Image | ⏳ 待开始 | 0% |
| PPL | ⏳ 待开始 | 0% |
| Role | ⏳ 待开始 | 0% |
| Evolution | ⏳ 待开始 | 0% |
| Feedback | ⏳ 待开始 | 0% |
| Query | ⏳ 待开始 | 0% |

---

## 🎯 本次会话目标

1. ✅ 创建 omni-agent-core 模块
2. 🔄 开始改造 HOPE 系统核心类
3. ⏳ 至少完成 2-3 个 HOPE 核心类的改造

---

**启动时间**: 2025-12-14 23:15  
**当前状态**: 🔄 Phase 2 已启动，Core 模块创建完成  
**下一步**: 改造 HOPEKnowledgeManager

