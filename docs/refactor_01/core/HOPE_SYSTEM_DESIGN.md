# HOPE 系统设计文档

**HOPE = Hierarchical Omni-Agent Persistent Engine**  
**中文名称：** 分层智能持久化引擎

**创建时间：** 2025-12-31  
**版本：** 1.0.0  
**状态：** ✅ 已实现

---

## 📋 目录

1. [系统概述](#系统概述)
2. [三层知识结构](#三层知识结构)
3. [核心组件](#核心组件)
4. [工作流程](#工作流程)
5. [持久化机制](#持久化机制)
6. [配置说明](#配置说明)
7. [API 参考](#api-参考)
8. [最佳实践](#最佳实践)
9. [与知识网络的关系](#与知识网络的关系)

---

## 🎯 系统概述

### 1.1 什么是 HOPE？

HOPE（Hierarchical Omni-Agent Persistent Engine）是 OmniAgent 的**核心知识管理系统**，实现了分层的知识存储和智能检索机制。

**核心理念：**
- 不同类型的知识应该存储在不同的层级
- 频繁访问的知识应该优先检索
- 核心知识应该长期稳定保存

### 1.2 设计目标

| 目标 | 说明 |
|------|------|
| 🎯 **智能分层** | 根据问题类型自动选择最合适的知识层级 |
| ⚡ **高效检索** | 优先检索高频知识，提高响应速度 |
| 💾 **持久化** | 核心知识长期保存，避免重复学习 |
| 🔄 **动态调整** | 根据访问频率动态调整知识层级 |
| 🧠 **智能学习** | 从用户交互中学习新知识 |

### 1.3 系统架构

```
┌─────────────────────────────────────────────────────────┐
│                  HOPE Knowledge Manager                  │
│           (核心协调器 - HOPEKnowledgeManager)            │
└─────────────────────────────────────────────────────────┘
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                   ↓                   ↓
┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│  持久层       │  │  普通层       │  │  高频层       │
│  (Permanent)  │  │  (Ordinary)   │  │(High Freq)    │
│               │  │               │  │               │
│  核心知识     │  │  一般性知识   │  │  热点知识     │
│  长期稳定     │  │  常规问答     │  │  频繁访问     │
└───────────────┘  └───────────────┘  └───────────────┘
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                   ↓                   ↓
┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│ 问题分类器    │  │  RAG Service  │  │  统计模块     │
│QuestionClass  │  │  (语义搜索)   │  │ LayerStats    │
└───────────────┘  └───────────────┘  └───────────────┘
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                                       ↓
┌───────────────────────────┐  ┌───────────────────────┐
│   持久化抽象层            │  │   知识注册表          │
│   (HopePersistence)       │  │  (KnowledgeRegistry)  │
│                           │  │                       │
│  - InMemory (默认)        │  │  - 元数据管理         │
│  - KnowledgeRegistry      │  │  - 配置存储           │
└───────────────────────────┘  └───────────────────────┘
```

---

## 📚 三层知识结构

### 2.1 持久层（Permanent Layer）

**特点：**
- 📌 **长期稳定** - 核心知识，很少变化
- 🎓 **权威可靠** - 经过验证的知识
- 🔒 **手动管理** - 通常由管理员配置

**适用场景：**
- 系统使用说明
- 核心概念定义
- 常见问题解答（官方）
- 产品特性说明

**示例问题类型：**
```yaml
question-types:
  - id: "system-core"
    name: "系统核心功能"
    layer: "permanent"
    keywords:
      - "是什么"
      - "核心功能"
      - "设计理念"
      - "架构"
```

### 2.2 普通层（Ordinary Layer）

**特点：**
- 📝 **一般性知识** - 常规业务知识
- 🔄 **动态更新** - 随着内容增加而更新
- 📊 **中等频率** - 访问频率适中

**适用场景：**
- 业务流程说明
- 功能使用指南
- 常规技术文档
- 开发文档

**示例问题类型：**
```yaml
question-types:
  - id: "usage-guide"
    name: "使用指南"
    layer: "ordinary"
    keywords:
      - "如何使用"
      - "怎么配置"
      - "操作步骤"
```

### 2.3 高频层（High Frequency Layer）

**特点：**
- 🔥 **热点知识** - 用户频繁访问
- ⚡ **快速响应** - 优先检索
- 🔄 **动态调整** - 根据访问频率自动调整

**适用场景：**
- 最近经常被问的问题
- 热门功能说明
- 新发布功能介绍
- 用户反馈的问题

**动态调整机制：**
```java
// 当某个问题的访问次数超过阈值时，自动提升到高频层
if (questionStats.getAccessCount() > HIGH_FREQUENCY_THRESHOLD) {
    moveToHighFrequencyLayer(question);
}
```

---

## 🔧 核心组件

### 3.1 HOPEKnowledgeManager（知识管理器）

**位置：** `top.yumbo.ai.omni.core.hope.HOPEKnowledgeManager`

**职责：**
- 协调三层知识结构
- 执行智能查询
- 维护层级统计信息
- 动态调整知识层级

**核心方法：**

```java
@Service
public class HOPEKnowledgeManager {
    
    /**
     * 查询知识
     * @param question 用户问题
     * @param maxResults 最大结果数
     * @return 查询结果
     */
    public QueryResult query(String question, int maxResults) {
        // 1. 分类问题
        String questionType = questionClassifier.classify(question);
        String suggestedLayer = questionClassifier.getSuggestedLayer(questionType);
        
        // 2. 使用 RAG 进行语义搜索
        List<Document> documents = ragService.semanticSearch(question, maxResults);
        
        // 3. 更新统计信息
        updateLayerStats(suggestedLayer);
        
        // 4. 构建结果
        return buildQueryResult(question, questionType, suggestedLayer, documents);
    }
    
    /**
     * 智能查询（增强版）
     * @param question 用户问题
     * @param context 上下文信息
     * @return 查询结果
     */
    public QueryResult smartQuery(String question, String context) {
        // 结合上下文进行更智能的查询
    }
    
    /**
     * 获取层级统计信息
     */
    public Map<String, LayerStats> getLayerStats() {
        return layerStatsMap;
    }
}
```

**查询结果模型：**

```java
@Data
public class QueryResult {
    private String question;              // 原始问题
    private String questionType;          // 问题类型
    private String suggestedLayer;        // 建议使用的知识层级
    private List<Document> documents;     // 检索到的文档
    private String answer;                // 答案（可选）
    private Double confidence;            // 置信度
    private Long queryTimeMs;             // 查询耗时
    private Boolean success;              // 是否成功
}
```

### 3.2 QuestionClassifier（问题分类器）

**位置：** `top.yumbo.ai.omni.core.hope.QuestionClassifier`

**职责：**
- 分析用户问题
- 确定问题类型
- 建议使用的知识层级

**分类机制：**

```java
@Component
public class QuestionClassifier {
    
    /**
     * 分类问题
     * @param question 用户问题
     * @return 问题类型ID
     */
    public String classify(String question) {
        // 1. 关键词匹配
        for (Map.Entry<String, List<String>> entry : keywordCache.entrySet()) {
            String typeId = entry.getKey();
            List<String> keywords = entry.getValue();
            
            for (String keyword : keywords) {
                if (question.contains(keyword)) {
                    return typeId;
                }
            }
        }
        
        // 2. 模式匹配（正则表达式）
        for (Map.Entry<String, List<Pattern>> entry : patternCache.entrySet()) {
            String typeId = entry.getKey();
            List<Pattern> patterns = entry.getValue();
            
            for (Pattern pattern : patterns) {
                if (pattern.matcher(question).find()) {
                    return typeId;
                }
            }
        }
        
        // 3. 默认类型
        return "general";
    }
    
    /**
     * 获取建议使用的知识层级
     * @param questionType 问题类型
     * @return 层级名称
     */
    public String getSuggestedLayer(String questionType) {
        QuestionTypeConfig config = configCache.get(questionType);
        return config != null ? config.getSuggestedLayer() : "ordinary";
    }
}
```

**问题类型配置：**

```java
@Data
@Builder
public class QuestionTypeConfig {
    private String id;                  // 类型ID
    private String name;                // 类型名称
    private String suggestedLayer;      // 建议层级
    private List<String> keywords;      // 关键词列表
    private List<String> patterns;      // 正则模式列表
    private Integer priority;           // 优先级
    private Boolean enabled;            // 是否启用
}
```

### 3.3 LayerStats（层级统计）

**职责：**
- 记录每层的访问次数
- 统计查询耗时
- 支持性能分析

**统计信息：**

```java
@Data
public class LayerStats {
    private String layerName;           // 层级名称
    private long queryCount;            // 查询次数
    private long totalQueryTimeMs;      // 总查询耗时
    private double avgQueryTimeMs;      // 平均查询耗时
    private long lastAccessTime;        // 最后访问时间
    
    public void incrementQueryCount() {
        this.queryCount++;
        this.lastAccessTime = System.currentTimeMillis();
    }
    
    public void addQueryTime(long timeMs) {
        this.totalQueryTimeMs += timeMs;
        this.avgQueryTimeMs = (double) totalQueryTimeMs / queryCount;
    }
}
```

---

## 🔄 工作流程

### 4.1 知识查询流程

```
用户提问
    ↓
┌─────────────────────────────────────┐
│  1. 问题分类器分析                  │
│     - 关键词匹配                    │
│     - 模式匹配                      │
│     - 确定问题类型                  │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  2. 确定知识层级                    │
│     - 根据问题类型                  │
│     - 获取建议层级                  │
│     - permanent/ordinary/high_freq  │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  3. RAG 语义搜索                    │
│     - 向量检索                      │
│     - 相似度排序                    │
│     - 返回 Top-K 文档               │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  4. 更新统计信息                    │
│     - 记录访问次数                  │
│     - 更新查询耗时                  │
│     - 动态调整层级                  │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  5. 构建查询结果                    │
│     - 计算置信度                    │
│     - 组装答案                      │
│     - 返回给用户                    │
└─────────────────────────────────────┘
```

### 4.2 知识学习流程

```
新知识输入
    ↓
┌─────────────────────────────────────┐
│  1. 知识预处理                      │
│     - 文本清洗                      │
│     - 格式标准化                    │
│     - 提取元数据                    │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  2. 知识分类                        │
│     - 确定知识类型                  │
│     - 选择目标层级                  │
│     - 设置优先级                    │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  3. 存储到持久层                    │
│     - 保存到 HopePersistence        │
│     - 更新索引                      │
│     - 同步到 RAG 系统               │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  4. 验证和测试                      │
│     - 测试查询效果                  │
│     - 验证准确性                    │
│     - 调整配置                      │
└─────────────────────────────────────┘
```

---

## 💾 持久化机制

### 5.1 HopePersistence 接口

**位置：** `top.yumbo.ai.omni.core.hope.persistence.HopePersistence`

**接口定义：**

```java
public interface HopePersistence {
    
    // ========== 问题类型管理 ==========
    
    /**
     * 获取所有问题类型
     */
    List<QuestionTypeConfig> getAllQuestionTypes();
    
    /**
     * 保存问题类型
     */
    void saveQuestionType(QuestionTypeConfig config);
    
    /**
     * 删除问题类型
     */
    void deleteQuestionType(String typeId);
    
    // ========== 关键词管理 ==========
    
    /**
     * 获取指定类型的关键词
     */
    List<String> getKeywords(String typeId);
    
    /**
     * 保存关键词
     */
    void saveKeywords(String typeId, List<String> keywords);
    
    // ========== 模式管理 ==========
    
    /**
     * 获取指定类型的正则模式
     */
    List<String> getPatterns(String typeId);
    
    /**
     * 保存正则模式
     */
    void savePatterns(String typeId, List<String> patterns);
}
```

### 5.2 实现方式

#### 5.2.1 InMemoryHopePersistence（默认实现）

**位置：** `top.yumbo.ai.omni.core.hope.persistence.impl.InMemoryHopePersistence`

**特点：**
- ✅ 零依赖，开箱即用
- ✅ 适合开发和测试
- ❌ 重启后数据丢失

**实现原理：**
```java
@Service
@ConditionalOnMissingBean(HopePersistence.class)
public class InMemoryHopePersistence implements HopePersistence {
    
    private final Map<String, QuestionTypeConfig> typeCache = new ConcurrentHashMap<>();
    private final Map<String, List<String>> keywordCache = new ConcurrentHashMap<>();
    private final Map<String, List<String>> patternCache = new ConcurrentHashMap<>();
    
    @PostConstruct
    public void init() {
        // 加载默认配置
        loadDefaultConfiguration();
    }
    
    private void loadDefaultConfiguration() {
        // 预定义的问题类型
        saveQuestionType(QuestionTypeConfig.builder()
            .id("system-core")
            .name("系统核心功能")
            .suggestedLayer("permanent")
            .keywords(Arrays.asList("是什么", "核心功能", "设计理念"))
            .build());
        
        // ... 更多默认配置
    }
}
```

#### 5.2.2 KnowledgeRegistryHopePersistence（推荐实现）

**位置：** `top.yumbo.ai.omni.core.hope.persistence.impl.KnowledgeRegistryHopePersistence`

**特点：**
- ✅ 数据持久化
- ✅ 支持多种存储后端（File/Mongo/Redis）
- ✅ 适合生产环境

**实现原理：**
```java
@Service
@ConditionalOnBean(KnowledgeRegistry.class)
public class KnowledgeRegistryHopePersistence implements HopePersistence {
    
    private final KnowledgeStorageService storageService;
    
    @Override
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        // 从知识注册表读取配置
        String configJson = storageService.load("hope", "question-types.json");
        return parseQuestionTypes(configJson);
    }
    
    @Override
    public void saveQuestionType(QuestionTypeConfig config) {
        // 保存到知识注册表
        List<QuestionTypeConfig> types = getAllQuestionTypes();
        types.add(config);
        String json = toJson(types);
        storageService.save("hope", "question-types.json", json);
    }
}
```

---

## ⚙️ 配置说明

### 6.1 Spring Boot 配置

```yaml
# application.yml

omni:
  hope:
    # 启用 HOPE 系统
    enabled: true
    
    # 持久化实现类型
    # 可选: memory, knowledge-registry
    persistence-type: knowledge-registry
    
    # 默认知识层级
    default-layer: ordinary
    
    # 高频层阈值（访问次数）
    high-frequency-threshold: 100
    
    # 层级权重配置
    layer-weights:
      permanent: 1.5      # 持久层权重
      ordinary: 1.0       # 普通层权重
      high-frequency: 2.0 # 高频层权重
    
    # 问题分类器配置
    classifier:
      # 是否启用缓存
      enable-cache: true
      
      # 缓存过期时间（秒）
      cache-ttl: 3600
```

### 6.2 问题类型配置（JSON）

```json
{
  "question-types": [
    {
      "id": "system-core",
      "name": "系统核心功能",
      "suggestedLayer": "permanent",
      "keywords": ["是什么", "核心功能", "设计理念", "架构"],
      "patterns": ["^什么是.*", "^.*的作用是什么"],
      "priority": 100,
      "enabled": true
    },
    {
      "id": "usage-guide",
      "name": "使用指南",
      "suggestedLayer": "ordinary",
      "keywords": ["如何使用", "怎么配置", "操作步骤"],
      "patterns": ["^如何.*", "^怎么.*"],
      "priority": 50,
      "enabled": true
    },
    {
      "id": "troubleshooting",
      "name": "问题排查",
      "suggestedLayer": "high_frequency",
      "keywords": ["报错", "异常", "失败", "不工作"],
      "patterns": ["^为什么.*失败", "^.*报错.*"],
      "priority": 80,
      "enabled": true
    }
  ]
}
```

### 6.3 自动配置

**位置：** `top.yumbo.ai.omni.core.hope.config.HopePersistenceAutoConfiguration`

```java
@Configuration
@ConditionalOnProperty(name = "omni.hope.enabled", havingValue = "true", matchIfMissing = true)
public class HopePersistenceAutoConfiguration {
    
    @Bean
    @ConditionalOnMissingBean
    public HopePersistence hopePersistence(
            @Autowired(required = false) KnowledgeRegistry knowledgeRegistry) {
        
        if (knowledgeRegistry != null) {
            log.info("✅ Using KnowledgeRegistryHopePersistence");
            return new KnowledgeRegistryHopePersistence(knowledgeRegistry);
        } else {
            log.info("✅ Using InMemoryHopePersistence (default)");
            return new InMemoryHopePersistence();
        }
    }
    
    @Bean
    public QuestionClassifier questionClassifier(HopePersistence persistence) {
        return new QuestionClassifier(persistence);
    }
    
    @Bean
    public HOPEKnowledgeManager hopeKnowledgeManager(
            QuestionClassifier classifier,
            RagService ragService) {
        return new HOPEKnowledgeManager(classifier, ragService);
    }
}
```

---

## 📖 API 参考

### 7.1 查询 API

```java
@RestController
@RequestMapping("/api/hope")
public class HopeController {
    
    @Autowired
    private HOPEKnowledgeManager hopeManager;
    
    /**
     * 查询知识
     * POST /api/hope/query
     */
    @PostMapping("/query")
    public QueryResult query(@RequestBody QueryRequest request) {
        return hopeManager.query(
            request.getQuestion(),
            request.getMaxResults()
        );
    }
    
    /**
     * 智能查询（带上下文）
     * POST /api/hope/smart-query
     */
    @PostMapping("/smart-query")
    public QueryResult smartQuery(@RequestBody SmartQueryRequest request) {
        return hopeManager.smartQuery(
            request.getQuestion(),
            request.getContext()
        );
    }
    
    /**
     * 获取层级统计
     * GET /api/hope/stats
     */
    @GetMapping("/stats")
    public Map<String, LayerStats> getStats() {
        return hopeManager.getLayerStats();
    }
}
```

### 7.2 管理 API

```java
@RestController
@RequestMapping("/api/hope/admin")
public class HopeAdminController {
    
    @Autowired
    private QuestionClassifier classifier;
    
    @Autowired
    private HopePersistence persistence;
    
    /**
     * 添加问题类型
     * POST /api/hope/admin/question-types
     */
    @PostMapping("/question-types")
    public void addQuestionType(@RequestBody QuestionTypeConfig config) {
        persistence.saveQuestionType(config);
        classifier.reload();  // 重新加载配置
    }
    
    /**
     * 添加关键词
     * POST /api/hope/admin/keywords
     */
    @PostMapping("/keywords")
    public void addKeywords(
            @RequestParam String typeId,
            @RequestBody List<String> keywords) {
        persistence.saveKeywords(typeId, keywords);
        classifier.reload();
    }
}
```

---

## 💡 最佳实践

### 8.1 问题类型设计

**原则：**
1. **从粗到细** - 先定义大类，再细分
2. **关键词互斥** - 避免关键词重叠导致分类错误
3. **优先级明确** - 重要的问题类型设置更高优先级

**示例：**
```json
{
  "question-types": [
    {
      "id": "security",
      "name": "安全相关",
      "suggestedLayer": "permanent",
      "keywords": ["安全", "漏洞", "CVE", "权限"],
      "priority": 100
    },
    {
      "id": "performance",
      "name": "性能优化",
      "suggestedLayer": "ordinary",
      "keywords": ["性能", "慢", "优化", "卡顿"],
      "priority": 80
    }
  ]
}
```

### 8.2 层级选择策略

| 问题类型 | 建议层级 | 原因 |
|---------|---------|------|
| 系统核心概念 | permanent | 长期稳定，很少变化 |
| API 文档 | ordinary | 可能更新，但访问频率适中 |
| 常见错误 | high_frequency | 用户经常遇到 |
| 新功能说明 | high_frequency | 短期内高频访问 |

### 8.3 性能优化建议

1. **启用缓存**
```yaml
omni:
  hope:
    classifier:
      enable-cache: true
      cache-ttl: 3600
```

2. **预编译正则表达式**
```java
// QuestionClassifier 自动在初始化时预编译
private final Map<String, List<Pattern>> patternCache = new ConcurrentHashMap<>();
```

3. **异步更新统计**
```java
@Async
public void updateStatsAsync(String layer, long queryTime) {
    LayerStats stats = layerStatsMap.get(layer);
    stats.incrementQueryCount();
    stats.addQueryTime(queryTime);
}
```

---

## 🔗 与知识网络的关系

### 9.1 协同工作模式

```
┌─────────────────────────────────────────────────────────────┐
│                      用户查询                                 │
└─────────────────────────────────────────────────────────────┘
                            ↓
        ┌───────────────────┴───────────────────┐
        ↓                                       ↓
┌───────────────────┐                  ┌───────────────────┐
│   HOPE 系统       │                  │   知识网络        │
│                   │                  │                   │
│  - 问题分类       │                  │  - 知识域管理     │
│  - 层级选择       │ ←─────协同─────→ │  - 知识提取       │
│  - 统计管理       │                  │  - 跨域查询       │
└───────────────────┘                  └───────────────────┘
        ↓                                       ↓
        └───────────────────┬───────────────────┘
                            ↓
        ┌─────────────────────────────────────┐
        │         RAG Service                 │
        │         (统一检索接口)              │
        └─────────────────────────────────────┘
```

### 9.2 数据流向

```
1. 文档上传 → 知识网络 → 知识域存储 → RAG 索引
                            ↓
2. 用户查询 → HOPE 系统 → 问题分类 → 确定层级
                            ↓
3. RAG 检索 ← HOPE 系统 ← 知识域 ← 知识网络
                            ↓
4. 返回结果 → 更新统计 → HOPE 系统 → 用户
```

### 9.3 互补关系

| 系统 | 职责 | 关注点 |
|------|------|--------|
| **HOPE 系统** | 知识查询和分层管理 | 如何快速找到正确的知识 |
| **知识网络** | 知识组织和关联 | 知识如何存储和关联 |
| **RAG 系统** | 向量检索和排序 | 如何计算相似度 |

**关键点：**
- HOPE 系统**不负责**知识存储，只负责智能检索
- 知识网络**不负责**查询优化，只负责知识组织
- 两者通过 RAG Service 进行协同

---

## 📊 监控和诊断

### 10.1 统计信息

```bash
# 获取层级统计
GET /api/hope/stats

# 响应示例
{
  "permanent": {
    "layerName": "permanent",
    "queryCount": 1520,
    "totalQueryTimeMs": 45600,
    "avgQueryTimeMs": 30.0,
    "lastAccessTime": 1735660800000
  },
  "ordinary": {
    "layerName": "ordinary",
    "queryCount": 8940,
    "totalQueryTimeMs": 267000,
    "avgQueryTimeMs": 29.9,
    "lastAccessTime": 1735660800000
  },
  "high_frequency": {
    "layerName": "high_frequency",
    "queryCount": 12350,
    "totalQueryTimeMs": 246000,
    "avgQueryTimeMs": 19.9,
    "lastAccessTime": 1735660800000
  }
}
```

### 10.2 日志配置

```yaml
logging:
  level:
    top.yumbo.ai.omni.core.hope: DEBUG
```

**关键日志：**
```
🎯 Question classified as: usage-guide (suggested layer: ordinary)
✅ Query completed in 25ms, found 5 documents, confidence: 0.85
📊 Layer statistics: permanent=1520, ordinary=8940, high_frequency=12350
```

---

## 🚀 快速开始

### 11.1 基本使用

```java
@Service
public class MyService {
    
    @Autowired
    private HOPEKnowledgeManager hopeManager;
    
    public void example() {
        // 查询知识
        QueryResult result = hopeManager.query("什么是知识网络？", 5);
        
        System.out.println("问题类型: " + result.getQuestionType());
        System.out.println("建议层级: " + result.getSuggestedLayer());
        System.out.println("置信度: " + result.getConfidence());
        System.out.println("找到文档数: " + result.getDocuments().size());
    }
}
```

### 11.2 自定义问题类型

```java
@Service
public class CustomConfiguration {
    
    @Autowired
    private HopePersistence persistence;
    
    @PostConstruct
    public void init() {
        // 添加自定义问题类型
        QuestionTypeConfig config = QuestionTypeConfig.builder()
            .id("custom-type")
            .name("自定义类型")
            .suggestedLayer("ordinary")
            .keywords(Arrays.asList("自定义", "特殊"))
            .priority(70)
            .enabled(true)
            .build();
        
        persistence.saveQuestionType(config);
    }
}
```

---

## 📚 相关文档

- [知识网络架构](./KNOWLEDGE_NETWORK_ARCHITECTURE.md)
- [智能问答系统设计](./INTELLIGENT_QA_SYSTEM_DESIGN.md)
- [RAG 架构设计](./KNOWLEDGE_NETWORK_AND_RAG_ARCHITECTURE.md)
- [批次1分析报告](../../analysis/BATCH_01_CORE_MODULES_ANALYSIS.md)

---

## ✅ 总结

### 核心价值

1. **智能分层** - 根据问题类型自动选择最优知识层级
2. **高效检索** - 优先检索高频知识，提高响应速度
3. **持久化** - 核心知识长期保存，避免重复学习
4. **可扩展** - 支持自定义问题类型和持久化后端

### 设计亮点

- ✅ 三层知识结构设计独特
- ✅ 问题分类器灵活可配置
- ✅ 持久化抽象支持多种后端
- ✅ 与知识网络完美协同

### 未来展望

1. **机器学习增强** - 使用 ML 模型自动学习问题分类
2. **动态调整** - 根据访问模式自动调整层级
3. **多语言支持** - 支持多语言问题分类
4. **可视化管理** - 提供 Web 界面管理问题类型

---

**文档版本：** 1.0.0  
**最后更新：** 2025-12-31  
**维护者：** OmniAgent Team

