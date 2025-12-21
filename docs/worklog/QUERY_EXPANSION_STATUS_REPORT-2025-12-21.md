# 🔍 查询扩展实现现状报告 (2025-12-21)

> **生成时间**: 2025年12月21日  
> **状态**: 已实现 ✅  
> **位置**: `omni-agent-marketplace` 模块

---

## 📋 实现概况

### ✅ 已实现的功能

查询扩展功能**已经完整实现**，位于以下两个模块：

#### 1. **EnhancedQueryService** (增强查询服务)
**路径**: `omni-agent-marketplace/src/main/java/top/yumbo/ai/omni/marketplace/EnhancedQueryService.java`

**核心功能**:
- ✅ 查询扩展 (Query Expansion)
- ✅ 多查询融合 (Multi-Query Fusion)
- ✅ 结果重排序 (Rerank)
- ✅ RRF (Reciprocal Rank Fusion) 融合算法

**公开方法**:
```java
// 完整增强查询 - 查询扩展 + 重排序
public List<SearchResult> fullyEnhancedSearch(String question, int topK)

// 仅查询扩展
public List<SearchResult> enhancedSearchWithExpansion(String question, int topK)

// 自定义增强 - 灵活控制
public List<SearchResult> enhancedSearch(String question, int topK, 
                                          boolean useExpansion, boolean useRerank)
```

#### 2. **AlgorithmMarketService** (算法市场服务)
**路径**: `omni-agent-marketplace/src/main/java/top/yumbo/ai/omni/marketplace/AlgorithmMarketService.java`

**内置组件**:
- ✅ `query_expansion` - 查询扩展组件
- ✅ `semantic_chunking` - 语义分块组件
- ✅ `rerank` - 重排序组件

---

## 🔬 查询扩展详细实现

### 1. 查询扩展策略

**位置**: `AlgorithmMarketService.registerBuiltinComponents()`

```java
registerComponent("query_expansion", new AlgorithmComponent() {
    @Override
    public Object execute(Object input, Map<String, Object> params) {
        String query = input.toString();
        List<String> expandedQueries = new ArrayList<>();
        expandedQueries.add(query); // 原始查询

        // 1. 同义词扩展
        String method = (String) params.getOrDefault("method", "synonym");
        if ("synonym".equals(method)) {
            // 添加同义词变体
            expandedQueries.add(query + " 相关");
            expandedQueries.add(query.replace("配置", "设置"));
            expandedQueries.add(query.replace("如何", "怎么"));
        }

        // 2. 添加领域相关词
        if (query.contains("Spring")) {
            expandedQueries.add(query + " Boot");
            expandedQueries.add(query + " Framework");
        }

        // 3. 限制扩展数量
        int maxExpansions = (int) params.getOrDefault("maxExpansions", 5);
        if (expandedQueries.size() > maxExpansions) {
            expandedQueries = expandedQueries.subList(0, maxExpansions);
        }

        return Map.of(
            "originalQuery", query,
            "expandedQueries", expandedQueries,
            "expansionCount", expandedQueries.size()
        );
    }
});
```

**当前实现的扩展方法**:
1. **同义词替换**: 配置→设置，如何→怎么
2. **相关词添加**: 原查询 + "相关"
3. **领域词扩展**: Spring → Spring Boot, Spring Framework

---

### 2. 多查询融合算法 (RRF)

**位置**: `EnhancedQueryService.fuseResults()`

**算法**: Reciprocal Rank Fusion (RRF)

**公式**:
```
score(d) = Σ 1 / (k + rank(d))
```
其中:
- `k = 60` (常数)
- `rank(d)` 是文档在某个结果列表中的排名

**步骤**:
1. 统计每个文档在多个查询结果中的 RRF 分数
2. 按 RRF 分数降序排序
3. 去重（基于文档ID）

---

### 3. 查询扩展工作流程

```
用户问题: "Spring Boot如何配置?"
   ↓
1. 查询扩展 (performQueryExpansion)
   ├─ 原始查询: "Spring Boot如何配置?"
   ├─ 扩展查询1: "Spring Boot如何配置? 相关"
   ├─ 扩展查询2: "Spring Boot怎么配置?"
   ├─ 扩展查询3: "Spring Boot如何设置?"
   └─ 扩展查询4: "Spring Boot如何配置? Framework"
   ↓
2. 多查询检索 (Multi-Query)
   ├─ ragService.searchByText("Spring Boot如何配置?", topK)
   ├─ ragService.searchByText("Spring Boot如何配置? 相关", topK)
   ├─ ragService.searchByText("Spring Boot怎么配置?", topK)
   └─ ...
   ↓
3. 结果融合 (fuseResults - RRF)
   ├─ 去重 (基于文档ID)
   ├─ 计算 RRF 分数
   └─ 排序
   ↓
4. 重排序 (performRerank) [可选]
   ├─ 调用 rerank 组件
   └─ 基于相关性重新排序
   ↓
5. 返回 Top-K 结果
```

---

## 📊 性能指标

根据内置组件的 metrics:

| 指标 | 值 | 说明 |
|-----|-----|-----|
| **精度提升** | +12.5% | 查询扩展带来的精度增益 |
| **召回率提升** | +15.0% | 更多相关文档被检索 |
| **延迟** | ~20ms | 查询扩展增加的时间 |

---

## 🔗 与其他模块的集成

### 1. 与 omni-agent-core 的关系

`omni-agent-core/query/QueryService.java` 是基础查询服务，只提供简单的 RAG 检索：
- `search(String queryText, int limit)` - 文本搜索
- `vectorSearch(float[] embedding, int limit)` - 向量搜索
- `hybridSearch(String queryText, float[] embedding, int limit)` - 混合搜索

**没有查询扩展功能**。

### 2. 与 omni-agent-web 的集成

`omni-agent-web` 的双轨问答系统调用 `EnhancedQueryService`：

**左轨 (RAG + LLM)**:
```java
// 使用增强查询
List<SearchResult> results = enhancedQueryService.fullyEnhancedSearch(question, topK);
// 拼接检索结果到 Prompt
// 调用 LLM 生成回答
```

**右轨 (HOPE知识演化)**:
```java
// 提取最小概念
// 查询 HOPE 知识图谱
// 基于知识演化推理生成回答
```

---

## 🚀 使用示例

### 示例1: 仅使用查询扩展

```java
@Autowired
private EnhancedQueryService enhancedQueryService;

// 使用查询扩展提高召回率
List<SearchResult> results = enhancedQueryService
    .enhancedSearchWithExpansion("Spring Boot如何配置?", 10);
```

### 示例2: 完整增强查询（扩展 + 重排序）

```java
// 查询扩展 + 重排序
List<SearchResult> results = enhancedQueryService
    .fullyEnhancedSearch("Spring Boot如何配置?", 10);
```

### 示例3: 自定义增强

```java
// 灵活控制：启用扩展，禁用重排序
List<SearchResult> results = enhancedQueryService
    .enhancedSearch("Spring Boot如何配置?", 10, true, false);
```

---

## ⚠️ 当前限制与改进建议

### 当前限制

1. **简化的扩展策略**
   - 当前实现是硬编码的同义词替换
   - 没有使用 LLM 生成高质量的查询变体
   - 领域词扩展只支持 "Spring"

2. **没有从 old 代码复用**
   - `old/ai-reviewer-base-file-rag` 中有更完整的查询扩展实现
   - 包括缓存、分页、高级过滤等功能

3. **缺少可配置性**
   - 扩展策略是硬编码的
   - 无法通过配置文件调整扩展规则

---

## 🎯 Phase 1 改进计划

### 目标：从 old 代码复用高级查询扩展逻辑

#### 1. 复用 old 代码中的功能

**old 代码路径**: `old/ai-reviewer-base-file-rag/src/main/java/top/yumbo/ai/rag/query/`

**可复用的类**:
- `AdvancedQueryProcessor` - 高级查询处理器
  - ✅ 缓存机制
  - ✅ 分数阈值过滤
  - ✅ 自定义排序
  - ✅ 分页支持

- `QueryRequest` - 查询请求对象
- `PagedResult` - 分页结果
- `CacheStatistics` - 缓存统计

#### 2. 集成 LLM 查询扩展

使用 LLM 生成高质量的查询变体：

```java
// 伪代码
String prompt = """
    你是一个查询扩展专家。请为以下用户问题生成3-5个语义相似但表达不同的查询变体。
    
    原始问题: {question}
    
    要求:
    1. 保持原始问题的核心意图
    2. 使用不同的词汇和表达方式
    3. 覆盖可能的同义词和领域词
    
    输出格式（JSON）:
    {
      "expandedQueries": ["查询1", "查询2", "查询3"]
    }
    """;

// 调用 LLM
String response = aiService.chat(prompt);
List<String> expandedQueries = parseJson(response);
```

#### 3. 可配置的扩展策略

**配置文件**: `application.yml`

```yaml
omni-agent:
  query-expansion:
    enabled: true
    max-expansions: 5
    strategies:
      - type: synonym
        weight: 0.3
      - type: llm
        weight: 0.5
        model: qwen2.5
      - type: domain
        weight: 0.2
        domains:
          - spring: [boot, framework, cloud]
          - java: [jdk, jvm, maven]
```

#### 4. 性能优化

- ✅ 添加查询扩展缓存（避免重复扩展）
- ✅ 并行执行多个查询（使用 CompletableFuture）
- ✅ 限制扩展查询的执行时间（超时降级）

---

## 📝 实现步骤

### Step 1: 创建高级查询处理器

```bash
# 从 old 复制并改造
cp old/ai-reviewer-base-file-rag/src/.../query/impl/AdvancedQueryProcessor.java \
   omni-agent-core/src/main/java/top/yumbo/ai/omni/core/query/
```

### Step 2: 集成到 EnhancedQueryService

```java
@Service
public class EnhancedQueryService {
    
    @Autowired
    private RAGService ragService;
    
    @Autowired
    private AlgorithmMarketService algorithmMarketService;
    
    @Autowired(required = false)
    private AIService aiService;  // 用于 LLM 查询扩展
    
    @Autowired
    private AdvancedQueryProcessor advancedQueryProcessor;  // 新增
    
    // ... 现有方法
    
    /**
     * LLM 驱动的查询扩展
     */
    private List<String> performLLMQueryExpansion(String question) {
        // 使用 LLM 生成高质量查询变体
    }
}
```

### Step 3: 添加配置支持

```java
@ConfigurationProperties(prefix = "omni-agent.query-expansion")
public class QueryExpansionConfig {
    private boolean enabled = true;
    private int maxExpansions = 5;
    private Map<String, Object> strategies;
    // getters and setters
}
```

### Step 4: 编写测试

```java
@SpringBootTest
class EnhancedQueryServiceTest {
    
    @Test
    void testQueryExpansion() {
        List<SearchResult> results = enhancedQueryService
            .enhancedSearchWithExpansion("Spring Boot如何配置?", 10);
        
        assertThat(results).isNotEmpty();
        assertThat(results.size()).isLessThanOrEqualTo(10);
    }
}
```

---

## 🎬 总结

### 现状
- ✅ 查询扩展功能**已实现**
- ✅ 位于 `omni-agent-marketplace` 模块
- ✅ 包含基础的同义词扩展和 RRF 融合
- ⚠️ 实现较为简化，扩展策略硬编码

### 下一步
1. 从 `old` 代码复用高级查询处理器
2. 集成 LLM 驱动的查询扩展
3. 添加可配置的扩展策略
4. 性能优化（缓存、并行、超时）

### 优先级
- **P0**: 保持现有功能正常运行 ✅
- **P1**: 从 old 复用缓存和分页功能
- **P2**: 集成 LLM 查询扩展
- **P3**: UI 可视化配置

---

**报告生成时间**: 2025年12月21日  
**作者**: OmniAgent Team  
**版本**: v1.0.0

