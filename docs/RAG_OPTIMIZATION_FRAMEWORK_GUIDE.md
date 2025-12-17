# 📚 RAG优化算法通用框架使用指南

## 🎯 设计目标

将原本硬编码的PPL算法抽象为通用的RAG优化框架，支持多种优化算法的灵活选择和扩展。

---

## 🏗️ 架构变更

### 重构前（v1.0）
```
PPLStorageService (硬编码)
    ↓
DocumentStorageService.savePPLData()
    ↓
只支持PPL算法
```

### 重构后（v2.0）
```
RAGOptimizationService (通用框架)
    ↓
DocumentStorageService.saveOptimizationData()
    ↓
支持13+种优化算法，可扩展
```

---

## 📦 核心类说明

### 1. `OptimizationData` - 通用优化数据模型

```java
@Data
@Builder
public class OptimizationData implements Serializable {
    private String documentId;              // 文档ID
    private String optimizationType;        // 优化类型（ppl, hyde, rerank等）
    private String algorithmVersion;        // 算法版本
    private Long processedAt;               // 处理时间
    private Map<String, Object> data;       // 优化数据（灵活存储）
    private Map<String, Object> metadata;   // 元数据
    private Map<String, Double> metrics;    // 性能指标
}
```

### 2. `OptimizationType` - 优化算法类型枚举

支持的算法类型：

| 类型 | Code | 中文名 | 用途 |
|------|------|--------|------|
| PPL | `ppl` | 提示词编程 | 程序化生成高质量提示词 |
| HYDE | `hyde` | 假设性文档嵌入 | 生成假设性文档进行检索 |
| RERANK | `rerank` | 语义重排序 | 使用更强模型重新计算相关度 |
| QUERY_EXPANSION | `query_expansion` | 查询扩展 | 生成多个查询变体 |
| QUERY_REWRITE | `query_rewrite` | 查询改写 | 优化查询表达 |
| METADATA_FILTER | `metadata_filter` | 元数据过滤 | 智能过滤条件提取 |
| CONTEXT_COMPRESSION | `context_compression` | 上下文压缩 | 压缩长文本 |
| SEMANTIC_CHUNKING | `semantic_chunking` | 语义分块 | 智能文档分块 |
| HYBRID_SEARCH | `hybrid_search` | 混合检索 | 向量+关键词检索 |
| KNOWLEDGE_GRAPH | `knowledge_graph` | 知识图谱 | 图谱增强检索 |
| HOPE_ROUTING | `hope_routing` | HOPE路由 | 三层知识路由 |
| BEHAVIOR_ANALYSIS | `behavior_analysis` | 行为分析 | 用户行为优化 |
| MULTI_MODEL_VOTING | `multi_model_voting` | 多模型投票 | 多模型答案投票 |
| CUSTOM | `custom` | 自定义 | 用户自定义算法 |

### 3. `RAGOptimizationService` - 通用优化服务

统一的优化数据管理服务，提供：
- 通用的保存/获取/删除方法
- 特定算法的便捷方法
- 批量操作支持

---

## 💡 使用示例

### 示例1：保存PPL优化数据

```java
@Autowired
private RAGOptimizationService optimizationService;

// 方式1：使用便捷方法
String id = optimizationService.savePPLData(
    "doc-123",
    List.of("point1", "point2", "point3"),
    Map.of("point1", 0.9f, "point2", 0.8f),
    "v1.0"
);

// 方式2：使用通用方法
Map<String, Object> data = Map.of(
    "probablePoints", List.of("point1", "point2"),
    "scores", Map.of("point1", 0.9f),
    "modelVersion", "v1.0"
);
optimizationService.saveOptimizationData(
    "doc-123", 
    OptimizationType.PPL.getCode(), 
    data
);
```

### 示例2：保存HyDE优化数据

```java
// 使用便捷方法
optimizationService.saveHyDEData(
    "doc-456",
    "这是一个假设性文档...",
    new float[]{0.1f, 0.2f, 0.3f},  // embedding向量
    0.85                             // 相似度
);
```

### 示例3：保存Rerank优化数据

```java
optimizationService.saveRerankData(
    "doc-789",
    List.of(2, 0, 1, 3),           // 重排序后的索引
    List.of(0.95, 0.88, 0.76, 0.65), // 分数
    "cross-encoder-v1"              // 模型名称
);
```

### 示例4：保存查询扩展数据

```java
optimizationService.saveQueryExpansionData(
    "doc-101",
    List.of(
        "原始查询",
        "扩展查询1",
        "扩展查询2"
    ),
    Map.of(
        "原始查询", 1.0,
        "扩展查询1", 0.8,
        "扩展查询2", 0.6
    )
);
```

### 示例5：保存上下文压缩数据

```java
String original = "很长的上下文内容...";
String compressed = "压缩后的关键内容...";
double ratio = (double) compressed.length() / original.length();

optimizationService.saveContextCompressionData(
    "doc-202",
    original,
    compressed,
    ratio
);
```

### 示例6：获取优化数据

```java
// 获取指定类型的优化数据
Optional<OptimizationData> pplData = optimizationService.getOptimizationData(
    "doc-123", 
    OptimizationType.PPL.getCode()
);

if (pplData.isPresent()) {
    OptimizationData data = pplData.get();
    List<String> points = (List<String>) data.getData().get("probablePoints");
    Map<String, Float> scores = (Map<String, Float>) data.getData().get("scores");
}

// 获取文档的所有优化数据
List<OptimizationData> allData = optimizationService.getAllOptimizationData("doc-123");
allData.forEach(data -> {
    System.out.println("Type: " + data.getOptimizationType());
    System.out.println("Data: " + data.getData());
});

// 获取文档的优化类型列表
List<String> types = optimizationService.getOptimizationTypes("doc-123");
// 例如: ["ppl", "hyde", "rerank"]
```

### 示例7：自定义优化算法

```java
// 保存自定义算法数据
Map<String, Object> customData = Map.of(
    "algorithm", "MyCustomAlgorithm",
    "parameters", Map.of("param1", 10, "param2", "value"),
    "results", List.of("result1", "result2")
);

Map<String, Object> metadata = Map.of(
    "author", "Your Name",
    "description", "Custom optimization algorithm"
);

Map<String, Double> metrics = Map.of(
    "precisionGain", 15.5,
    "recallGain", 12.3,
    "processingTime", 125.0
);

optimizationService.saveOptimizationData(
    "doc-303",
    "my_custom_algorithm",  // 自定义类型
    customData,
    metadata,
    metrics
);
```

---

## 🔄 向后兼容

原有的 `PPLStorageService` 保留，但已标记为 `@Deprecated`，内部委托给新的 `RAGOptimizationService`。

### 旧代码（仍然可用）
```java
@Autowired
private PPLStorageService pplService;

pplService.savePPLData("doc-123", "content", "metadata");
Optional<PPLData> data = pplService.getPPLData("doc-123");
```

### 新代码（推荐）
```java
@Autowired
private RAGOptimizationService optimizationService;

optimizationService.savePPLData("doc-123", points, scores, version);
Optional<OptimizationData> data = optimizationService.getOptimizationData(
    "doc-123", 
    OptimizationType.PPL.getCode()
);
```

---

## 🚀 实际应用场景

### 场景1：多算法组合优化

```java
String docId = "important-doc";

// 1. 应用语义分块
optimizationService.saveOptimizationData(
    docId, "semantic_chunking",
    Map.of("chunkMethod", "semantic", "threshold", 0.7)
);

// 2. 应用查询扩展
optimizationService.saveQueryExpansionData(
    docId, expandedQueries, weights
);

// 3. 应用语义重排序
optimizationService.saveRerankData(
    docId, rerankedIndices, scores, model
);

// 4. 检查应用的优化类型
List<String> applied = optimizationService.getOptimizationTypes(docId);
System.out.println("Applied optimizations: " + applied);
// 输出: [semantic_chunking, query_expansion, rerank]
```

### 场景2：A/B测试不同优化算法

```java
// 测试PPL vs HyDE
String docId = "test-doc";

// 方案A：PPL
optimizationService.savePPLData(docId, pplPoints, pplScores, "v1.0");

// 方案B：HyDE
optimizationService.saveHyDEData(docId, hypotheticalDoc, embedding, similarity);

// 对比效果
OptimizationData pplResult = optimizationService.getOptimizationData(
    docId, "ppl"
).orElse(null);

OptimizationData hydeResult = optimizationService.getOptimizationData(
    docId, "hyde"
).orElse(null);

// 根据metrics选择最优算法
```

### 场景3：动态选择优化策略

```java
public String optimizeDocument(String docId, String question) {
    // 根据问题类型选择优化策略
    String optimizationType = selectOptimizationType(question);
    
    switch (optimizationType) {
        case "ppl":
            return applyPPLOptimization(docId, question);
        case "hyde":
            return applyHyDEOptimization(docId, question);
        case "rerank":
            return applyRerankOptimization(docId, question);
        default:
            return defaultOptimization(docId, question);
    }
}

private String selectOptimizationType(String question) {
    if (question.length() < 20) {
        return "query_expansion";  // 短查询用查询扩展
    } else if (isComplexQuestion(question)) {
        return "hyde";             // 复杂问题用HyDE
    } else {
        return "ppl";              // 默认用PPL
    }
}
```

---

## 📊 性能监控

利用 `metrics` 字段记录优化效果：

```java
Map<String, Double> metrics = Map.of(
    "precisionGain", 18.5,      // 精度提升18.5%
    "recallGain", 12.3,         // 召回率提升12.3%
    "processingTime", 125.0,    // 处理时间125ms
    "qualityScore", 0.88        // 质量评分0.88
);

optimizationService.saveOptimizationData(
    docId, optimizationType, data, metadata, metrics
);

// 后续分析
OptimizationData result = optimizationService.getOptimizationData(
    docId, optimizationType
).orElse(null);

if (result != null) {
    double precisionGain = result.getMetric("precisionGain");
    System.out.println("Precision improved by: " + precisionGain + "%");
}
```

---

## 🎯 最佳实践

### 1. 选择合适的优化算法

| 场景 | 推荐算法 | 理由 |
|------|----------|------|
| 短查询 | QUERY_EXPANSION | 提高召回率 |
| 复杂问题 | HYDE + RERANK | 提高理解能力 |
| 长文档 | CONTEXT_COMPRESSION | 减少token消耗 |
| 技术文档 | SEMANTIC_CHUNKING + METADATA_FILTER | 保持结构完整性 |
| 实时系统 | PPL + HYBRID_SEARCH | 平衡速度和精度 |

### 2. 组合使用多种算法

```java
// 推荐的组合方案
String docId = "production-doc";

// 第一层：文档处理
optimizationService.saveOptimizationData(
    docId, "semantic_chunking", chunkingData
);

// 第二层：查询优化
optimizationService.saveQueryExpansionData(
    docId, expandedQueries, weights
);

// 第三层：结果优化
optimizationService.saveRerankData(
    docId, rerankedIndices, scores, model
);

// 第四层：上下文管理
optimizationService.saveContextCompressionData(
    docId, original, compressed, ratio
);
```

### 3. 记录和分析性能指标

```java
// 保存时记录指标
Map<String, Double> metrics = new HashMap<>();
metrics.put("precisionGain", calculatePrecisionGain());
metrics.put("recallGain", calculateRecallGain());
metrics.put("processingTime", measureProcessingTime());

optimizationService.saveOptimizationData(
    docId, type, data, metadata, metrics
);

// 定期分析最优算法
List<OptimizationData> allOptimizations = 
    optimizationService.getAllOptimizationData(docId);

OptimizationData bestOptimization = allOptimizations.stream()
    .max(Comparator.comparing(d -> d.getMetric("precisionGain")))
    .orElse(null);
```

---

## 🔧 扩展自定义算法

```java
// 1. 定义自己的算法类型
public class MyCustomOptimization {
    
    private final RAGOptimizationService service;
    
    public String applyCustomAlgorithm(String docId, String input) {
        // 你的算法逻辑
        Map<String, Object> results = runMyAlgorithm(input);
        
        // 保存结果
        Map<String, Object> data = Map.of(
            "input", input,
            "output", results.get("output"),
            "confidence", results.get("confidence")
        );
        
        Map<String, Double> metrics = Map.of(
            "accuracyScore", (Double) results.get("accuracy"),
            "performanceScore", (Double) results.get("performance")
        );
        
        return service.saveOptimizationData(
            docId, 
            "my_custom_algorithm",  // 自定义类型名
            data, 
            null, 
            metrics
        );
    }
    
    private Map<String, Object> runMyAlgorithm(String input) {
        // 你的算法实现
        return Map.of(
            "output", "处理后的结果",
            "confidence", 0.95,
            "accuracy", 0.92,
            "performance", 98.5
        );
    }
}
```

---

## 📝 总结

### ✅ 重构优势

1. **通用化**：从PPL单一算法扩展到13+种算法支持
2. **可扩展**：轻松添加自定义优化算法
3. **灵活性**：Map存储结构适应不同算法需求
4. **向后兼容**：旧代码无需修改仍可正常运行
5. **性能监控**：内置metrics支持效果评估

### 🎯 使用建议

- **新项目**：直接使用 `RAGOptimizationService`
- **旧项目**：逐步迁移，利用向后兼容性
- **生产环境**：组合使用多种算法，记录性能指标
- **A/B测试**：对比不同算法效果，选择最优方案

---

**文档版本**: v2.0  
**创建时间**: 2025-12-17  
**更新时间**: 2025-12-17  
**维护团队**: OmniAgent Team

