# 🏪 算法市场实现指南

**版本**: v3.0  
**创建时间**: 2025-12-17

---

## 🎯 设计理念

算法市场允许用户上传、分享和使用自定义的 RAG 优化算法，无需编译 Java 代码。

### 核心问题

**问题**: Java 算法需要编译，如何实现动态加载？

**解决方案**: 支持 3 种算法类型

1. **Pipeline（配置化）** - 组合已有组件 ⭐⭐⭐⭐⭐
2. **Script（脚本）** - JavaScript/Python 实现 ⭐⭐⭐⭐
3. **Remote（远程）** - HTTP 服务调用 ⭐⭐⭐

---

## 📦 三种算法类型详解

### 类型1: Pipeline（配置化算法）

**原理**: 用户通过 YAML/JSON 配置，组合已有的算法组件

#### 示例：自定义检索增强Pipeline

```yaml
algorithm:
  name: "MyEnhancedSearch"
  version: "1.0"
  author: "张三"
  description: "结合查询扩展、语义分块和重排序的增强检索"
  type: "pipeline"
  
  steps:
    # 步骤1: 查询扩展
    - type: "query_expansion"
      params:
        method: "synonym"
        threshold: 0.8
        maxExpansions: 5
    
    # 步骤2: 语义分块
    - type: "semantic_chunking"
      params:
        chunkSize: 512
        overlap: 50
        model: "bge-large"
    
    # 步骤3: 重排序
    - type: "rerank"
      params:
        model: "bge-reranker"
        topK: 10
    
    # 步骤4: 条件过滤（可选）
    - type: "metadata_filter"
      condition: "context.userLevel == 'premium'"  # 只对高级用户执行
      params:
        filters:
          category: "premium"
```

#### Java 代码使用

```java
@Autowired
private AlgorithmMarketService marketService;

// 发布算法
MarketAlgorithm algorithm = MarketAlgorithm.builder()
    .name("MyEnhancedSearch")
    .version("1.0")
    .type(MarketAlgorithm.AlgorithmType.PIPELINE)
    .pipelineConfig(MarketAlgorithm.PipelineConfig.builder()
        .steps(List.of(
            MarketAlgorithm.PipelineStep.builder()
                .type("query_expansion")
                .params(Map.of("method", "synonym", "threshold", 0.8))
                .build(),
            MarketAlgorithm.PipelineStep.builder()
                .type("semantic_chunking")
                .params(Map.of("chunkSize", 512, "overlap", 50))
                .build()
        ))
        .build())
    .build();

String algorithmId = marketService.publishAlgorithm(algorithm);

// 执行算法
OptimizationData result = marketService.executeMarketAlgorithm(
    algorithmId, 
    "doc-123", 
    Map.of("query", "用户查询")
);
```

**优点**:
- ✅ 无需编译，即时生效
- ✅ 可视化配置（可做拖拽式UI）
- ✅ 安全可控
- ✅ 性能好（原生组件）

**适用场景**:
- 组合现有算法
- 快速原型验证
- 非技术用户使用

---

### 类型2: Script（脚本算法）

**原理**: 用户编写 JavaScript/Python 脚本，运行时执行

#### 示例：JavaScript 自定义算法

```javascript
// 用户上传的脚本
function optimize(documentId, context) {
    // 获取文档内容
    var document = context.document;
    var query = context.query;
    
    // 自定义算法逻辑
    var score = 0;
    var keywords = query.split(" ");
    
    for (var i = 0; i < keywords.length; i++) {
        if (document.indexOf(keywords[i]) >= 0) {
            score += 10;
        }
    }
    
    // 返回结果
    return {
        data: {
            processedDocument: document,
            relevanceScore: score,
            matchedKeywords: keywords
        },
        metrics: {
            precisionGain: score / 10.0,
            latency: 50.0
        }
    };
}
```

#### Java 代码使用

```java
String scriptCode = """
function optimize(documentId, context) {
    // ... 脚本内容
    return {
        data: { score: 95 },
        metrics: { precisionGain: 15.5 }
    };
}
""";

MarketAlgorithm algorithm = MarketAlgorithm.builder()
    .name("CustomScoreAlgorithm")
    .version("1.0")
    .type(MarketAlgorithm.AlgorithmType.SCRIPT)
    .script(scriptCode)
    .scriptLanguage("javascript")
    .build();

String algorithmId = marketService.publishAlgorithm(algorithm);

// 执行
OptimizationData result = marketService.executeMarketAlgorithm(
    algorithmId,
    "doc-456",
    Map.of("document", "文档内容", "query", "用户查询")
);
```

**优点**:
- ✅ 灵活性极高
- ✅ 支持复杂逻辑
- ✅ 无需编译

**缺点**:
- ⚠️ 性能不如原生 Java
- ⚠️ 需要沙箱隔离

**适用场景**:
- 复杂的自定义逻辑
- 快速迭代验证
- 技术用户使用

---

### 类型3: Remote（远程服务）

**原理**: 算法部署为独立的 HTTP 服务

#### 示例：Python Flask 服务

```python
# 用户的算法服务（algorithm_service.py）
from flask import Flask, request, jsonify

app = Flask(__name__)

@app.route('/execute', methods=['POST'])
def execute():
    data = request.json
    document_id = data['documentId']
    query = data['query']
    
    # 自定义算法实现
    result = my_custom_algorithm(document_id, query)
    
    return jsonify({
        'data': result,
        'metrics': {
            'precisionGain': 18.5,
            'latency': 150.0
        }
    })

if __name__ == '__main__':
    app.run(port=5000)
```

#### Java 代码使用

```java
MarketAlgorithm algorithm = MarketAlgorithm.builder()
    .name("PythonMLAlgorithm")
    .version("1.0")
    .type(MarketAlgorithm.AlgorithmType.REMOTE)
    .remoteEndpoint("http://localhost:5000")
    .build();

String algorithmId = marketService.publishAlgorithm(algorithm);

// 执行（会HTTP调用远程服务）
OptimizationData result = marketService.executeMarketAlgorithm(
    algorithmId,
    "doc-789",
    Map.of("query", "查询内容")
);
```

**优点**:
- ✅ 完全隔离，安全性最高
- ✅ 支持任何语言（Python/Go/Rust等）
- ✅ 独立扩展和部署

**缺点**:
- ❌ 网络延迟
- ❌ 部署复杂

**适用场景**:
- 使用Python深度学习模型
- 高安全性要求
- 独立团队维护的算法

---

## 🎨 算法市场UI设计

### 发布算法页面

```
┌──────────────────────────────────────┐
│  🏪 发布新算法                        │
├──────────────────────────────────────┤
│                                       │
│  算法名称: [MyCustomAlgorithm      ] │
│  版本号:   [1.0                    ] │
│  作者:     [张三                   ] │
│  描述:     [这是一个...            ] │
│                                       │
│  算法类型: ( ) Pipeline (配置化)     │
│            (•) Script (脚本)         │
│            ( ) Remote (远程服务)     │
│                                       │
│  ┌────────────────────────────────┐ │
│  │ // 脚本编辑器                  │ │
│  │ function optimize(doc, ctx) {  │ │
│  │     return {                   │ │
│  │         data: {...},           │ │
│  │         metrics: {...}         │ │
│  │     };                         │ │
│  │ }                              │ │
│  └────────────────────────────────┘ │
│                                       │
│  标签: [#RAG] [#检索] [+添加]        │
│                                       │
│  [ 验证 ]  [ 发布 ]  [ 取消 ]        │
└──────────────────────────────────────┘
```

### 算法市场浏览页面

```
┌──────────────────────────────────────┐
│  🏪 算法市场                          │
│  [搜索算法...] [我的算法] [发布新算法]│
├──────────────────────────────────────┤
│                                       │
│  🔥 热门算法                          │
│                                       │
│  ┌────────────────────────────────┐ │
│  │ EnhancedSearch v2.0    ⭐⭐⭐⭐⭐│ │
│  │ 作者: 李四    类型: Pipeline    │ │
│  │ 精度提升: +35%  延迟: 120ms     │ │
│  │ 使用: 1,234次                   │ │
│  │ [查看详情] [安装]              │ │
│  └────────────────────────────────┘ │
│                                       │
│  ┌────────────────────────────────┐ │
│  │ MLReranker v1.0       ⭐⭐⭐⭐  │ │
│  │ 作者: 王五    类型: Script      │ │
│  │ 精度提升: +28%  延迟: 200ms     │ │
│  │ 使用: 856次                     │ │
│  │ [查看详情] [安装]              │ │
│  └────────────────────────────────┘ │
└──────────────────────────────────────┘
```

---

## 🔒 安全性考虑

### Script 类型安全策略

```java
@Service
public class SecureScriptExecutor {
    
    public Object executeSecurely(String script, Map<String, Object> context) {
        // 1. 沙箱隔离
        ScriptEngine engine = createSandboxedEngine();
        
        // 2. 资源限制
        engine.put("__maxExecutionTime", 5000);  // 5秒超时
        engine.put("__maxMemory", 100 * 1024 * 1024);  // 100MB内存
        
        // 3. API白名单
        Bindings bindings = engine.createBindings();
        bindings.put("context", createSafeContext(context));
        // 不暴露敏感API
        
        // 4. 执行监控
        return executeWithTimeout(engine, script, bindings, 5000);
    }
    
    private ScriptEngine createSandboxedEngine() {
        // 使用 GraalVM 的沙箱特性
        return new ScriptEngineManager().getEngineByName("graal.js");
    }
}
```

### Pipeline 类型安全策略

- ✅ 只能使用已注册的组件
- ✅ 参数类型验证
- ✅ 资源配额限制

### Remote 类型安全策略

- ✅ HTTPS 强制
- ✅ 超时设置
- ✅ 速率限制
- ✅ 服务鉴权

---

## 📊 算法评分和推荐

### 评分体系

```java
@Service
public class AlgorithmRatingService {
    
    public void rateAlgorithm(String algorithmId, String userId, int rating) {
        // 1-5星评分
        // 计算平均分
    }
    
    public List<MarketAlgorithm> getTopRatedAlgorithms(int limit) {
        return marketService.listPublishedAlgorithms().stream()
            .sorted(Comparator.comparing(MarketAlgorithm::getRating).reversed())
            .limit(limit)
            .toList();
    }
    
    public List<MarketAlgorithm> getRecommendedAlgorithms(String userId) {
        // 基于用户历史使用推荐
        // 协同过滤算法
    }
}
```

---

## 🚀 部署和运维

### 算法版本管理

```java
@Service
public class AlgorithmVersionService {
    
    public String publishNewVersion(String algorithmId, MarketAlgorithm newVersion) {
        // 版本号自动递增
        // 保留历史版本
        // 支持回滚
    }
    
    public List<MarketAlgorithm> getAlgorithmVersions(String algorithmId) {
        // 获取所有版本
    }
}
```

### 监控和告警

```java
@Service
public class AlgorithmMonitorService {
    
    public void recordExecution(String algorithmId, boolean success, long latency) {
        // 记录执行情况
        // 计算成功率
        // 告警通知
    }
}
```

---

## 💡 最佳实践

### 1. 优先使用 Pipeline 类型

```java
// ✅ 好的做法
MarketAlgorithm.builder()
    .type(MarketAlgorithm.AlgorithmType.PIPELINE)
    .pipelineConfig(...) // 组合已有组件
    .build();
```

### 2. Script 类型添加性能指标

```javascript
function optimize(doc, ctx) {
    var startTime = Date.now();
    
    // 算法逻辑...
    
    return {
        data: result,
        metrics: {
            precisionGain: 15.5,
            latency: Date.now() - startTime
        }
    };
}
```

### 3. Remote 类型实现健康检查

```python
@app.route('/health', methods=['GET'])
def health():
    return jsonify({'status': 'ok'})
```

---

## 📝 总结

| 类型 | 复杂度 | 性能 | 安全性 | 适用场景 |
|------|--------|------|--------|----------|
| **Pipeline** | ⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | 组合现有算法 |
| **Script** | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐ | 自定义逻辑 |
| **Remote** | ⭐⭐⭐⭐⭐ | ⭐⭐ | ⭐⭐⭐⭐ | 复杂ML模型 |

**推荐策略**:
- 80% 场景用 **Pipeline**
- 15% 场景用 **Script**
- 5% 场景用 **Remote**

---

**文档版本**: v1.0  
**最后更新**: 2025-12-17  
**维护团队**: OmniAgent Team

