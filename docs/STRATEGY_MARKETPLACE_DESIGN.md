# 🎯 策略市场（Strategy Marketplace）设计文档

**版本**: v1.0  
**日期**: 2025-12-19  
**状态**: ✅ 接口设计完成

---

## 📋 设计目标

创建一个可扩展的策略市场接口，支持：
1. ✅ **多种策略类型**：分块、重排序、查询扩展、向量化等
2. ✅ **版本隔离**：避免版本冲突
3. ✅ **安全沙箱**：防止恶意代码
4. ✅ **资源限制**：防止资源耗尽
5. ✅ **性能监控**：跟踪策略性能
6. ✅ **优雅降级**：失败时自动降级

---

## 🏗️ 核心架构

### 1. MarketplaceStrategy 接口

**核心接口**，定义所有策略的统一规范。

```java
public interface MarketplaceStrategy {
    // 基本信息
    String getStrategyId();
    String getStrategyName();
    StrategyCategory getCategory();
    String getVersion();
    
    // 兼容性和依赖
    String getRequiredFrameworkVersion();
    List<StrategyDependency> getDependencies();
    CompatibilityCheck checkCompatibility();
    
    // 配置和参数
    String getParameterSchema();  // JSON Schema
    Map<String, Object> getDefaultParameters();
    ValidationResult validateParameters(Map<String, Object> params);
    
    // 执行
    <I, O> ExecutionResult<O> execute(...);
    
    // 生命周期
    void initialize(Map<String, Object> config);
    void destroy();
    HealthStatus checkHealth();
    
    // 性能和监控
    PerformanceMetrics getMetrics();
    ResourceUsage getResourceUsage();
    ExecutionLimits getLimits();
    
    // 安全
    List<Permission> getRequiredPermissions();
    SecurityLevel getSecurityLevel();
    
    // 测试和验证
    TestResult runSelfTest();
    List<UsageExample> getExamples();
}
```

---

## 🎨 关键设计决策

### 1. 早期规避的隐藏问题

#### ❌ 问题1：版本冲突

**问题**：不同策略依赖不同版本的库，导致冲突。

**解决方案**：
```java
// 策略ID包含版本信息
String getStrategyId(); // com.example.chunking.semantic.v1

// 声明框架版本要求
String getRequiredFrameworkVersion(); // >=3.0.0 <4.0.0

// 依赖管理
List<StrategyDependency> getDependencies();
```

#### ❌ 问题2：资源耗尽

**问题**：恶意或buggy策略消耗大量资源。

**解决方案**：
```java
// 资源限制
ExecutionLimits getLimits();
- timeoutMs: 30000  // 超时
- maxMemoryBytes: 512MB  // 内存限制
- maxCpuTimeMs: 10000  // CPU时间限制
- maxConcurrentExecutions: 10  // 并发限制

// 执行上下文
ExecutionContext context = ExecutionContext.builder()
    .timeoutMs(30000)
    .maxMemoryBytes(512 * 1024 * 1024)
    .build();
```

#### ❌ 问题3：安全漏洞

**问题**：策略可能执行危险操作（文件删除、网络攻击）。

**解决方案**：
```java
// 安全级别声明
SecurityLevel getSecurityLevel();
- SAFE: 纯配置，无代码执行
- SANDBOXED: 沙箱隔离
- TRUSTED: 经过审核
- UNRESTRICTED: 需要管理员权限

// 权限系统
List<Permission> getRequiredPermissions();
- FILE_READ, FILE_WRITE
- NETWORK_ACCESS
- DATABASE_ACCESS
```

#### ❌ 问题4：参数验证缺失

**问题**：无效参数导致运行时错误。

**解决方案**：
```java
// JSON Schema 定义参数
String getParameterSchema(); // 返回 JSON Schema

// 参数验证
ValidationResult validateParameters(Map<String, Object> params);

// 验证结果包含详细错误信息
ValidationError {
    parameterName: "chunkSize"
    errorType: OUT_OF_RANGE
    message: "chunkSize must be between 100 and 2000"
    expectedValue: "100-2000"
    actualValue: 50
}
```

#### ❌ 问题5：性能监控缺失

**问题**：无法追踪策略性能问题。

**解决方案**：
```java
// 自动收集性能指标
PerformanceMetrics getMetrics();
- totalExecutions: 1000
- averageLatencyMs: 45.2
- p95LatencyMs: 120.0
- successRate: 0.98

// 资源使用监控
ResourceUsage getResourceUsage();
- currentMemoryBytes
- peakMemoryBytes
- cpuTimeMs
```

#### ❌ 问题6：依赖地狱

**问题**：策略之间的依赖关系复杂。

**解决方案**：
```java
// 声明式依赖
StrategyDependency {
    dependencyId: "com.example.tokenizer.v1"
    versionRange: ">=1.0.0 <2.0.0"
    optional: false
    type: STRATEGY | LIBRARY | SERVICE | MODEL
}

// 自动依赖检查
CompatibilityCheck checkCompatibility();
- dependenciesMet: true
- missingDependencies: []
```

#### ❌ 问题7：调试困难

**问题**：策略失败时难以排查。

**解决方案**：
```java
// 详细的执行结果
ExecutionResult<T> {
    success: false
    error: "Parameter validation failed"
    executionTimeMs: 120
    metadata: {
        "errorCode": "INVALID_PARAMETERS",
        "stackTrace": "...",
        "requestId": "req-123"
    }
    warnings: ["Memory usage high"]
}

// 自测试
TestResult runSelfTest();

// 使用示例
List<UsageExample> getExamples();
```

---

## 📦 类型层次结构

```
MarketplaceStrategy (接口)
    ↓
AbstractMarketplaceStrategy (抽象基类)
    ↓
具体策略实现
    ├─ SimpleUppercaseStrategy (示例)
    ├─ SemanticChunkingStrategy (分块)
    ├─ PPLRerankStrategy (重排序)
    └─ QueryExpansionStrategy (查询扩展)
```

---

## 🔧 使用示例

### 实现一个策略

```java
public class MyCustomStrategy extends AbstractMarketplaceStrategy {
    
    @Override
    public String getStrategyName() {
        return "My Custom Strategy";
    }
    
    @Override
    public StrategyCategory getCategory() {
        return StrategyCategory.CHUNKING;
    }
    
    @Override
    public String getDescription() {
        return "My awesome strategy";
    }
    
    @Override
    protected <I, O> O doExecute(I input, Map<String, Object> params, 
                                 ExecutionContext context) 
            throws StrategyExecutionException {
        // 实现你的逻辑
        return (O) processInput(input, params);
    }
    
    @Override
    public String getParameterSchema() {
        return """
            {
              "type": "object",
              "properties": {
                "threshold": {
                  "type": "number",
                  "minimum": 0,
                  "maximum": 1,
                  "default": 0.5
                }
              }
            }
            """;
    }
}
```

### 注册和使用策略

```java
// 1. 注册策略
StrategyMarketplaceManager manager = ...;
manager.registerStrategy(new MyCustomStrategy());

// 2. 执行策略
ExecutionResult<String> result = manager.executeStrategy(
    "my_custom_strategy",  // 策略ID或别名
    "input data",
    Map.of("threshold", 0.7)
);

if (result.isSuccess()) {
    String output = result.getData();
    System.out.println("结果: " + output);
} else {
    System.err.println("错误: " + result.getError());
}
```

### 查询和管理

```java
// 列出所有策略
List<StrategyMetadata> all = manager.listAllStrategies();

// 按类别查询
List<StrategyMetadata> chunking = manager.listStrategiesByCategory(
    StrategyCategory.CHUNKING
);

// 搜索策略
List<StrategyMetadata> results = manager.searchStrategies("semantic");

// 健康检查
Map<String, HealthStatus> health = manager.checkAllHealth();

// 性能报告
Map<String, PerformanceMetrics> performance = manager.getPerformanceReport();

// 统计信息
Map<String, Object> stats = manager.getStatistics();
```

---

## 🎯 策略类别

```java
public enum StrategyCategory {
    CHUNKING,              // 分块策略
    RERANK,                // 重排序策略
    QUERY_EXPANSION,       // 查询扩展策略
    EMBEDDING,             // 向量化策略
    PROMPT_OPTIMIZATION,   // 提示词优化策略
    CUSTOM                 // 自定义策略
}
```

---

## 🔐 安全级别

```java
public enum SecurityLevel {
    SAFE,          // 完全安全（纯配置）
    SANDBOXED,     // 沙箱隔离
    TRUSTED,       // 可信任（经过审核）
    UNRESTRICTED   // 无限制（需要管理员权限）
}
```

---

## 📊 性能指标

```java
PerformanceMetrics {
    totalExecutions: 10000
    successCount: 9800
    failureCount: 200
    
    averageLatencyMs: 45.2
    p50LatencyMs: 35.0
    p95LatencyMs: 120.0
    p99LatencyMs: 250.0
    minLatencyMs: 10.0
    maxLatencyMs: 500.0
    
    throughput: 100.0  // 次/秒
    successRate: 0.98
}
```

---

## ✅ 验证清单

- [x] 核心接口设计完成
- [x] 类型定义完成
- [x] 异常类定义完成
- [x] 抽象基类实现
- [x] 示例策略实现
- [x] 策略管理器实现
- [x] 早期问题规避
- [x] 文档完成
- [ ] 单元测试（TODO）
- [ ] 与现有分块策略集成（TODO）
- [ ] UI 可视化（TODO）

---

## 🚀 下一步

### 1. 集成现有分块策略

将现有的 5 个分块策略适配到市场接口：

```java
public class MarketplaceChunkingAdapter extends AbstractMarketplaceStrategy {
    private final ChunkingStrategy delegate;
    
    @Override
    protected <I, O> O doExecute(...) {
        return (O) delegate.chunk(...);
    }
}
```

### 2. 实现沙箱隔离

```java
public class SandboxExecutor {
    public <T> T executeInSandbox(Callable<T> task, ExecutionLimits limits) {
        // 使用 SecurityManager 或容器技术实现隔离
    }
}
```

### 3. UI 可视化

- 策略浏览器
- 参数配置界面
- 性能监控面板
- 测试工具

---

## 📚 文件清单

```
omni-agent-marketplace/
└── src/main/java/.../marketplace/strategy/
    ├── MarketplaceStrategy.java          ✅ 核心接口
    ├── StrategyTypes.java                ✅ 类型定义
    ├── StrategyExecutionException.java   ✅ 执行异常
    ├── StrategyInitializationException.java ✅ 初始化异常
    ├── AbstractMarketplaceStrategy.java  ✅ 抽象基类
    ├── StrategyMarketplaceManager.java   ✅ 管理器
    └── examples/
        └── SimpleUppercaseStrategy.java  ✅ 示例
```

---

## 🎉 总结

### ✅ 已解决的早期问题

1. **版本冲突** - 通过版本声明和语义化版本管理
2. **资源耗尽** - 通过 ExecutionLimits 限制
3. **安全漏洞** - 通过安全级别和权限系统
4. **参数验证** - 通过 JSON Schema 和验证框架
5. **性能监控** - 通过自动指标收集
6. **依赖管理** - 通过声明式依赖和兼容性检查
7. **调试困难** - 通过详细错误信息和自测试

### 🎯 核心优势

- ✅ **可扩展** - 易于添加新策略
- ✅ **安全** - 多层安全机制
- ✅ **可监控** - 完整的性能指标
- ✅ **易用** - 抽象基类简化实现
- ✅ **健壮** - 完善的错误处理
- ✅ **向后兼容** - 版本控制机制

---

**设计完成！接口已就绪，可以开始集成和实现具体策略。** 🎉

**版本**: v1.0  
**作者**: OmniAgent Team  
**日期**: 2025-12-19

