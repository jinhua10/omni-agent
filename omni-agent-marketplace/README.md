# 🏪 OmniAgent Marketplace

算法市场模块 - 支持用户上传、分享和使用自定义RAG优化算法

## 📦 功能特性

### 三种算法类型

1. **Pipeline（配置化）** ⭐⭐⭐⭐⭐ 推荐
   - 组合已有组件
   - 无需编译
   - 最安全

2. **Script（脚本）** ⭐⭐⭐⭐
   - JavaScript实现
   - 沙箱隔离
   - 运行时执行

3. **Remote（远程服务）** ⭐⭐⭐
   - HTTP API调用
   - 支持任何语言
   - HTTPS+认证

### 安全措施

- ✅ 算法审核机制
- ✅ 沙箱隔离执行（Script）
- ✅ 超时控制
- ✅ 资源限制
- ✅ 黑名单过滤
- ✅ SSRF防护（Remote）
- ✅ 内网访问拦截

## 🚀 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-marketplace</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 2. 自动配置

无需额外配置，引入依赖即可使用。

### 3. 使用示例

```java
@Autowired
private AlgorithmMarketService marketService;

// 发布Pipeline算法
MarketAlgorithm algorithm = MarketAlgorithm.builder()
    .name("MyCustomAlgorithm")
    .type(MarketAlgorithm.AlgorithmType.PIPELINE)
    .pipelineConfig(...)
    .build();

String algorithmId = marketService.publishAlgorithm(algorithm);

// 审核通过
marketService.approveAlgorithm(algorithmId);

// 执行算法
OptimizationData result = marketService.executeMarketAlgorithm(
    algorithmId,
    "doc-123",
    Map.of("query", "用户查询")
);
```

## 🎨 架构优势

### 独立模块设计

```
omni-agent-marketplace/  (独立模块)
├── AlgorithmMarketService
├── security/
│   ├── SecureScriptExecutor     (沙箱执行)
│   └── SecureRemoteExecutor     (HTTP调用)
└── config/
    └── MarketplaceAutoConfiguration
```

**优点**：
- ✅ 职责清晰
- ✅ 可选依赖
- ✅ 独立升级
- ✅ 可以使用 RestTemplate/OkHttp3

## 📚 详细文档

- **实现指南**: `docs/ALGORITHM_MARKET_GUIDE.md`
- **使用示例**: `AlgorithmMarketExample.java`
- **安全说明**: 见本文档安全措施章节

## 🔒 安全配置

### Script 安全

```java
// 默认配置
- 超时时间: 5秒
- 黑名单: Runtime, System, File, Socket等
- 沙箱隔离: 只暴露安全上下文
```

### Remote 安全

```java
// 默认配置
- HTTPS强制: false (开发), true (生产)
- 内网拦截: 禁止访问 127.0.0.1, 192.168.*, 10.*, 172.16-31.*
- 认证支持: Bearer Token
```

## 💡 最佳实践

1. **优先使用 Pipeline** - 80%场景
2. **Script用于复杂逻辑** - 15%场景
3. **Remote用于特殊需求** - 5%场景

## 🎯 HTTP 客户端选择

### 默认使用 RestTemplate

自动配置提供基于 RestTemplate 的实现。

### 切换到 OkHttp3（高性能）

```xml
<dependency>
    <groupId>com.squareup.okhttp3</groupId>
    <artifactId>okhttp</artifactId>
</dependency>
```

```java
@Bean
public HttpClientAdapter httpClientAdapter() {
    OkHttpClient client = new OkHttpClient.Builder()
        .connectionPool(new ConnectionPool(50, 5, TimeUnit.MINUTES))
        .build();
    return new OkHttp3Adapter(client);
}
```

---

**版本**: 3.0.0  
**作者**: OmniAgent Team  
**最后更新**: 2025-12-17

