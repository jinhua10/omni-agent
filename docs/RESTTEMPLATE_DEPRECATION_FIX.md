# RestTemplate 超时配置过时 API 修复报告

## ✅ 问题描述

在 Spring Boot 3.4.0+ 中，`RestTemplateBuilder` 的以下方法被标记为过时并计划移除：
- `setConnectTimeout(Duration)`
- `setReadTimeout(Duration)`
- `ClientHttpRequestFactorySettings`（也在 3.4.0 中过时）

## 🔧 解决方案

使用 `SimpleClientHttpRequestFactory` 直接配置超时，然后通过 `requestFactory()` 方法传递给 `RestTemplateBuilder`。

### Before（过时的方式）❌
```java
RestTemplate restTemplate = new RestTemplateBuilder()
    .setConnectTimeout(Duration.ofSeconds(30))  // ❌ 过时
    .setReadTimeout(Duration.ofSeconds(30))     // ❌ 过时
    .build();
```

### After（推荐的方式）✅
```java
// 配置请求工厂以设置超时
SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
requestFactory.setConnectTimeout(Duration.ofSeconds(30));
requestFactory.setReadTimeout(Duration.ofSeconds(30));

RestTemplate restTemplate = new RestTemplateBuilder()
    .requestFactory(() -> requestFactory)  // ✅ 使用 lambda 提供工厂
    .build();
```

## 📝 修复的文件

### 1. VisionLLMStrategy.java
**路径**: `omni-agent-web/src/main/java/top/yumbo/ai/omni/web/util/parser/image/VisionLLMStrategy.java`

**修改内容**:
```java
// 添加导入
import org.springframework.http.client.SimpleClientHttpRequestFactory;

// 修改配置代码
SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
requestFactory.setConnectTimeout(Duration.ofSeconds(DEFAULT_TIMEOUT));
requestFactory.setReadTimeout(Duration.ofSeconds(DEFAULT_TIMEOUT));

this.restTemplate = new RestTemplateBuilder()
        .requestFactory(() -> requestFactory)
        .build();
```

### 2. MarketplaceAutoConfiguration.java
**路径**: `omni-agent-marketplace/src/main/java/top/yumbo/ai/omni/marketplace/config/MarketplaceAutoConfiguration.java`

**修改内容**:
```java
// 添加导入
import org.springframework.http.client.SimpleClientHttpRequestFactory;

// 修改 httpClientAdapter() 方法
SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
requestFactory.setConnectTimeout(Duration.ofSeconds(30));
requestFactory.setReadTimeout(Duration.ofSeconds(30));

RestTemplate restTemplate = new RestTemplateBuilder()
        .requestFactory(() -> requestFactory)
        .build();
```

### 3. OllamaAutoConfiguration.java
**路径**: `omni-agent-ai-starter-ollama/src/main/java/top/yumbo/ai/ai/ollama/OllamaAutoConfiguration.java`

**修改内容**:
```java
// 添加导入
import org.springframework.http.client.SimpleClientHttpRequestFactory;

// 修改 ollamaRestTemplate() 方法
SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
requestFactory.setConnectTimeout(Duration.ofMillis(properties.getTimeout()));
requestFactory.setReadTimeout(Duration.ofMillis(properties.getTimeout()));

return new RestTemplateBuilder()
        .requestFactory(() -> requestFactory)
        .build();
```

### 4. OnlineAPIAutoConfiguration.java
**路径**: `omni-agent-ai-starter-online-api/src/main/java/top/yumbo/ai/ai/online/OnlineAPIAutoConfiguration.java`

**修改内容**:
```java
// 添加导入
import org.springframework.http.client.SimpleClientHttpRequestFactory;

// 修改 onlineApiRestTemplate() 方法
SimpleClientHttpRequestFactory requestFactory = new SimpleClientHttpRequestFactory();
requestFactory.setConnectTimeout(Duration.ofMillis(properties.getTimeout()));
requestFactory.setReadTimeout(Duration.ofMillis(properties.getTimeout()));

return new RestTemplateBuilder()
        .requestFactory(() -> requestFactory)
        .build();
```

## ✅ 验证结果

```bash
mvn compile -pl omni-agent-web,omni-agent-marketplace,omni-agent-ai-starter-ollama,omni-agent-ai-starter-online-api
# ✅ 编译成功，无警告
```

## 📊 修复统计

| 文件 | 行数变化 | 状态 |
|------|---------|------|
| VisionLLMStrategy.java | +7, -2 | ✅ |
| MarketplaceAutoConfiguration.java | +5, -2 | ✅ |
| OllamaAutoConfiguration.java | +5, -2 | ✅ |
| OnlineAPIAutoConfiguration.java | +5, -2 | ✅ |
| **总计** | **+22, -8** | ✅ |

## 🎯 技术说明

### 为什么这样修改？

1. **Spring Boot 3.4.0+ 的变化**:
   - `setConnectTimeout()` 和 `setReadTimeout()` 被标记为过时
   - `ClientHttpRequestFactorySettings` 也被标记为过时
   - 推荐直接配置 `ClientHttpRequestFactory`

2. **SimpleClientHttpRequestFactory**:
   - 这是 Spring 提供的默认 HTTP 请求工厂
   - 支持直接设置连接超时和读取超时
   - 轻量级，适合大多数场景

3. **Lambda 表达式 `() -> requestFactory`**:
   - `requestFactory()` 方法接受 `Supplier<ClientHttpRequestFactory>`
   - 使用 lambda 延迟创建请求工厂实例
   - 保证线程安全

### 其他可选方案

如果需要更高级的 HTTP 配置（如连接池），可以使用：
- `HttpComponentsClientHttpRequestFactory`（Apache HttpClient）
- `OkHttp3ClientHttpRequestFactory`（OkHttp）

示例：
```java
// 使用 Apache HttpClient
HttpComponentsClientHttpRequestFactory factory = new HttpComponentsClientHttpRequestFactory();
factory.setConnectTimeout(Duration.ofSeconds(30));
factory.setReadTimeout(Duration.ofSeconds(30));

RestTemplate restTemplate = new RestTemplateBuilder()
    .requestFactory(() -> factory)
    .build();
```

## 📚 参考资料

- [Spring Boot 3.4.0 Release Notes](https://github.com/spring-projects/spring-boot/wiki/Spring-Boot-3.4-Release-Notes)
- [RestTemplateBuilder JavaDoc](https://docs.spring.io/spring-boot/docs/current/api/org/springframework/boot/web/client/RestTemplateBuilder.html)
- [ClientHttpRequestFactory JavaDoc](https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/http/client/ClientHttpRequestFactory.html)

## 🎉 总结

✅ **所有过时 API 警告已修复**
- 4 个文件全部更新
- 使用 Spring Boot 3.4+ 推荐的方式
- 保持原有功能不变
- 编译成功，无警告

---

**修复时间**: 2025-12-25  
**修复人员**: OmniAgent Team  
**状态**: ✅ **完成**

