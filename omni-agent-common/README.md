# OmniAgent Common

**版本：** 1.0.0  
**描述：** 通用工具模块 - HTTP客户端适配器

---

## 📋 模块概述

`omni-agent-common` 是 OmniAgent 项目的基础工具模块，提供了灵活的 HTTP 客户端适配器，支持多种实现方式。

### 核心功能

- ✅ **HTTP客户端适配器** - 统一的HTTP请求接口
- ✅ **多实现支持** - RestTemplate 和 OkHttp3
- ✅ **URL验证** - 完善的URL格式验证工具
- ✅ **高性能** - 支持连接池、超时配置等

---

## 🏗️ 模块结构

```
omni-agent-common/
├── src/
│   ├── main/java/top/yumbo/ai/omni/common/http/
│   │   ├── HttpClientAdapter.java        # HTTP客户端适配器接口
│   │   ├── OkHttp3Adapter.java          # OkHttp3实现
│   │   ├── RestTemplateAdapter.java     # RestTemplate实现
│   │   └── UrlValidator.java            # URL验证工具
│   └── test/java/top/yumbo/ai/omni/common/http/
│       ├── HttpClientAdapterTest.java    # (接口测试)
│       ├── OkHttp3AdapterTest.java      # OkHttp3单元测试
│       ├── RestTemplateAdapterTest.java # RestTemplate单元测试
│       └── UrlValidatorTest.java        # URL验证单元测试
└── pom.xml
```

---

## 🚀 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-common</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 2. 使用RestTemplate适配器（零依赖）

```java
import top.yumbo.ai.omni.common.http.HttpClientAdapter;
import top.yumbo.ai.omni.common.http.RestTemplateAdapter;
import org.springframework.web.client.RestTemplate;

// 创建适配器
RestTemplate restTemplate = new RestTemplate();
HttpClientAdapter client = new RestTemplateAdapter(restTemplate);

// GET请求
Map<String, String> headers = new HashMap<>();
headers.put("Accept", "application/json");
String response = client.get("https://api.example.com/users", headers);

// POST请求
String body = "{\"name\":\"John\"}";
headers.put("Content-Type", "application/json");
String postResponse = client.post("https://api.example.com/users", headers, body);
```

### 3. 使用OkHttp3适配器（高性能）

```java
import top.yumbo.ai.omni.common.http.HttpClientAdapter;
import top.yumbo.ai.omni.common.http.OkHttp3Adapter;

// 创建适配器（使用默认配置）
HttpClientAdapter client = new OkHttp3Adapter();

// 发送请求
String response = client.get("https://api.example.com/data", null);

// 自定义超时
client.setTimeout(30, 60);  // 连接30秒，读取60秒
```

---

## 💡 详细使用指南

### HTTP方法支持

#### GET请求

```java
HttpClientAdapter client = new OkHttp3Adapter();

Map<String, String> headers = new HashMap<>();
headers.put("Authorization", "Bearer token");

String response = client.get("https://api.example.com/users/123", headers);
```

#### POST请求

```java
String jsonBody = "{\"name\":\"John\",\"age\":30}";

Map<String, String> headers = new HashMap<>();
headers.put("Content-Type", "application/json");

String response = client.post("https://api.example.com/users", headers, jsonBody);
```

#### PUT请求

```java
String updateData = "{\"name\":\"John Updated\"}";
String response = client.put("https://api.example.com/users/123", headers, updateData);
```

#### DELETE请求

```java
Map<String, String> headers = new HashMap<>();
headers.put("Authorization", "Bearer token");

String response = client.delete("https://api.example.com/users/123", headers);
```

### URL验证

```java
import top.yumbo.ai.omni.common.http.UrlValidator;

// 基础验证
try {
    UrlValidator.validateBasic("https://example.com");
    // URL有效
} catch (IllegalArgumentException e) {
    // URL无效
}

// 完整验证（推荐）
UrlValidator.validateFull("https://api.example.com/v1/users");

// 严格验证
UrlValidator.validateStrict("https://example.com:443/path");

// 检查是否有效
boolean isValid = UrlValidator.isValid("https://example.com");

// 检查是否HTTPS
boolean isHttps = UrlValidator.isHttps("https://secure.com");

// 规范化URL
String normalized = UrlValidator.normalize("  https://example.com  ");
```

### 配置超时

#### OkHttp3Adapter - 动态配置

```java
HttpClientAdapter client = new OkHttp3Adapter();

// 动态设置超时（支持）
client.setTimeout(30, 60);
```

#### RestTemplateAdapter - 创建时配置

```java
@Configuration
public class HttpClientConfig {
    
    @Bean
    public RestTemplate restTemplate(RestTemplateBuilder builder) {
        return builder
            .setConnectTimeout(Duration.ofSeconds(30))
            .setReadTimeout(Duration.ofSeconds(60))
            .build();
    }
    
    @Bean
    public HttpClientAdapter httpClientAdapter(RestTemplate restTemplate) {
        return new RestTemplateAdapter(restTemplate);
    }
}
```

### 自定义OkHttp配置

```java
import okhttp3.OkHttpClient;
import okhttp3.ConnectionPool;
import java.util.concurrent.TimeUnit;

OkHttpClient customClient = new OkHttpClient.Builder()
    .connectTimeout(30, TimeUnit.SECONDS)
    .readTimeout(60, TimeUnit.SECONDS)
    .writeTimeout(60, TimeUnit.SECONDS)
    .connectionPool(new ConnectionPool(50, 5, TimeUnit.MINUTES))
    .retryOnConnectionFailure(true)
    .build();

HttpClientAdapter client = new OkHttp3Adapter(customClient);
```

---

## 🎯 最佳实践

### 1. 选择合适的实现

| 场景 | 推荐实现 | 原因 |
|------|---------|------|
| 简单项目 | RestTemplateAdapter | Spring自带，零依赖 |
| 生产环境 | OkHttp3Adapter | 高性能，连接池管理 |
| 高频调用 | OkHttp3Adapter | HTTP/2支持，性能更好 |
| 低频调用 | RestTemplateAdapter | 简单够用 |

### 2. 异常处理

```java
try {
    String response = client.get(url, headers);
    // 处理响应
} catch (IllegalArgumentException e) {
    // URL格式错误
    log.error("Invalid URL: {}", url, e);
} catch (Exception e) {
    // HTTP请求失败（4xx, 5xx等）
    log.error("HTTP request failed: {}", url, e);
}
```

### 3. 空值安全

```java
// ✅ headers可以为null
client.get("https://api.example.com", null);

// ✅ body可以为null
client.post("https://api.example.com", null, null);
```

### 4. Spring Boot集成

```java
@Configuration
public class HttpClientConfig {
    
    @Bean
    @ConditionalOnProperty(name = "http.client.type", havingValue = "okhttp", matchIfMissing = true)
    public HttpClientAdapter okHttpAdapter() {
        return new OkHttp3Adapter();
    }
    
    @Bean
    @ConditionalOnProperty(name = "http.client.type", havingValue = "resttemplate")
    public HttpClientAdapter restTemplateAdapter(RestTemplate restTemplate) {
        return new RestTemplateAdapter(restTemplate);
    }
}
```

---

## 📊 性能对比

| 实现 | 连接池 | HTTP/2 | 性能 | 内存占用 | 依赖 |
|------|--------|--------|------|---------|------|
| OkHttp3Adapter | ✅ 优秀 | ✅ 支持 | ⭐⭐⭐⭐⭐ | 中等 | okhttp3 |
| RestTemplateAdapter | ⚠️ 有限 | ❌ 不支持 | ⭐⭐⭐ | 较低 | 无（Spring自带） |

---

## 🧪 测试

### 运行测试

```bash
# 运行所有测试
mvn test

# 运行特定测试
mvn test -Dtest=UrlValidatorTest

# 查看测试报告
mvn surefire-report:report
```

### 测试覆盖率

- **总测试数：** 49个
- **测试覆盖率：** ~90%
- **通过率：** 100%

详见：[单元测试报告](../UNIT_TEST_REPORT.md)

---

## 📚 API文档

### HttpClientAdapter接口

```java
public interface HttpClientAdapter {
    // HTTP方法
    String get(String url, Map<String, String> headers) throws Exception;
    String post(String url, Map<String, String> headers, String body) throws Exception;
    String put(String url, Map<String, String> headers, String body) throws Exception;
    String delete(String url, Map<String, String> headers) throws Exception;
    
    // 超时配置
    void setTimeout(int connectTimeoutSeconds, int readTimeoutSeconds);
    
    // URL验证
    void validateUrl(String url);
    
    // 元数据
    String getName();
}
```

### UrlValidator工具类

```java
public class UrlValidator {
    // 验证方法
    static void validateBasic(String url);      // 基础验证
    static void validateFull(String url);       // 完整验证（推荐）
    static void validateStrict(String url);     // 严格验证
    
    // 辅助方法
    static boolean isValid(String url);         // 检查是否有效
    static boolean isHttps(String url);         // 检查是否HTTPS
    static String normalize(String url);        // 规范化URL
}
```

---

## 🔧 依赖说明

### 必需依赖

```xml
<!-- Spring Web (for RestTemplate) -->
<dependency>
    <groupId>org.springframework</groupId>
    <artifactId>spring-web</artifactId>
</dependency>
```

### 可选依赖

```xml
<!-- OkHttp3 (可选，高性能) -->
<dependency>
    <groupId>com.squareup.okhttp3</groupId>
    <artifactId>okhttp</artifactId>
    <optional>true</optional>
</dependency>
```

### 测试依赖

```xml
<!-- JUnit 5 -->
<dependency>
    <groupId>org.junit.jupiter</groupId>
    <artifactId>junit-jupiter</artifactId>
    <scope>test</scope>
</dependency>

<!-- Mockito -->
<dependency>
    <groupId>org.mockito</groupId>
    <artifactId>mockito-core</artifactId>
    <scope>test</scope>
</dependency>

<!-- MockWebServer -->
<dependency>
    <groupId>com.squareup.okhttp3</groupId>
    <artifactId>mockwebserver</artifactId>
    <scope>test</scope>
</dependency>
```

---

## 📝 更新日志

### Version 1.0.0 (2025-12-31)

#### 新增功能
- ✅ 完整的HTTP客户端适配器接口
- ✅ GET、POST、PUT、DELETE方法支持
- ✅ RestTemplateAdapter实现
- ✅ OkHttp3Adapter实现
- ✅ UrlValidator验证工具
- ✅ 完整的单元测试（49个测试，100%通过）

#### 技术改进
- ✅ 移除旧的I18N类（改用Spring MessageSource）
- ✅ 移除SnakeYAML依赖
- ✅ 添加完整的测试覆盖（~90%）

---

## 🤝 贡献指南

### 代码规范

遵循项目代码规范：[docs/code_standard/README.md](../docs/code_standard/README.md)

### 提交测试

```bash
# 运行测试
mvn test

# 检查代码风格
mvn checkstyle:check

# 构建
mvn clean package
```

---

## 📄 许可证

Apache License 2.0 - 详见 [LICENSE.txt](../LICENSE.txt)

---

## 🔗 相关文档

- [HTTP客户端扩展文档](../HTTP_CLIENT_EXTENSION.md)
- [URL验证报告](../URL_VALIDATION_REPORT.md)
- [单元测试报告](../UNIT_TEST_REPORT.md)
- [批次1分析报告](../batch_01.md)

---

**维护者：** OmniAgent Team  
**最后更新：** 2025-12-31  
**模块状态：** ✅ 稳定

