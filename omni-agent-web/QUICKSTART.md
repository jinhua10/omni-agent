# 🚀 omni-agent-web 快速开始指南

## 📦 引入依赖

### Maven

```xml
<!-- 在你的Spring Boot项目中添加 -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-web</artifactId>
    <version>1.0.0</version>
</dependency>

<!-- 选择一个存储实现 -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-persistence-starter-file</artifactId>
    <version>1.0.0</version>
</dependency>

<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-document-storage-starter-file</artifactId>
    <version>1.0.0</version>
</dependency>
```

## ⚡ 零配置启动

```java
@SpringBootApplication
public class MyApplication {
    public static void main(String[] args) {
        SpringApplication.run(MyApplication.class, args);
    }
}
```

**就这样！** 所有API自动可用：

- `GET  http://localhost:8080/api/optimization/dashboard`
- `POST http://localhost:8080/api/rag/auto-select`
- `GET  http://localhost:8080/swagger-ui.html` (如果引入了springdoc)

## 📝 简单示例

### 1. 自动选择最佳算法

```bash
curl -X POST http://localhost:8080/api/rag/auto-select \
  -H "Content-Type: application/json" \
  -d '{
    "query": "如何配置Spring Boot数据库",
    "documentType": "technical",
    "latencyRequirementMs": 200,
    "precisionRequirement": 0.90
  }'
```

**响应**:
```json
{
  "primaryAlgorithms": ["ppl", "query_expansion"],
  "secondaryAlgorithms": ["hybrid_search"],
  "expectedPrecisionGain": 45.2,
  "expectedLatencyMs": 45,
  "reasoning": "基于以下因素选择算法组合..."
}
```

### 2. 查看Dashboard数据

```bash
curl http://localhost:8080/api/optimization/dashboard
```

### 3. 记录性能指标

```bash
curl -X POST http://localhost:8080/api/optimization/metrics \
  -H "Content-Type: application/json" \
  -d '{
    "documentId": "doc-123",
    "algorithmType": "ppl",
    "precisionGain": 22.5,
    "latencyMs": 15
  }'
```

## 🎨 可选配置

### application.yml

```yaml
# 以下都是可选配置，使用默认值即可

spring:
  application:
    name: my-omni-agent-app

# 如果需要自定义CORS
omni-agent:
  web:
    cors:
      allowed-origins: "https://your-domain.com"
```

## 🔧 自定义扩展

### 如果默认API不满足需求，自己实现：

```java
@RestController
@RequestMapping("/api/custom")
public class MyCustomController {
    
    @Autowired
    private RAGOptimizationService optimizationService;
    
    @PostMapping("/my-endpoint")
    public ResponseEntity<?> myEndpoint(@RequestBody MyRequest request) {
        // 你的自定义逻辑
        return ResponseEntity.ok(result);
    }
}
```

### 如果完全不需要默认API，可以禁用：

```java
@SpringBootApplication(exclude = OmniAgentWebAutoConfiguration.class)
public class MyApplication {
    // 完全自己实现
}
```

## 📊 查看API文档

添加依赖：
```xml
<dependency>
    <groupId>org.springdoc</groupId>
    <artifactId>springdoc-openapi-starter-webmvc-ui</artifactId>
    <version>2.3.0</version>
</dependency>
```

访问：`http://localhost:8080/swagger-ui.html`

## 🎯 完整示例

见 `omni-agent-example-basic` 模块。

---

**就是这么简单！引入即用，零配置启动！** 🚀

