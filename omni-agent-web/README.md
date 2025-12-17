# 🌐 OmniAgent Web

OmniAgent Web模块 - 提供开箱即用的REST API控制器，方便第三方集成使用。

## 📦 功能特性

### 核心控制器

1. **RAGOptimizationController** - RAG优化算法管理
   - 自动算法选择
   - 优化数据管理
   - 批量场景评估

2. **OptimizationDashboardController** - 性能监控Dashboard
   - 实时指标收集
   - 统计数据查询
   - 算法性能对比

### 自动配置

- ✅ 自动扫描并注册所有Controller
- ✅ 自动配置CORS跨域
- ✅ 可选的OpenAPI/Swagger文档
- ✅ 开箱即用，零配置

---

## 🚀 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-web</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 2. 启动应用

```java
@SpringBootApplication
public class YourApplication {
    public static void main(String[] args) {
        SpringApplication.run(YourApplication.class, args);
    }
}
```

**就这么简单！** 所有API自动可用。

### 3. 访问API

```bash
# 自动算法选择
POST http://localhost:8080/api/rag/auto-select

# Dashboard数据
GET http://localhost:8080/api/optimization/dashboard

# OpenAPI文档（如果启用）
GET http://localhost:8080/swagger-ui.html
```

---

## 📚 API文档

### RAG优化API

#### 1. 自动选择算法

```http
POST /api/rag/auto-select
Content-Type: application/json

{
  "query": "如何配置Spring Boot",
  "queryLength": 15,
  "documentType": "technical",
  "latencyRequirementMs": 200,
  "precisionRequirement": 0.90
}
```

**响应**:
```json
{
  "primaryAlgorithms": ["ppl", "query_expansion"],
  "secondaryAlgorithms": ["hybrid_search"],
  "algorithmScores": {
    "ppl": 0.95,
    "query_expansion": 0.85
  },
  "reasoning": "基于以下因素选择算法组合...",
  "expectedPrecisionGain": 45.2,
  "expectedLatencyMs": 45
}
```

#### 2. 保存优化数据

```http
POST /api/rag/optimization-data
Content-Type: application/json

{
  "documentId": "doc-123",
  "optimizationType": "ppl",
  "data": {
    "probablePoints": ["point1", "point2"],
    "scores": {"point1": 0.9}
  },
  "metadata": {
    "version": "v1.0"
  },
  "metrics": {
    "precisionGain": 22.5,
    "latency": 15.0
  }
}
```

#### 3. 获取优化数据

```http
GET /api/rag/optimization-data/{documentId}/{optimizationType}
```

### Dashboard API

#### 1. 获取Dashboard数据

```http
GET /api/optimization/dashboard
```

**响应**:
```json
{
  "timestamp": 1702800000000,
  "algorithmStats": {
    "ppl": {
      "totalExecutions": 150,
      "avgPrecisionGain": 22.5,
      "avgLatencyMs": 15.0
    }
  },
  "overall": {
    "totalQueries": 500,
    "avgPrecisionGain": 35.2,
    "mostUsedAlgorithm": "ppl"
  }
}
```

#### 2. 记录性能指标

```http
POST /api/optimization/metrics
Content-Type: application/json

{
  "documentId": "doc-123",
  "algorithmType": "ppl",
  "precisionGain": 22.5,
  "latencyMs": 15,
  "relevanceScore": 0.92
}
```

---

## ⚙️ 配置选项

### application.yml

```yaml
# 可选配置（使用默认值即可）
omni-agent:
  web:
    cors:
      enabled: true
      allowed-origins: "*"
      allowed-methods: "GET,POST,PUT,DELETE,OPTIONS"
    
    swagger:
      enabled: true
      title: "OmniAgent API"
      version: "3.0.0"
```

---

## 🎨 自定义扩展

### 1. 自定义Controller

如果默认Controller不满足需求，可以自己实现：

```java
@RestController
@RequestMapping("/api/custom")
public class CustomController {
    
    @Autowired
    private RAGOptimizationService optimizationService;
    
    @PostMapping("/my-endpoint")
    public ResponseEntity<MyResponse> myEndpoint(@RequestBody MyRequest request) {
        // 自定义逻辑
        return ResponseEntity.ok(response);
    }
}
```

### 2. 禁用默认Controller

如果完全自己实现，可以排除自动配置：

```java
@SpringBootApplication(exclude = OmniAgentWebAutoConfiguration.class)
public class YourApplication {
    // ...
}
```

### 3. 扩展CORS配置

```java
@Configuration
public class CustomCorsConfig implements WebMvcConfigurer {
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/api/**")
                .allowedOrigins("https://your-domain.com")
                .allowedMethods("GET", "POST")
                .allowCredentials(true);
    }
}
```

---

## 📊 集成示例

### 示例1: 最小化集成

```xml
<!-- pom.xml -->
<dependencies>
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-web</artifactId>
    </dependency>
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-persistence-starter-file</artifactId>
    </dependency>
</dependencies>
```

```java
// Application.java
@SpringBootApplication
public class Application {
    public static void main(String[] args) {
        SpringApplication.run(Application.class, args);
    }
}
```

**完成！** 所有API已可用。

### 示例2: 生产环境集成

```xml
<dependencies>
    <!-- Web层 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-web</artifactId>
    </dependency>
    
    <!-- 存储层（生产建议MongoDB/Redis）-->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-persistence-starter-mongodb</artifactId>
    </dependency>
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-document-storage-starter-mongodb</artifactId>
    </dependency>
    
    <!-- RAG实现 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-rag-starter-mongodb</artifactId>
    </dependency>
</dependencies>
```

```yaml
# application-prod.yml
spring:
  data:
    mongodb:
      uri: mongodb://localhost:27017/omni-agent
```

---

## 🔧 开发指南

### 运行测试

```bash
mvn test
```

### 构建模块

```bash
mvn clean install
```

### 查看API文档

启动应用后访问：
```
http://localhost:8080/swagger-ui.html
```

---

## 📝 完整API列表

### RAG优化API

| 方法 | 路径 | 说明 |
|------|------|------|
| POST | /api/rag/auto-select | 自动选择算法 |
| POST | /api/rag/evaluate-scenarios | 批量评估场景 |
| POST | /api/rag/optimization-data | 保存优化数据 |
| GET | /api/rag/optimization-data/{documentId}/{type} | 获取优化数据 |
| GET | /api/rag/optimization-data/{documentId} | 获取所有优化数据 |
| GET | /api/rag/optimization-types/{documentId} | 获取优化类型列表 |
| DELETE | /api/rag/optimization-data/{documentId}/{type} | 删除优化数据 |
| DELETE | /api/rag/optimization-data/{documentId} | 删除所有优化数据 |

### Dashboard API

| 方法 | 路径 | 说明 |
|------|------|------|
| GET | /api/optimization/dashboard | 获取Dashboard数据 |
| GET | /api/optimization/statistics | 获取所有算法统计 |
| GET | /api/optimization/statistics/{algorithmType} | 获取特定算法统计 |
| GET | /api/optimization/metrics/recent | 获取最近指标 |
| POST | /api/optimization/metrics | 记录单条指标 |
| POST | /api/optimization/metrics/batch | 批量记录指标 |
| GET | /api/optimization/summary | 获取统计摘要 |
| DELETE | /api/optimization/metrics/old | 清除旧数据 |

---

## 🎯 设计理念

### 开箱即用

引入依赖即可使用，无需额外配置。

### 可选覆盖

默认实现可用，需要时可自定义。

### 标准REST

遵循RESTful设计规范，易于集成。

### 文档齐全

OpenAPI/Swagger自动生成文档。

---

## 🐛 故障排查

### 问题1: Controller未生效

**症状**: 访问API返回404

**解决**:
1. 确认已添加omni-agent-web依赖
2. 检查主类上是否有`@SpringBootApplication`
3. 确认主类包路径不冲突

### 问题2: CORS错误

**症状**: 前端跨域请求被阻止

**解决**:
```java
// 自定义CORS配置
@Configuration
public class CorsConfig implements WebMvcConfigurer {
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/**")
                .allowedOrigins("*");
    }
}
```

### 问题3: Swagger不显示

**症状**: /swagger-ui.html 404

**解决**:
```xml
<!-- 添加依赖 -->
<dependency>
    <groupId>org.springdoc</groupId>
    <artifactId>springdoc-openapi-starter-webmvc-ui</artifactId>
    <version>2.3.0</version>
</dependency>
```

---

## 📞 技术支持

- **文档**: [完整文档](../docs/)
- **示例**: [Example项目](../omni-agent-example-basic/)
- **Issues**: [GitHub Issues](https://github.com/omni-agent/issues)

---

## 📄 License

Apache License 2.0

---

**版本**: 3.0.0  
**作者**: OmniAgent Team  
**最后更新**: 2025-12-17

