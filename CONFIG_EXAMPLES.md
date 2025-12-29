# OmniAgent 配置示例

本文档提供各种场景下的配置示例。

---

## 基础配置（最小配置）

适合快速开始和本地开发。

```yaml
# application.yml
omni:
  # 文档存储
  document-storage:
    file:
      base-path: ./data/storage
      
  # RAG检索
  rag:
    file:
      enabled: true
      index-path: ./data/rag/lucene
      
  # 分块策略
  chunking:
    default-strategy: sentence-boundary
```

---

## 标准配置（推荐）

包含常用功能，适合大多数场景。

```yaml
# application.yml
server:
  port: 8080

omni:
  # 文档存储
  document-storage:
    file:
      base-path: ./data/storage
      enable-metadata: true
      max-file-size: 50MB
      
  # RAG检索
  rag:
    file:
      enabled: true
      index-path: ./data/rag/lucene
      default-domain-id: default
      
  # 分块策略
  chunking:
    default-strategy: sentence-boundary
    max-chunk-size: 512
    overlap: 50
    
  # 知识域
  knowledge-registry:
    enabled: true
    auto-create-default: true

# 日志配置
logging:
  level:
    top.yumbo.ai.omni: INFO
```

---

## 高级配置（完整功能）

启用所有高级功能。

```yaml
# application.yml
server:
  port: 8080

omni:
  # 文档存储
  document-storage:
    file:
      base-path: ./data/storage
      enable-metadata: true
      max-file-size: 100MB
      
  # RAG检索
  rag:
    file:
      enabled: true
      index-path: ./data/rag/lucene
      default-domain-id: default
      max-results: 20
      
  # 分块策略
  chunking:
    default-strategy: sentence-boundary
    max-chunk-size: 512
    overlap: 50
    ppl-enabled: true
    semantic-enabled: true
    
  # 知识域
  knowledge-registry:
    enabled: true
    auto-create-default: true
    
  # 知识网络（可选）
  knowledge-network:
    enabled: true
    auto-scan: true
    scan-interval: 300000  # 5分钟

# AI Embedding（可选）
embedding:
  onnx:
    enabled: true
    model-path: ./models/bge-base-zh-v1.5/model.onnx
    max-sequence-length: 512
    batch-size: 32

# AI服务（可选）
ai:
  # 方案1：使用本地Ollama
  ollama:
    enabled: false
    base-url: http://localhost:11434
    model: qwen2.5:0.5b
    timeout: 60s
    
  # 方案2：使用在线API
  online:
    enabled: false
    provider: openai
    api-key: ${AI_API_KEY}
    model: gpt-3.5-turbo
    max-tokens: 1000

# 日志配置
logging:
  level:
    top.yumbo.ai.omni: DEBUG
    org.springframework: INFO
  pattern:
    console: "%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n"
```

---

## 生产环境配置

适合生产部署的配置。

```yaml
# application-prod.yml
server:
  port: 8080
  compression:
    enabled: true
    
spring:
  application:
    name: omni-agent
  profiles:
    active: prod

omni:
  document-storage:
    file:
      base-path: /data/omni-agent/storage
      enable-metadata: true
      max-file-size: 100MB
      
  rag:
    file:
      enabled: true
      index-path: /data/omni-agent/rag
      default-domain-id: default
      
  chunking:
    default-strategy: sentence-boundary
    max-chunk-size: 512
    
  knowledge-registry:
    enabled: true

# 性能优化
spring:
  task:
    execution:
      pool:
        core-size: 4
        max-size: 16
        queue-capacity: 100

# 监控
management:
  endpoints:
    web:
      exposure:
        include: health,info,metrics
  metrics:
    export:
      prometheus:
        enabled: true

# 日志
logging:
  level:
    top.yumbo.ai.omni: INFO
  file:
    name: /var/log/omni-agent/application.log
    max-size: 100MB
    max-history: 30
```

---

## Docker配置

使用Docker部署的配置。

```yaml
# application-docker.yml
server:
  port: 8080

omni:
  document-storage:
    file:
      base-path: /app/data/storage
      
  rag:
    file:
      enabled: true
      index-path: /app/data/rag
      
  chunking:
    default-strategy: sentence-boundary

logging:
  level:
    top.yumbo.ai.omni: INFO
```

配套的 `docker-compose.yml`：

```yaml
version: '3.8'

services:
  omni-agent:
    image: omni-agent:latest
    ports:
      - "8080:8080"
    volumes:
      - ./data:/app/data
      - ./models:/app/models
    environment:
      - SPRING_PROFILES_ACTIVE=docker
      - JAVA_OPTS=-Xmx2g -Xms1g
    restart: unless-stopped
```

---

## 常见配置问题

### Q1: 如何修改端口？

```yaml
server:
  port: 9090  # 改为你需要的端口
```

### Q2: 如何增加文件大小限制？

```yaml
omni:
  document-storage:
    file:
      max-file-size: 200MB  # 默认50MB
```

### Q3: 如何启用调试日志？

```yaml
logging:
  level:
    top.yumbo.ai.omni: DEBUG
```

### Q4: 如何配置多个知识域？

在代码中创建：

```java
@Configuration
public class DomainConfig {
    @Autowired
    private KnowledgeDomainService domainService;
    
    @PostConstruct
    public void initDomains() {
        // 技术文档域
        domainService.createDomain(
            KnowledgeDomain.builder()
                .domainId("tech-docs")
                .name("技术文档域")
                .build()
        );
        
        // 财务报表域
        domainService.createDomain(
            KnowledgeDomain.builder()
                .domainId("finance")
                .name("财务域")
                .build()
        );
    }
}
```

---

## 环境变量配置

支持通过环境变量覆盖配置：

```bash
# 设置存储路径
export OMNI_STORAGE_PATH=/data/storage

# 设置RAG索引路径
export OMNI_RAG_INDEX_PATH=/data/rag

# 设置AI API Key
export AI_API_KEY=your-api-key
```

在配置文件中引用：

```yaml
omni:
  document-storage:
    file:
      base-path: ${OMNI_STORAGE_PATH:./data/storage}
      
ai:
  online:
    api-key: ${AI_API_KEY}
```

---

## 配置文件位置

Spring Boot按以下顺序查找配置文件：

1. `./config/application.yml` （当前目录的config子目录）
2. `./application.yml` （当前目录）
3. `classpath:/config/application.yml` （classpath的config目录）
4. `classpath:/application.yml` （classpath根目录）

---

**更多帮助**: 查看 [QUICKSTART.md](QUICKSTART.md)

