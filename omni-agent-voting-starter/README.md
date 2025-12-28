# OmniAgent Voting Starter

统一的投票服务 Starter，集成多种数据源实现。

## 特性

- 🎯 **统一接口**：基于 `VotingService` API 统一接口
- 🔄 **多种实现**：支持 Memory、MongoDB、Redis、Elasticsearch
- ⚙️ **灵活配置**：通过配置文件轻松切换数据源
- 📦 **开箱即用**：默认 Memory 实现，无需外部依赖
- 🚀 **自动配置**：Spring Boot 自动配置，零代码集成

## 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-voting-starter</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 2. 配置（可选）

默认使用 Memory 实现，无需配置。如需使用其他数据源：

```yaml
omni-agent:
  voting:
    type: mongodb  # memory, mongodb, redis, elasticsearch
```

### 3. 使用

```java
@Autowired
private VotingService votingService;

public void example() {
    // 投票
    votingService.vote("proposal-123", "option-A", "user-001");
    
    // 获取结果
    VotingResult result = votingService.getVotingResult("proposal-123");
}
```

## 支持的数据源

### Memory（默认）

无需任何外部依赖，适合开发和测试。

```yaml
omni-agent:
  voting:
    type: memory
    memory:
      max-size: 10000
      ttl: 3600000  # 1小时
```

### MongoDB

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-mongodb</artifactId>
</dependency>
```

```yaml
omni-agent:
  voting:
    type: mongodb
    mongodb:
      collection-name: voting_records
spring:
  data:
    mongodb:
      uri: mongodb://localhost:27017/omni-voting
```

### Redis

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-redis</artifactId>
</dependency>
```

```yaml
omni-agent:
  voting:
    type: redis
    redis:
      key-prefix: "omni-voting:"
      ttl: 86400  # 24小时
spring:
  data:
    redis:
      host: localhost
      port: 6379
```

### Elasticsearch

```xml
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-elasticsearch</artifactId>
</dependency>
```

```yaml
omni-agent:
  voting:
    type: elasticsearch
    elasticsearch:
      index-name: omni-voting
```

## 架构设计

```
VotingService (API)
       ↑
       |
VotingServiceFactory
       |
       ├── MemoryVotingService
       ├── MongoVotingService
       ├── RedisVotingService
       └── ElasticsearchVotingService
```

## License

Apache License 2.0

