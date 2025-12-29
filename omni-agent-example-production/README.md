# OmniAgent Production Example

生产级应用示例 - 展示如何在生产环境中使用 OmniAgent 框架

## 架构配置

### 四维可插拔架构（生产级配置）

| 维度 | 实现 | 说明 |
|------|------|------|
| **Persistence** | Elasticsearch | 生产级持久化，支持分布式、高可用 |
| **Document Storage** | AWS S3 | 公有云对象存储，全球可用、高可靠 |
| **RAG** | Elasticsearch | 向量检索，分布式架构 |
| **AI** | Online API (GPT-4) | OpenAI GPT-4，最强大的语言模型 |

## 特性

- ✅ **生产级架构** - 全部使用生产级组件
- ✅ **高可用** - Elasticsearch 3分片2副本
- ✅ **全球部署** - AWS S3 全球可用
- ✅ **监控完善** - Spring Boot Actuator + Prometheus
- ✅ **日志管理** - 完整的日志配置
- ✅ **环境变量** - 敏感信息使用环境变量

## 快速开始

### 1. 环境准备

需要准备以下环境：

```bash
# Elasticsearch
export ES_PASSWORD=your-elasticsearch-password

# AWS S3
export AWS_ACCESS_KEY_ID=your-access-key
export AWS_SECRET_ACCESS_KEY=your-secret-key

# OpenAI API
export OPENAI_API_KEY=your-openai-api-key
```

### 2. 启动应用

```bash
mvn spring-boot:run
```

### 3. 访问接口

#### 健康检查
```bash
curl http://localhost:8080/api/v1/health
```

#### 对话接口
```bash
curl -X POST http://localhost:8080/api/v1/chat \
  -H "Content-Type: application/json" \
  -d '{"message": "你好，请介绍一下自己"}'
```

#### 存储统计
```bash
curl http://localhost:8080/api/v1/stats
```

#### AI 状态
```bash
curl http://localhost:8080/api/v1/ai/status
```

### 4. 监控端点

```bash
# 健康检查
curl http://localhost:8080/actuator/health

# 指标
curl http://localhost:8080/actuator/metrics

# Prometheus 格式
curl http://localhost:8080/actuator/prometheus
```

## 配置说明

### Elasticsearch 配置

```yaml
omni-agent:
  persistence:
    type: elasticsearch
    elasticsearch:
      host: elasticsearch.production.com:9200
      username: elastic
      password: ${ES_PASSWORD}
      index-prefix: omni-agent-prod
      number-of-shards: 3      # 3个分片，支持水平扩展
      number-of-replicas: 2    # 2个副本，高可用
```

### AWS S3 配置

```yaml
omni-agent:
  document-storage:
    type: s3
    s3:
      region: us-east-1
      access-key-id: ${AWS_ACCESS_KEY_ID}
      secret-access-key: ${AWS_SECRET_ACCESS_KEY}
      bucket-name: omni-agent-production-documents
```

### OpenAI 配置

```yaml
omni-agent:
  ai:
    type: online-api
    online:
      provider: openai
      base-url: https://api.openai.com/v1
      api-key: ${OPENAI_API_KEY}
      default-model: gpt-4
      temperature: 0.7
      max-tokens: 2048
```

## 部署建议

### 容器化部署

```dockerfile
FROM openjdk:21-jdk-slim
COPY target/omni-agent-example-production-1.0.0.jar app.jar
ENTRYPOINT ["java", "-jar", "/app.jar"]
```

### Kubernetes 部署

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: omni-agent-production
spec:
  replicas: 3
  selector:
    matchLabels:
      app: omni-agent
  template:
    metadata:
      labels:
        app: omni-agent
    spec:
      containers:
      - name: omni-agent
        image: omni-agent-production:1.0.0
        ports:
        - containerPort: 8080
        env:
        - name: ES_PASSWORD
          valueFrom:
            secretKeyRef:
              name: omni-agent-secrets
              key: es-password
        - name: AWS_ACCESS_KEY_ID
          valueFrom:
            secretKeyRef:
              name: omni-agent-secrets
              key: aws-access-key
        - name: AWS_SECRET_ACCESS_KEY
          valueFrom:
            secretKeyRef:
              name: omni-agent-secrets
              key: aws-secret-key
        - name: OPENAI_API_KEY
          valueFrom:
            secretKeyRef:
              name: omni-agent-secrets
              key: openai-api-key
```

## 性能调优

### JVM 参数

```bash
java -Xms2g -Xmx4g \
     -XX:+UseG1GC \
     -XX:MaxGCPauseMillis=200 \
     -jar omni-agent-p2p-production-1.0.0.jar
```

### 并发配置

```yaml
server:
  tomcat:
    threads:
      max: 200
      min-spare: 10
```

## 监控和告警

### Prometheus 配置

```yaml
scrape_configs:
  - job_name: 'omni-agent'
    static_configs:
      - targets: ['localhost:8080']
    metrics_path: '/actuator/prometheus'
```

### Grafana Dashboard

导入 Spring Boot Dashboard 进行监控可视化

## 成本估算

### AWS S3
- 存储: $0.023/GB/月
- 请求: $0.005/1000 请求

### Elasticsearch
- 3节点集群: ~$300/月（AWS ES）

### OpenAI API
- GPT-4: $0.03/1K tokens (输入)
- GPT-4: $0.06/1K tokens (输出)

## 安全建议

1. **环境变量** - 所有敏感信息使用环境变量
2. **HTTPS** - 生产环境必须使用 HTTPS
3. **认证授权** - 添加 Spring Security
4. **限流** - 使用 API Gateway 进行限流
5. **审计日志** - 记录所有关键操作

## 技术支持

- GitHub: https://github.com/jinhua10/omni-agent
- Email: 1015770492@qq.com

## 许可证

Apache License 2.0

