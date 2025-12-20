# OmniAgent Workflow Engine

工作流引擎模块 - 支持工作流定义、执行、编排和市场功能

## 🎯 功能特性

- ✅ **工作流定义**：YAML/JSON 格式，易读易写
- ✅ **工作流执行**：同步/异步执行，依赖解析
- ✅ **版本管理**：语义化版本，版本归档
- ✅ **工作流市场**：发布、搜索、下载、安装
- ✅ **灵活持久化**：支持 SQLite/MongoDB/Elasticsearch/File
- ✅ **评分评论**：社区互动
- ✅ **自动配置**：Spring Boot 自动配置

## 📦 Maven 依赖

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-workflow</artifactId>
    <version>1.0.0-SNAPSHOT</version>
</dependency>
```

## ⚙️ 配置

在 `application.yml` 中添加：

```yaml
omni-agent:
  workflow:
    # 存储类型: file | sqlite | mongodb | elasticsearch | auto
    # auto: 根据项目依赖自动选择（推荐）⭐
    storage-type: auto
    
    # SQLite 配置（当使用 SQLite 时）
    sqlite:
      db-path: ./data/workflows/workflows.db
    
    # 市场配置
    market:
      enabled: true
      page-size: 20
```

### 🎯 自动检测存储类型（推荐）⭐

设置 `storage-type: auto`，系统会根据项目依赖自动选择存储方式：

1. **MongoDB** - 如果检测到 `spring-data-mongodb`
2. **Elasticsearch** - 如果检测到 `elasticsearch-java`
3. **SQLite** - 如果检测到 `sqlite-jdbc`
4. **File** - 默认回退（YAML 文件）

**示例**：

```xml
<!-- 项目中添加 SQLite 依赖 -->
<dependency>
    <groupId>org.xerial</groupId>
    <artifactId>sqlite-jdbc</artifactId>
</dependency>

<!-- 配置使用 auto -->
<!-- 系统会自动使用 SQLite 存储 -->
```

详细配置说明请查看：[存储配置指南](./STORAGE_CONFIGURATION.md)

## 🚀 快速开始

### 1. 执行工作流

```java
@Autowired
private WorkflowEngine workflowEngine;

// 同步执行
WorkflowResult result = workflowEngine.execute("MyWorkflow", input);

// 异步执行
CompletableFuture<WorkflowResult> future = workflowEngine.executeAsync("MyWorkflow", input);
```

### 2. 工作流市场

```java
@Autowired
private WorkflowMarketService marketService;

// 发布工作流
String marketId = marketService.publishWorkflow(workflow, userId, userName);

// 搜索工作流
List<MarketWorkflow> results = marketService.searchWorkflows("数据处理", 0, 20);

// 安装工作流
marketService.installWorkflow(workflowId, userId);

// 评分
marketService.rateWorkflow(workflowId, userId, userName, 5, "很好用！");
```

### 3. 定义工作流（YAML）

创建 `data/workflows/definitions/example/MyWorkflow.yml`：

```yaml
name: "MyWorkflow"
version: "1.0.0"
description: "我的工作流"
author: "Your Name"
status: "active"
tags:
  - "example"

steps:
  - id: "step1"
    name: "第一步"
    agent: "EchoAgent"
    input: "${workflow.input}"
  
  - id: "step2"
    name: "第二步"
    agent: "EchoAgent"
    input: "${step1.output}"
    dependencies: ["step1"]
```

## 🏗️ 模块结构

```
omni-agent-workflow/
├── src/main/java/top/yumbo/ai/omni/workflow/
│   ├── Workflow.java                  # 工作流定义
│   ├── WorkflowEngine.java            # 工作流引擎
│   ├── WorkflowRegistry.java          # 工作流注册表
│   ├── market/
│   │   ├── MarketWorkflow.java        # 市场工作流
│   │   └── WorkflowMarketService.java # 市场服务
│   ├── repository/
│   │   ├── WorkflowRepository.java    # 存储接口
│   │   └── impl/
│   │       └── SQLiteWorkflowRepository.java  # SQLite 实现
│   └── agents/
│       └── EchoAgent.java             # 示例 Agent
└── src/main/resources/
    └── application-workflow.yml        # 默认配置
```

## 🔧 自定义 Agent

```java
@Component("MyAgent")
public class MyCustomAgent implements Agent {
    
    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        // 你的业务逻辑
        return result;
    }
    
    @Override
    public String getName() {
        return "MyAgent";
    }
}
```

在工作流中使用：

```yaml
steps:
  - id: "my_step"
    agent: "MyAgent"
    input: "${workflow.input}"
```

## 📊 数据库表结构

### market_workflows
- 存储工作流定义和市场信息

### workflow_ratings
- 存储用户评分和评论

### workflow_installations
- 存储用户安装记录

## 🎨 存储后端切换

### 自动检测（推荐）⭐
```yaml
omni-agent:
  workflow:
    storage-type: auto  # 根据依赖自动选择
```

### SQLite（默认）
```yaml
omni-agent:
  workflow:
    storage-type: sqlite
    sqlite:
      db-path: ./data/workflows/workflows.db
```

### File（开发）
```yaml
omni-agent:
  workflow:
    storage-type: file
    file:
      definitions-dir: ./data/workflows/definitions
      versions-dir: ./data/workflows/versions
```

### MongoDB（大规模）
```yaml
omni-agent:
  workflow:
    storage-type: mongodb
    mongodb:
      uri: mongodb://localhost:27017
      database: omniagent
```

### Elasticsearch（搜索优化）
```yaml
omni-agent:
  workflow:
    storage-type: elasticsearch
    elasticsearch:
      uris: http://localhost:9200
      index: market-workflows
```

**详细配置**：查看 [存储配置指南](./STORAGE_CONFIGURATION.md)

## 📚 文档

- [快速开始指南](../WORKFLOW_QUICK_START.md)
- [工作流市场设计](../WORKFLOW_MARKET_DESIGN.md)
- [实施计划](../WORKFLOW_IMPLEMENTATION_PLAN.md)
- [迁移报告](../WORKFLOW_MIGRATION_REPORT.md)

## 🎉 特性

- **模块独立**：不依赖 omni-agent-core
- **自动配置**：引入即用
- **灵活存储**：一行配置切换存储后端
- **易于扩展**：清晰的接口设计
- **社区化**：完整的工作流市场功能

## 🚀 版本历史

- **v1.0.0-SNAPSHOT** - 初始版本
  - 工作流引擎核心
  - SQLite 持久化
  - 工作流市场
  - 评分评论
  - 自动配置

---

**OmniAgent Workflow Engine - 让工作流更简单！** 🎯

