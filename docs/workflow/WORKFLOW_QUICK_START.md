# 🚀 工作流引擎快速开始

## Phase 1 已完成 ✅

工作流引擎的核心基础设施已经实现并可以使用！

---

## 📦 已实现的功能

- ✅ 工作流定义（YAML/JSON）
- ✅ 工作流注册和版本管理
- ✅ 工作流执行（同步/异步）
- ✅ 依赖解析和拓扑排序
- ✅ 变量替换
- ✅ 执行追踪
- ✅ 持久化存储

---

## 🎯 快速体验

### 1. 创建工作流定义

创建文件 `data/workflows/definitions/example/MyWorkflow.yml`：

```yaml
name: "MyWorkflow"
version: "1.0.0"
description: "我的第一个工作流"
author: "Your Name"
status: "active"
tags:
  - "p2p"

steps:
  - id: "step1"
    name: "第一步：回显输入"
    agent: "EchoAgent"
    input: "${workflow.input}"
  
  - id: "step2"
    name: "第二步：处理结果"
    agent: "EchoAgent"
    input:
      message: "处理 step1 的结果"
      data: "${step1.output}"
    dependencies:
      - "step1"
```

### 2. 在代码中执行工作流

```java
@Autowired
private WorkflowEngine workflowEngine;

@Autowired
private WorkflowRegistry workflowRegistry;

public void runWorkflow() {
    // 工作流会在启动时自动加载
    // 或者手动注册
    Workflow workflow = Workflow.builder()
        .name("MyWorkflow")
        .version("1.0.0")
        .steps(...)
        .build();
    
    workflowRegistry.register(workflow);
    
    // 执行工作流
    Map<String, Object> input = Map.of(
        "message", "Hello, Workflow!",
        "timestamp", System.currentTimeMillis()
    );
    
    WorkflowResult result = workflowEngine.execute("MyWorkflow", input);
    
    if (result.isSuccess()) {
        System.out.println("✅ 执行成功");
        System.out.println("耗时: " + result.getDuration() + "ms");
        System.out.println("结果: " + result.getFinalResult());
    } else {
        System.out.println("❌ 执行失败: " + result.getError());
    }
}
```

### 3. 异步执行

```java
CompletableFuture<WorkflowResult> future = 
    workflowEngine.executeAsync("MyWorkflow", input);

future.thenAccept(result -> {
    System.out.println("异步执行完成: " + result.getFinalResult());
});
```

---

## 📝 工作流定义语法

### 基本结构

```yaml
name: "WorkflowName"          # 工作流名称（必填）
version: "1.0.0"              # 版本号（必填）
description: "描述"            # 描述（可选）
author: "作者"                 # 作者（可选）
status: "active"              # 状态: draft/active/deprecated
tags:                         # 标签（可选）
  - "category1"
  - "category2"

steps:                        # 步骤列表（必填）
  - id: "step1"               # 步骤ID（必填，唯一）
    name: "步骤名称"            # 步骤名称（可选）
    agent: "AgentName"        # Agent 名称（必填）
    input: "..."              # 输入配置（可选）
    config: {}                # Agent 配置（可选）
    dependencies: []          # 依赖的步骤ID（可选）
    allowFailure: false       # 是否允许失败（可选，默认false）
    timeout: 60000            # 超时时间ms（可选，默认60000）
    retries: 0                # 重试次数（可选，默认0）
    condition: "..."          # 条件表达式（可选）
```

### 变量替换

```yaml
steps:
  - id: "step1"
    input: "${workflow.input}"
    # 使用工作流的输入
  
  - id: "step2"
    input: "${step1.output}"
    # 使用 step1 的输出
  
  - id: "step3"
    input:
      message: "Combined"
      data1: "${step1.output}"
      data2: "${step2.output}"
    # 组合多个步骤的输出
```

### 依赖管理

```yaml
steps:
  - id: "stepA"
    # 无依赖，最先执行
  
  - id: "stepB"
    dependencies: ["stepA"]
    # stepA 完成后才执行
  
  - id: "stepC"
    dependencies: ["stepA", "stepB"]
    # stepA 和 stepB 都完成后才执行
```

执行顺序会自动根据依赖关系排序：`stepA → stepB → stepC`

---

## 🛠️ 创建自定义 Agent

### 1. 实现 Agent 接口

```java
package com.example.agents;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.workflow.Agent;
import top.yumbo.ai.omni.workflow.WorkflowContext;

@Slf4j
@Component("MyCustomAgent")
public class MyCustomAgent implements Agent {
    
    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        log.info("MyCustomAgent 执行: input={}", input);
        
        // 你的业务逻辑
        String result = processData(input);
        
        return Map.of(
            "result", result,
            "timestamp", System.currentTimeMillis()
        );
    }
    
    @Override
    public String getName() {
        return "MyCustomAgent";
    }
    
    @Override
    public String getDescription() {
        return "我的自定义 Agent";
    }
    
    @Override
    public boolean validateInput(Object input) {
        // 输入验证逻辑
        return input != null;
    }
    
    private String processData(Object input) {
        // 处理数据...
        return "processed: " + input;
    }
}
```

### 2. 在工作流中使用

```yaml
steps:
  - id: "my_step"
    agent: "MyCustomAgent"
    input: "${workflow.input}"
```

---

## 📊 执行追踪

### 查询执行结果

```java
// 通过 executionId 查询
WorkflowResult result = workflowEngine.getExecutionResult(executionId);

// 查询所有执行记录
List<WorkflowResult> allExecutions = workflowEngine.getAllExecutions();
```

### 执行结果包含

```java
result.getExecutionId();      // 执行ID
result.getStatus();            // 状态
result.getStartTime();         // 开始时间
result.getEndTime();           // 结束时间
result.getDuration();          // 耗时
result.getStepResults();       // 所有步骤的结果
result.getFinalResult();       // 最终结果
result.getError();             // 错误信息（如果失败）
```

---

## 🔄 版本管理

### 注册新版本

```java
// 获取现有工作流
Workflow workflow = workflowRegistry.getLatestWorkflow("MyWorkflow");

// 修改并递增版本
String newVersion = workflowRegistry.incrementVersion(workflow.getVersion());
workflow.setVersion(newVersion);
workflow.setUpdatedAt(System.currentTimeMillis());

// 注册新版本
workflowRegistry.register(workflow);
```

### 版本存储

```
data/workflows/
├── definitions/
│   └── example/
│       └── MyWorkflow.yml        # 最新版本
└── versions/
    └── MyWorkflow/
        ├── v1.0.0.yml             # 历史版本
        ├── v1.1.0.yml
        └── v2.0.0.yml
```

### 执行指定版本

```java
// 执行最新版本
WorkflowResult result = workflowEngine.execute("MyWorkflow", input);

// 执行指定版本
WorkflowResult result = workflowEngine.execute("MyWorkflow", "1.0.0", input);
```

---

## 🧪 测试

### 单元测试示例

```java
@SpringBootTest
class MyWorkflowTest {
    
    @Autowired
    private WorkflowEngine workflowEngine;
    
    @Test
    void testMyWorkflow() {
        // Given
        Map<String, Object> input = Map.of("test", "data");
        
        // When
        WorkflowResult result = workflowEngine.execute("MyWorkflow", input);
        
        // Then
        assertTrue(result.isSuccess());
        assertNotNull(result.getFinalResult());
        System.out.println("Result: " + result.getFinalResult());
    }
}
```

---

## 📚 更多示例

查看已有的示例：
- `data/workflows/definitions/example/HelloWorld.yml`
- `omni-agent-core/src/test/java/top/yumbo/ai/omni/workflow/WorkflowEngineTest.java`

---

## 🏪 工作流市场（规划中）⭐

工作流引擎将支持类似 GitHub Marketplace 的工作流市场！

### 核心特性

1. **发布和分享** - 用户可以发布自己的工作流到市场
2. **搜索和浏览** - 按分类、标签、评分搜索工作流
3. **下载和安装** - 一键安装其他用户的工作流
4. **评分和评论** - 社区互动，提升工作流质量
5. **版本管理** - 工作流支持多版本，可以回滚

### 灵活持久化 ⭐

支持多种存储后端（可插拔）：

| 存储类型 | 适用场景 | 特点 |
|---------|---------|------|
| **File（YAML）** | 开发/小规模 | 简单、易读、易编辑 |
| **SQLite** | 单机/中小规模 | 轻量、无需独立服务 |
| **MongoDB** | 分布式/大规模 | 高性能、易扩展 |
| **Elasticsearch** | 全文搜索 | 强大的搜索能力 |

**配置示例**：

```yaml
omni-agent:
  workflow:
    # 选择存储类型
    storage-type: sqlite  # file | sqlite | mongodb | elasticsearch
    
    # SQLite 配置
    sqlite:
      db-path: ./data/workflows/workflows.db
    
    # MongoDB 配置
    mongodb:
      uri: mongodb://localhost:27017
      database: omniagent
    
    # 市场配置
    market:
      enabled: true
      page-size: 20
```

### 使用示例

```java
@Autowired
private WorkflowMarketService marketService;

// 发布工作流到市场
String id = marketService.publishWorkflow(workflow, userId, userName);

// 搜索工作流
List<MarketWorkflow> results = marketService.searchWorkflows("数据分析", 0, 20);

// 安装工作流
marketService.installWorkflow(workflowId, userId);

// 评分
marketService.rateWorkflow(workflowId, userId, userName, 5, "非常好用！");
```

### REST API

```bash
# 搜索工作流
GET /api/workflows/market/search?keyword=数据处理

# 热门工作流
GET /api/workflows/market/popular?limit=10

# 下载工作流
GET /api/workflows/market/{workflowId}/download

# 安装工作流
POST /api/workflows/market/{workflowId}/install

# 评分
POST /api/workflows/market/{workflowId}/rate
```

查看完整设计：`WORKFLOW_MARKET_DESIGN.md`

---

## 🎯 下一步

Phase 2 将实现：
- **WorkflowInvokerAgent** - 工作流调用工作流
- **工作流市场** ⭐ - 发布、分享、下载
- **灵活持久化** ⭐ - SQLite/MongoDB/ES 支持
- **并行执行** - 无依赖步骤并行执行
- **工作流 API** - REST API 接口

---

## 💡 提示

1. **工作流定义文件**会在应用启动时自动加载
2. **YAML 格式**易读易写，支持注释
3. **版本管理**自动归档历史版本
4. **变量替换**让步骤间数据流转更简单
5. **依赖管理**自动解析执行顺序

---

## 🆘 常见问题

### Q: 工作流定义文件放在哪里？
A: `data/workflows/definitions/{category}/{WorkflowName}.yml`

### Q: 如何调试工作流？
A: 查看日志，每个步骤都有详细的执行日志

### Q: 如何处理步骤失败？
A: 设置 `allowFailure: true` 允许步骤失败但继续执行

### Q: 如何传递大量数据？
A: 使用 `WorkflowContext.setSharedData()` 共享数据

### Q: 如何实现条件执行？
A: 设置 `condition` 字段（Phase 2 将完整实现）

---

**开始使用工作流引擎，构建强大的自动化流程！** 🚀

