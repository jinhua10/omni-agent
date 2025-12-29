# 🚀 工作流引擎集成示例

## 📦 快速集成到你的项目

### 1. 添加依赖

在你的 `pom.xml` 中添加：

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-workflow</artifactId>
    <version>1.0.0-SNAPSHOT</version>
</dependency>

<!-- SQLite 驱动（如果使用 SQLite 存储）-->
<dependency>
    <groupId>org.xerial</groupId>
    <artifactId>sqlite-jdbc</artifactId>
</dependency>
```

### 2. 配置 application.yml

```yaml
omni-agent:
  workflow:
    # 自动检测存储类型（推荐）
    storage-type: auto
    
    # SQLite 配置
    sqlite:
      db-path: ./data/workflows/workflows.db
    
    # 工作流市场
    market:
      enabled: true
      page-size: 20
```

### 3. 创建自定义 Agent

```java
package com.example.agents;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.workflow.Agent;
import top.yumbo.ai.omni.workflow.WorkflowContext;

@Slf4j
@Component("DataTransformer")
public class DataTransformerAgent implements Agent {

    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        log.info("🔄 转换数据: {}", input);
        
        // 你的业务逻辑
        String data = (String) input;
        String transformed = data.toUpperCase();
        
        return Map.of(
            "original", data,
            "transformed", transformed,
            "timestamp", System.currentTimeMillis()
        );
    }

    @Override
    public String getName() {
        return "DataTransformer";
    }

    @Override
    public String getDescription() {
        return "数据转换 Agent - 将输入数据转换为大写";
    }
}
```

### 4. 定义工作流（YAML）

创建文件：`data/workflows/definitions/example/DataProcessing.yml`

```yaml
name: "DataProcessing"
version: "1.0.0"
description: "数据处理示例工作流"
author: "Your Name"
tags:
  - "data"
  - "p2p"

steps:
  # 步骤1：转换数据
  - id: "transform"
    name: "转换数据"
    agent: "DataTransformer"
    input: "${workflow.input}"
  
  # 步骤2：输出结果
  - id: "echo"
    name: "输出结果"
    agent: "EchoAgent"
    input: "${transform.output}"
    dependencies: ["transform"]
```

### 5. 在代码中使用

```java
package com.example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.workflow.WorkflowEngine;
import top.yumbo.ai.omni.workflow.WorkflowResult;

@RestController
@RequestMapping("/api/p2p")
public class ExampleController {

    @Autowired
    private WorkflowEngine workflowEngine;

    /**
     * 执行工作流
     */
    @PostMapping("/process")
    public Map<String, Object> processData(@RequestBody String data) {
        // 同步执行工作流
        WorkflowResult result = workflowEngine.execute("DataProcessing", data);
        
        if (result.isSuccess()) {
            return Map.of(
                "success", true,
                "result", result.getFinalResult(),
                "duration", result.getDuration() + "ms"
            );
        } else {
            return Map.of(
                "success", false,
                "error", result.getError()
            );
        }
    }

    /**
     * 异步执行工作流
     */
    @PostMapping("/process-async")
    public Map<String, Object> processDataAsync(@RequestBody String data) {
        CompletableFuture<WorkflowResult> future = 
            workflowEngine.executeAsync("DataProcessing", data);
        
        return Map.of(
            "success", true,
            "message", "工作流已提交，正在异步执行"
        );
    }
}
```

---

## 🎨 实际场景示例

### 场景1：数据处理流水线

```yaml
name: "ETLPipeline"
version: "1.0.0"

steps:
  - id: "extract"
    agent: "DataExtractor"
    input: "${workflow.input.source}"
  
  - id: "transform"
    agent: "DataTransformer"
    input: "${extract.output}"
    dependencies: ["extract"]
  
  - id: "load"
    agent: "DataLoader"
    input: "${transform.output}"
    dependencies: ["transform"]
```

### 场景2：批量处理（并行）

```yaml
name: "BatchProcessing"
version: "1.0.0"

steps:
  # 提取数据列表
  - id: "extract_list"
    agent: "DataExtractor"
    input: "${workflow.input}"
  
  # 并行处理每一项
  - id: "parallel_process"
    agent: "WorkflowInvoker"
    input:
      mode: "parallel"
      workflow: "ProcessItem"
      maxParallel: 10
      items: "${extract_list.output.items}"
    dependencies: ["extract_list"]
  
  # 聚合结果
  - id: "aggregate"
    agent: "ResultAggregator"
    input: "${parallel_process.output.results}"
    dependencies: ["parallel_process"]
```

### 场景3：微服务编排

```yaml
name: "OrderProcessing"
version: "1.0.0"

steps:
  # 步骤1：验证用户
  - id: "auth"
    agent: "AuthService"
    input: "${workflow.input.userId}"
  
  # 步骤2：并行调用多个服务
  - id: "services"
    agent: "WorkflowInvoker"
    input:
      mode: "parallel"
      workflow: "ServiceCall"
      items:
        - { service: "UserService", userId: "${auth.output.userId}" }
        - { service: "InventoryService", productId: "${workflow.input.productId}" }
        - { service: "PaymentService", amount: "${workflow.input.amount}" }
    dependencies: ["auth"]
  
  # 步骤3：创建订单
  - id: "create_order"
    agent: "OrderService"
    input:
      user: "${services.output.results[0].result}"
      inventory: "${services.output.results[1].result}"
      payment: "${services.output.results[2].result}"
    dependencies: ["services"]
```

---

## 🔧 高级用法

### 1. 使用工作流市场 API

```java
@Autowired
private WorkflowMarketService marketService;

// 搜索工作流
List<MarketWorkflow> results = marketService.searchWorkflows("数据处理", 0, 20);

// 下载并安装
String workflowId = results.get(0).getId();
marketService.installWorkflow(workflowId, "user123");

// 评分
marketService.rateWorkflow(workflowId, "user123", "张三", 5, "非常好用！");
```

### 2. 发布工作流到市场

```java
@Autowired
private WorkflowRegistry workflowRegistry;

@Autowired
private WorkflowMarketService marketService;

// 获取工作流
Optional<Workflow> workflow = workflowRegistry.getWorkflow("DataProcessing");

// 发布到市场
if (workflow.isPresent()) {
    String marketId = marketService.publishWorkflow(
        workflow.get(), 
        "user123", 
        "张三"
    );
    System.out.println("发布成功: " + marketId);
}
```

### 3. 通过 REST API 调用

```bash
# 搜索工作流
curl "http://localhost:8080/api/workflows/market/search?keyword=数据处理"

# 获取热门工作流
curl "http://localhost:8080/api/workflows/market/popular?limit=10"

# 下载工作流
curl "http://localhost:8080/api/workflows/market/{workflowId}/download" \
  -H "X-User-Id: user123"

# 安装工作流
curl -X POST "http://localhost:8080/api/workflows/market/{workflowId}/install" \
  -H "X-User-Id: user123"

# 评分
curl -X POST "http://localhost:8080/api/workflows/market/{workflowId}/rate" \
  -H "Content-Type: application/json" \
  -H "X-User-Id: user123" \
  -H "X-User-Name: 张三" \
  -d '{"rating": 5, "comment": "很好用！"}'
```

---

## 📱 前端集成示例（JavaScript）

```javascript
// 搜索工作流
async function searchWorkflows(keyword) {
  const response = await fetch(
    `/api/workflows/market/search?keyword=${encodeURIComponent(keyword)}`
  );
  const data = await response.json();
  return data.data;
}

// 安装工作流
async function installWorkflow(workflowId, userId) {
  const response = await fetch(
    `/api/workflows/market/${workflowId}/install`,
    {
      method: 'POST',
      headers: {
        'X-User-Id': userId
      }
    }
  );
  return await response.json();
}

// 评分
async function rateWorkflow(workflowId, userId, userName, rating, comment) {
  const response = await fetch(
    `/api/workflows/market/${workflowId}/rate`,
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'X-User-Id': userId,
        'X-User-Name': userName
      },
      body: JSON.stringify({ rating, comment })
    }
  );
  return await response.json();
}

// 使用示例
async function demo() {
  // 搜索
  const workflows = await searchWorkflows('数据处理');
  console.log('搜索结果:', workflows);
  
  // 安装
  const result = await installWorkflow(workflows[0].id, 'user123');
  console.log('安装结果:', result);
  
  // 评分
  const ratingResult = await rateWorkflow(
    workflows[0].id, 
    'user123', 
    '张三', 
    5, 
    '很好用！'
  );
  console.log('评分结果:', ratingResult);
}
```

---

## 🎯 完整示例项目结构

```
your-project/
├── src/
│   ├── main/
│   │   ├── java/
│   │   │   └── com/example/
│   │   │       ├── agents/
│   │   │       │   ├── DataExtractorAgent.java
│   │   │       │   ├── DataTransformerAgent.java
│   │   │       │   └── DataLoaderAgent.java
│   │   │       │
│   │   │       ├── controller/
│   │   │       │   └── WorkflowController.java
│   │   │       │
│   │   │       └── Application.java
│   │   │
│   │   └── resources/
│   │       └── application.yml
│   │
│   └── test/
│       └── java/
│
├── data/
│   └── workflows/
│       ├── definitions/
│       │   └── example/
│       │       ├── DataProcessing.yml
│       │       └── ETLPipeline.yml
│       │
│       └── workflows.db  # SQLite 数据库（自动创建）
│
└── pom.xml
```

---

## ⚡ 性能优化建议

### 1. 使用并行执行

```yaml
# 批量处理时使用 parallel 模式
steps:
  - agent: "WorkflowInvoker"
    input:
      mode: "parallel"  # 而不是 forEach
      maxParallel: 20   # 根据系统资源调整
      items: [...]
```

### 2. 异步执行

```java
// 对于耗时操作，使用异步执行
CompletableFuture<WorkflowResult> future = 
    workflowEngine.executeAsync("LongRunningWorkflow", data);

// 继续处理其他任务
// ...

// 需要结果时再等待
WorkflowResult result = future.get();
```

### 3. 合理配置线程池

```yaml
# 根据系统资源调整
omni-agent:
  workflow:
    executor:
      core-pool-size: 10
      max-pool-size: 50
      queue-capacity: 1000
```

---

## 🐛 常见问题

### Q1: 工作流找不到？

**A**: 确保 YAML 文件放在正确的目录：
```
data/workflows/definitions/{category}/{WorkflowName}.yml
```

### Q2: Agent 找不到？

**A**: 确保 Agent 类上有 `@Component` 注解，且名称与 YAML 中配置的一致。

### Q3: 数据库初始化失败？

**A**: 确保数据库目录存在，应用有写入权限：
```bash
mkdir -p ./data/workflows
chmod 755 ./data/workflows
```

### Q4: REST API 404？

**A**: 确保 `spring-boot-starter-web` 依赖已添加，且应用正确启动。

---

## 📚 更多资源

- [完整文档](../WORKFLOW_README.md)
- [API 参考](../PHASE3_REST_API_COMPLETION.md)
- [编排示例](../omni-agent-workflow/WORKFLOW_INVOKER_EXAMPLES.md)
- [存储配置](../omni-agent-workflow/STORAGE_CONFIGURATION.md)

---

**开始使用 OmniAgent 工作流引擎，让你的应用更强大！** 🚀

