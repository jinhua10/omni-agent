# 🔗 WorkflowInvoker 使用示例

## 1. 单个工作流调用（single）

调用另一个工作流：

```yaml
name: "MainWorkflow"
version: "1.0.0"
description: "主工作流 - 调用子工作流"

steps:
  - id: "data_process"
    name: "调用数据处理工作流"
    agent: "WorkflowInvoker"
    input:
      mode: "single"
      workflow: "DataProcessing"
      version: "1.0.0"
      input: "${workflow.input}"
```

---

## 2. 批量顺序执行（forEach）

对多个数据项顺序执行工作流：

```yaml
name: "BatchProcessWorkflow"
version: "1.0.0"
description: "批量处理工作流 - 顺序执行"

steps:
  - id: "batch_process"
    name: "批量处理数据"
    agent: "WorkflowInvoker"
    input:
      mode: "forEach"
      workflow: "DataCleaning"
      items:
        - { id: 1, data: "item1" }
        - { id: 2, data: "item2" }
        - { id: 3, data: "item3" }
```

**输出**：
```json
{
  "total": 3,
  "success": 3,
  "failure": 0,
  "results": [
    {
      "index": 0,
      "success": true,
      "result": {...},
      "executionId": "uuid-1"
    },
    {
      "index": 1,
      "success": true,
      "result": {...},
      "executionId": "uuid-2"
    },
    {
      "index": 2,
      "success": true,
      "result": {...},
      "executionId": "uuid-3"
    }
  ]
}
```

---

## 3. 批量并行执行（parallel）

对多个数据项并行执行工作流：

```yaml
name: "ParallelProcessWorkflow"
version: "1.0.0"
description: "并行处理工作流"

steps:
  - id: "parallel_process"
    name: "并行处理数据"
    agent: "WorkflowInvoker"
    input:
      mode: "parallel"
      workflow: "ImageProcessing"
      maxParallel: 5
      items:
        - { imageUrl: "https://example.com/img1.jpg" }
        - { imageUrl: "https://example.com/img2.jpg" }
        - { imageUrl: "https://example.com/img3.jpg" }
        - { imageUrl: "https://example.com/img4.jpg" }
        - { imageUrl: "https://example.com/img5.jpg" }
```

---

## 4. 复杂工作流编排

多级工作流调用：

```yaml
name: "ComplexOrchestrationWorkflow"
version: "1.0.0"
description: "复杂编排工作流"

steps:
  # 第1步：数据提取
  - id: "extract"
    name: "提取数据"
    agent: "DataExtractor"
    input: "${workflow.input}"

  # 第2步：调用清洗工作流（单个）
  - id: "clean"
    name: "清洗数据"
    agent: "WorkflowInvoker"
    input:
      mode: "single"
      workflow: "DataCleaning"
      input: "${extract.output}"
    dependencies: ["extract"]

  # 第3步：批量转换（并行）
  - id: "transform"
    name: "并行转换"
    agent: "WorkflowInvoker"
    input:
      mode: "parallel"
      workflow: "DataTransform"
      maxParallel: 10
      items: "${clean.output.data}"
    dependencies: ["clean"]

  # 第4步：聚合结果
  - id: "aggregate"
    name: "聚合结果"
    agent: "DataAggregator"
    input: "${transform.output.results}"
    dependencies: ["transform"]
```

---

## 5. 动态工作流调用

使用变量替换调用不同的工作流：

```yaml
name: "DynamicInvokerWorkflow"
version: "1.0.0"
description: "动态工作流调用"

steps:
  - id: "select_workflow"
    name: "选择工作流"
    agent: "WorkflowSelector"
    input: "${workflow.input}"

  - id: "invoke_selected"
    name: "调用选中的工作流"
    agent: "WorkflowInvoker"
    input:
      mode: "single"
      workflow: "${select_workflow.output.workflowName}"
      version: "${select_workflow.output.version}"
      input: "${select_workflow.output.data}"
    dependencies: ["select_workflow"]
```

---

## 6. 错误处理

批量执行时的错误处理：

```yaml
name: "RobustBatchWorkflow"
version: "1.0.0"
description: "健壮的批量工作流"

steps:
  - id: "batch_process"
    name: "批量处理（允许部分失败）"
    agent: "WorkflowInvoker"
    input:
      mode: "forEach"
      workflow: "RiskyOperation"
      items: "${workflow.input.items}"
    allowFailure: true  # 允许该步骤失败

  - id: "handle_results"
    name: "处理结果"
    agent: "ResultHandler"
    input:
      total: "${batch_process.output.total}"
      success: "${batch_process.output.success}"
      failure: "${batch_process.output.failure}"
      results: "${batch_process.output.results}"
    dependencies: ["batch_process"]
```

---

## 📊 性能对比

### forEach vs parallel

假设单个工作流执行耗时 100ms：

| 模式 | 10个任务 | 100个任务 | 1000个任务 |
|------|---------|----------|-----------|
| **forEach** | 1秒 | 10秒 | 100秒 |
| **parallel (10)** | 100ms | 1秒 | 10秒 |
| **parallel (100)** | 100ms | 100ms | 1秒 |

**建议**：
- 小批量（<10）：使用 forEach
- 中批量（10-100）：使用 parallel (maxParallel=10)
- 大批量（>100）：使用 parallel (maxParallel=50-100)

---

## 🎯 使用场景

### 1. 数据流水线

```
ExtractWorkflow → TransformWorkflow → LoadWorkflow
     (单个)            (并行批量)          (单个)
```

### 2. 微服务编排

```
AuthWorkflow → [
    UserServiceWorkflow (并行)
    OrderServiceWorkflow (并行)
    PaymentServiceWorkflow (并行)
] → AggregateWorkflow
```

### 3. ETL 处理

```
Extract → [
    Clean (forEach)
    Transform (parallel)
    Validate (forEach)
] → Load
```

---

## ⚙️ 配置参数

| 参数 | 类型 | 必填 | 说明 |
|------|------|------|------|
| mode | string | 否 | 执行模式：single/forEach/parallel（默认：single）|
| workflow | string | 是 | 要调用的工作流名称 |
| version | string | 否 | 工作流版本（默认：最新版本）|
| input | any | 单个模式必填 | 单个模式的输入数据 |
| items | array | 批量模式必填 | 批量模式的输入数据列表 |
| maxParallel | integer | 否 | 并行模式的最大并行数（默认：10）|

---

## 🎉 总结

WorkflowInvokerAgent 提供了强大的工作流编排能力：

- ✅ **灵活调用** - 支持单个、批量、并行
- ✅ **性能优化** - 并行执行大幅提升效率
- ✅ **错误处理** - 完善的异常处理和结果收集
- ✅ **易于使用** - 简单的配置即可实现复杂编排

**让工作流调用工作流变得简单！** 🚀

