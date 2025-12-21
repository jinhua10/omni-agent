# 🎯 OmniAgent 工作流引擎完整方案

## 📌 核心需求

### 1. 工作流持久化 ⭐
- 工作流定义可以保存和加载
- 支持版本管理（v1.0, v1.1, v2.0...）
- 用户可以创建、修改、删除工作流

### 2. 工作流组合和编排 ⭐
不同的工作流服务于不同的目的，可以相互调用和组合：

#### 源码项目场景示例

```
工作流 A: 分析项目模块依赖和整体架构
  ↓ 输出：项目结构、模块列表、依赖关系

工作流 B: 分析项目漏洞
  ↓ 输出：漏洞列表、安全评分

工作流 C: 提取核心模块
  ↓ 依赖工作流 A 的输出
  ↓ 输出：核心模块列表（Top 5）

工作流 D: 深度分析单个模块
  ↓ 依赖工作流 C 的输出
  ↓ 对每个核心模块执行：
    - 功能细节分析
    - 优缺点分析
    - 扩展性分析
  ↓ 输出：每个模块的详细报告
```

### 3. MCP 集成 ⭐
- 支持通过 MCP 协议调用外部工具
- Agent 可以使用 MCP Server 的能力
- 扩展工作流的处理能力

---

## 🏗️ 完整架构

### 三层架构

```
┌─────────────────────────────────────────────────────────────┐
│                    应用层                                      │
├─────────────────────────────────────────────────────────────┤
│  文档上传 UI  │  工作流管理 UI  │  可视化编辑器  │  AI Chat  │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    工作流引擎层 ⭐                            │
├─────────────────────────────────────────────────────────────┤
│  WorkflowEngine  │  WorkflowRegistry  │  WorkflowInvoker    │
│       ↓                   ↓                    ↓             │
│  Agent 生态        MCP Client         执行器                  │
│  - MCPAgent               ↓              - 串行执行            │
│  - CodeAnalyzer      MCP Protocol       - 并行执行            │
│  - WorkflowInvoker       ↓              - 批量执行            │
│  - ...            MCP Servers                                │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    数据层                                      │
├─────────────────────────────────────────────────────────────┤
│  基础知识库          增强知识库         工作流数据             │
│  data/storage/      data/workflows/    data/workflows/      │
│  - documents/       - knowledge/       - definitions/        │
│  - chunks/          - code-analysis/   - executions/         │
│  - images/          - requirements/    - versions/           │
└─────────────────────────────────────────────────────────────┘
```

---

## 🔄 核心功能

### 1. 工作流定义和持久化

#### 工作流定义（YAML/JSON）

```yaml
workflow:
  name: "SourceCode-StructureAnalysis"
  version: "1.0.0"
  description: "分析项目模块依赖和整体架构"
  category: "源码分析"
  author: "OmniAgent Team"
  
  input:
    fileName: "string"
    projectName: "string"
  
  output:
    structure: "object"
    modules: "array"
    dependencies: "object"
  
  steps:
    - id: "extract_files"
      name: "提取代码文件"
      agent: "CodeFileExtractor"
      input: "${workflow.input.fileName}"
      output: "代码文件列表"
    
    - id: "parse_ast"
      name: "解析 AST"
      agent: "ASTParser"
      input: "${extract_files.output}"
      output: "AST 树"
    
    - id: "build_dependency_graph"
      name: "构建依赖图"
      agent: "DependencyGraphBuilder"
      input: "${parse_ast.output}"
      output: "依赖图"
```

#### 工作流存储结构

```
data/workflows/
├── definitions/                      # 工作流定义
│   ├── source-code/
│   │   ├── structure-analysis.yml   # 工作流 A
│   │   ├── vulnerability.yml        # 工作流 B
│   │   ├── core-modules.yml         # 工作流 C
│   │   └── module-deep-analysis.yml # 工作流 D
│   ├── requirement/
│   └── tech-doc/
│
├── versions/                         # 版本历史
│   ├── source-code-structure-analysis/
│   │   ├── v1.0.0.yml
│   │   ├── v1.1.0.yml
│   │   └── v2.0.0.yml
│   └── ...
│
├── executions/                       # 执行记录
│   ├── 2025-12-20/
│   │   ├── exec-abc123.json
│   │   └── exec-def456.json
│   └── ...
│
└── knowledge/                        # 增强知识库
    ├── code-analysis/
    │   ├── OmniAgent/
    │   │   ├── structure.json
    │   │   ├── dependencies.json
    │   │   └── core-modules.json
    │   └── MyProject/
    └── ...
```

### 2. 工作流编排（链式调用）⭐

#### WorkflowInvokerAgent

允许一个工作流调用另一个工作流：

```yaml
workflow:
  name: "SourceCode-ComprehensiveAnalysis"
  description: "综合分析（编排多个工作流）"
  
  steps:
    # Step 1: 调用工作流 A
    - id: "structure"
      agent: "WorkflowInvoker"
      config:
        workflow: "SourceCode-StructureAnalysis"
        input:
          fileName: "${workflow.input.fileName}"
    
    # Step 2: 并行调用工作流 B 和 C
    - id: "vulnerability"
      agent: "WorkflowInvoker"
      config:
        workflow: "SourceCode-VulnerabilityAnalysis"
        input: "${structure.output}"
      parallel: true  # ⭐ 并行执行
    
    - id: "core_modules"
      agent: "WorkflowInvoker"
      config:
        workflow: "SourceCode-CoreModules"
        input: "${structure.output}"
      parallel: true  # ⭐ 并行执行
    
    # Step 3: 等待并行任务完成
    - id: "sync"
      agent: "SyncPoint"
      dependencies:
        - vulnerability
        - core_modules
    
    # Step 4: 批量调用工作流 D（对每个核心模块）
    - id: "deep_analysis"
      agent: "WorkflowInvoker"
      config:
        workflow: "SourceCode-ModuleDeepAnalysis"
        input: "${core_modules.output.modules}"
        forEach: true  # ⭐ 批量执行
    
    # Step 5: 汇总结果
    - id: "report"
      agent: "ReportAggregator"
      input:
        structure: "${structure.output}"
        vulnerability: "${vulnerability.output}"
        coreModules: "${core_modules.output}"
        deepAnalysis: "${deep_analysis.output}"
```

#### 执行效果

```
用户触发: SourceCode-ComprehensiveAnalysis
    ↓
执行工作流 A: StructureAnalysis
  ✓ 完成，输出：项目结构、模块列表
    ↓
并行执行:
  ├─ 工作流 B: VulnerabilityAnalysis ✓
  └─ 工作流 C: CoreModules ✓
    ↓
批量执行工作流 D: ModuleDeepAnalysis
  ├─ 模块 1: UserService ✓
  ├─ 模块 2: OrderService ✓
  ├─ 模块 3: PaymentService ✓
  ├─ 模块 4: NotificationService ✓
  └─ 模块 5: SecurityService ✓
    ↓
汇总报告 ✓
```

### 3. MCP 集成 ⭐

#### MCPAgent

通过 MCP 协议调用外部工具：

```yaml
steps:
  # 使用 MCP 调用 GitHub API
  - id: "fetch_repo"
    agent: "MCPAgent"
    config:
      mcpServer: "github"
      tool: "get_repository"
      arguments:
        owner: "facebook"
        repo: "react"
    output: "仓库信息"
  
  # 使用 MCP 查询数据库
  - id: "query_db"
    agent: "MCPAgent"
    config:
      mcpServer: "database"
      tool: "execute_query"
      arguments:
        sql: "SELECT * FROM projects WHERE status = 'active'"
    output: "项目列表"
  
  # 使用 MCP 读取文件
  - id: "read_file"
    agent: "MCPAgent"
    config:
      mcpServer: "filesystem"
      tool: "read_file"
      arguments:
        path: "/workspace/README.md"
    output: "文件内容"
```

#### MCP Server 配置

```yaml
# application.yml
omni-agent:
  mcp:
    servers:
      - name: github
        type: stdio
        command: node
        args: ["/path/to/mcp-server-github/dist/index.js"]
        env:
          GITHUB_TOKEN: ${GITHUB_TOKEN}
      
      - name: filesystem
        type: stdio
        command: node
        args: ["/path/to/mcp-server-filesystem/dist/index.js"]
      
      - name: database
        type: stdio
        command: python
        args: ["-m", "mcp_server_database"]
        env:
          DB_URL: ${DB_URL}
```

---

## 🎨 用户交互

### 1. 工作流管理 UI

```vue
<template>
  <div class="workflow-management">
    <!-- 工作流列表 -->
    <el-table :data="workflows">
      <el-table-column prop="name" label="名称" />
      <el-table-column prop="version" label="版本" />
      <el-table-column prop="category" label="分类" />
      <el-table-column label="操作">
        <template #default="{ row }">
          <el-button @click="executeWorkflow(row)">执行</el-button>
          <el-button @click="editWorkflow(row)">编辑</el-button>
          <el-button @click="viewVersions(row)">版本</el-button>
        </template>
      </el-table-column>
    </el-table>
    
    <!-- 创建新工作流 -->
    <el-button @click="createWorkflow">创建工作流</el-button>
  </div>
</template>
```

### 2. 可视化工作流编辑器

```vue
<template>
  <div class="workflow-editor">
    <!-- 拖拽式编辑 -->
    <VueFlow
      :nodes="nodes"
      :edges="edges"
      @nodesChange="onNodesChange"
    >
      <!-- 节点模板 -->
      <template #node-agent="{ data }">
        <div class="agent-node">
          {{ data.agent }}
        </div>
      </template>
      
      <template #node-mcp="{ data }">
        <div class="mcp-node">
          🔌 MCP: {{ data.mcpServer }}
        </div>
      </template>
      
      <template #node-workflow="{ data }">
        <div class="workflow-node">
          🔗 {{ data.workflow }}
        </div>
      </template>
    </VueFlow>
    
    <!-- 工具栏 -->
    <div class="toolbar">
      <el-button @click="addAgent">添加 Agent</el-button>
      <el-button @click="addMCP">添加 MCP 调用</el-button>
      <el-button @click="addWorkflow">添加子工作流</el-button>
      <el-button @click="save">保存</el-button>
      <el-button @click="test">测试</el-button>
    </div>
  </div>
</template>
```

### 3. 工作流执行详情

```vue
<template>
  <div class="execution-detail">
    <!-- 执行概览 -->
    <el-card>
      <h2>{{ execution.workflowName }}</h2>
      <el-tag :type="statusType">{{ execution.status }}</el-tag>
      <p>耗时: {{ execution.duration }}ms</p>
    </el-card>
    
    <!-- 步骤流程图 -->
    <el-card>
      <div class="steps">
        <div 
          v-for="(step, index) in execution.steps"
          :key="step.id"
          class="step"
          :class="step.status"
        >
          <div class="step-number">{{ index + 1 }}</div>
          <div class="step-name">{{ step.name }}</div>
          <div class="step-agent">{{ step.agent }}</div>
          <div class="step-duration">{{ step.duration }}ms</div>
          
          <!-- 子工作流展开 -->
          <div v-if="step.agent === 'WorkflowInvoker'" class="sub-workflow">
            <el-collapse>
              <el-collapse-item title="子工作流执行详情">
                <!-- 递归展示子工作流 -->
              </el-collapse-item>
            </el-collapse>
          </div>
        </div>
      </div>
    </el-card>
    
    <!-- 执行结果 -->
    <el-card>
      <h3>执行结果</h3>
      <pre>{{ JSON.stringify(execution.result, null, 2) }}</pre>
    </el-card>
  </div>
</template>
```

---

## 📊 完整使用场景

### 场景：上传源码项目并进行综合分析

#### 1. 用户上传

```
用户上传 MyWebApp.zip
  + 文档类型: "源码项目 - Java"
  + 项目名: MyWebApp
    ↓
文件自动处理（FileWatcherService）
  - 基础 RAG 索引
  - 自动触发工作流 A: StructureAnalysis
    ↓
增强知识库已构建:
  data/workflows/knowledge/code-analysis/MyWebApp/
    ├── structure.json
    ├── dependencies.json
    └── modules.json
```

#### 2. 用户主动触发综合分析

```
用户在工作流管理页面:
  选择: "SourceCode-ComprehensiveAnalysis"
  输入: MyWebApp.zip
  点击: 执行
    ↓
工作流执行:
  1. 调用工作流 A (已有结果，直接使用) ✓
  2. 并行执行:
     - 工作流 B: VulnerabilityAnalysis ⏳
     - 工作流 C: CoreModules ⏳
  3. 批量执行工作流 D（5个核心模块）⏸️
  4. 生成综合报告 ⏸️
    ↓
执行完成:
  - 总耗时: 2分30秒
  - 发现高危漏洞: 3个
  - 核心模块: 5个
  - 每个模块的详细分析报告已生成
```

#### 3. AI Chat 查询

```
用户: "MyWebApp 的核心模块有哪些？"
    ↓
系统: 从增强知识库查询
  data/workflows/knowledge/code-analysis/MyWebApp/core-modules.json
    ↓
回答: "MyWebApp 有 5 个核心模块：
  1. UserService - 用户管理核心
  2. OrderService - 订单处理核心
  3. PaymentService - 支付核心
  4. NotificationService - 通知核心
  5. SecurityService - 安全核心
  
  [查看详细分析]"

用户: "分析 UserService 的扩展性"
    ↓
系统: 从工作流 D 的执行结果查询
  data/workflows/executions/xxx/module-UserService.json
    ↓
回答: "UserService 的扩展性分析：
  优点:
    - 使用了策略模式，易于扩展认证方式
    - 接口设计清晰，符合开闭原则
  
  缺点:
    - 部分业务逻辑耦合较紧
    - 缺少插件机制
  
  建议:
    - 引入插件系统
    - 抽象用户操作为事件
  
  [查看完整报告]"
```

---

## 🔧 技术实现

### 核心类

```java
// 1. 工作流引擎
public class WorkflowEngine {
    public WorkflowResult execute(String workflowName, Object input);
    public CompletableFuture<WorkflowResult> executeAsync(String workflowName, Object input);
}

// 2. 工作流注册表
public class WorkflowRegistry {
    public void register(WorkflowDefinition definition);
    public WorkflowDefinition getWorkflow(String name, String version);
    public List<WorkflowInfo> getAllWorkflows();
}

// 3. WorkflowInvoker Agent
@Component("WorkflowInvoker")
public class WorkflowInvokerAgent implements Agent {
    public Object execute(Object input, WorkflowContext context) {
        // 支持串行、并行、批量调用其他工作流
    }
}

// 4. MCP Agent
@Component("MCPAgent")
public class MCPAgent implements Agent {
    public Object execute(Object input, WorkflowContext context) {
        // 通过 MCP 协议调用外部工具
    }
}

// 5. MCP Client
public interface MCPClient {
    List<MCPTool> listTools();
    MCPToolResult callTool(String toolName, Map<String, Object> arguments);
}
```

### API 接口

```java
// 工作流管理 API
@RestController
@RequestMapping("/api/workflows")
public class WorkflowManagementController {
    
    @PostMapping("/definitions")
    public WorkflowDefinition createWorkflow(@RequestBody WorkflowDefinition definition);
    
    @PutMapping("/definitions/{name}")
    public WorkflowDefinition updateWorkflow(@PathVariable String name, @RequestBody WorkflowDefinition definition);
    
    @GetMapping("/definitions")
    public List<WorkflowInfo> listWorkflows(@RequestParam(required = false) String category);
    
    @PostMapping("/execute")
    public WorkflowExecution executeWorkflow(@RequestBody WorkflowExecutionRequest request);
    
    @GetMapping("/executions/{id}")
    public WorkflowExecution getExecution(@PathVariable String id);
}
```

---

## 🎯 实施计划

### Phase 1: 基础设施（2周）
- ✅ WorkflowEngine 核心
- ✅ WorkflowRegistry
- ✅ 基础 Agent 实现
- ✅ 工作流定义持久化

### Phase 2: 工作流编排（1周）
- ✅ WorkflowInvokerAgent
- ✅ 支持串行/并行/批量调用
- ✅ 工作流版本管理

### Phase 3: MCP 集成（2周）
- ✅ MCP Client 实现
- ✅ MCPAgent
- ✅ MCP Server 配置

### Phase 4: UI 实现（2周）
- ✅ 工作流管理页面
- ✅ 工作流执行详情
- ✅ 可视化编辑器

### Phase 5: 场景工作流（2周）
- ✅ 源码分析工作流（A/B/C/D）
- ✅ 需求分析工作流
- ✅ 技术文档工作流

### Phase 6: 集成和优化（1周）
- ✅ FileWatcherService 集成
- ✅ AI Chat 集成
- ✅ 性能优化

**总计：10周完成完整系统！** 🚀

---

## 💡 核心价值

1. **工作流持久化** ⭐
   - 用户可以创建和管理自己的工作流
   - 支持版本管理和回滚
   - 易于分享和复用

2. **工作流编排** ⭐
   - 工作流可以调用其他工作流
   - 支持复杂的分析流程
   - 灵活组合，满足各种场景

3. **MCP 集成** ⭐
   - 无需修改代码即可扩展能力
   - 统一的工具调用接口
   - 丰富的 MCP Server 生态

4. **可视化编辑** ⭐
   - 降低使用门槛
   - 拖拽式创建工作流
   - 实时预览和测试

5. **知识积累** ⭐
   - 每个工作流的结果都存储在增强知识库
   - 后续分析可以复用之前的结果
   - 知识图谱不断完善

**OmniAgent 将从知识检索工具升级为智能分析和决策平台！** 🎉

