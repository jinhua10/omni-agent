# 🔌 工作流引擎 MCP 集成方案

## 📋 MCP (Model Context Protocol) 概述

MCP 是一个开放协议，允许 AI 应用连接到各种数据源和工具：
- **Resources**: 暴露数据和内容（文件、数据库、API 等）
- **Prompts**: 预定义的提示词模板
- **Tools**: 可以被 AI 调用的功能（搜索、计算、API 调用等）

### MCP 的优势

1. **标准化接口**：统一的协议，易于集成
2. **丰富的工具生态**：可以接入各种 MCP Server
3. **动态能力扩展**：无需修改代码即可添加新能力
4. **安全隔离**：每个 MCP Server 独立运行

---

## 🎯 工作流引擎 + MCP 架构设计

### 整体架构

```
┌─────────────────────────────────────────────────────────────┐
│                    工作流引擎层                                │
├─────────────────────────────────────────────────────────────┤
│  WorkflowEngine                                              │
│      ↓                                                        │
│  WorkflowStep  →  Agent  →  MCP Client ⭐                   │
│                              ↓                                │
│                         MCP Protocol                          │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    MCP Server 层 ⭐                          │
├─────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │ Filesystem  │  │   GitHub    │  │   Database  │         │
│  │ MCP Server  │  │ MCP Server  │  │ MCP Server  │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
│                                                               │
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐         │
│  │   Search    │  │     AI      │  │   Custom    │         │
│  │ MCP Server  │  │ MCP Server  │  │ MCP Server  │         │
│  └─────────────┘  └─────────────┘  └─────────────┘         │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    外部资源/工具                              │
├─────────────────────────────────────────────────────────────┤
│  文件系统  │  GitHub API  │  数据库  │  搜索引擎  │  AI 服务 │
└─────────────────────────────────────────────────────────────┘
```

---

## 🔧 核心组件设计

### 1. MCP Client 接口

```java
package top.yumbo.ai.omni.workflow.mcp;

import java.util.List;
import java.util.Map;

/**
 * MCP Client 接口
 * 
 * 用于连接和调用 MCP Server
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
public interface MCPClient {
    
    /**
     * 连接到 MCP Server
     * 
     * @param serverConfig MCP Server 配置
     * @return 是否连接成功
     */
    boolean connect(MCPServerConfig serverConfig);
    
    /**
     * 断开连接
     */
    void disconnect();
    
    /**
     * 列出可用的 Tools
     * 
     * @return Tool 列表
     */
    List<MCPTool> listTools();
    
    /**
     * 调用 Tool
     * 
     * @param toolName Tool 名称
     * @param arguments 参数
     * @return 执行结果
     */
    MCPToolResult callTool(String toolName, Map<String, Object> arguments);
    
    /**
     * 列出可用的 Resources
     * 
     * @return Resource 列表
     */
    List<MCPResource> listResources();
    
    /**
     * 读取 Resource
     * 
     * @param resourceUri Resource URI
     * @return Resource 内容
     */
    MCPResourceContent readResource(String resourceUri);
    
    /**
     * 列出可用的 Prompts
     * 
     * @return Prompt 列表
     */
    List<MCPPrompt> listPrompts();
    
    /**
     * 获取 Prompt
     * 
     * @param promptName Prompt 名称
     * @param arguments 参数
     * @return Prompt 内容
     */
    String getPrompt(String promptName, Map<String, Object> arguments);
}
```

### 2. MCP Agent 实现

```java
package top.yumbo.ai.omni.workflow.agents;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.workflow.Agent;
import top.yumbo.ai.omni.workflow.WorkflowContext;
import top.yumbo.ai.omni.workflow.mcp.MCPClient;
import top.yumbo.ai.omni.workflow.mcp.MCPClientFactory;

import java.util.Map;

/**
 * MCP Agent - 通过 MCP 协议调用外部工具
 * 
 * <p>配置示例:</p>
 * <pre>{@code
 * {
 *   "mcpServer": "github",
 *   "tool": "search_repositories",
 *   "arguments": {
 *     "query": "spring boot",
 *     "language": "java"
 *   }
 * }
 * }</pre>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@Component("MCPAgent")
public class MCPAgent implements Agent {
    
    @Autowired
    private MCPClientFactory mcpClientFactory;
    
    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        @SuppressWarnings("unchecked")
        Map<String, Object> config = (Map<String, Object>) input;
        
        String mcpServer = (String) config.get("mcpServer");
        String tool = (String) config.get("tool");
        @SuppressWarnings("unchecked")
        Map<String, Object> arguments = (Map<String, Object>) config.get("arguments");
        
        log.info("🔌 MCP Agent: 调用 MCP Server [{}], Tool [{}]", mcpServer, tool);
        
        // 获取 MCP Client
        MCPClient client = mcpClientFactory.getClient(mcpServer);
        if (client == null) {
            throw new Exception("MCP Server 不存在: " + mcpServer);
        }
        
        // 调用 Tool
        var result = client.callTool(tool, arguments);
        
        if (!result.isSuccess()) {
            throw new Exception("MCP Tool 调用失败: " + result.getError());
        }
        
        log.info("✅ MCP Tool 调用成功: {}", result.getContent());
        return result.getContent();
    }
    
    @Override
    public String getName() {
        return "MCPAgent";
    }
    
    @Override
    public String getDescription() {
        return "通过 MCP 协议调用外部工具和服务";
    }
}
```

### 3. MCP Server 配置

```yaml
# application.yml
omni-agent:
  mcp:
    servers:
      # GitHub MCP Server
      - name: github
        type: stdio
        command: node
        args:
          - /path/to/mcp-server-github/dist/index.js
        env:
          GITHUB_TOKEN: ${GITHUB_TOKEN}
      
      # Filesystem MCP Server
      - name: filesystem
        type: stdio
        command: node
        args:
          - /path/to/mcp-server-filesystem/dist/index.js
        env:
          ALLOWED_DIRECTORIES: /workspace,/data
      
      # Database MCP Server
      - name: database
        type: stdio
        command: python
        args:
          - -m
          - mcp_server_database
        env:
          DB_URL: ${DB_URL}
      
      # Custom MCP Server
      - name: custom
        type: sse
        url: http://localhost:3000/sse
        apiKey: ${CUSTOM_MCP_API_KEY}
```

---

## 🎨 工作流中使用 MCP

### 场景 1: 源码项目 - GitHub 仓库分析

```yaml
workflow:
  name: "SourceCode-GitHubAnalysis"
  description: "分析 GitHub 仓库"
  
  steps:
    # ⭐ 使用 MCP 获取仓库信息
    - id: "fetch_repo_info"
      name: "获取仓库信息"
      agent: "MCPAgent"
      config:
        mcpServer: "github"
        tool: "get_repository"
        arguments:
          owner: "${workflow.input.owner}"
          repo: "${workflow.input.repo}"
      output: "仓库信息"
    
    # ⭐ 使用 MCP 获取 Issues
    - id: "fetch_issues"
      name: "获取 Issues"
      agent: "MCPAgent"
      config:
        mcpServer: "github"
        tool: "list_issues"
        arguments:
          owner: "${workflow.input.owner}"
          repo: "${workflow.input.repo}"
          state: "open"
      output: "Issues 列表"
    
    # ⭐ 使用 MCP 获取 PRs
    - id: "fetch_prs"
      name: "获取 Pull Requests"
      agent: "MCPAgent"
      config:
        mcpServer: "github"
        tool: "list_pull_requests"
        arguments:
          owner: "${workflow.input.owner}"
          repo: "${workflow.input.repo}"
          state: "open"
      output: "PR 列表"
    
    # 分析仓库活跃度
    - id: "analyze_activity"
      name: "分析仓库活跃度"
      agent: "ActivityAnalyzer"
      input:
        repoInfo: "${fetch_repo_info.output}"
        issues: "${fetch_issues.output}"
        prs: "${fetch_prs.output}"
      output: "活跃度报告"
    
    # 生成报告
    - id: "generate_report"
      name: "生成分析报告"
      agent: "ReportGenerator"
      input: "${analyze_activity.output}"
      output: "最终报告"
```

### 场景 2: 需求文档 - 文件系统操作

```yaml
workflow:
  name: "Requirement-FileAnalysis"
  description: "分析需求文档文件结构"
  
  steps:
    # ⭐ 使用 MCP 读取文件列表
    - id: "list_files"
      name: "列出文件"
      agent: "MCPAgent"
      config:
        mcpServer: "filesystem"
        tool: "list_directory"
        arguments:
          path: "${workflow.input.projectPath}"
          recursive: true
      output: "文件列表"
    
    # ⭐ 使用 MCP 读取特定文件
    - id: "read_requirements"
      name: "读取需求文档"
      agent: "MCPAgent"
      config:
        mcpServer: "filesystem"
        tool: "read_file"
        arguments:
          path: "${workflow.input.projectPath}/requirements.md"
      output: "需求文档内容"
    
    # 解析需求
    - id: "parse_requirements"
      name: "解析需求"
      agent: "RequirementParser"
      input: "${read_requirements.output}"
      output: "结构化需求"
    
    # ⭐ 使用 MCP 写入分析结果
    - id: "write_analysis"
      name: "保存分析结果"
      agent: "MCPAgent"
      config:
        mcpServer: "filesystem"
        tool: "write_file"
        arguments:
          path: "${workflow.input.projectPath}/analysis.json"
          content: "${parse_requirements.output}"
```

### 场景 3: 数据库查询和分析

```yaml
workflow:
  name: "Database-SchemaAnalysis"
  description: "分析数据库结构"
  
  steps:
    # ⭐ 使用 MCP 查询表结构
    - id: "query_tables"
      name: "查询表列表"
      agent: "MCPAgent"
      config:
        mcpServer: "database"
        tool: "list_tables"
        arguments:
          database: "${workflow.input.database}"
      output: "表列表"
    
    # ⭐ 使用 MCP 查询表详情
    - id: "query_schema"
      name: "查询表结构"
      agent: "MCPAgent"
      config:
        mcpServer: "database"
        tool: "describe_table"
        arguments:
          database: "${workflow.input.database}"
          table: "${workflow.input.table}"
      output: "表结构"
    
    # 分析表关系
    - id: "analyze_relationships"
      name: "分析表关系"
      agent: "RelationshipAnalyzer"
      input:
        tables: "${query_tables.output}"
        schema: "${query_schema.output}"
      output: "关系图"
```

---

## 🔗 工作流之间的数据流转 ⭐

### 1. 工作流链式调用

```yaml
workflow:
  name: "SourceCode-ComprehensiveAnalysis"
  description: "源码项目综合分析（链式调用多个工作流）"
  
  steps:
    # Step 1: 调用结构分析工作流
    - id: "structure_analysis"
      name: "结构分析"
      agent: "WorkflowInvoker"  # ⭐ 特殊 Agent：调用其他工作流
      config:
        workflow: "SourceCode-StructureAnalysis"
        input:
          fileName: "${workflow.input.fileName}"
      output: "结构分析结果"
    
    # Step 2: 基于结构分析结果，提取核心模块
    - id: "core_module_extraction"
      name: "提取核心模块"
      agent: "CoreModuleExtractor"
      input:
        structure: "${structure_analysis.output}"
        criteria:
          - 高调用频率
          - 多依赖关系
          - 复杂度高
      output: "核心模块列表"
    
    # Step 3: 针对每个核心模块，调用深度分析工作流
    - id: "deep_analysis"
      name: "核心模块深度分析"
      agent: "WorkflowInvoker"
      config:
        workflow: "SourceCode-ModuleDeepAnalysis"  # ⭐ 调用另一个工作流
        input:
          modules: "${core_module_extraction.output}"
        forEach: true  # ⭐ 对每个模块执行一次
      output: "模块分析结果列表"
    
    # Step 4: 汇总分析结果
    - id: "aggregate_results"
      name: "汇总结果"
      agent: "ResultAggregator"
      input:
        structure: "${structure_analysis.output}"
        coreModules: "${core_module_extraction.output}"
        deepAnalysis: "${deep_analysis.output}"
      output: "综合分析报告"
```

### 2. WorkflowInvoker Agent 实现

```java
package top.yumbo.ai.omni.workflow.agents;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.workflow.Agent;
import top.yumbo.ai.omni.workflow.WorkflowContext;
import top.yumbo.ai.omni.workflow.WorkflowEngine;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

/**
 * WorkflowInvoker Agent - 调用其他工作流
 * 
 * <p>支持两种模式:</p>
 * <ul>
 *   <li>单次调用: 调用一个工作流</li>
 *   <li>批量调用: 对列表中的每个元素调用一次工作流（forEach）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@Component("WorkflowInvoker")
public class WorkflowInvokerAgent implements Agent {
    
    @Autowired
    private WorkflowEngine workflowEngine;
    
    @Override
    public Object execute(Object input, WorkflowContext context) throws Exception {
        @SuppressWarnings("unchecked")
        Map<String, Object> config = (Map<String, Object>) input;
        
        String workflowName = (String) config.get("workflow");
        Object workflowInput = config.get("input");
        Boolean forEach = (Boolean) config.getOrDefault("forEach", false);
        
        log.info("🔗 WorkflowInvoker: 调用工作流 [{}], forEach={}", workflowName, forEach);
        
        if (forEach && workflowInput instanceof List) {
            // 批量调用模式
            return executeBatch(workflowName, (List<?>) workflowInput, context);
        } else {
            // 单次调用模式
            return executeSingle(workflowName, workflowInput, context);
        }
    }
    
    /**
     * 单次调用
     */
    private Object executeSingle(String workflowName, Object input, WorkflowContext parentContext) 
            throws Exception {
        log.info("  → 执行工作流: {}", workflowName);
        
        var result = workflowEngine.execute(workflowName, input);
        
        log.info("  ✓ 工作流完成: {}", workflowName);
        return result.getFinalResult();
    }
    
    /**
     * 批量调用（并行执行）⭐
     */
    private Object executeBatch(String workflowName, List<?> inputs, WorkflowContext parentContext) 
            throws Exception {
        log.info("  → 批量执行工作流: {}, 数量: {}", workflowName, inputs.size());
        
        List<CompletableFuture<Object>> futures = new ArrayList<>();
        
        for (int i = 0; i < inputs.size(); i++) {
            final Object input = inputs.get(i);
            final int index = i;
            
            CompletableFuture<Object> future = CompletableFuture.supplyAsync(() -> {
                try {
                    log.info("  → [{}] 执行工作流: {}", index, workflowName);
                    var result = workflowEngine.execute(workflowName, input);
                    log.info("  ✓ [{}] 工作流完成: {}", index, workflowName);
                    return result.getFinalResult();
                } catch (Exception e) {
                    log.error("  ✗ [{}] 工作流失败: {}", index, workflowName, e);
                    return Map.of("error", e.getMessage());
                }
            });
            
            futures.add(future);
        }
        
        // 等待所有工作流完成
        CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
        
        List<Object> results = futures.stream()
                .map(CompletableFuture::join)
                .toList();
        
        log.info("  ✓ 批量执行完成: 成功 {}/{}", 
                results.stream().filter(r -> !(r instanceof Map && ((Map<?, ?>) r).containsKey("error"))).count(),
                results.size());
        
        return results;
    }
    
    @Override
    public String getName() {
        return "WorkflowInvoker";
    }
    
    @Override
    public String getDescription() {
        return "调用其他工作流，支持单次调用和批量并行调用";
    }
}
```

---

## 📊 完整场景示例：源码项目深度分析

### 工作流 A: 结构分析

```yaml
# workflows/source-code-structure-analysis.yml
workflow:
  name: "SourceCode-StructureAnalysis"
  description: "分析项目模块依赖和整体架构"
  
  steps:
    - id: "extract_files"
      agent: "CodeFileExtractor"
      input: "${workflow.input.fileName}"
    
    - id: "parse_ast"
      agent: "ASTParser"
      input: "${extract_files.output}"
    
    - id: "build_dependency_graph"
      agent: "DependencyGraphBuilder"
      input: "${parse_ast.output}"
    
    - id: "identify_modules"
      agent: "ModuleIdentifier"
      input: "${build_dependency_graph.output}"
      output:
        modules: [...]
        dependencies: {...}
```

### 工作流 B: 漏洞分析

```yaml
# workflows/source-code-vulnerability-analysis.yml
workflow:
  name: "SourceCode-VulnerabilityAnalysis"
  description: "分析项目漏洞"
  
  steps:
    # ⭐ 使用 MCP 查询 CVE 数据库
    - id: "scan_dependencies"
      agent: "MCPAgent"
      config:
        mcpServer: "vulnerability-db"
        tool: "scan_dependencies"
        arguments:
          dependencies: "${workflow.input.dependencies}"
    
    - id: "scan_code_patterns"
      agent: "CodePatternScanner"
      input: "${workflow.input.codeFiles}"
    
    - id: "generate_report"
      agent: "VulnerabilityReportGenerator"
      input:
        dependencyVulnerabilities: "${scan_dependencies.output}"
        codeVulnerabilities: "${scan_code_patterns.output}"
```

### 工作流 C: 核心模块提取

```yaml
# workflows/source-code-core-modules.yml
workflow:
  name: "SourceCode-CoreModules"
  description: "提取项目核心模块"
  
  steps:
    # 依赖工作流 A 的结果 ⭐
    - id: "load_structure"
      agent: "WorkflowResultLoader"
      config:
        workflow: "SourceCode-StructureAnalysis"
        fileName: "${workflow.input.fileName}"
    
    - id: "calculate_metrics"
      agent: "ModuleMetricsCalculator"
      input: "${load_structure.output}"
      metrics:
        - call_frequency
        - dependency_count
        - cyclomatic_complexity
    
    - id: "rank_modules"
      agent: "ModuleRanker"
      input: "${calculate_metrics.output}"
      criteria:
        weights:
          call_frequency: 0.4
          dependency_count: 0.3
          cyclomatic_complexity: 0.3
      output:
        coreModules: [...]  # Top 5 核心模块
```

### 工作流 D: 核心模块深度分析

```yaml
# workflows/source-code-module-deep-analysis.yml
workflow:
  name: "SourceCode-ModuleDeepAnalysis"
  description: "深度分析单个模块"
  
  input:
    moduleName: "string"
    moduleCode: "string"
  
  steps:
    - id: "analyze_functionality"
      agent: "FunctionalityAnalyzer"
      input: "${workflow.input.moduleCode}"
      output: "功能描述"
    
    - id: "analyze_quality"
      agent: "CodeQualityAnalyzer"
      input: "${workflow.input.moduleCode}"
      output:
        strengths: [...]
        weaknesses: [...]
    
    - id: "analyze_extensibility"
      agent: "ExtensibilityAnalyzer"
      input: "${workflow.input.moduleCode}"
      output:
        extensionPoints: [...]
        recommendations: [...]
    
    # ⭐ 使用 MCP 搜索类似代码
    - id: "search_similar_code"
      agent: "MCPAgent"
      config:
        mcpServer: "github"
        tool: "search_code"
        arguments:
          query: "${workflow.input.moduleName}"
          language: "java"
    
    - id: "generate_module_report"
      agent: "ModuleReportGenerator"
      input:
        moduleName: "${workflow.input.moduleName}"
        functionality: "${analyze_functionality.output}"
        quality: "${analyze_quality.output}"
        extensibility: "${analyze_extensibility.output}"
        similarCode: "${search_similar_code.output}"
```

### 主工作流: 综合分析

```yaml
# workflows/source-code-comprehensive-analysis.yml
workflow:
  name: "SourceCode-ComprehensiveAnalysis"
  description: "源码项目综合分析（编排所有工作流）"
  
  steps:
    # Step 1: 结构分析（工作流 A）
    - id: "structure_analysis"
      agent: "WorkflowInvoker"
      config:
        workflow: "SourceCode-StructureAnalysis"
        input:
          fileName: "${workflow.input.fileName}"
    
    # Step 2: 漏洞分析（工作流 B）⭐ 并行执行
    - id: "vulnerability_analysis"
      agent: "WorkflowInvoker"
      config:
        workflow: "SourceCode-VulnerabilityAnalysis"
        input:
          dependencies: "${structure_analysis.output.dependencies}"
          codeFiles: "${structure_analysis.output.codeFiles}"
      parallel: true  # ⭐ 与下一步并行
    
    # Step 3: 核心模块提取（工作流 C）⭐ 并行执行
    - id: "core_modules"
      agent: "WorkflowInvoker"
      config:
        workflow: "SourceCode-CoreModules"
        input:
          fileName: "${workflow.input.fileName}"
      parallel: true  # ⭐ 与上一步并行
    
    # Step 4: 等待并行任务完成
    - id: "wait_parallel"
      agent: "SyncPoint"
      dependencies:
        - vulnerability_analysis
        - core_modules
    
    # Step 5: 核心模块深度分析（工作流 D）⭐ 批量并行执行
    - id: "module_deep_analysis"
      agent: "WorkflowInvoker"
      config:
        workflow: "SourceCode-ModuleDeepAnalysis"
        input: "${core_modules.output.coreModules}"
        forEach: true  # ⭐ 对每个核心模块执行一次
    
    # Step 6: 汇总所有结果
    - id: "final_report"
      agent: "ComprehensiveReportGenerator"
      input:
        structure: "${structure_analysis.output}"
        vulnerabilities: "${vulnerability_analysis.output}"
        coreModules: "${core_modules.output}"
        moduleAnalysis: "${module_deep_analysis.output}"
      output: "综合分析报告"
```

---

## 🎨 工作流持久化和版本管理

### 1. 工作流定义持久化

```
data/workflows/
├── definitions/                    # 工作流定义
│   ├── source-code/
│   │   ├── structure-analysis.yml
│   │   ├── vulnerability-analysis.yml
│   │   ├── core-modules.yml
│   │   ├── module-deep-analysis.yml
│   │   └── comprehensive-analysis.yml
│   ├── requirement/
│   │   └── ...
│   └── tech-doc/
│       └── ...
│
├── versions/                       # 版本历史
│   ├── source-code-structure-analysis/
│   │   ├── v1.0.0.yml
│   │   ├── v1.1.0.yml
│   │   └── v2.0.0.yml
│   └── ...
│
└── templates/                      # 工作流模板
    ├── basic-analysis.yml
    ├── comprehensive-analysis.yml
    └── custom-workflow.yml
```

### 2. 工作流版本管理

```java
package top.yumbo.ai.omni.workflow;

import lombok.Data;

/**
 * 工作流定义（支持版本管理）
 */
@Data
public class WorkflowDefinition {
    private String name;
    private String version;         // ⭐ 版本号（语义化版本）
    private String description;
    private List<WorkflowStep> steps;
    
    private String author;          // 作者
    private long createdAt;         // 创建时间
    private long updatedAt;         // 更新时间
    private List<String> tags;      // 标签
    
    private WorkflowMetadata metadata;
    
    @Data
    public static class WorkflowMetadata {
        private String category;    // 分类（源码/需求/技术文档等）
        private String status;      // 状态（draft/active/deprecated）
        private List<String> dependencies;  // 依赖的其他工作流
        private Map<String, Object> inputSchema;   // 输入参数 Schema
        private Map<String, Object> outputSchema;  // 输出结果 Schema
    }
}
```

### 3. 工作流 CRUD API

```java
@RestController
@RequestMapping("/api/workflows")
public class WorkflowManagementController {
    
    @Autowired
    private WorkflowRegistry workflowRegistry;
    
    /**
     * 创建工作流
     */
    @PostMapping("/definitions")
    public Map<String, Object> createWorkflow(@RequestBody WorkflowDefinition definition) {
        workflowRegistry.register(definition);
        return Map.of("success", true, "name", definition.getName());
    }
    
    /**
     * 更新工作流（创建新版本）
     */
    @PutMapping("/definitions/{name}")
    public Map<String, Object> updateWorkflow(
            @PathVariable String name,
            @RequestBody WorkflowDefinition definition) {
        
        // 自动递增版本号
        String newVersion = incrementVersion(definition.getVersion());
        definition.setVersion(newVersion);
        
        workflowRegistry.register(definition);
        workflowRegistry.archiveVersion(name, definition.getVersion());
        
        return Map.of(
            "success", true,
            "name", name,
            "version", newVersion
        );
    }
    
    /**
     * 获取工作流定义
     */
    @GetMapping("/definitions/{name}")
    public WorkflowDefinition getWorkflow(
            @PathVariable String name,
            @RequestParam(required = false) String version) {
        
        if (version != null) {
            return workflowRegistry.getWorkflow(name, version);
        } else {
            return workflowRegistry.getLatestWorkflow(name);
        }
    }
    
    /**
     * 列出所有工作流
     */
    @GetMapping("/definitions")
    public List<WorkflowInfo> listWorkflows(
            @RequestParam(required = false) String category) {
        
        if (category != null) {
            return workflowRegistry.getWorkflowsByCategory(category);
        } else {
            return workflowRegistry.getAllWorkflows();
        }
    }
    
    /**
     * 删除工作流
     */
    @DeleteMapping("/definitions/{name}")
    public Map<String, Object> deleteWorkflow(@PathVariable String name) {
        workflowRegistry.deactivate(name);
        return Map.of("success", true);
    }
}
```

---

## 🔧 工作流编辑器 UI

### 1. 可视化工作流编辑器

```vue
<template>
  <div class="workflow-editor">
    <!-- 工具栏 -->
    <div class="toolbar">
      <el-button @click="saveWorkflow">保存</el-button>
      <el-button @click="validateWorkflow">验证</el-button>
      <el-button @click="testWorkflow">测试运行</el-button>
      <el-button @click="publishWorkflow">发布</el-button>
    </div>
    
    <!-- 画布区域 -->
    <div class="canvas">
      <!-- ⭐ 使用 Vue Flow 或类似库实现拖拽式编辑 -->
      <VueFlow
        :nodes="nodes"
        :edges="edges"
        @nodesChange="onNodesChange"
        @edgesChange="onEdgesChange"
      >
        <!-- 自定义节点 -->
        <template #node-agent="{ data }">
          <div class="agent-node">
            <div class="node-header">
              <span>{{ data.agent }}</span>
            </div>
            <div class="node-body">
              <p>{{ data.name }}</p>
            </div>
          </div>
        </template>
        
        <template #node-mcp="{ data }">
          <div class="mcp-node">
            <div class="node-header">
              <el-icon><Connection /></el-icon>
              <span>MCP: {{ data.mcpServer }}</span>
            </div>
            <div class="node-body">
              <p>Tool: {{ data.tool }}</p>
            </div>
          </div>
        </template>
        
        <template #node-workflow="{ data }">
          <div class="workflow-node">
            <div class="node-header">
              <el-icon><Share /></el-icon>
              <span>子工作流</span>
            </div>
            <div class="node-body">
              <p>{{ data.workflow }}</p>
            </div>
          </div>
        </template>
      </VueFlow>
    </div>
    
    <!-- 右侧属性面板 -->
    <div class="properties-panel">
      <h3>节点属性</h3>
      <el-form v-if="selectedNode" :model="selectedNode">
        <el-form-item label="步骤ID">
          <el-input v-model="selectedNode.id" />
        </el-form-item>
        <el-form-item label="步骤名称">
          <el-input v-model="selectedNode.data.name" />
        </el-form-item>
        <el-form-item label="Agent">
          <el-select v-model="selectedNode.data.agent">
            <el-option label="MCPAgent" value="MCPAgent" />
            <el-option label="WorkflowInvoker" value="WorkflowInvoker" />
            <el-option label="CodeAnalyzer" value="CodeAnalyzer" />
          </el-select>
        </el-form-item>
        
        <!-- MCP 特定配置 -->
        <template v-if="selectedNode.data.agent === 'MCPAgent'">
          <el-form-item label="MCP Server">
            <el-select v-model="selectedNode.data.config.mcpServer">
              <el-option label="GitHub" value="github" />
              <el-option label="Filesystem" value="filesystem" />
              <el-option label="Database" value="database" />
            </el-select>
          </el-form-item>
          <el-form-item label="Tool">
            <el-select v-model="selectedNode.data.config.tool">
              <!-- 动态加载可用的 Tools -->
              <el-option 
                v-for="tool in availableTools"
                :key="tool.name"
                :label="tool.name"
                :value="tool.name"
              />
            </el-select>
          </el-form-item>
        </template>
      </el-form>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue';
import { VueFlow } from '@vue-flow/core';

const nodes = ref([
  {
    id: '1',
    type: 'agent',
    position: { x: 100, y: 100 },
    data: { agent: 'CodeFileExtractor', name: '提取代码文件' }
  },
  {
    id: '2',
    type: 'mcp',
    position: { x: 100, y: 200 },
    data: { 
      agent: 'MCPAgent',
      name: 'GitHub API 调用',
      mcpServer: 'github',
      tool: 'get_repository'
    }
  },
  {
    id: '3',
    type: 'workflow',
    position: { x: 100, y: 300 },
    data: { 
      agent: 'WorkflowInvoker',
      name: '调用子工作流',
      workflow: 'SourceCode-ModuleAnalysis'
    }
  }
]);

const edges = ref([
  { id: 'e1-2', source: '1', target: '2' },
  { id: 'e2-3', source: '2', target: '3' }
]);

const selectedNode = ref(null);
const availableTools = ref([]);

async function saveWorkflow() {
  // 保存工作流定义
}

async function validateWorkflow() {
  // 验证工作流
}

async function testWorkflow() {
  // 测试运行工作流
}
</script>
```

---

## 💡 总结

### MCP 集成的核心价值

1. **能力扩展** ⭐
   - 无需修改代码即可接入新的工具和服务
   - 统一的接口，易于集成
   - 丰富的 MCP Server 生态

2. **工作流组合** ⭐
   - 工作流可以调用其他工作流（WorkflowInvoker）
   - 支持串行、并行、批量执行
   - 灵活的数据流转

3. **持久化和版本管理** ⭐
   - 工作流定义持久化到文件
   - 语义化版本管理
   - 支持工作流的增删改查

4. **可视化编辑** ⭐
   - 拖拽式工作流编辑器
   - 实时预览和验证
   - 降低使用门槛

### 实施路径

1. **Phase 1**: MCP Client 实现（1周）
   - 实现 MCP 协议客户端
   - 支持 stdio 和 SSE 传输
   - 配置管理

2. **Phase 2**: MCP Agent 和 WorkflowInvoker（1周）
   - 实现 MCPAgent
   - 实现 WorkflowInvokerAgent
   - 支持工作流链式调用

3. **Phase 3**: 工作流持久化和版本管理（1周）
   - 工作流定义 CRUD API
   - 版本管理
   - 工作流注册表

4. **Phase 4**: 可视化编辑器（2周）
   - 拖拽式编辑器
   - 属性面板
   - 测试和发布

**总计：5周完成 MCP 集成！** 🚀

