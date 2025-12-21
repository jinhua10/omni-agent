# 工作流引擎 Spring Boot 启动问题修复报告

## 问题描述

当启动 `omni-agent-example-basic` 应用时，出现以下错误：

```
Field workflowEngine in top.yumbo.ai.omni.example.basic.workflow.controller.WorkflowExampleController 
required a bean of type 'top.yumbo.ai.omni.workflow.WorkflowEngine' that could not be found.
```

后续又出现循环依赖问题：

```
The dependencies of some of the beans in the application context form a cycle:
   workflowExampleController -> workflowEngine -> WorkflowInvoker -> workflowEngine
```

## 问题原因

1. **GroupId 不一致**：`omni-agent-workflow` 模块的 parent groupId 是 `top.yumbo.ai`，而主项目使用的是 `top.yumbo.ai.omni`
2. **循环依赖**：`WorkflowEngine` 依赖所有的 `Agent`（包括 `WorkflowInvokerAgent`），而 `WorkflowInvokerAgent` 又依赖 `WorkflowEngine`

## 修复方案

### 1. 修复 GroupId 不一致

修改 `omni-agent-workflow/pom.xml`：

```xml
<parent>
    <groupId>top.yumbo.ai.omni</groupId>  <!-- 从 top.yumbo.ai 改为 top.yumbo.ai.omni -->
    <artifactId>omni-agent</artifactId>
    <version>1.0.0</version>
</parent>
```

修改 `omni-agent-example-basic/pom.xml`：

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>  <!-- 从 top.yumbo.ai 改为 top.yumbo.ai.omni -->
    <artifactId>omni-agent-workflow</artifactId>
    <version>${project.version}</version>
</dependency>
```

### 2. 修复循环依赖

在 `WorkflowInvokerAgent.java` 中添加 `@Lazy` 注解：

```java
@Slf4j
@Component("WorkflowInvoker")
public class WorkflowInvokerAgent implements Agent {

    @Lazy  // 添加 @Lazy 注解打破循环依赖
    @Autowired
    private WorkflowEngine workflowEngine;
    
    // ...
}
```

### 3. 添加配置类（可选）

创建 `WorkflowConfiguration.java` 确保 workflow 包被扫描：

```java
@Configuration
@ComponentScan(basePackages = "top.yumbo.ai.omni.workflow")
public class WorkflowConfiguration {
}
```

修改 `BasicExampleApplication.java`，添加 workflow 包扫描：

```java
@SpringBootApplication
@EnableScheduling
@ComponentScan(basePackages = {
    "top.yumbo.ai.omni.example.basic",
    "top.yumbo.ai.omni.core",
    "top.yumbo.ai.omni.marketplace",
    "top.yumbo.ai.omni.web",
    "top.yumbo.ai.omni.workflow"  // 添加此行
})
public class BasicExampleApplication {
    // ...
}
```

## 验证

执行以下命令验证修复：

```bash
# 1. 重新编译 workflow 模块
cd D:\Jetbrains\omni-agent
mvn clean install -DskipTests -pl omni-agent-workflow -am

# 2. 启动示例应用
cd omni-agent-example-basic
mvn spring-boot:run
```

应用应该能够成功启动，日志中会显示：

```
2025-12-21 00:41:18 [main] INFO  t.y.a.omni.workflow.WorkflowRegistry - ✅ 工作流注册表初始化完成，已加载 0 个工作流
```

## 测试工作流 API

启动成功后，可以通过以下 API 测试工作流功能：

```bash
# 列出所有工作流
curl http://localhost:8080/api/example/workflow/list

# 执行数据处理工作流
curl -X POST http://localhost:8080/api/example/workflow/execute/data-processing \
  -H "Content-Type: application/json" \
  -d '{"data": "test"}'
```

## 总结

问题已成功解决。主要是通过：
1. 统一 Maven groupId
2. 使用 `@Lazy` 注解打破循环依赖
3. 确保 Spring Boot 能够扫描到 workflow 包

这样 `WorkflowEngine` Bean 就能正确注入到 `WorkflowExampleController` 中了。

