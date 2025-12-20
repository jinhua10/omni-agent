package top.yumbo.ai.omni.workflow;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.workflow.agents.EchoAgent;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * WorkflowEngine 单元测试
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@SpringBootTest(classes = {
    WorkflowEngineTest.TestConfig.class,
    WorkflowEngine.class,
    WorkflowRegistry.class,
    EchoAgent.class
})
@DisplayName("WorkflowEngine Tests")
class WorkflowEngineTest {

    @Autowired
    private WorkflowEngine workflowEngine;

    @Autowired
    private WorkflowRegistry workflowRegistry;

    @Configuration
    static class TestConfig {
        @Bean
        public Map<String, Agent> agents(EchoAgent echoAgent) {
            Map<String, Agent> agentMap = new HashMap<>();
            agentMap.put("EchoAgent", echoAgent);
            return agentMap;
        }
    }

    @BeforeEach
    void setUp() {
        // 注册测试工作流
        Workflow workflow = Workflow.builder()
                .name("TestWorkflow")
                .version("1.0.0")
                .description("测试工作流")
                .steps(List.of(
                        WorkflowStep.builder()
                                .id("step1")
                                .name("第一步")
                                .agent("EchoAgent")
                                .input("${workflow.input}")
                                .build(),
                        WorkflowStep.builder()
                                .id("step2")
                                .name("第二步")
                                .agent("EchoAgent")
                                .input("${step1.output}")
                                .dependencies(List.of("step1"))
                                .build()
                ))
                .build();

        workflowRegistry.register(workflow);
    }

    @Test
    @DisplayName("测试基本工作流执行")
    void testBasicWorkflowExecution() {
        // Given
        String input = "Hello, Workflow!";

        // When
        WorkflowResult result = workflowEngine.execute("TestWorkflow", input);

        // Then
        assertNotNull(result);
        assertEquals(WorkflowResult.ExecutionStatus.SUCCESS, result.getStatus());
        assertNotNull(result.getExecutionId());
        assertNotNull(result.getStepResults());
        assertEquals(2, result.getStepResults().size());
        assertTrue(result.isSuccess());
        assertFalse(result.isFailed());

        System.out.println("✅ 工作流执行成功");
        System.out.println("   ExecutionId: " + result.getExecutionId());
        System.out.println("   Duration: " + result.getDuration() + "ms");
        System.out.println("   Step Results: " + result.getStepResults());
    }

    @Test
    @DisplayName("测试工作流依赖解析")
    void testWorkflowDependencyResolution() {
        // Given
        Workflow workflow = Workflow.builder()
                .name("DependencyTest")
                .version("1.0.0")
                .steps(List.of(
                        WorkflowStep.builder()
                                .id("stepA")
                                .agent("EchoAgent")
                                .input("A")
                                .build(),
                        WorkflowStep.builder()
                                .id("stepB")
                                .agent("EchoAgent")
                                .input("B")
                                .dependencies(List.of("stepA"))
                                .build(),
                        WorkflowStep.builder()
                                .id("stepC")
                                .agent("EchoAgent")
                                .input("C")
                                .dependencies(List.of("stepA", "stepB"))
                                .build()
                ))
                .build();

        workflowRegistry.register(workflow);

        // When
        WorkflowResult result = workflowEngine.execute("DependencyTest", "test");

        // Then
        assertTrue(result.isSuccess());
        assertEquals(3, result.getStepResults().size());

        System.out.println("✅ 依赖解析测试通过");
    }

    @Test
    @DisplayName("测试工作流不存在的情况")
    void testWorkflowNotFound() {
        // When
        WorkflowResult result = workflowEngine.execute("NonExistentWorkflow", "test");

        // Then
        assertEquals(WorkflowResult.ExecutionStatus.FAILED, result.getStatus());
        assertTrue(result.isFailed());
        assertNotNull(result.getError());
        assertTrue(result.getError().contains("工作流不存在"));

        System.out.println("✅ 工作流不存在异常处理正确");
    }

    @Test
    @DisplayName("测试异步工作流执行")
    void testAsyncWorkflowExecution() throws Exception {
        // When
        var future = workflowEngine.executeAsync("TestWorkflow", "Async Test");
        WorkflowResult result = future.get();

        // Then
        assertTrue(result.isSuccess());
        assertEquals("TestWorkflow", result.getWorkflowName());

        System.out.println("✅ 异步执行测试通过");
    }

    @Test
    @DisplayName("测试变量替换")
    void testVariableReplacement() {
        // Given
        Map<String, Object> input = Map.of(
                "name", "OmniAgent",
                "version", "4.0.0"
        );

        // When
        WorkflowResult result = workflowEngine.execute("TestWorkflow", input);

        // Then
        assertTrue(result.isSuccess());

        // 验证 step1 接收到完整的 input
        @SuppressWarnings("unchecked")
        Map<String, Object> step1Result = (Map<String, Object>) result.getStepResults().get("step1");
        assertNotNull(step1Result);
        assertTrue(step1Result.containsKey("echo"));

        System.out.println("✅ 变量替换测试通过");
        System.out.println("   Step1 Result: " + step1Result);
    }
}

