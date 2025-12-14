package top.yumbo.ai.omni.benchmark;

import org.openjdk.jmh.annotations.*;
import org.openjdk.jmh.runner.Runner;
import org.openjdk.jmh.runner.RunnerException;
import org.openjdk.jmh.runner.options.Options;
import org.openjdk.jmh.runner.options.OptionsBuilder;
import top.yumbo.ai.omni.core.evolution.ConceptVersion;
import top.yumbo.ai.omni.core.evolution.EvolutionService;
import top.yumbo.ai.omni.core.feedback.FeedbackService;
import top.yumbo.ai.omni.core.role.Role;
import top.yumbo.ai.omni.core.role.RoleService;

import java.util.Arrays;
import java.util.HashMap;
import java.util.concurrent.TimeUnit;

/**
 * 核心服务性能基准测试
 * (Core Services Performance Benchmark)
 * <p>
 * 测试FeedbackService、RoleService、EvolutionService性能
 * (Tests performance of FeedbackService, RoleService, EvolutionService)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@BenchmarkMode(Mode.AverageTime)
@OutputTimeUnit(TimeUnit.MICROSECONDS)
@State(Scope.Thread)
@Fork(value = 1, warmups = 1)
@Warmup(iterations = 3, time = 1)
@Measurement(iterations = 5, time = 1)
public class CoreServicesBenchmark {

    private FeedbackService feedbackService;
    private RoleService roleService;
    private EvolutionService evolutionService;

    private static int counter = 0;

    @Setup(Level.Trial)
    public void setup() {
        feedbackService = new FeedbackService();
        roleService = new RoleService();
        evolutionService = new EvolutionService();

        roleService.init();

        // Pre-register some roles
        for (int i = 0; i < 10; i++) {
            Role role = Role.builder()
                    .id("role-" + i)
                    .name("Role " + i)
                    .description("Test role " + i)
                    .keywords(Arrays.asList("keyword" + i, "test"))
                    .enabled(true)
                    .priority(i)
                    .properties(new HashMap<>())
                    .build();
            roleService.registerRole(role);
        }

        // Pre-create some concept versions
        for (int i = 0; i < 10; i++) {
            evolutionService.createVersion(
                    "concept-" + i,
                    "Content " + i,
                    "Initial version",
                    ConceptVersion.ChangeType.CREATE,
                    "system"
            );
        }
    }

    // ==================== FeedbackService Benchmarks ====================

    /**
     * 基准测试1: 收集显式反馈
     */
    @Benchmark
    public void testCollectExplicitFeedback() {
        feedbackService.collectExplicit(
                "session-" + (counter++),
                "user-1",
                "Test question",
                "Test answer",
                1.0,
                new String[]{"test"},
                "Test comment"
        );
    }

    /**
     * 基准测试2: 收集隐式反馈
     */
    @Benchmark
    public void testCollectImplicitFeedback() {
        feedbackService.collectImplicit(
                "session-" + (counter++),
                "user-1",
                "Test question",
                "Test answer",
                0.8
        );
    }

    /**
     * 基准测试3: 获取会话反馈
     */
    @Benchmark
    public int testGetSessionFeedback() {
        return feedbackService.getSessionFeedback("session-0").size();
    }

    /**
     * 基准测试4: 获取反馈统计
     */
    @Benchmark
    public Object testGetFeedbackStatistics() {
        return feedbackService.getStatistics();
    }

    // ==================== RoleService Benchmarks ====================

    /**
     * 基准测试5: 注册角色
     */
    @Benchmark
    public void testRegisterRole() {
        Role role = Role.builder()
                .id("bench-role-" + (counter++))
                .name("Benchmark Role")
                .description("For benchmarking")
                .keywords(Arrays.asList("benchmark", "test"))
                .enabled(true)
                .priority(5)
                .properties(new HashMap<>())
                .build();
        roleService.registerRole(role);
    }

    /**
     * 基准测试6: 获取角色
     */
    @Benchmark
    public Role testGetRole() {
        return roleService.getRole("role-5");
    }

    /**
     * 基准测试7: 获取所有启用角色
     */
    @Benchmark
    public int testGetEnabledRoles() {
        return roleService.getEnabledRoles().size();
    }

    /**
     * 基准测试8: 关键词匹配角色
     */
    @Benchmark
    public int testMatchRolesByKeywords() {
        return roleService.matchRolesByKeywords(Arrays.asList("test", "keyword5")).size();
    }

    /**
     * 基准测试9: 记录角色使用
     */
    @Benchmark
    public void testRecordRoleUsage() {
        roleService.recordUsage("role-5");
    }

    /**
     * 基准测试10: 获取角色使用统计
     */
    @Benchmark
    public Object testGetRoleUsageStats() {
        return roleService.getUsageStats();
    }

    // ==================== EvolutionService Benchmarks ====================

    /**
     * 基准测试11: 创建概念版本
     */
    @Benchmark
    public ConceptVersion testCreateVersion() {
        return evolutionService.createVersion(
                "bench-concept-" + (counter++),
                "Benchmark content",
                "Benchmark version",
                ConceptVersion.ChangeType.CREATE,
                "benchmark-user"
        );
    }

    /**
     * 基准测试12: 获取当前版本
     */
    @Benchmark
    public ConceptVersion testGetCurrentVersion() {
        return evolutionService.getCurrentVersion("concept-5");
    }

    /**
     * 基准测试13: 获取版本历史
     */
    @Benchmark
    public int testGetVersionHistory() {
        return evolutionService.getVersionHistory("concept-5").size();
    }

    /**
     * 基准测试14: 获取版本统计
     */
    @Benchmark
    public Object testGetVersionStatistics() {
        return evolutionService.getStatistics();
    }

    // ==================== 综合基准测试 ====================

    /**
     * 基准测试15: 综合工作流（反馈+角色+演化）
     */
    @Benchmark
    public void testIntegratedWorkflow() {
        int id = counter++;
        
        // 1. 收集反馈
        feedbackService.collectExplicit(
                "workflow-session-" + id,
                "workflow-user-" + id,
                "Workflow question",
                "Workflow answer",
                0.9,
                new String[]{"workflow"},
                "Workflow comment"
        );

        // 2. 记录角色使用
        roleService.recordUsage("role-5");

        // 3. 创建版本
        evolutionService.createVersion(
                "workflow-concept-" + id,
                "Workflow content",
                "Workflow version",
                ConceptVersion.ChangeType.UPDATE,
                "workflow-user"
        );
    }

    /**
     * 主函数 - 运行所有基准测试
     */
    public static void main(String[] args) throws RunnerException {
        Options opt = new OptionsBuilder()
                .include(CoreServicesBenchmark.class.getSimpleName())
                .forks(1)
                .build();

        new Runner(opt).run();
    }
}
