package top.yumbo.ai.omni.marketplace.strategy;

import java.util.Map;
import java.util.List;
import static top.yumbo.ai.omni.marketplace.strategy.StrategyTypes.*;

/**
 * 策略市场接口（Strategy Marketplace Interface）
 * <p>
 * 这是所有可插拔策略的统一接口，支持：
 * - 分块策略（Chunking Strategy）
 * - 重排序策略（Rerank Strategy）
 * - 查询扩展策略（Query Expansion Strategy）
 * - 向量化策略（Embedding Strategy）
 * - 自定义策略（Custom Strategy）
 * <p>
 * 设计原则：
 * 1. **版本隔离** - 避免版本冲突
 * 2. **安全沙箱** - 防止恶意代码
 * 3. **资源限制** - 防止资源耗尽
 * 4. **性能监控** - 跟踪策略性能
 * 5. **优雅降级** - 失败时自动降级
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface MarketplaceStrategy {

    // ========== 基本信息 ==========

    /**
     * 获取策略ID（全局唯一）
     * 格式：{namespace}.{category}.{name}.{version}
     * 示例：com.example.chunking.semantic.v1
     */
    String getStrategyId();

    /**
     * 获取策略名称
     */
    String getStrategyName();

    /**
     * 获取策略类别
     *
     * @return chunking | rerank | query_expansion | embedding | custom
     */
    StrategyCategory getCategory();

    /**
     * 获取策略版本（语义化版本）
     * 格式：major.minor.patch
     * 示例：1.0.0
     */
    String getVersion();

    /**
     * 获取策略描述
     */
    String getDescription();

    /**
     * 获取策略作者信息
     */
    AuthorInfo getAuthor();

    // ========== 兼容性和依赖 ==========

    /**
     * 获取所需的框架版本
     *
     * @return 语义化版本范围，例如 ">=3.0.0 <4.0.0"
     */
    String getRequiredFrameworkVersion();

    /**
     * 获取策略依赖
     *
     * @return 依赖的其他策略或库
     */
    List<StrategyDependency> getDependencies();

    /**
     * 检查当前环境是否满足策略运行条件
     *
     * @return 兼容性检查结果
     */
    CompatibilityCheck checkCompatibility();

    // ========== 配置和参数 ==========

    /**
     * 获取策略参数定义（JSON Schema）
     * <p>
     * 用于：
     * - 参数验证
     * - UI 自动生成
     * - 文档生成
     *
     * @return JSON Schema 格式的参数定义
     */
    String getParameterSchema();

    /**
     * 获取默认参数
     */
    Map<String, Object> getDefaultParameters();

    /**
     * 验证参数
     *
     * @param params 用户提供的参数
     * @return 验证结果
     */
    ValidationResult validateParameters(Map<String, Object> params);

    // ========== 执行接口 ==========

    /**
     * 执行策略（核心方法）
     *
     * @param input   输入数据
     * @param params  参数
     * @param context 执行上下文（包含安全令牌、资源限制等）
     * @return 执行结果
     * @throws StrategyExecutionException 执行失败
     */
    <I, O> ExecutionResult<O> execute(I input, Map<String, Object> params, ExecutionContext context)
            throws StrategyExecutionException;

    // ========== 生命周期 ==========

    /**
     * 初始化策略
     * <p>
     * 在策略加载后调用一次
     *
     * @param config 初始化配置
     * @throws StrategyInitializationException 初始化失败
     */
    void initialize(Map<String, Object> config) throws StrategyInitializationException;

    /**
     * 销毁策略
     * <p>
     * 在策略卸载前调用
     * 用于释放资源（连接、文件句柄等）
     */
    void destroy();

    /**
     * 健康检查
     *
     * @return 策略是否健康
     */
    HealthStatus checkHealth();

    // ========== 性能和监控 ==========

    /**
     * 获取性能指标
     *
     * @return 性能指标（延迟、吞吐量、成功率等）
     */
    PerformanceMetrics getMetrics();

    /**
     * 获取资源使用情况
     *
     * @return 资源使用（内存、CPU、网络等）
     */
    ResourceUsage getResourceUsage();

    /**
     * 获取执行限制
     *
     * @return 限制配置（超时、内存、并发等）
     */
    ExecutionLimits getLimits();

    // ========== 安全 ==========

    /**
     * 获取所需权限
     *
     * @return 权限列表（文件读写、网络访问等）
     */
    List<Permission> getRequiredPermissions();

    /**
     * 获取安全级别
     *
     * @return SAFE | SANDBOXED | TRUSTED | UNRESTRICTED
     */
    SecurityLevel getSecurityLevel();

    // ========== 测试和验证 ==========

    /**
     * 运行自测试
     *
     * @return 测试结果
     */
    TestResult runSelfTest();

    /**
     * 获取示例用法
     *
     * @return 示例代码和数据
     */
    List<UsageExample> getExamples();

    // ========== 元数据 ==========

    /**
     * 获取完整的策略元数据
     */
    StrategyMetadata getMetadata();

    /**
     * 获取策略标签（用于搜索和分类）
     */
    List<String> getTags();

    /**
     * 获取策略评分
     */
    StrategyRating getRating();
}

