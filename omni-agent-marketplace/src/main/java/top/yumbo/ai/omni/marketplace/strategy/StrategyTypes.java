package top.yumbo.ai.omni.marketplace.strategy;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * 策略市场相关的数据类型定义
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class StrategyTypes {

    /**
     * 策略类别
     */
    public enum StrategyCategory {
        /** 分块策略 */
        CHUNKING("chunking", "分块策略"),

        /** 重排序策略 */
        RERANK("rerank", "重排序策略"),

        /** 查询扩展策略 */
        QUERY_EXPANSION("query_expansion", "查询扩展策略"),

        /** 向量化策略 */
        EMBEDDING("embedding", "向量化策略"),

        /** 提示词优化策略 */
        PROMPT_OPTIMIZATION("prompt_optimization", "提示词优化策略"),

        /** 自定义策略 */
        CUSTOM("custom", "自定义策略");

        private final String code;
        private final String description;

        StrategyCategory(String code, String description) {
            this.code = code;
            this.description = description;
        }

        public String getCode() {
            return code;
        }

        public String getDescription() {
            return description;
        }
    }

    /**
     * 安全级别
     */
    public enum SecurityLevel {
        /** 完全安全（纯配置，无代码执行） */
        SAFE,

        /** 沙箱隔离（受限环境中执行） */
        SANDBOXED,

        /** 可信任（经过审核的代码） */
        TRUSTED,

        /** 无限制（需要管理员权限） */
        UNRESTRICTED
    }

    /**
     * 作者信息
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class AuthorInfo {
        /** 作者名称 */
        private String name;

        /** 作者邮箱 */
        private String email;

        /** 作者主页 */
        private String homepage;

        /** 组织/公司 */
        private String organization;
    }

    /**
     * 策略依赖
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class StrategyDependency {
        /** 依赖ID */
        private String dependencyId;

        /** 依赖名称 */
        private String name;

        /** 版本要求（语义化版本范围） */
        private String versionRange;

        /** 是否可选 */
        private boolean optional;

        /** 依赖类型 */
        private DependencyType type;

        public enum DependencyType {
            /** 其他策略 */
            STRATEGY,

            /** Java 库 */
            LIBRARY,

            /** 外部服务 */
            SERVICE,

            /** 模型文件 */
            MODEL
        }
    }

    /**
     * 兼容性检查结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class CompatibilityCheck {
        /** 是否兼容 */
        private boolean compatible;

        /** 不兼容原因 */
        private List<String> incompatibilityReasons;

        /** 警告信息 */
        private List<String> warnings;

        /** 框架版本匹配 */
        private boolean frameworkVersionMatch;

        /** 依赖满足 */
        private boolean dependenciesMet;

        /** 缺失的依赖 */
        private List<String> missingDependencies;
    }

    /**
     * 参数验证结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ValidationResult {
        /** 是否有效 */
        private boolean valid;

        /** 错误信息 */
        private List<ValidationError> errors;

        /** 警告信息 */
        private List<String> warnings;

        /** 验证后的参数（可能包含默认值） */
        private Map<String, Object> validatedParams;
    }

    /**
     * 验证错误
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ValidationError {
        /** 参数名 */
        private String parameterName;

        /** 错误类型 */
        private ErrorType errorType;

        /** 错误消息 */
        private String message;

        /** 期望值 */
        private Object expectedValue;

        /** 实际值 */
        private Object actualValue;

        public enum ErrorType {
            MISSING,
            INVALID_TYPE,
            OUT_OF_RANGE,
            INVALID_FORMAT,
            CONSTRAINT_VIOLATION
        }
    }

    /**
     * 执行结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ExecutionResult<T> {
        /** 是否成功 */
        private boolean success;

        /** 结果数据 */
        private T data;

        /** 错误信息 */
        private String error;

        /** 执行时间（毫秒） */
        private long executionTimeMs;

        /** 元数据 */
        private Map<String, Object> metadata;

        /** 警告信息 */
        private List<String> warnings;
    }

    /**
     * 执行上下文
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ExecutionContext {
        /** 请求ID */
        private String requestId;

        /** 用户ID */
        private String userId;

        /** 安全令牌 */
        private String securityToken;

        /** 超时时间（毫秒） */
        private long timeoutMs;

        /** 最大内存（字节） */
        private long maxMemoryBytes;

        /** 最大CPU时间（毫秒） */
        private long maxCpuTimeMs;

        /** 环境变量 */
        private Map<String, String> environment;

        /** 跟踪信息 */
        private Map<String, Object> traceContext;
    }

    /**
     * 健康状态
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class HealthStatus {
        /** 是否健康 */
        private boolean healthy;

        /** 状态码 */
        private HealthStatusCode status;

        /** 详细信息 */
        private String details;

        /** 上次检查时间 */
        private long lastCheckTime;

        /** 问题列表 */
        private List<String> issues;

        public enum HealthStatusCode {
            HEALTHY,
            DEGRADED,
            UNHEALTHY,
            UNKNOWN
        }
    }

    /**
     * 性能指标
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PerformanceMetrics {
        /** 总执行次数 */
        private long totalExecutions;

        /** 成功次数 */
        private long successCount;

        /** 失败次数 */
        private long failureCount;

        /** 平均延迟（毫秒） */
        private double averageLatencyMs;

        /** P50 延迟 */
        private double p50LatencyMs;

        /** P95 延迟 */
        private double p95LatencyMs;

        /** P99 延迟 */
        private double p99LatencyMs;

        /** 最小延迟 */
        private double minLatencyMs;

        /** 最大延迟 */
        private double maxLatencyMs;

        /** 吞吐量（次/秒） */
        private double throughput;

        /** 成功率 */
        private double successRate;

        /** 最后更新时间 */
        private long lastUpdateTime;
    }

    /**
     * 资源使用情况
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ResourceUsage {
        /** 当前内存使用（字节） */
        private long currentMemoryBytes;

        /** 峰值内存使用（字节） */
        private long peakMemoryBytes;

        /** CPU 时间（毫秒） */
        private long cpuTimeMs;

        /** 线程数 */
        private int threadCount;

        /** 打开的文件句柄数 */
        private int openFileHandles;

        /** 网络连接数 */
        private int networkConnections;

        /** 最后更新时间 */
        private long lastUpdateTime;
    }

    /**
     * 执行限制
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ExecutionLimits {
        /** 超时时间（毫秒） */
        private long timeoutMs;

        /** 最大内存（字节） */
        private long maxMemoryBytes;

        /** 最大CPU时间（毫秒） */
        private long maxCpuTimeMs;

        /** 最大并发执行数 */
        private int maxConcurrentExecutions;

        /** 最大输入大小（字节） */
        private long maxInputSizeBytes;

        /** 最大输出大小（字节） */
        private long maxOutputSizeBytes;

        /** 是否允许网络访问 */
        private boolean allowNetworkAccess;

        /** 是否允许文件访问 */
        private boolean allowFileAccess;
    }

    /**
     * 权限
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class Permission {
        /** 权限类型 */
        private PermissionType type;

        /** 权限描述 */
        private String description;

        /** 权限详情（如文件路径、网络地址） */
        private String details;

        /** 是否必需 */
        private boolean required;

        public enum PermissionType {
            FILE_READ,
            FILE_WRITE,
            NETWORK_ACCESS,
            DATABASE_ACCESS,
            SYSTEM_PROPERTY,
            ENVIRONMENT_VARIABLE,
            EXTERNAL_PROCESS
        }
    }

    /**
     * 测试结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class TestResult {
        /** 测试是否通过 */
        private boolean passed;

        /** 测试用例结果 */
        private List<TestCase> testCases;

        /** 总测试数 */
        private int totalTests;

        /** 通过测试数 */
        private int passedTests;

        /** 失败测试数 */
        private int failedTests;

        /** 测试时间（毫秒） */
        private long testDurationMs;
    }

    /**
     * 测试用例
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class TestCase {
        /** 测试名称 */
        private String name;

        /** 是否通过 */
        private boolean passed;

        /** 错误信息 */
        private String error;

        /** 执行时间（毫秒） */
        private long executionTimeMs;
    }

    /**
     * 使用示例
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class UsageExample {
        /** 示例标题 */
        private String title;

        /** 示例描述 */
        private String description;

        /** 输入数据 */
        private Object input;

        /** 参数 */
        private Map<String, Object> parameters;

        /** 期望输出 */
        private Object expectedOutput;

        /** 代码示例 */
        private String codeExample;
    }

    /**
     * 策略元数据
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class StrategyMetadata {
        /** 策略ID */
        private String strategyId;

        /** 名称 */
        private String name;

        /** 类别 */
        private StrategyCategory category;

        /** 版本 */
        private String version;

        /** 描述 */
        private String description;

        /** 作者 */
        private AuthorInfo author;

        /** 创建时间 */
        private long createdAt;

        /** 更新时间 */
        private long updatedAt;

        /** 许可证 */
        private String license;

        /** 主页 */
        private String homepage;

        /** 仓库地址 */
        private String repository;

        /** 文档链接 */
        private String documentation;

        /** 标签 */
        private List<String> tags;

        /** 截图 */
        private List<String> screenshots;

        /** 图标 */
        private String icon;

        /** 价格（0表示免费） */
        private double price;

        /** 货币 */
        private String currency;
    }

    /**
     * 策略评分
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class StrategyRating {
        /** 平均评分（1-5星） */
        private double averageRating;

        /** 评分总数 */
        private int totalRatings;

        /** 5星数量 */
        private int fiveStarCount;

        /** 4星数量 */
        private int fourStarCount;

        /** 3星数量 */
        private int threeStarCount;

        /** 2星数量 */
        private int twoStarCount;

        /** 1星数量 */
        private int oneStarCount;

        /** 下载次数 */
        private long downloadCount;

        /** 使用次数 */
        private long usageCount;
    }
}

