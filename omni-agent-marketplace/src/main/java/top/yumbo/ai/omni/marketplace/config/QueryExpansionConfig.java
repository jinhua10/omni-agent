package top.yumbo.ai.omni.marketplace.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.HashMap;
import java.util.Map;

/**
 * 查询扩展配置类
 * (Query Expansion Configuration)
 *
 * <p>
 * 支持配置查询扩展的各种策略和参数
 * (Supports configuring various query expansion strategies and parameters)
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "omni-agent.query-expansion")
public class QueryExpansionConfig {

    /**
     * 是否启用查询扩展
     * (Whether query expansion is enabled)
     */
    private boolean enabled = true;

    /**
     * 最大扩展查询数量
     * (Maximum number of expanded queries)
     */
    private int maxExpansions = 5;

    /**
     * 是否启用LLM查询扩展
     * (Whether LLM-based query expansion is enabled)
     */
    private boolean llmEnabled = false;

    /**
     * LLM模型名称
     * (LLM model name)
     */
    private String llmModel = "qwen2.5";

    /**
     * 查询扩展策略权重
     * (Query expansion strategy weights)
     */
    private Map<String, Double> strategyWeights = new HashMap<>() {{
        put("synonym", 0.3);      // 同义词策略
        put("llm", 0.5);          // LLM策略
        put("domain", 0.2);       // 领域词策略
    }};

    /**
     * 领域词映射
     * (Domain word mapping)
     */
    private Map<String, String[]> domainWords = new HashMap<>() {{
        put("META-INF/spring/spring", new String[]{"boot", "framework", "cloud"});
        put("java", new String[]{"jdk", "jvm", "maven"});
        put("数据库", new String[]{"mysql", "postgresql", "mongodb"});
        put("缓存", new String[]{"redis", "memcached", "caffeine"});
    }};

    /**
     * 缓存配置
     * (Cache configuration)
     */
    private CacheConfig cache = new CacheConfig();

    /**
     * 并行执行配置
     * (Parallel execution configuration)
     */
    private ParallelConfig parallel = new ParallelConfig();

    /**
     * 缓存配置
     */
    @Data
    public static class CacheConfig {
        /**
         * 是否启用缓存
         */
        private boolean enabled = true;

        /**
         * 缓存最大条目数
         */
        private int maxSize = 1000;

        /**
         * 缓存过期时间（分钟）
         */
        private int expireMinutes = 60;
    }

    /**
     * 并行执行配置
     */
    @Data
    public static class ParallelConfig {
        /**
         * 是否启用并行执行
         */
        private boolean enabled = true;

        /**
         * 并行执行超时时间（毫秒）
         */
        private long timeoutMs = 5000;

        /**
         * 线程池大小
         */
        private int threadPoolSize = 10;
    }
}

