package top.yumbo.ai.omni.storage.impl.redis;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Redis 文档存储配置属性
 * (Redis Document Storage Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.document-storage.redis")
public class RedisStorageProperties {

    /**
     * Redis 主机
     * 默认: localhost
     */
    private String host = "localhost";

    /**
     * Redis 端口
     * 默认: 6379
     */
    private int port = 6379;

    /**
     * Redis 密码
     */
    private String password;

    /**
     * Redis 数据库索引
     * 默认: 0
     */
    private int database = 0;

    /**
     * Key 前缀
     * 默认: omni-agent:documents:
     */
    private String keyPrefix = "omni-agent:documents:";

    /**
     * 数据过期时间（秒）
     * 0 表示不过期，默认: 0
     */
    private long ttl = 0;

    /**
     * 索引过期时间偏移（秒）
     * 索引的TTL = 数据TTL + 此偏移值，避免孤儿引用
     * 默认: 3600（1小时）
     */
    private long indexTtlOffset = 3600;

    /**
     * Pipeline批量处理大小 - Chunks
     * 默认: 100
     */
    private int chunkBatchSize = 100;

    /**
     * Pipeline批量处理大小 - Images
     * 默认: 50（图片通常较大）
     */
    private int imageBatchSize = 50;

    /**
     * 是否启用性能优化
     * 默认: true
     */
    private boolean enableOptimizations = true;

    /**
     * 是否启用数据压缩
     * 默认: false（为保持兼容性）
     */
    private boolean enableCompression = false;

    /**
     * 压缩阈值（字节）
     * 只有超过此大小的数据才考虑压缩
     * 默认: 1024（1KB）
     */
    private int compressionThreshold = 1024;

    /**
     * 压缩比阈值
     * 只有压缩比超过此值时才使用压缩结果
     * 默认: 0.2（20%）
     */
    private double compressionRatioThreshold = 0.2;
}

