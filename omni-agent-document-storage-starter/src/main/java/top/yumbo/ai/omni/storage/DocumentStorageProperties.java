package top.yumbo.ai.omni.storage;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.ArrayList;
import java.util.List;

/**
 * 文档存储统一配置属性
 *
 * <p>支持多实例配置，类似 RAG 的配置方式</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.document-storage")
public class DocumentStorageProperties {

    /**
     * 存储实例列表
     *
     * <p>如果为空，系统会自动创建默认的 File 实例</p>
     */
    private List<StorageInstanceConfig> instances = new ArrayList<>();

    /**
     * 存储实例配置
     */
    @Data
    public static class StorageInstanceConfig {
        /**
         * 实例 ID（唯一标识）
         */
        private String id;

        /**
         * 实例名称（描述性）
         */
        private String name;

        /**
         * 存储类型：file, mongodb, redis, s3, minio, elasticsearch
         */
        private String type = "file";

        /**
         * 是否为主实例
         */
        private boolean primary = false;

        /**
         * File 配置
         */
        private FileConfig file;

        /**
         * MongoDB 配置
         */
        private MongoDBConfig mongodb;

        /**
         * Redis 配置
         */
        private RedisConfig redis;

        /**
         * S3 配置
         */
        private S3Config s3;

        /**
         * MinIO 配置
         */
        private MinIOConfig minio;

        /**
         * Elasticsearch 配置
         */
        private ElasticsearchConfig elasticsearch;

        /**
         * 获取或生成实例 ID
         */
        public String getOrGenerateId() {
            if (id == null || id.isEmpty()) {
                return type + "-" + System.currentTimeMillis();
            }
            return id;
        }
    }

    /**
     * File 存储配置
     */
    @Data
    public static class FileConfig {
        private String baseDirectory = "data/documents";
    }

    /**
     * MongoDB 存储配置
     */
    @Data
    public static class MongoDBConfig {
        private String database = "omni-agent";
        private String chunkCollection = "document_chunks";
        private String imageCollection = "document_images";
    }

    /**
     * Redis 存储配置
     */
    @Data
    public static class RedisConfig {
        private String keyPrefix = "omni:storage:";
        private Long ttl; // 过期时间（秒）
    }

    /**
     * S3 存储配置
     */
    @Data
    public static class S3Config {
        private String bucketName = "omni-agent-documents";
        private String endpoint;
        private String accessKey;
        private String secretKey;
        private String region = "us-east-1";
    }

    /**
     * MinIO 存储配置
     */
    @Data
    public static class MinIOConfig {
        private String endpoint = "http://localhost:9000";
        private String accessKey;
        private String secretKey;
        private String bucketName = "omni-agent-documents";
    }

    /**
     * Elasticsearch 存储配置
     */
    @Data
    public static class ElasticsearchConfig {
        private String chunkIndex = "document-chunks";
        private String imageIndex = "document-images";
        private String documentIndex = "documents";
    }

    /**
     * 获取主实例配置
     */
    public StorageInstanceConfig getPrimaryInstance() {
        return instances.stream()
                .filter(StorageInstanceConfig::isPrimary)
                .findFirst()
                .orElse(instances.isEmpty() ? null : instances.get(0));
    }

    /**
     * 检查是否配置了多个实例
     */
    public boolean hasMultipleInstances() {
        return instances.size() > 1;
    }
}

