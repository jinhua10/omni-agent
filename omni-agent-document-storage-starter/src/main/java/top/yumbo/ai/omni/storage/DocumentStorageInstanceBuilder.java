package top.yumbo.ai.omni.storage;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import io.minio.MinioClient;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.redis.core.RedisTemplate;
import software.amazon.awssdk.services.s3.S3Client;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.impl.elasticsearch.ElasticsearchDocumentStorage;
import top.yumbo.ai.omni.storage.impl.elasticsearch.ElasticsearchStorageProperties;
import top.yumbo.ai.omni.storage.impl.file.FileDocumentStorage;
import top.yumbo.ai.omni.storage.impl.minio.MinIODocumentStorage;
import top.yumbo.ai.omni.storage.impl.minio.MinIOStorageProperties;
import top.yumbo.ai.omni.storage.impl.mongodb.MongoDBDocumentStorage;
import top.yumbo.ai.omni.storage.impl.redis.RedisDocumentStorage;
import top.yumbo.ai.omni.storage.impl.redis.RedisStorageProperties;
import top.yumbo.ai.omni.storage.impl.s3.S3DocumentStorage;
import top.yumbo.ai.omni.storage.impl.s3.S3StorageProperties;

/**
 * 文档存储实例构建器
 *
 * <p>根据配置创建不同类型的文档存储服务实例</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class DocumentStorageInstanceBuilder {

    private final DocumentStorageProperties.StorageInstanceConfig config;
    private Object mongoTemplate;
    private Object redisTemplate;
    private Object s3Client;
    private Object minioClient;
    private Object elasticsearchClient;

    public DocumentStorageInstanceBuilder(DocumentStorageProperties.StorageInstanceConfig config) {
        this.config = config;
    }

    public DocumentStorageInstanceBuilder withMongoTemplate(Object mongoTemplate) {
        this.mongoTemplate = mongoTemplate;
        return this;
    }

    public DocumentStorageInstanceBuilder withRedisTemplate(Object redisTemplate) {
        this.redisTemplate = redisTemplate;
        return this;
    }

    public DocumentStorageInstanceBuilder withS3Client(Object s3Client) {
        this.s3Client = s3Client;
        return this;
    }

    public DocumentStorageInstanceBuilder withMinioClient(Object minioClient) {
        this.minioClient = minioClient;
        return this;
    }

    public DocumentStorageInstanceBuilder withElasticsearchClient(Object elasticsearchClient) {
        this.elasticsearchClient = elasticsearchClient;
        return this;
    }

    /**
     * 构建文档存储服务实例
     */
    public DocumentStorageService build() {
        String instanceId = config.getOrGenerateId();
        String type = config.getType().toLowerCase();

        log.info("🔨 构建文档存储实例: id={}, type={}", instanceId, type);

        try {
            return switch (type) {
                case "file" -> buildFileStorage();
                case "mongodb", "mongo" -> buildMongoDBStorage();
                case "redis" -> buildRedisStorage();
                case "s3" -> buildS3Storage();
                case "minio" -> buildMinIOStorage();
                case "elasticsearch", "es" -> buildElasticsearchStorage();
                default -> {
                    log.warn("⚠️ 未知的存储类型: {}, 使用 File 存储", type);
                    yield buildFileStorage();
                }
            };
        } catch (Exception e) {
            log.error("❌ 创建文档存储实例失败: id={}, type={}", instanceId, type, e);
            log.info("降级使用 File 存储");
            return buildFileStorage();
        }
    }

    private DocumentStorageService buildFileStorage() {
        String baseDir = config.getFile() != null ?
                config.getFile().getBaseDirectory() : "data/documents";

        log.info("✅ 创建 File 存储实例: {}", baseDir);
        return new FileDocumentStorage(baseDir);
    }

    private DocumentStorageService buildMongoDBStorage() {
        if (mongoTemplate == null) {
            throw new IllegalStateException("MongoTemplate 未配置，无法创建 MongoDB 存储实例");
        }

        MongoTemplate template = (MongoTemplate) mongoTemplate;
        String bucketName = "documents";  // 默认 bucket 名称

        if (config.getMongodb() != null && config.getMongodb().getDatabase() != null) {
            // 注意：这里只能设置 bucketName，database 由 MongoTemplate 管理
            bucketName = config.getMongodb().getDatabase();
        }

        log.info("✅ 创建 MongoDB 存储实例，bucket: {}", bucketName);
        return new MongoDBDocumentStorage(template, bucketName);
    }

    private DocumentStorageService buildRedisStorage() {
        if (redisTemplate == null) {
            throw new IllegalStateException("RedisTemplate 未配置，无法创建 Redis 存储实例");
        }

        @SuppressWarnings("unchecked")
        RedisTemplate<String, Object> template = (RedisTemplate<String, Object>) redisTemplate;
        RedisStorageProperties props = new RedisStorageProperties();

        if (config.getRedis() != null) {
            props.setKeyPrefix(config.getRedis().getKeyPrefix());
            if (config.getRedis().getTtl() != null) {
                props.setTtl(config.getRedis().getTtl());
            }
        }

        log.info("✅ 创建 Redis 存储实例");
        return new RedisDocumentStorage(template, props);
    }

    private DocumentStorageService buildS3Storage() {
        if (s3Client == null) {
            throw new IllegalStateException("S3Client 未配置，无法创建 S3 存储实例");
        }

        S3Client client = (S3Client) s3Client;
        S3StorageProperties props = new S3StorageProperties();

        if (config.getS3() != null) {
            DocumentStorageProperties.S3Config s3Config = config.getS3();
            props.setBucketName(s3Config.getBucketName());
            props.setRegion(s3Config.getRegion());
            if (s3Config.getAccessKey() != null) {
                props.setAccessKeyId(s3Config.getAccessKey());
            }
            if (s3Config.getSecretKey() != null) {
                props.setSecretAccessKey(s3Config.getSecretKey());
            }
            if (s3Config.getEndpoint() != null) {
                props.setEndpoint(s3Config.getEndpoint());
            }
        }

        log.info("✅ 创建 S3 存储实例，bucket: {}", props.getBucketName());
        return new S3DocumentStorage(client, props);
    }

    private DocumentStorageService buildMinIOStorage() {
        if (minioClient == null) {
            throw new IllegalStateException("MinioClient 未配置，无法创建 MinIO 存储实例");
        }

        MinioClient client = (MinioClient) minioClient;
        MinIOStorageProperties props = new MinIOStorageProperties();

        if (config.getMinio() != null) {
            DocumentStorageProperties.MinIOConfig minioConfig = config.getMinio();
            props.setEndpoint(minioConfig.getEndpoint());
            props.setBucketName(minioConfig.getBucketName());
            if (minioConfig.getAccessKey() != null) {
                props.setAccessKey(minioConfig.getAccessKey());
            }
            if (minioConfig.getSecretKey() != null) {
                props.setSecretKey(minioConfig.getSecretKey());
            }
        }

        log.info("✅ 创建 MinIO 存储实例，bucket: {}", props.getBucketName());
        return new MinIODocumentStorage(client, props);
    }

    private DocumentStorageService buildElasticsearchStorage() {
        if (elasticsearchClient == null) {
            throw new IllegalStateException("ElasticsearchClient 未配置，无法创建 Elasticsearch 存储实例");
        }

        ElasticsearchClient client = (ElasticsearchClient) elasticsearchClient;
        ElasticsearchStorageProperties props = new ElasticsearchStorageProperties();

        if (config.getElasticsearch() != null) {
            DocumentStorageProperties.ElasticsearchConfig esConfig = config.getElasticsearch();
            if (esConfig.getChunkIndex() != null) {
                props.setIndexPrefix(esConfig.getChunkIndex().replace("-chunks", ""));
            }
        }

        log.info("✅ 创建 Elasticsearch 存储实例");
        return new ElasticsearchDocumentStorage(client, props);
    }
}
