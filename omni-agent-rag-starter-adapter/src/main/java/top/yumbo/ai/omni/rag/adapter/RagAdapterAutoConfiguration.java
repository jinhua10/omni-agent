package top.yumbo.ai.omni.rag.adapter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.RagServiceFactory;

/**
 * RAG 适配器自动配置
 *
 * <p>提供 RAG 服务工厂，支持多域知识网络架构</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@EnableConfigurationProperties(RagAdapterProperties.class)
public class RagAdapterAutoConfiguration {

    /**
     * 注册 RAG 服务工厂
     *
     * @param properties RAG 适配器配置
     * @param ragServiceProvider RAG 服务提供者（从 Spring 容器注入）
     * @return RAG 服务工厂实例
     */
    @Bean
    public RagServiceFactory ragServiceFactory(
            RagAdapterProperties properties,
            ObjectProvider<RagService> ragServiceProvider) {

        log.info("🔧 配置 RAG 服务工厂");
        log.info("  - RAG 类型: {}", properties.getType());
        log.info("  - 向量维度: {}", properties.getVectorDimension());

        // 根据类型显示具体配置
        String type = properties.getType().toLowerCase();
        switch (type) {
            case "file", "lucene" ->
                log.info("  - File 索引路径: {}", properties.getFile().getIndexPath());
            case "sqlite" ->
                log.info("  - SQLite 数据库: {}", properties.getSqlite().getDatabasePath());
            case "mongodb", "mongo" ->
                log.info("  - MongoDB 集合: {}", properties.getMongodb().getCollectionName());
            case "redis" ->
                log.info("  - Redis 前缀: {}", properties.getRedis().getKeyPrefix());
            case "h2" ->
                log.info("  - H2 数据库: {}", properties.getH2().getDatabasePath());
            case "elasticsearch", "es" ->
                log.info("  - Elasticsearch 前缀: {}", properties.getElasticsearch().getIndexPrefix());
            default ->
                log.info("  - 使用默认配置");
        }

        return new DefaultRagServiceFactory(properties, ragServiceProvider);
    }
}

