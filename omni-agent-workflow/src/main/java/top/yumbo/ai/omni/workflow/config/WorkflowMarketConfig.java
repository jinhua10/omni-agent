package top.yumbo.ai.omni.workflow.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import top.yumbo.ai.omni.workflow.repository.WorkflowRepository;
import top.yumbo.ai.omni.workflow.repository.impl.SQLiteWorkflowRepository;

import javax.sql.DataSource;

/**
 * 工作流市场配置
 * (Workflow Market Configuration)
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@Configuration
@ConditionalOnProperty(prefix = "omni-agent.workflow.market", name = "enabled", havingValue = "true", matchIfMissing = true)
public class WorkflowMarketConfig {

    @Value("${omni-agent.workflow.storage-type:auto}")
    private String storageType;

    @Value("${omni-agent.workflow.sqlite.db-path:./data/workflows/workflows.db}")
    private String sqliteDbPath;

    /**
     * 自动检测存储类型
     */
    private String detectStorageType() {
        if (!"auto".equalsIgnoreCase(storageType)) {
            return storageType;
        }

        log.info("🔍 自动检测工作流存储类型...");

        // 检测 MongoDB
        if (isClassPresent("org.springframework.data.mongodb.core.MongoTemplate")) {
            log.info("✅ 检测到 MongoDB 依赖，使用 MongoDB 存储");
            return "mongodb";
        }

        // 检测 Elasticsearch
        if (isClassPresent("co.elastic.clients.elasticsearch.ElasticsearchClient")) {
            log.info("✅ 检测到 Elasticsearch 依赖，使用 Elasticsearch 存储");
            return "elasticsearch";
        }

        // 检测 SQLite
        if (isClassPresent("org.sqlite.JDBC")) {
            log.info("✅ 检测到 SQLite 依赖，使用 SQLite 存储");
            return "sqlite";
        }

        // 默认使用 File
        log.info("ℹ️ 未检测到特定存储依赖，使用 File 存储（YAML）");
        return "file";
    }

    /**
     * 检查类是否存在
     */
    private boolean isClassPresent(String className) {
        try {
            Class.forName(className);
            return true;
        } catch (ClassNotFoundException e) {
            return false;
        }
    }

    /**
     * 创建 SQLite 数据源
     */
    @Bean
    @ConditionalOnProperty(prefix = "omni-agent.workflow", name = "storage-type", havingValue = "sqlite")
    public DataSource workflowDataSource() {
        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName("org.sqlite.JDBC");
        dataSource.setUrl("jdbc:sqlite:" + sqliteDbPath);

        log.info("✅ 工作流数据源已配置: type=sqlite, path={}", sqliteDbPath);
        return dataSource;
    }

    /**
     * 自动模式：根据依赖创建数据源
     */
    @Bean
    @ConditionalOnProperty(prefix = "omni-agent.workflow", name = "storage-type", havingValue = "auto", matchIfMissing = true)
    public DataSource autoWorkflowDataSource() {
        String detectedType = detectStorageType();

        if ("sqlite".equals(detectedType)) {
            DriverManagerDataSource dataSource = new DriverManagerDataSource();
            dataSource.setDriverClassName("org.sqlite.JDBC");
            dataSource.setUrl("jdbc:sqlite:" + sqliteDbPath);
            log.info("✅ 工作流数据源已配置: type=sqlite (auto), path={}", sqliteDbPath);
            return dataSource;
        }

        // 其他存储类型返回 null，由对应的配置类处理
        return null;
    }

    /**
     * 创建 JdbcTemplate（SQLite）
     */
    @Bean
    @ConditionalOnProperty(prefix = "omni-agent.workflow", name = "storage-type", havingValue = "sqlite")
    public JdbcTemplate workflowJdbcTemplate(DataSource workflowDataSource) {
        return new JdbcTemplate(workflowDataSource);
    }

    /**
     * 自动模式：创建 JdbcTemplate
     */
    @Bean
    @ConditionalOnProperty(prefix = "omni-agent.workflow", name = "storage-type", havingValue = "auto", matchIfMissing = true)
    public JdbcTemplate autoWorkflowJdbcTemplate() {
        String detectedType = detectStorageType();

        if ("sqlite".equals(detectedType)) {
            DataSource dataSource = autoWorkflowDataSource();
            if (dataSource != null) {
                return new JdbcTemplate(dataSource);
            }
        }

        return null;
    }

    /**
     * 创建 WorkflowRepository（SQLite）
     */
    @Bean
    @ConditionalOnProperty(prefix = "omni-agent.workflow", name = "storage-type", havingValue = "sqlite")
    public WorkflowRepository workflowRepository(JdbcTemplate workflowJdbcTemplate, ObjectMapper objectMapper) {
        log.info("✅ 使用 SQLite 工作流存储");
        return new SQLiteWorkflowRepository(workflowJdbcTemplate, objectMapper);
    }

    /**
     * 自动模式：创建 WorkflowRepository
     */
    @Bean
    @ConditionalOnProperty(prefix = "omni-agent.workflow", name = "storage-type", havingValue = "auto", matchIfMissing = true)
    public WorkflowRepository autoWorkflowRepository(ObjectMapper objectMapper) {
        String detectedType = detectStorageType();

        if ("sqlite".equals(detectedType)) {
            JdbcTemplate jdbcTemplate = autoWorkflowJdbcTemplate();
            if (jdbcTemplate != null) {
                log.info("✅ 使用 SQLite 工作流存储 (auto)");
                return new SQLiteWorkflowRepository(jdbcTemplate, objectMapper);
            }
        } else if ("mongodb".equals(detectedType)) {
            log.warn("⚠️ MongoDB 存储尚未实现，回退到 File 存储");
            // TODO: 实现 MongoWorkflowRepository
        } else if ("elasticsearch".equals(detectedType)) {
            log.warn("⚠️ Elasticsearch 存储尚未实现，回退到 File 存储");
            // TODO: 实现 ElasticsearchWorkflowRepository
        } else if ("file".equals(detectedType)) {
            log.info("✅ 使用 File 工作流存储 (YAML)");
            // TODO: 实现 FileWorkflowRepository
        }

        // 默认返回 null，市场功能将被禁用
        log.warn("⚠️ 工作流存储未配置，市场功能将被禁用");
        return null;
    }
}

