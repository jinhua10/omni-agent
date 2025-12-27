package top.yumbo.ai.omni.rag.sqlite;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;
import top.yumbo.ai.omni.rag.RagService;

import javax.sql.DataSource;
import java.io.File;

/**
 * SQLite RAG 自动配置
 * (SQLite RAG Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnClass(org.sqlite.JDBC.class)
@ConditionalOnProperty(
        prefix = "omni-agent.rag",
        name = "type",
        havingValue = "sqlite"
)
@EnableConfigurationProperties(SQLiteRAGProperties.class)
public class SQLiteRAGAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public DataSource sqliteDataSource(SQLiteRAGProperties properties) {
        log.info("配置 SQLite 数据源");
        log.info("数据库路径: {}", properties.getDatabasePath());

        // 确保数据库目录存在
        File dbFile = new File(properties.getDatabasePath());
        File parentDir = dbFile.getParentFile();
        if (parentDir != null && !parentDir.exists()) {
            boolean created = parentDir.mkdirs();
            if (created) {
                log.info("创建数据库目录: {}", parentDir.getAbsolutePath());
            }
        }

        // 配置 HikariCP
        HikariConfig config = new HikariConfig();
        config.setJdbcUrl("jdbc:sqlite:" + properties.getDatabasePath());
        config.setDriverClassName("org.sqlite.JDBC");
        config.setMaximumPoolSize(properties.getMaxPoolSize());
        config.setMinimumIdle(properties.getMinIdle());
        config.setConnectionTimeout(properties.getConnectionTimeout());
        config.setPoolName("SQLiteRAGPool");

        // SQLite 特定配置
        config.addDataSourceProperty("journal_mode", "WAL");
        config.addDataSourceProperty("synchronous", "NORMAL");
        config.addDataSourceProperty("cache_size", "10000");
        config.addDataSourceProperty("temp_store", "MEMORY");

        return new HikariDataSource(config);
    }

    @Bean
    @ConditionalOnMissingBean
    public JdbcTemplate sqliteJdbcTemplate(DataSource sqliteDataSource) {
        return new JdbcTemplate(sqliteDataSource);
    }

    @Bean
    @ConditionalOnMissingBean
    public RagService ragService(JdbcTemplate sqliteJdbcTemplate, SQLiteRAGProperties properties) {
        log.info("自动配置 SQLite RAG 服务");
        log.info("数据库路径: {}", properties.getDatabasePath());
        log.info("启用 FTS5: {}", properties.isEnableFts());
        log.info("最大连接数: {}", properties.getMaxPoolSize());
        return new SQLiteRAGService(sqliteJdbcTemplate, properties);
    }
}
