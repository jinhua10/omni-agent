package top.yumbo.ai.p2p.starter.sqlite;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import top.yumbo.ai.p2p.api.P2PDataTransferService;

import javax.sql.DataSource;

/**
 * SQLite P2P自动配置
 * (SQLite P2P Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
@ConditionalOnClass(JdbcTemplate.class)
@EnableConfigurationProperties(SqliteP2PProperties.class)
public class SqliteP2PAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(name = "sqliteP2PDataSource")
    public DataSource sqliteP2PDataSource(SqliteP2PProperties properties) {
        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName("org.sqlite.JDBC");
        dataSource.setUrl("jdbc:sqlite:" + properties.getDatabasePath());
        return dataSource;
    }

    @Bean
    @ConditionalOnMissingBean(name = "sqliteP2PJdbcTemplate")
    public JdbcTemplate sqliteP2PJdbcTemplate(DataSource sqliteP2PDataSource) {
        return new JdbcTemplate(sqliteP2PDataSource);
    }

    @Bean
    @ConditionalOnMissingBean(P2PDataTransferService.class)
    public P2PDataTransferService p2pDataTransferService(
            JdbcTemplate sqliteP2PJdbcTemplate,
            SqliteP2PProperties properties) {
        return new SqliteP2PDataTransferService(sqliteP2PJdbcTemplate, properties);
    }
}
