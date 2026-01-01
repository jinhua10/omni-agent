package top.yumbo.ai.omni.p2p.starter.h2;

import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;

import javax.sql.DataSource;

/**
 * H2 P2P自动配置
 * (H2 P2P Auto Configuration)
 *
 * <p>自动配置H2数据库和P2P数据传输服务</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
@ConditionalOnClass({JdbcTemplate.class, org.h2.Driver.class})
@EnableConfigurationProperties(H2P2PProperties.class)
public class H2P2PAutoConfiguration {

    /**
     * 配置H2数据源
     * (Configure H2 DataSource)
     */
    @Bean
    @ConditionalOnMissingBean(name = "h2P2PDataSource")
    public DataSource h2P2PDataSource(H2P2PProperties properties) {
        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName("org.h2.Driver");
        dataSource.setUrl(properties.getJdbcUrl());
        dataSource.setUsername(properties.getUsername());
        dataSource.setPassword(properties.getPassword());
        return dataSource;
    }

    /**
     * 配置H2 JdbcTemplate
     * (Configure H2 JdbcTemplate)
     */
    @Bean
    @ConditionalOnMissingBean(name = "h2P2PJdbcTemplate")
    public JdbcTemplate h2P2PJdbcTemplate(DataSource h2P2PDataSource) {
        return new JdbcTemplate(h2P2PDataSource);
    }

    /**
     * 配置P2P数据传输服务
     * (Configure P2P Data Transfer Service)
     */
    @Bean
    @ConditionalOnMissingBean(P2PDataTransferService.class)
    public P2PDataTransferService p2pDataTransferService(
            JdbcTemplate h2P2PJdbcTemplate,
            H2P2PProperties properties) {
        return new H2P2PDataTransferService(h2P2PJdbcTemplate, properties);
    }
}
