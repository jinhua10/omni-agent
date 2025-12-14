package top.yumbo.ai.persistence.h2;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * H2 持久化配置属性
 * (H2 Persistence Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.persistence.h2")
public class H2PersistenceProperties {

    /**
     * 数据库 URL
     * 默认: jdbc:h2:./data/omni-agent (嵌入式模式)
     */
    private String url = "jdbc:h2:./data/omni-agent";

    /**
     * 用户名
     */
    private String username = "sa";

    /**
     * 密码
     */
    private String password = "";

    /**
     * 是否自动创建表
     */
    private boolean autoCreateTables = true;

    /**
     * 是否启用 SQL 日志
     */
    private boolean showSql = false;
}

