package top.yumbo.ai.rag.h2;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * H2 RAG 配置属性
 * (H2 RAG Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.rag.h2")
public class H2RAGProperties {

    /**
     * H2数据库URL
     */
    private String url = "jdbc:h2:./data/omni-agent-rag;AUTO_SERVER=TRUE";

    /**
     * 数据库用户名
     */
    private String username = "sa";

    /**
     * 数据库密码
     */
    private String password = "";

    /**
     * 最大连接池大小
     */
    private int maxPoolSize = 10;

    /**
     * 最小空闲连接数
     */
    private int minPoolSize = 2;

    /**
     * 连接超时时间（毫秒）
     */
    private long connectionTimeout = 30000;

    /**
     * 空闲超时时间（毫秒）
     */
    private long idleTimeout = 600000;

    /**
     * 连接最大生命周期（毫秒）
     */
    private long maxLifetime = 1800000;
}
