package top.yumbo.ai.persistence.api.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.ArrayList;
import java.util.List;

/**
 * 组合持久化配置属性
 * (Composite Persistence Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.persistence")
public class PersistenceCompositeProperties {

    /**
     * 持久化策略
     * simple: 单一后端
     * composite: 组合模式（多写）
     */
    private String strategy = "simple";

    /**
     * 主持久化类型
     * 用于读写操作
     */
    private String primaryType;

    /**
     * 次要持久化类型列表
     * 用于异步备份写入
     */
    private List<String> secondaryTypes = new ArrayList<>();

    /**
     * 是否启用组合持久化
     */
    public boolean isComposite() {
        return "composite".equalsIgnoreCase(strategy);
    }
}

