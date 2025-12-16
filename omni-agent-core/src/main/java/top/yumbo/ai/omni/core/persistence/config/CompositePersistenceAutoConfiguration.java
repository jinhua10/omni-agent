package top.yumbo.ai.omni.core.persistence.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Primary;
import top.yumbo.ai.persistence.api.CompositePersistence;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.persistence.api.config.PersistenceCompositeProperties;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * 组合持久化自动配置
 * (Composite Persistence Auto Configuration)
 *
 * <p>
 * 支持多持久化后端同时写入策略：
 * - 主后端：用于读写操作（同步）
 * - 次要后端：用于备份（异步）
 * </p>
 *
 * <p>
 * 配置示例：
 * <pre>
 * omni-agent:
 *   persistence:
 *     strategy: composite
 *     primary-type: sqlite
 *     secondary-types:
 *       - h2
 *       - redis
 * </pre>
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@EnableConfigurationProperties(PersistenceCompositeProperties.class)
@ConditionalOnProperty(
    name = "omni-agent.persistence.strategy",
    havingValue = "composite"
)
public class CompositePersistenceAutoConfiguration {

    /**
     * 创建组合持久化 Bean
     *
     * @param properties   组合持久化配置
     * @param context      Spring 应用上下文
     * @return 组合持久化实例
     */
    @Bean
    @Primary
    public QuestionClassifierPersistence compositePersistence(
            PersistenceCompositeProperties properties,
            ApplicationContext context) {

        log.info("🔧 Configuring CompositePersistence...");
        log.info("   Primary: {}", properties.getPrimaryType());
        log.info("   Secondaries: {}", properties.getSecondaryTypes());

        // 1. 获取主持久化后端
        QuestionClassifierPersistence primary = getPersistenceByType(
            context,
            properties.getPrimaryType()
        );

        if (primary == null) {
            throw new IllegalStateException(
                "Primary persistence backend not found: " + properties.getPrimaryType()
            );
        }

        // 2. 获取次要持久化后端
        List<QuestionClassifierPersistence> secondaries = new ArrayList<>();
        for (String type : properties.getSecondaryTypes()) {
            QuestionClassifierPersistence secondary = getPersistenceByType(context, type);
            if (secondary != null) {
                secondaries.add(secondary);
                log.info("   ✅ Secondary backend loaded: {}", type);
            } else {
                log.warn("   ⚠️  Secondary backend not found (skipped): {}", type);
            }
        }

        // 3. 创建组合持久化
        CompositePersistence composite = new CompositePersistence(primary, secondaries);

        log.info("✅ CompositePersistence configured successfully");
        log.info("   Total backends: 1 primary + {} secondaries", secondaries.size());

        return composite;
    }

    /**
     * 根据类型获取持久化实现
     *
     * @param context Spring 上下文
     * @param type    持久化类型
     * @return 持久化实例
     */
    private QuestionClassifierPersistence getPersistenceByType(
            ApplicationContext context,
            String type) {

        try {
            // 尝试获取所有 QuestionClassifierPersistence beans
            Map<String, QuestionClassifierPersistence> beans =
                context.getBeansOfType(QuestionClassifierPersistence.class);

            // 根据 bean 名称或类名匹配
            for (Map.Entry<String, QuestionClassifierPersistence> entry : beans.entrySet()) {
                String beanName = entry.getKey().toLowerCase();
                String className = entry.getValue().getClass().getSimpleName().toLowerCase();

                if (beanName.contains(type.toLowerCase()) ||
                    className.contains(type.toLowerCase())) {
                    return entry.getValue();
                }
            }

            log.warn("Persistence backend not found for type: {}", type);
            return null;

        } catch (Exception e) {
            log.error("Error getting persistence backend: {}", type, e);
            return null;
        }
    }
}

