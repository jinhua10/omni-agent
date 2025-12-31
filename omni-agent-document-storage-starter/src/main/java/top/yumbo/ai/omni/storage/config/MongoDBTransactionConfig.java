package top.yumbo.ai.omni.storage.config;

import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.mongodb.MongoDatabaseFactory;
import org.springframework.data.mongodb.MongoTransactionManager;
import org.springframework.context.annotation.Bean;
import lombok.extern.slf4j.Slf4j;

/**
 * MongoDB事务配置
 *
 * <p>✅ P2优化：启用MongoDB事务支持</p>
 * <p>需要MongoDB副本集（Replica Set）才能使用事务</p>
 * <p>单节点MongoDB不支持事务，会自动降级为手动回滚模式</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 * @version P2 Optimized - MongoDB Transactions
 */
@Slf4j
@Configuration
@ConditionalOnProperty(
    prefix = "omni-agent.document-storage.mongodb",
    name = "enable-transactions",
    havingValue = "true",
    matchIfMissing = false
)
public class MongoDBTransactionConfig {

    /**
     * MongoDB事务管理器
     *
     * @param dbFactory MongoDB数据库工厂
     * @return 事务管理器
     */
    @Bean
    public MongoTransactionManager transactionManager(MongoDatabaseFactory dbFactory) {
        log.info("✅ MongoDB事务管理器已启用（需要副本集支持）");
        return new MongoTransactionManager(dbFactory);
    }
}

