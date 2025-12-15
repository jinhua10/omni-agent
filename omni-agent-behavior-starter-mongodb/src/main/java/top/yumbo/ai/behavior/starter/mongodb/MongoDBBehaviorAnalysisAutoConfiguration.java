package top.yumbo.ai.behavior.starter.mongodb;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.behavior.api.BehaviorAnalysisService;

/**
 * MongoDB 行为分析服务自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
@ConditionalOnClass(MongoTemplate.class)
public class MongoDBBehaviorAnalysisAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(BehaviorAnalysisService.class)
    public BehaviorAnalysisService mongoDBBehaviorAnalysisService(MongoTemplate mongoTemplate) {
        log.info("🚀 Auto-configuring MongoDBBehaviorAnalysisService (Historical Analysis Mode)");
        return new MongoDBBehaviorAnalysisService(mongoTemplate);
    }
}
