package top.yumbo.ai.omni.voting.starter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.voting.VotingService;

/**
 * Voting 服务自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(VotingProperties.class)
public class VotingAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public VotingServiceFactory votingServiceFactory(VotingProperties properties) {
        log.info("🔧 初始化 Voting 服务工厂");
        return new VotingServiceFactory(properties, null);
    }

    @Bean
    @ConditionalOnMissingBean
    public VotingService votingService(VotingServiceFactory factory) {
        log.info("🔧 初始化 Voting 服务");
        return factory.getVotingService();
    }
}

