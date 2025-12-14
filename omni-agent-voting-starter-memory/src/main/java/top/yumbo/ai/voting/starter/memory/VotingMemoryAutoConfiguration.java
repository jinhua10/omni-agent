package top.yumbo.ai.voting.starter.memory;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.AutoConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.context.annotation.Bean;
import top.yumbo.ai.voting.api.VotingService;

/**
 * Voting Memory Starter 自动配置
 * (Voting Memory Starter Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@AutoConfiguration
public class VotingMemoryAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public VotingService votingService() {
        log.info("Initializing Memory Voting Service");
        return new MemoryVotingService();
    }
}

