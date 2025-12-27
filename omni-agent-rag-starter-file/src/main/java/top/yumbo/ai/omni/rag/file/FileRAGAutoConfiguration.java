package top.yumbo.ai.omni.rag.file;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.rag.RagService;

/**
 * File RAG 自动配置
 * (File RAG Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnClass(org.apache.lucene.index.IndexWriter.class)
@ConditionalOnProperty(
        prefix = "omni-agent.rag",
        name = "type",
        havingValue = "file",
        matchIfMissing = true  // 默认使用 File RAG
)
@EnableConfigurationProperties(FileRAGProperties.class)
public class FileRAGAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean
    public RagService ragService(FileRAGProperties properties) {
        log.info("自动配置 File RAG (Lucene) 服务");
        log.info("索引路径: {}", properties.getIndexPath());
        log.info("RAM 缓冲区: {} MB", properties.getRamBufferSizeMb());
        return new LuceneRAGService(properties);
    }
}
