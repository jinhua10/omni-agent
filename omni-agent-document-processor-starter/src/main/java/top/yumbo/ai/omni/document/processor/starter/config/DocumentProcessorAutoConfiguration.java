package top.yumbo.ai.omni.document.processor.starter.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.document.processor.DocumentProcessor;
import top.yumbo.ai.omni.document.processor.starter.CompositeDocumentProcessor;

import java.util.List;

/**
 * 文档处理器自动配置
 * (Document Processor Auto Configuration)
 *
 * <p>
 * 自动配置所有文档处理器，包括：
 * - ExcelProcessor (Excel 处理器)
 * - PDFProcessor (PDF 处理器)
 * - WordProcessor (Word 处理器)
 * - PPTProcessor (PowerPoint 处理器)
 * - TextProcessor (文本处理器)
 * - MediaFileProcessor (媒体文件处理器，默认禁用)
 * - VisionLLMDocumentProcessor (Vision LLM 处理器，默认禁用)
 * </p>
 *
 * <p>
 * 所有处理器通过 @Component + @ConditionalOnProperty 自动注册，
 * 可以通过配置文件启用/禁用各个处理器。
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(DocumentProcessorProperties.class)
@ComponentScan(basePackages = "top.yumbo.ai.omni.document.processor.starter.processor")
@ConditionalOnProperty(
    prefix = "omni-agent.document-processor",
    name = "enabled",
    havingValue = "true",
    matchIfMissing = true
)
public class DocumentProcessorAutoConfiguration {

    /**
     * 组合文档处理器
     *
     * <p>
     * 自动注入所有已注册的 DocumentProcessor，
     * 根据文件扩展名选择合适的处理器进行处理。
     * </p>
     *
     * @param processors 所有已注册的文档处理器
     * @return 组合文档处理器
     */
    @Bean
    @ConditionalOnMissingBean(name = "documentProcessor")
    public DocumentProcessor documentProcessor(List<DocumentProcessor> processors) {
        log.info("✅ 初始化组合文档处理器，注册了 {} 个处理器", processors.size());
        return new CompositeDocumentProcessor(processors);
    }
}

