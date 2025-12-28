package top.yumbo.ai.omni.document.processor.starter.config;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.document.processor.DocumentProcessor;
import top.yumbo.ai.omni.document.processor.starter.CompositeDocumentProcessor;
import top.yumbo.ai.omni.document.processor.starter.processor.*;

import java.util.List;

/**
 * 文档处理器自动配置
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@EnableConfigurationProperties(DocumentProcessorProperties.class)
@ConditionalOnProperty(prefix = "omni-agent.document-processor", name = "enabled", havingValue = "true", matchIfMissing = true)
public class DocumentProcessorAutoConfiguration {

    @Bean
    public PDFDocumentProcessor pdfProcessor(DocumentProcessorProperties properties) {
        log.info("✅ 注册 PDF 处理器");
        return new PDFDocumentProcessor();
    }

    @Bean
    public WordDocumentProcessor wordProcessor(DocumentProcessorProperties properties) {
        log.info("✅ 注册 Word 处理器");
        return new WordDocumentProcessor();
    }

    @Bean
    public ExcelDocumentProcessor excelProcessor(DocumentProcessorProperties properties) {
        log.info("✅ 注册 Excel 处理器");
        return new ExcelDocumentProcessor();
    }


    @Bean
    public PlainTextDocumentProcessor textProcessor() {
        log.info("✅ 注册 Text 处理器");
        return new PlainTextDocumentProcessor();
    }

    @Bean
    @ConditionalOnMissingBean
    public DocumentProcessor documentProcessor(List<DocumentProcessor> processors) {
        log.info("✅ 初始化组合文档处理器，注册了 {} 个处理器", processors.size());
        return new CompositeDocumentProcessor(processors);
    }
}

