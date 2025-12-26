package top.yumbo.ai.ocr.tesseract;

import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * Tesseract OCR 自动配置
 * (Tesseract OCR Auto Configuration)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Configuration
@ConditionalOnClass(net.sourceforge.tess4j.Tesseract.class)
@ConditionalOnProperty(
    prefix = "omni-agent.ocr.tesseract",
    name = "enabled",
    havingValue = "true"
)
@EnableConfigurationProperties(TesseractOCRProperties.class)
public class TesseractOCRAutoConfiguration {

    @Bean
    public TesseractOCRService tesseractOCRService(TesseractOCRProperties properties) {
        log.info("🔧 [OCR] 正在配置 Tesseract OCR 服务...");

        TesseractOCRService service = new TesseractOCRService(properties);

        log.info("✅ [OCR] Tesseract OCR 服务已配置");
        log.info("   - 语言: {}", properties.getLanguage());
        log.info("   - DPI: {}", properties.getDpi());
        log.info("   - 数据路径: {}",
                properties.getDataPath() != null && !properties.getDataPath().isEmpty()
                    ? properties.getDataPath()
                    : "系统默认");

        return service;
    }
}

