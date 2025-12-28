package top.yumbo.ai.omni.document.processor.starter.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * 文档处理器配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.document-processor")
public class DocumentProcessorProperties {

    /**
     * 是否启用文档处理器
     */
    private boolean enabled = true;

    /**
     * PDF 配置
     */
    private Pdf pdf = new Pdf();

    /**
     * Word 配置
     */
    private Word word = new Word();

    /**
     * Excel 配置
     */
    private Excel excel = new Excel();

    /**
     * PPT 配置
     */
    private Ppt ppt = new Ppt();

    /**
     * PDF 配置
     */
    @Data
    public static class Pdf {
        /**
         * 是否提取图片
         */
        private boolean extractImages = false;

        /**
         * 是否启用 OCR
         */
        private boolean ocrEnabled = false;
    }

    /**
     * Word 配置
     */
    @Data
    public static class Word {
        /**
         * 是否保留格式
         */
        private boolean preserveFormatting = false;
    }

    /**
     * Excel 配置
     */
    @Data
    public static class Excel {
        /**
         * 最大读取行数
         */
        private int maxRows = 10000;

        /**
         * 是否包含表头
         */
        private boolean includeHeaders = true;
    }

    /**
     * PPT 配置
     */
    @Data
    public static class Ppt {
        /**
         * 是否提取备注
         */
        private boolean extractNotes = true;
    }
}

