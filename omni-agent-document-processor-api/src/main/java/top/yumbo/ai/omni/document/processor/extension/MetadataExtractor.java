package top.yumbo.ai.omni.document.processor.extension;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingContext;

import java.util.Map;

/**
 * 文档元数据提取器
 * (Document Metadata Extractor)
 *
 * <p>
 * 提取文档的元数据信息，可用于：
 * - 文档属性提取（作者、标题、创建时间等）
 * - 版本信息
 * - 安全标签
 * - 自定义属性
 * </p>
 *
 * <p>示例实现：</p>
 * <pre>{@code
 * @Component
 * @Order(1)
 * public class BasicMetadataExtractor implements MetadataExtractor {
 *     @Override
 *     public ExtractedMetadata extract(ProcessingContext context) {
 *         return ExtractedMetadata.builder()
 *                 .author("John Doe")
 *                 .title("Sample Document")
 *                 .createdDate("2024-01-01")
 *                 .build();
 *     }
 * }
 * }</pre>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface MetadataExtractor extends DocumentProcessorExtension {

    /**
     * 提取元数据
     *
     * @param context 处理上下文
     * @return 提取的元数据
     * @throws Exception 提取失败时抛出异常
     */
    ExtractedMetadata extract(ProcessingContext context) throws Exception;

    /**
     * 提取的元数据
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ExtractedMetadata {
        /** 作者 */
        private String author;

        /** 标题 */
        private String title;

        /** 主题 */
        private String subject;

        /** 关键词 */
        private String keywords;

        /** 创建日期 */
        private String createdDate;

        /** 修改日期 */
        private String modifiedDate;

        /** 文档版本 */
        private String version;

        /** 文档语言 */
        private String language;

        /** 安全级别 */
        private String securityLevel;

        /** 自定义属性 */
        private Map<String, Object> customProperties;
    }
}

