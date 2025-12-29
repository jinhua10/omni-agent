package top.yumbo.ai.omni.document.processor.extension;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingContext;

import java.util.Map;

/**
 * 内容增强器
 * (Content Enhancer)
 *
 * <p>
 * 对提取的内容进行增强处理，可用于：
 * - 内容格式转换（Markdown、HTML）
 * - 文本摘要生成
 * - 关键词提取
 * - 语义分析
 * - 翻译
 * - 内容分类
 * - 实体识别
 * </p>
 *
 * <p>示例实现：</p>
 * <pre>{@code
 * @Component
 * @Order(20)
 * public class SummaryContentEnhancer implements ContentEnhancer {
 *     @Override
 *     public EnhancedContent enhance(ProcessingContext context, String originalContent) {
 *         String summary = generateSummary(originalContent);
 *         return EnhancedContent.builder()
 *                 .content(originalContent)
 *                 .summary(summary)
 *                 .build();
 *     }
 * }
 * }</pre>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface ContentEnhancer extends DocumentProcessorExtension {

    /**
     * 增强内容
     *
     * @param context 处理上下文
     * @param originalContent 原始内容
     * @return 增强后的内容
     * @throws Exception 处理失败时抛出异常
     */
    EnhancedContent enhance(ProcessingContext context, String originalContent) throws Exception;

    /**
     * 增强后的内容
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class EnhancedContent {
        /** 处理后的内容（可以修改原内容） */
        private String content;

        /** 摘要 */
        private String summary;

        /** 关键词 */
        private java.util.List<String> keywords;

        /** 分类 */
        private String category;

        /** 实体列表 */
        private java.util.List<Entity> entities;

        /** 扩展元数据 */
        private Map<String, Object> metadata;
    }

    /**
     * 实体信息
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class Entity {
        /** 实体名称 */
        private String name;

        /** 实体类型（人名、地名、组织等） */
        private String type;

        /** 在文本中的位置 */
        private int startOffset;

        /** 结束位置 */
        private int endOffset;

        /** 置信度 */
        private double confidence;
    }
}

