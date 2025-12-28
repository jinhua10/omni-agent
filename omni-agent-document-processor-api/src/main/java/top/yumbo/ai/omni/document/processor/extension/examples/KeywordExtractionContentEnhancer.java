package top.yumbo.ai.omni.document.processor.extension.examples;

import lombok.extern.slf4j.Slf4j;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingContext;
import top.yumbo.ai.omni.document.processor.extension.ContentEnhancer;

import java.util.List;

/**
 * 关键词提取内容增强器示例
 * (Keyword Extraction Content Enhancer Example)
 *
 * <p>
 * 这是一个示例，展示如何使用内容增强器来提取关键词。
 * 用户可以参考这个示例创建自己的内容增强器。
 * </p>
 *
 * <p>功能：</p>
 * <ul>
 *   <li>简单的关键词提取（基于词频）</li>
 *   <li>生成简单摘要（取前几句话）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
@Order(20)  // 较低优先级
public class KeywordExtractionContentEnhancer implements ContentEnhancer {

    private static final int MAX_KEYWORDS = 10;
    private static final int SUMMARY_SENTENCES = 3;

    @Override
    public String getName() {
        return "KeywordExtractionContentEnhancer";
    }

    @Override
    public int getOrder() {
        return 20;
    }

    @Override
    public boolean supports(String processorName) {
        // 仅支持文本类文档处理器
        return processorName.contains("Word") ||
               processorName.contains("PDF") ||
               processorName.contains("Excel");
    }

    @Override
    public EnhancedContent enhance(ProcessingContext context, String originalContent) throws Exception {
        log.debug("📋 [KeywordExtraction] 开始提取关键词");

        if (originalContent == null || originalContent.isEmpty()) {
            return EnhancedContent.builder()
                    .content(originalContent)
                    .build();
        }

        // 提取关键词（简单实现，基于词频）
        List<String> keywords = extractKeywords(originalContent);

        // 生成摘要（取前几句话）
        String summary = generateSummary(originalContent);

        log.debug("✅ [KeywordExtraction] 提取完成: {} 个关键词", keywords.size());

        return EnhancedContent.builder()
                .content(originalContent)  // 保持原内容不变
                .keywords(keywords)
                .summary(summary)
                .build();
    }

    /**
     * 简单的关键词提取（基于词频）
     */
    private List<String> extractKeywords(String content) {
        // 移除特殊字符，只保留中文和英文单词
        String cleanedContent = content.replaceAll("[^\\u4e00-\\u9fa5a-zA-Z0-9\\s]", " ");

        // 分词（简单按空格分）
        String[] words = cleanedContent.split("\\s+");

        // 统计词频
        java.util.Map<String, Integer> wordCount = new java.util.HashMap<>();
        for (String word : words) {
            word = word.trim();
            if (word.length() >= 2) {  // 至少2个字符
                wordCount.put(word, wordCount.getOrDefault(word, 0) + 1);
            }
        }

        // 排序并取前 N 个
        return wordCount.entrySet().stream()
                .sorted((a, b) -> b.getValue().compareTo(a.getValue()))
                .limit(MAX_KEYWORDS)
                .map(java.util.Map.Entry::getKey)
                .toList();
    }

    /**
     * 生成简单摘要（取前几句话）
     */
    private String generateSummary(String content) {
        // 按句号、问号、感叹号分句
        String[] sentences = content.split("[。！？.!?]");

        StringBuilder summary = new StringBuilder();
        int count = 0;
        for (String sentence : sentences) {
            sentence = sentence.trim();
            if (sentence.length() > 5) {
                summary.append(sentence).append("。");
                count++;
                if (count >= SUMMARY_SENTENCES) {
                    break;
                }
            }
        }

        return summary.toString();
    }
}

