package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.document.processor.*;
import top.yumbo.ai.omni.document.processor.starter.config.DocumentProcessorProperties;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 文本文档处理器
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class TextProcessor implements DocumentProcessor {

    public TextProcessor() {
    }

    @Override
    public ProcessedDocument process(String documentId, InputStream input) throws ProcessorException {
        try {
            log.debug("📄 开始处理文本文档: {}", documentId);

            StringBuilder text = new StringBuilder();
            try (BufferedReader reader = new BufferedReader(
                    new InputStreamReader(input, StandardCharsets.UTF_8))) {
                String line;
                int lineCount = 0;
                while ((line = reader.readLine()) != null) {
                    text.append(line).append("\n");
                    lineCount++;
                }

                Map<String, Object> metadata = new HashMap<>();
                metadata.put("lineCount", lineCount);

                String content = text.toString();
                log.info("✅ 文本处理完成: {} ({} 行, {} 字符)",
                    documentId, lineCount, content.length());

                return ProcessedDocument.builder()
                        .documentId(documentId)
                        .documentType(DocumentType.TEXT)
                        .text(content)
                        .characterCount(content.length())
                        .metadata(metadata)
                        .success(true)
                        .build();
            }

        } catch (Exception e) {
            log.error("❌ 文本处理失败: {}", documentId, e);
            throw new ProcessorException("文本处理失败: " + e.getMessage(), e);
        }
    }

    @Override
    public List<DocumentType> getSupportedTypes() {
        return Arrays.asList(DocumentType.TEXT);
    }

    @Override
    public boolean supports(DocumentType type) {
        return type == DocumentType.TEXT;
    }

    @Override
    public boolean supportsExtension(String extension) {
        return ".txt".equalsIgnoreCase(extension) ||
               ".md".equalsIgnoreCase(extension) ||
               ".log".equalsIgnoreCase(extension);
    }
}

