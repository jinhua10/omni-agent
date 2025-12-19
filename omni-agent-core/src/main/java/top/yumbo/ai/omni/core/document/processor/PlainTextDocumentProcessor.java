package top.yumbo.ai.omni.core.document.processor;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.core.document.DocumentProcessor;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;

/**
 * 纯文本文档处理器
 * (Plain Text Document Processor)
 *
 * <p>
 * 支持的文件类型：
 * - 文本文件 (.txt)
 * - Markdown (.md, .markdown)
 * - 代码文件 (.java, .py, .js, .ts, .go, .rs, etc.)
 * - 配置文件 (.yml, .yaml, .json, .xml, .properties)
 * - 日志文件 (.log)
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
public class PlainTextDocumentProcessor implements DocumentProcessor {

    /**
     * 支持的文件扩展名
     */
    private static final Set<String> SUPPORTED_EXTENSIONS = Set.of(
            // 文本文件
            "txt", "md", "markdown", "log",
            // 代码文件
            "java", "py", "js", "ts", "go", "rs", "c", "cpp", "h", "hpp",
            "cs", "php", "rb", "swift", "kt", "scala",
            // 配置文件
            "yml", "yaml", "json", "xml", "properties", "ini", "conf", "config",
            // Web 文件
            "html", "htm", "css", "scss", "sass", "less",
            // 脚本文件
            "sh", "bash", "bat", "ps1", "sql"
    );

    @Override
    public boolean supports(String fileExtension) {
        return SUPPORTED_EXTENSIONS.contains(fileExtension.toLowerCase());
    }

    @Override
    public String getName() {
        return "PlainTextProcessor";
    }

    @Override
    public int getPriority() {
        return 50;  // 中等优先级
    }

    @Override
    public ProcessingResult process(ProcessingContext context) throws DocumentProcessingException {
        log.info("📝 [PlainText] 开始处理文档: {}", context.getOriginalFileName());

        long startTime = System.currentTimeMillis();

        try {
            String content;

            // 从字节数组或文件读取
            if (context.getFileBytes() != null) {
                content = new String(context.getFileBytes(), StandardCharsets.UTF_8);
            } else if (context.getFilePath() != null) {
                content = Files.readString(Paths.get(context.getFilePath()), StandardCharsets.UTF_8);
            } else {
                throw new DocumentProcessingException("未提供文件数据或路径");
            }

            // 构建元数据
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("processor", "PlainText");
            metadata.put("encoding", "UTF-8");
            metadata.put("extension", context.getFileExtension());
            metadata.put("lineCount", content.split("\n").length);
            metadata.put("charCount", content.length());

            // 如果是代码文件，添加代码相关元数据
            if (isCodeFile(context.getFileExtension())) {
                metadata.put("fileType", "code");
                metadata.put("language", getLanguageName(context.getFileExtension()));
            }

            long processingTime = System.currentTimeMillis() - startTime;

            log.info("✅ [PlainText] 处理完成: 耗时={}ms, 字符数={}, 行数={}",
                    processingTime, content.length(), metadata.get("lineCount"));

            return ProcessingResult.builder()
                    .success(true)
                    .content(content)
                    .metadata(metadata)
                    .images(Collections.emptyList())
                    .processingTimeMs(processingTime)
                    .processorName(getName())
                    .build();

        } catch (IOException e) {
            log.error("❌ [PlainText] 读取文件失败: {}", context.getOriginalFileName(), e);
            throw new DocumentProcessingException("读取文件失败", e);
        } catch (Exception e) {
            log.error("❌ [PlainText] 处理失败: {}", context.getOriginalFileName(), e);
            throw new DocumentProcessingException("处理失败", e);
        }
    }

    /**
     * 判断是否为代码文件
     */
    private boolean isCodeFile(String extension) {
        Set<String> codeExtensions = Set.of(
                "java", "py", "js", "ts", "go", "rs", "c", "cpp", "h", "hpp",
                "cs", "php", "rb", "swift", "kt", "scala", "sh", "bash", "sql"
        );
        return codeExtensions.contains(extension.toLowerCase());
    }

    /**
     * 获取编程语言名称
     */
    private String getLanguageName(String extension) {
        Map<String, String> languageMap = Map.ofEntries(
                Map.entry("java", "Java"),
                Map.entry("py", "Python"),
                Map.entry("js", "JavaScript"),
                Map.entry("ts", "TypeScript"),
                Map.entry("go", "Go"),
                Map.entry("rs", "Rust"),
                Map.entry("c", "C"),
                Map.entry("cpp", "C++"),
                Map.entry("cs", "C#"),
                Map.entry("php", "PHP"),
                Map.entry("rb", "Ruby"),
                Map.entry("swift", "Swift"),
                Map.entry("kt", "Kotlin"),
                Map.entry("scala", "Scala"),
                Map.entry("sh", "Shell"),
                Map.entry("bash", "Bash"),
                Map.entry("sql", "SQL")
        );
        return languageMap.getOrDefault(extension.toLowerCase(), "Unknown");
    }

    @Override
    public ValidationResult validate(ProcessingContext context) {
        // 检查文件大小（最大 10MB 对于文本文件）
        if (context.getFileSize() > 10 * 1024 * 1024) {
            return ValidationResult.builder()
                    .valid(false)
                    .message("文本文件过大（超过10MB）")
                    .build();
        }

        return ValidationResult.builder()
                .valid(true)
                .message("验证通过")
                .build();
    }
}

