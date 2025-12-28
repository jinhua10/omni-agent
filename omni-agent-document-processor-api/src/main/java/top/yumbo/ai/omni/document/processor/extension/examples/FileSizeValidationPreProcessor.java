package top.yumbo.ai.omni.document.processor.extension.examples;

import lombok.extern.slf4j.Slf4j;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingContext;
import top.yumbo.ai.omni.document.processor.extension.PreProcessor;

/**
 * 文件大小验证前置处理器示例
 * (File Size Validation Pre-Processor Example)
 *
 * <p>
 * 这是一个示例，展示如何使用前置处理器来验证文件大小。
 * 用户可以参考这个示例创建自己的前置处理器。
 * </p>
 *
 * <p>启用方式：</p>
 * <pre>
 * # 在 application.yml 或 application.properties 中配置
 * omni-agent.document.validation.max-file-size: 10485760  # 10MB
 * </pre>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
@Order(1)  // 最高优先级
public class FileSizeValidationPreProcessor implements PreProcessor {

    private static final long MAX_FILE_SIZE = 100 * 1024 * 1024; // 100MB 默认限制

    @Override
    public String getName() {
        return "FileSizeValidationPreProcessor";
    }

    @Override
    public int getOrder() {
        return 1; // 最高优先级
    }

    @Override
    public ProcessingContext preProcess(ProcessingContext context) throws Exception {
        log.debug("📋 [FileSizeValidation] 验证文件大小: {} bytes", context.getFileSize());

        if (context.getFileSize() > MAX_FILE_SIZE) {
            throw new IllegalArgumentException(
                String.format("文件大小超过限制: %d bytes (最大: %d bytes)",
                    context.getFileSize(), MAX_FILE_SIZE)
            );
        }

        log.debug("✅ [FileSizeValidation] 文件大小验证通过");
        return context;
    }
}

