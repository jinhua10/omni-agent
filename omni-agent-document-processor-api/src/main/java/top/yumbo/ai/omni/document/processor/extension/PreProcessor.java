package top.yumbo.ai.omni.document.processor.extension;

import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingContext;

/**
 * 文档处理前置处理器
 * (Document Pre-Processor)
 *
 * <p>
 * 在文档处理前执行，可用于：
 * - 文档验证和预处理
 * - 文件格式转换
 * - 参数补充和修改
 * - 权限检查
 * - 日志记录
 * </p>
 *
 * <p>示例实现：</p>
 * <pre>{@code
 * @Component
 * @Order(1)
 * public class DocumentValidationPreProcessor implements PreProcessor {
 *     @Override
 *     public ProcessingContext preProcess(ProcessingContext context) {
 *         // 验证文件大小
 *         if (context.getFileSize() > MAX_SIZE) {
 *             throw new IllegalArgumentException("文件过大");
 *         }
 *         return context;
 *     }
 * }
 * }</pre>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface PreProcessor extends DocumentProcessorExtension {

    /**
     * 前置处理
     *
     * @param context 处理上下文
     * @return 修改后的上下文（可以返回原上下文或新上下文）
     * @throws Exception 处理失败时抛出异常
     */
    ProcessingContext preProcess(ProcessingContext context) throws Exception;
}

