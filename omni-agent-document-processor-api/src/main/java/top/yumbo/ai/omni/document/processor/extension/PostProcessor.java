package top.yumbo.ai.omni.document.processor.extension;

import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingContext;
import top.yumbo.ai.omni.document.processor.DocumentProcessor.ProcessingResult;

/**
 * 文档处理后置处理器
 * (Document Post-Processor)
 *
 * <p>
 * 在文档处理后执行，可用于：
 * - 结果验证和清理
 * - 内容格式化和美化
 * - 敏感信息过滤
 * - 统计信息收集
 * - 结果持久化
 * - 通知发送
 * </p>
 *
 * <p>示例实现：</p>
 * <pre>{@code
 * @Component
 * @Order(10)
 * public class ContentFilterPostProcessor implements PostProcessor {
 *     @Override
 *     public ProcessingResult postProcess(ProcessingContext context, ProcessingResult result) {
 *         // 过滤敏感信息
 *         String filteredContent = filterSensitiveInfo(result.getContent());
 *         result.setContent(filteredContent);
 *         return result;
 *     }
 * }
 * }</pre>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface PostProcessor extends DocumentProcessorExtension {

    /**
     * 后置处理
     *
     * @param context 处理上下文
     * @param result 处理结果
     * @return 修改后的结果（可以返回原结果或新结果）
     * @throws Exception 处理失败时抛出异常
     */
    ProcessingResult postProcess(ProcessingContext context, ProcessingResult result) throws Exception;
}

