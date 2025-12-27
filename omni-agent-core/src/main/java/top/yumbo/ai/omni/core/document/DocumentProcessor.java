package top.yumbo.ai.omni.core.document;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * 文档处理器接口 - 可扩展的文档内容提取
 * (Document Processor Interface - Extensible document content extraction)
 *
 * <p>
 * 设计原则：
 * - 策略模式：每种文档类型一个处理器
 * - 可插拔：通过 SPI 或 Spring 自动发现
 * - 异步支持：处理大文件时异步执行
 * - 进度反馈：支持处理进度回调
 * </p>
 *
 * <p>
 * 支持的文档类型（当前 + 未来）：
 * - Office文档：PDF, Word, Excel, PPT
 * - 图片文档：PNG, JPG, TIFF（OCR）
 * - 媒体文件：视频（字幕提取）、音频（转文字）
 * - 代码文件：Java, Python, etc.
 * - 压缩文件：ZIP, RAR
 * - 其他：HTML, Markdown, etc.
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface DocumentProcessor {

    /**
     * 判断是否支持该文件类型
     *
     * @param fileExtension 文件扩展名（如：pdf, docx, xlsx）
     * @return true 如果支持
     */
    boolean supports(String fileExtension);

    /**
     * 获取处理器名称
     *
     * @return 处理器名称
     */
    String getName();

    /**
     * 获取处理器优先级（数字越小优先级越高）
     *
     * @return 优先级
     */
    default int getPriority() {
        return 100;
    }

    /**
     * 处理文档（同步）
     *
     * @param context 处理上下文
     * @return 处理结果
     * @throws DocumentProcessingException 处理失败
     */
    ProcessingResult process(ProcessingContext context) throws DocumentProcessingException;

    /**
     * 处理文档（异步，用于大文件）
     *
     * @param context 处理上下文
     * @param callback 进度回调
     * @return 处理任务ID
     */
    default String processAsync(ProcessingContext context, ProgressCallback callback) {
        // 默认实现：同步转异步
        String taskId = java.util.UUID.randomUUID().toString();
        new Thread(() -> {
            try {
                if (callback != null) {
                    callback.onProgress(taskId, 0, "开始处理");
                }
                ProcessingResult result = process(context);
                if (callback != null) {
                    callback.onComplete(taskId, result);
                }
            } catch (Exception e) {
                if (callback != null) {
                    callback.onError(taskId, e);
                }
            }
        }).start();
        return taskId;
    }

    /**
     * 验证文档（可选）
     *
     * @param context 处理上下文
     * @return 验证结果
     */
    default ValidationResult validate(ProcessingContext context) {
        return ValidationResult.builder()
                .valid(true)
                .message("默认验证通过")
                .build();
    }

    /**
     * 处理上下文
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ProcessingContext {
        /** 文件路径 */
        private String filePath;

        /** 文件字节数组（可选，内存处理） */
        private byte[] fileBytes;

        /** 文件扩展名 */
        private String fileExtension;

        /** 原始文件名 */
        private String originalFileName;

        /** 文件大小（字节） */
        private long fileSize;

        /** 处理选项 */
        private Map<String, Object> options;

        /** 临时目录（用于中间文件） */
        private String tempDir;
    }

    /**
     * 处理结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ProcessingResult {
        /** 是否成功 */
        private boolean success;

        /** 提取的文本内容 */
        private String content;

        /** 文档元数据 */
        private Map<String, Object> metadata;

        /** 提取的图片（如果有） */
        private List<ExtractedImage> images;

        /** 处理时间（毫秒） */
        private long processingTimeMs;

        /** 错误信息 */
        private String error;

        /** 处理器名称 */
        private String processorName;
    }

    /**
     * 提取的图片
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ExtractedImage {
        /** 图片数据 */
        private byte[] data;

        /** 图片格式（png, jpg） */
        private String format;

        /** 图片在文档中的页码 */
        private int pageNumber;

        /** 图片描述（可选） */
        private String description;

        /** 图片在页面中的位置（可选，用于 PPT 等布局重要的文档） */
        private Object position;  // 可以是任意位置对象，如 ImagePosition

        /** 元数据（可选，用于存储 Vision LLM 分析结果等） ⭐ */
        private java.util.Map<String, Object> metadata;
    }

    /**
     * 验证结果
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    class ValidationResult {
        /** 是否有效 */
        private boolean valid;

        /** 验证消息 */
        private String message;

        /** 建议的处理器（如果当前处理器不合适） */
        private String suggestedProcessor;
    }

    /**
     * 进度回调
     */
    interface ProgressCallback {
        /**
         * 进度更新
         *
         * @param taskId 任务ID
         * @param progress 进度（0-100）
         * @param message 进度消息
         */
        void onProgress(String taskId, int progress, String message);

        /**
         * 处理完成
         *
         * @param taskId 任务ID
         * @param result 处理结果
         */
        void onComplete(String taskId, ProcessingResult result);

        /**
         * 处理错误
         *
         * @param taskId 任务ID
         * @param error 错误信息
         */
        void onError(String taskId, Exception error);
    }

    /**
     * 文档处理异常
     */
    class DocumentProcessingException extends Exception {
        public DocumentProcessingException(String message) {
            super(message);
        }

        public DocumentProcessingException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}


