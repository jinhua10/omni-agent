package top.yumbo.ai.omni.document.processor.starter;


import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import top.yumbo.ai.omni.document.processor.DocumentProcessor;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 文档处理器管理服务
 * (Document Processor Manager Service)
 *
 * <p>
 * 职责：
 * - 注册和管理所有文档处理器
 * - 根据文件类型选择合适的处理器
 * - 支持处理器优先级排序
 * - 提供统一的文档处理入口
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public class DocumentProcessorManager {

    /**
     * 所有注册的处理器（按优先级排序）
     */
    private final List<DocumentProcessor> processors = new ArrayList<>();

    /**
     * 文件扩展名 -> 处理器映射（缓存）
     */
    private final Map<String, DocumentProcessor> processorCache = new ConcurrentHashMap<>();

    /**
     * 异步任务状态
     */
    private final Map<String, TaskStatus> asyncTasks = new ConcurrentHashMap<>();

    /**
     * 构造函数：自动注入所有 DocumentProcessor Bean
     */
    @Autowired(required = false)
    public DocumentProcessorManager(List<DocumentProcessor> processors) {
        if (processors != null && !processors.isEmpty()) {
            // 按优先级排序
            this.processors.addAll(processors.stream()
                    .sorted(Comparator.comparingInt(DocumentProcessor::getPriority))
                    .collect(Collectors.toList()));

            log.info("📚 文档处理器管理器初始化完成，注册了 {} 个处理器:", this.processors.size());
            for (DocumentProcessor processor : this.processors) {
                log.info("  - {} (优先级: {})", processor.getName(), processor.getPriority());
            }
        } else {
            log.warn("⚠️ 未找到任何文档处理器实现");
        }
    }

    /**
     * 手动注册处理器
     *
     * @param processor 文档处理器
     */
    public void registerProcessor(DocumentProcessor processor) {
        processors.add(processor);
        // 重新排序
        processors.sort(Comparator.comparingInt(DocumentProcessor::getPriority));
        // 清空缓存
        processorCache.clear();
        log.info("✅ 注册文档处理器: {}", processor.getName());
    }

    /**
     * 查找支持该文件类型的处理器
     *
     * @param fileExtension 文件扩展名
     * @return 处理器，找不到返回 null
     */
    public DocumentProcessor findProcessor(String fileExtension) {
        if (fileExtension == null || fileExtension.isEmpty()) {
            return null;
        }

        String ext = fileExtension.toLowerCase().trim();

        // 先查缓存
        if (processorCache.containsKey(ext)) {
            return processorCache.get(ext);
        }

        // 查找支持该扩展名的处理器（优先级从高到低）
        for (DocumentProcessor processor : processors) {
            if (processor.supports(ext)) {
                processorCache.put(ext, processor);
                log.debug("🔍 为 [{}] 找到处理器: {}", ext, processor.getName());
                return processor;
            }
        }

        log.warn("⚠️ 未找到支持 [{}] 的处理器", ext);
        return null;
    }

    /**
     * 处理文档（同步）
     *
     * @param context 处理上下文
     * @return 处理结果
     * @throws DocumentProcessor.DocumentProcessingException 处理失败
     */
    public DocumentProcessor.ProcessingResult processDocument(DocumentProcessor.ProcessingContext context)
            throws DocumentProcessor.DocumentProcessingException {

        log.info("📄 开始处理文档: {}", context.getOriginalFileName());

        // 查找处理器
        DocumentProcessor processor = findProcessor(context.getFileExtension());
        if (processor == null) {
            throw new DocumentProcessor.DocumentProcessingException(
                    "不支持的文件类型: " + context.getFileExtension());
        }

        // 验证
        DocumentProcessor.ValidationResult validation = processor.validate(context);
        if (!validation.isValid()) {
            throw new DocumentProcessor.DocumentProcessingException(
                    "文档验证失败: " + validation.getMessage());
        }

        // 处理
        long startTime = System.currentTimeMillis();
        try {
            DocumentProcessor.ProcessingResult result = processor.process(context);
            result.setProcessingTimeMs(System.currentTimeMillis() - startTime);
            result.setProcessorName(processor.getName());

            log.info("✅ 文档处理完成: {} (耗时: {}ms, 内容长度: {})",
                    context.getOriginalFileName(),
                    result.getProcessingTimeMs(),
                    result.getContent() != null ? result.getContent().length() : 0);

            return result;
        } catch (Exception e) {
            log.error("❌ 文档处理失败: {}", context.getOriginalFileName(), e);
            throw new DocumentProcessor.DocumentProcessingException("文档处理失败", e);
        }
    }

    /**
     * 处理文档（异步）
     *
     * @param context 处理上下文
     * @param callback 进度回调
     * @return 任务ID
     */
    public String processDocumentAsync(DocumentProcessor.ProcessingContext context,
                                       DocumentProcessor.ProgressCallback callback) {

        log.info("📄 异步处理文档: {}", context.getOriginalFileName());

        // 查找处理器
        DocumentProcessor processor = findProcessor(context.getFileExtension());
        if (processor == null) {
            String error = "不支持的文件类型: " + context.getFileExtension();
            log.error("❌ {}", error);
            if (callback != null) {
                callback.onError(null, new DocumentProcessor.DocumentProcessingException(error));
            }
            return null;
        }

        // 创建任务
        String taskId = processor.processAsync(context, callback);

        // 记录任务状态
        asyncTasks.put(taskId, TaskStatus.builder()
                .taskId(taskId)
                .fileName(context.getOriginalFileName())
                .processorName(processor.getName())
                .status("PROCESSING")
                .startTime(System.currentTimeMillis())
                .build());

        log.info("✅ 异步任务已提交: taskId={}", taskId);
        return taskId;
    }

    /**
     * 获取异步任务状态
     *
     * @param taskId 任务ID
     * @return 任务状态
     */
    public TaskStatus getTaskStatus(String taskId) {
        return asyncTasks.get(taskId);
    }

    /**
     * 获取所有支持的文件类型
     *
     * @return 文件扩展名列表
     */
    public List<String> getSupportedExtensions() {
        Set<String> extensions = new HashSet<>();
        for (DocumentProcessor processor : processors) {
            // 注意：这里需要每个处理器提供支持的扩展名列表
            // 暂时返回空，子类可以扩展
        }
        return new ArrayList<>(extensions);
    }

    /**
     * 获取所有已注册的处理器
     *
     * @return 处理器列表
     */
    public List<DocumentProcessor> getAllProcessors() {
        return new ArrayList<>(processors);
    }

    /**
     * 获取统计信息
     *
     * @return 统计信息
     */
    public Map<String, Object> getStatistics() {
        Map<String, Object> stats = new HashMap<>();
        stats.put("totalProcessors", processors.size());
        stats.put("processorNames", processors.stream()
                .map(DocumentProcessor::getName)
                .collect(Collectors.toList()));
        stats.put("cacheSize", processorCache.size());
        stats.put("asyncTaskCount", asyncTasks.size());
        return stats;
    }

    /**
     * 异步任务状态
     */
    @lombok.Data
    @lombok.Builder
    @lombok.NoArgsConstructor
    @lombok.AllArgsConstructor
    public static class TaskStatus {
        private String taskId;
        private String fileName;
        private String processorName;
        private String status;  // PROCESSING, COMPLETED, FAILED
        private long startTime;
        private long endTime;
        private String error;
        private DocumentProcessor.ProcessingResult result;
    }
}





