package top.yumbo.ai.omni.web.service;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.web.config.FileWatcherConfig;
import top.yumbo.ai.omni.web.model.FileChangeRecord;
import top.yumbo.ai.omni.web.model.FileChangeRecord.ChangeType;
import top.yumbo.ai.omni.web.util.DocumentParserUtil;
import top.yumbo.ai.omni.web.util.FileHashUtil;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

/**
 * 文件监听服务
 *
 * 新逻辑：
 * 1. 监听目录扫描未索引文件
 * 2. 处理文件：解析 → 分块 → 存储 → RAG索引
 * 3. 成功后归档到 data/storage/documents（保留目录结构）
 * 4. 失败则记录详细日志，保留在监听目录等待重试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class FileWatcherService {

    private final ConfigPersistenceService configService;
    private final RAGService ragService;
    private final DocumentStorageService storageService;
    private final top.yumbo.ai.omni.core.chunking.DocumentChunkingService chunkingService;
    private final top.yumbo.ai.omni.core.document.DocumentProcessorManager documentProcessorManager;
    private final top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager chunkingStrategyManager;
    private final top.yumbo.ai.omni.core.image.ImageStorageService imageStorageService;
    private final top.yumbo.ai.omni.web.service.rag.ProcessingProgressService progressService;  // ⭐ 新增
    private final SystemRAGConfigService ragConfigService;  // ⭐ 新增：系统RAG配置服务

    private WatchService watchService;
    private ExecutorService executorService;
    private ScheduledExecutorService scanExecutor;
    private volatile boolean running = false;

    // 文件处理记录（相对路径 -> 记录）
    private final ConcurrentHashMap<String, FileChangeRecord> processingRecords = new ConcurrentHashMap<>();

    // 已归档文件缓存（相对路径 -> 归档时间）
    private final ConcurrentHashMap<String, Long> archivedFiles = new ConcurrentHashMap<>();

    // 当前配置
    @Getter
    private FileWatcherConfig currentConfig;

    /**
     * 启动时初始化
     */
    @PostConstruct
    public void init() {
        try {
            // 加载持久化配置
            currentConfig = configService.loadFileWatcherConfig();
            log.info("📋 加载文件监听配置: enabled={}, autoIndex={}",
                    currentConfig.getEnabled(), currentConfig.getAutoIndex());

            // 如果启用，则启动文件监听
            if (Boolean.TRUE.equals(currentConfig.getEnabled())) {
                startWatching();
            } else {
                log.info("ℹ️ 文件监听已禁用");
            }

        } catch (Exception e) {
            log.error("❌ 初始化文件监听服务失败", e);
        }
    }

    /**
     * 启动文件监听
     */
    public synchronized void startWatching() {
        if (running) {
            log.warn("⚠️ 文件监听已在运行");
            return;
        }

        try {
            Path watchPath = Paths.get(currentConfig.getWatchDirectory());

            // 确保目录存在
            if (!Files.exists(watchPath)) {
                Files.createDirectories(watchPath);
                log.info("✅ 创建监听目录: {}", watchPath.toAbsolutePath());
            }

            // 创建 WatchService（监听新文件）
            watchService = FileSystems.getDefault().newWatchService();
            registerWatchDirectory(watchPath);

            // 启动监听线程
            executorService = Executors.newSingleThreadExecutor();
            running = true;

            executorService.submit(this::watchLoop);

            // 启动定期扫描任务（每30秒扫描一次未处理文件）⭐
            scanExecutor = Executors.newScheduledThreadPool(1);
            scanExecutor.scheduleWithFixedDelay(
                    this::scanAndProcessUnindexedFiles,
                    5,  // 启动后5秒开始
                    30, // 每30秒扫描一次
                    TimeUnit.SECONDS
            );

            log.info("✅ 文件监听已启动: {}", watchPath.toAbsolutePath());
            log.info("🔍 定期扫描任务已启动（每30秒）");

        } catch (IOException e) {
            log.error("❌ 启动文件监听失败", e);
        }
    }

    /**
     * 递归注册目录监听（包括子目录）
     */
    private void registerWatchDirectory(Path dir) throws IOException {
        dir.register(
                watchService,
                StandardWatchEventKinds.ENTRY_CREATE,
                StandardWatchEventKinds.ENTRY_DELETE
        );

        // 递归注册子目录
        Files.walk(dir, 1)
                .filter(Files::isDirectory)
                .filter(p -> !p.equals(dir))
                .forEach(subDir -> {
                    try {
                        registerWatchDirectory(subDir);
                    } catch (IOException e) {
                        log.error("注册子目录监听失败: {}", subDir, e);
                    }
                });
    }

    /**
     * 停止文件监听
     */
    public synchronized void stopWatching() {
        if (!running) {
            return;
        }

        running = false;

        try {
            if (watchService != null) {
                watchService.close();
            }
            if (executorService != null) {
                executorService.shutdown();
                executorService.awaitTermination(5, TimeUnit.SECONDS);
            }
            if (scanExecutor != null) {
                scanExecutor.shutdown();
                scanExecutor.awaitTermination(5, TimeUnit.SECONDS);
            }
            log.info("✅ 文件监听已停止");
        } catch (Exception e) {
            log.error("❌ 停止文件监听失败", e);
        }
    }

    /**
     * 扫描并注册未索引的文件（定期任务）⭐ 核心方法
     *
     * 新逻辑：
     * 1. 扫描文件并生成documentId
     * 2. 注册到SystemRAGConfigService（状态：PENDING）
     * 3. 不自动处理，由用户在UI中决定何时处理
     */
    private void scanAndProcessUnindexedFiles() {
        try {
            Path watchPath = Paths.get(currentConfig.getWatchDirectory());
            log.info("🔍 扫描未注册文件: {}", watchPath);

            // 递归扫描所有文件（包括子目录）
            Files.walk(watchPath)
                    .filter(Files::isRegularFile)
                    .filter(path -> {
                        String name = path.getFileName().toString();
                        // 过滤临时文件和隐藏文件
                        return !name.startsWith(".") && !name.startsWith("~") && !name.endsWith(".tmp");
                    })
                    .forEach(filePath -> {
                        try {
                            // 获取相对路径（用于判断是否已处理）
                            Path relativePath = watchPath.relativize(filePath);
                            String relativePathStr = relativePath.toString().replace('\\', '/');

                            // ⭐ 使用相对路径作为documentId（见名知意）
                            String documentId = relativePathStr;

                            // 检查是否已注册到RAG配置服务
                            SystemRAGConfigService.DocumentRAGConfig existingConfig =
                                ragConfigService.getDocumentConfig(documentId);

                            // 如果已经注册且不是PENDING状态，跳过
                            if (existingConfig.getCreatedAt() > 0 &&
                                !"PENDING".equals(existingConfig.getStatus())) {
                                log.debug("⏭️ 文档已处理或正在处理，跳过: {}", documentId);
                                return;
                            }

                            // 注册新文档（状态：PENDING，等待用户决定如何处理）
                            if (existingConfig.getCreatedAt() == 0) {
                                log.info("📝 注册新文档: {} (等待用户配置)", documentId);
                                SystemRAGConfigService.DocumentRAGConfig newConfig =
                                    new SystemRAGConfigService.DocumentRAGConfig();
                                newConfig.setDocumentId(documentId);
                                newConfig.setStatus("PENDING");
                                newConfig.setTextExtractionModel(ragConfigService.getDefaultTextExtractionModel());
                                newConfig.setChunkingStrategy(ragConfigService.getDefaultChunkingStrategy());
                                newConfig.setCreatedAt(System.currentTimeMillis());
                                newConfig.setUpdatedAt(System.currentTimeMillis());
                                ragConfigService.setDocumentConfig(documentId, newConfig);
                            }

                        } catch (Exception e) {
                            log.error("❌ 注册文件失败: {}", filePath, e);
                        }
                    });

            log.info("✅ 文件扫描完成");

        } catch (IOException e) {
            log.error("❌ 扫描文件失败", e);
        }
    }

    /**
     * 监听循环（简化版：只响应新文件创建）
     */
    private void watchLoop() {
        log.info("🔍 开始监听文件变化...");

        while (running) {
            try {
                WatchKey key = watchService.poll(1, TimeUnit.SECONDS);
                if (key == null) {
                    continue;
                }

                for (WatchEvent<?> event : key.pollEvents()) {
                    WatchEvent.Kind<?> kind = event.kind();

                    if (kind == StandardWatchEventKinds.OVERFLOW) {
                        continue;
                    }

                    // 只处理新文件创建，定期扫描会处理所有未处理的文件
                    if (kind == StandardWatchEventKinds.ENTRY_CREATE) {
                        @SuppressWarnings("unchecked")
                        WatchEvent<Path> ev = (WatchEvent<Path>) event;
                        Path filename = ev.context();
                        log.info("📄 检测到新文件: {}", filename);
                      }
                }

                key.reset();

            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            } catch (Exception e) {
                log.error("❌ 处理文件变化失败", e);
            }
        }

        log.info("🛑 文件监听循环结束");
    }


    /**
     * 处理新文件（完整流程：解析 → 分块 → 存储 → RAG索引 → 归档）⭐
     */
    private void processNewFile(Path filePath, Path relativePath) {
        String relativePathStr = relativePath.toString().replace('\\', '/');
        String filename = filePath.getFileName().toString();

        // ⭐ 使用有意义的相对路径作为 documentId（见名知意）
        // 例如: "报告/2024年报.pdf" 而不是 "doc_123456_报告_2024年报.pdf"
        String documentId = relativePathStr;

        // ⭐ 使用文件名作为进度追踪的标识（用户友好）
        progressService.startProcessing(filename, filename);

        // 创建处理记录
        FileChangeRecord record = FileChangeRecord.builder()
                .id(relativePathStr)  // ⭐ 使用相对路径作为ID，而不是UUID
                .filePath(filePath.toString())
                .fileName(filename)
                .changeType(ChangeType.CREATE)
                .changedAt(System.currentTimeMillis())
                .processed(false)
                .build();

        processingRecords.put(relativePathStr, record);

        try {
            log.info("🔄 开始处理文件: {}", relativePathStr);


            // ========== 步骤1: 读取文件 ==========
            byte[] fileData = Files.readAllBytes(filePath);
            log.info("📄 读取文件: {} bytes", fileData.length);
            // ⭐ 更新进度：上传完成 (10%)
            progressService.updateProgress(filename,
                top.yumbo.ai.omni.web.model.rag.ProcessingStage.UPLOAD, 10);

            // ========== 步骤2: 使用 DocumentProcessorManager 处理文档 ==========
            String content;
            List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage> images = null;

            try {
                log.info("🔄 使用 DocumentProcessorManager 处理文档...");
                // ⭐ 更新进度：开始提取 (20%)
                progressService.updateProgress(filename,
                    top.yumbo.ai.omni.web.model.rag.ProcessingStage.EXTRACT, 20);

                top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext context =
                    top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingContext.builder()
                        .fileBytes(fileData)
                        .fileExtension(getFileExtension(filename))
                        .originalFileName(filename)
                        .fileSize((long) fileData.length)
                        .options(new HashMap<>())
                        .build();

                top.yumbo.ai.omni.core.document.DocumentProcessor.ProcessingResult result =
                    documentProcessorManager.processDocument(context);

                if (result.isSuccess()) {
                    content = result.getContent();
                    images = result.getImages();
                    log.info("✅ 文档处理成功: {} chars, {} images",
                            content.length(), images != null ? images.size() : 0);
                    // ⭐ 更新进度：提取完成 (40%)
                    progressService.updateProgress(filename,
                        top.yumbo.ai.omni.web.model.rag.ProcessingStage.EXTRACT, 40);
                } else {
                    throw new Exception("文档处理失败: " + result.getError());
                }

            } catch (Exception e) {
                log.warn("⚠️ DocumentProcessor 失败，降级使用 DocumentParserUtil: {}", e.getMessage());
                content = DocumentParserUtil.parseDocument(filePath.toFile());
            }

            if (content == null || content.trim().isEmpty()) {
                throw new Exception("文档内容为空");
            }

            // ========== 步骤3: 保存原始文档到存储 ==========
            log.info("💾 保存原始文档到存储服务...");
            String savedDocId = storageService.saveDocument(documentId, relativePathStr, fileData);
            if (savedDocId == null) {
                throw new Exception("保存原始文档失败");
            }

            // ========== 步骤4: 保存提取的图片 ==========
            if (images != null && !images.isEmpty()) {
                log.info("🖼️ 保存提取的图片: {} 张", images.size());

                // ⭐ 按页码分组图片
                Map<Integer, List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage>> imagesByPage = new HashMap<>();
                for (var img : images) {
                    int pageNum = img.getPageNumber() > 0 ? img.getPageNumber() : 1;
                    imagesByPage.computeIfAbsent(pageNum, k -> new ArrayList<>()).add(img);
                }

                int savedImageCount = 0;
                // ⭐ 遍历每一页，为该页的图片添加序号
                for (Map.Entry<Integer, List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage>> entry : imagesByPage.entrySet()) {
                    int pageNum = entry.getKey();
                    List<top.yumbo.ai.omni.core.document.DocumentProcessor.ExtractedImage> pageImages = entry.getValue();

                    for (int imgIndex = 0; imgIndex < pageImages.size(); imgIndex++) {
                        var extractedImage = pageImages.get(imgIndex);

                        try {
                            // ⭐ 在 metadata 中添加图片序号
                            Map<String, Object> metadata = extractedImage.getMetadata();
                            if (metadata == null) {
                                metadata = new HashMap<>();
                            }
                            metadata.put("imageIndex", imgIndex);  // 图片在该页的序号
                            metadata.put("pageNumber", pageNum);   // 确保页码信息存在

                            // ⭐ 使用文件名而不是 documentId
                            String imageId = imageStorageService.saveImage(
                                    filename,  // ⭐ 使用文件名
                                    extractedImage.getData(),
                                    extractedImage.getFormat(),
                                    metadata);  // 传递包含序号的 metadata
                            if (imageId != null) {
                                savedImageCount++;
                            }
                        } catch (Exception ex) {
                            log.warn("⚠️ 保存图片失败 (page={}, img={}): {}", pageNum, imgIndex, ex.getMessage());
                        }
                    }
                }
                log.info("✅ 图片已保存: {} 张 (共 {} 页)", savedImageCount, imagesByPage.size());
            }

            // ========== 步骤5: 智能分块 ==========
            log.info("✂️ 智能分块...");
            // ⭐ 更新进度：开始分块 (50%)
            progressService.updateProgress(filename,
                top.yumbo.ai.omni.web.model.rag.ProcessingStage.CHUNK, 50);

            List<Chunk> chunks = chunkingStrategyManager.chunkWithAutoStrategy(
                    documentId, content, filename);
            log.info("✅ 分块完成: {} 个块", chunks.size());
            // ⭐ 更新进度：分块完成 (60%)
            progressService.updateProgress(filename,
                top.yumbo.ai.omni.web.model.rag.ProcessingStage.CHUNK, 60);

            // ========== 步骤6: 保存分块 ==========
            log.info("💾 保存分块到存储...");
            List<String> chunkIds = storageService.saveChunks(filename, chunks);
            log.info("✅ 分块已保存: {} 个", chunkIds.size());

            // ========== 步骤7: RAG索引 ==========
            log.info("📇 索引到 RAG...");
            // ⭐ 更新进度：开始向量化 (70%)
            progressService.updateProgress(filename,
                top.yumbo.ai.omni.web.model.rag.ProcessingStage.VECTORIZE, 70);

            for (Chunk chunk : chunks) {
                top.yumbo.ai.rag.api.model.Document document = top.yumbo.ai.rag.api.model.Document.builder()
                        .id(chunk.getId())
                        .title(filename + " (块 " + chunk.getSequence() + ")")
                        .content(chunk.getContent())
                        .summary("块 " + chunk.getSequence())
                        .source("file-watcher")
                        .type(getFileType(filename))
                        .metadata(Map.of(
                                "fileName", filename,
                                "relativePath", relativePathStr,           // ⭐ 相对路径
                                "storagePath", relativePathStr,            // ⭐ 存储路径（用于下载）
                                "documentId", documentId,
                                "chunkIndex", chunk.getSequence()
                        ))
                        .build();

                ragService.indexDocument(document);
            }
            log.info("✅ RAG索引完成");
            // ⭐ 更新进度：索引中 (90%)
            progressService.updateProgress(filename,
                top.yumbo.ai.omni.web.model.rag.ProcessingStage.INDEX, 90);

            // ========== 步骤8: 归档成功，从监听目录移除 ==========
            Files.delete(filePath);
            log.info("🗑️ 已从监听目录移除: {}", relativePathStr);

            // 标记为已归档
            archivedFiles.put(relativePathStr, System.currentTimeMillis());
            record.setProcessed(true);
            record.setProcessedAt(System.currentTimeMillis());
            record.setNote("成功归档到: " + relativePathStr);

            log.info("✅ 处理完成: {}", relativePathStr);

            // ⭐ 标记处理完成 (100%)
            progressService.markCompleted(filename);

        } catch (Exception e) {
            log.error("❌ 处理失败: {} - {}", relativePathStr, e.getMessage(), e);
            record.setProcessed(false);
            record.setNote("失败: " + e.getMessage());

            // ⭐ 标记处理失败
            progressService.markFailed(filename,
                top.yumbo.ai.omni.web.model.rag.ProcessingStage.INDEX,
                e.getMessage());

            // 失败的文件保留在监听目录，等待下次扫描重试
        }
    }

    /**
     * 获取文件扩展名
     */
    private String getFileExtension(String filename) {
        int lastDot = filename.lastIndexOf('.');
        if (lastDot > 0 && lastDot < filename.length() - 1) {
            return filename.substring(lastDot + 1);
        }
        return "";
    }

    // ========== 分块策略相关 ==========
    //
    // ✅ 已实现：根据文档类型自动选择分块策略
    // - DocumentChunkingService → ChunkingStrategyManager → 具体Strategy
    // - 支持多种内置策略：固定大小、句子边界、段落、语义感知等
    //
    // 🔮 未来扩展：通过 marketplace 模块加载自定义算法
    //
    // 当前架构：
    // FileWatcherService
    //   → DocumentChunkingService
    //       → ChunkingStrategyManager (管理所有策略)
    //           ├─ FixedSizeChunkingStrategy (默认)
    //           ├─ SentenceBoundaryChunkingStrategy
    //           ├─ ParagraphChunkingStrategy
    //           ├─ SemanticChunkingStrategy (TODO)
    //           ├─ PPLChunkingStrategy (TODO - 基于困惑度)
    //           └─ MarketplaceChunkingStrategy (TODO - 从市场加载)
    //
    // 扩展示例：
    // 1. 在配置文件中指定策略：
    //    "chunkingStrategy": "semantic"  // 强制使用语义分块
    //
    // 2. 从算法市场加载：
    //    String algorithmId = currentConfig.getChunkingAlgorithmId();
    //    if (algorithmId != null) {
    //        chunks = marketplaceService.executeChunkingAlgorithm(
    //            algorithmId, docId, content, fileName
    //        );
    //    } else {
    //        chunks = chunkingService.chunkDocument(docId, content, fileName);
    //    }

    /**
     * 推断文件类型
     */
    private String getFileType(String fileName) {
        String lower = fileName.toLowerCase();
        if (lower.endsWith(".pdf")) return "pdf";
        if (lower.endsWith(".docx") || lower.endsWith(".doc")) return "word";
        if (lower.endsWith(".xlsx") || lower.endsWith(".xls")) return "excel";
        if (lower.endsWith(".pptx") || lower.endsWith(".ppt")) return "powerpoint";
        if (lower.endsWith(".txt")) return "text";
        return "document";
    }

    /**
     * 从文件名提取 documentId
     */
    private String extractDocumentId(String fileName) {
        if (fileName.startsWith("doc_")) {
            int idx = fileName.indexOf('_', 4);
            if (idx > 0) {
                return fileName.substring(0, idx);
            }
        }
        return null;
    }

    // ========== 公开API ==========

    /**
     * 检查文件是否正在处理中 ⭐
     *
     * @param relativePathOrFileName 相对路径或文件名
     * @return true 如果文件正在处理中
     */
    public boolean isFileProcessing(String relativePathOrFileName) {
        // 检查完整的相对路径
        FileChangeRecord record = processingRecords.get(relativePathOrFileName);
        if (record != null && !Boolean.TRUE.equals(record.getProcessed())) {
            return true;
        }

        // 如果传入的是文件名，遍历查找
        if (!relativePathOrFileName.contains("/") && !relativePathOrFileName.contains("\\")) {
            for (Map.Entry<String, FileChangeRecord> entry : processingRecords.entrySet()) {
                String key = entry.getKey();
                FileChangeRecord rec = entry.getValue();

                // 提取文件名比较
                String fileName = key.contains("/") ? key.substring(key.lastIndexOf('/') + 1) : key;
                if (fileName.equals(relativePathOrFileName) && !Boolean.TRUE.equals(rec.getProcessed())) {
                    return true;
                }
            }
        }

        return false;
    }

    public List<FileChangeRecord> getUnprocessedChanges() {
        return processingRecords.values().stream()
                .filter(r -> !Boolean.TRUE.equals(r.getProcessed()))
                .sorted(Comparator.comparing(FileChangeRecord::getChangedAt).reversed())
                .toList();
    }

    public List<FileChangeRecord> getAllChanges() {
        return processingRecords.values().stream()
                .sorted(Comparator.comparing(FileChangeRecord::getChangedAt).reversed())
                .toList();
    }

    public boolean processChange(String recordId) {
        // 手动触发重试（暂不实现，因为自动扫描会处理）
        return false;
    }

    public int processAllUnprocessed() {
        // 触发立即扫描
        scanAndProcessUnindexedFiles();
        return (int) processingRecords.values().stream()
                .filter(r -> Boolean.TRUE.equals(r.getProcessed()))
                .count();
    }

    public int clearProcessedRecords() {
        int count = 0;
        Iterator<Map.Entry<String, FileChangeRecord>> it = processingRecords.entrySet().iterator();
        while (it.hasNext()) {
            if (Boolean.TRUE.equals(it.next().getValue().getProcessed())) {
                it.remove();
                count++;
            }
        }
        return count;
    }

    public boolean updateConfig(FileWatcherConfig newConfig) {
        if (!configService.saveFileWatcherConfig(newConfig)) {
            return false;
        }

        boolean wasRunning = running;
        currentConfig = newConfig;

        if (Boolean.TRUE.equals(newConfig.getEnabled()) && !wasRunning) {
            startWatching();
        } else if (Boolean.FALSE.equals(newConfig.getEnabled()) && wasRunning) {
            stopWatching();
        }

        return true;
    }

    @PreDestroy
    public void destroy() {
        stopWatching();
    }
}

