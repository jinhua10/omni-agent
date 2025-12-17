package top.yumbo.ai.omni.web.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.web.config.FileWatcherConfig;
import top.yumbo.ai.omni.web.model.FileChangeRecord;
import top.yumbo.ai.omni.web.model.FileChangeRecord.ChangeType;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.omni.web.util.DocumentParserUtil;
import top.yumbo.ai.omni.web.util.FileHashUtil;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

/**
 * 文件监听服务
 *
 * 监听 data/documents/ 目录的文件变化
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

    private WatchService watchService;
    private ExecutorService executorService;
    private volatile boolean running = false;

    // 文件变化记录（内存存储，也可以持久化到数据库）
    private final ConcurrentHashMap<String, FileChangeRecord> changeRecords = new ConcurrentHashMap<>();

    // 文件哈希缓存（文件名 -> 哈希值）
    private final ConcurrentHashMap<String, String> fileHashCache = new ConcurrentHashMap<>();

    // 当前配置
    private FileWatcherConfig currentConfig;

    /**
     * 启动时初始化
     */
    @PostConstruct
    public void init() {
        try {
            // 1. 加载持久化配置
            currentConfig = configService.loadFileWatcherConfig();
            log.info("📋 加载文件监听配置: enabled={}, autoIndex={}",
                    currentConfig.getEnabled(), currentConfig.getAutoIndex());

            // 2. 如果启用，则启动文件监听
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

            // 创建 WatchService
            watchService = FileSystems.getDefault().newWatchService();

            // 注册监听事件
            watchPath.register(
                    watchService,
                    StandardWatchEventKinds.ENTRY_CREATE,
                    StandardWatchEventKinds.ENTRY_MODIFY,
                    StandardWatchEventKinds.ENTRY_DELETE
            );

            // 启动监听线程
            executorService = Executors.newSingleThreadExecutor();
            running = true;

            executorService.submit(this::watchLoop);

            // 扫描现有文件，建立初始哈希缓存
            scanExistingFiles(watchPath);

            log.info("✅ 文件监听已启动: {}", watchPath.toAbsolutePath());

        } catch (IOException e) {
            log.error("❌ 启动文件监听失败", e);
        }
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
            log.info("✅ 文件监听已停止");
        } catch (Exception e) {
            log.error("❌ 停止文件监听失败", e);
        }
    }

    /**
     * 扫描现有文件，建立初始哈希缓存
     */
    private void scanExistingFiles(Path watchPath) {
        try {
            log.info("🔍 扫描现有文件，建立哈希缓存...");

            Files.list(watchPath)
                    .filter(Files::isRegularFile)
                    .filter(path -> {
                        String name = path.getFileName().toString();
                        return !name.startsWith(".") && !name.startsWith("~") && !name.endsWith(".tmp");
                    })
                    .forEach(path -> {
                        String fileName = path.getFileName().toString();
                        String hash = FileHashUtil.calculateMD5(path);
                        if (hash != null) {
                            fileHashCache.put(fileName, hash);
                            log.debug("  📌 {} -> {}", fileName, hash.substring(0, 8) + "...");
                        }
                    });

            log.info("✅ 哈希缓存建立完成，共 {} 个文件", fileHashCache.size());

        } catch (IOException e) {
            log.error("❌ 扫描现有文件失败", e);
        }
    }

    /**
     * 监听循环
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

                    @SuppressWarnings("unchecked")
                    WatchEvent<Path> ev = (WatchEvent<Path>) event;
                    Path filename = ev.context();

                    handleFileChange(kind, filename);
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
     * 处理文件变化
     */
    private void handleFileChange(WatchEvent.Kind<?> kind, Path filename) {
        String fileName = filename.toString();

        // 忽略临时文件和隐藏文件
        if (fileName.startsWith(".") || fileName.startsWith("~") || fileName.endsWith(".tmp")) {
            return;
        }

        Path filePath = Paths.get(currentConfig.getWatchDirectory(), fileName);

        ChangeType changeType;
        if (kind == StandardWatchEventKinds.ENTRY_CREATE) {
            changeType = ChangeType.CREATE;
            log.info("📄 检测到新文件: {}", fileName);

            // 计算新文件的哈希值并缓存
            String hash = FileHashUtil.calculateMD5(filePath);
            if (hash != null) {
                fileHashCache.put(fileName, hash);
                log.debug("📌 缓存文件哈希: {} -> {}", fileName, hash.substring(0, 8) + "...");
            }

        } else if (kind == StandardWatchEventKinds.ENTRY_MODIFY) {
            // ⭐ 关键改进：使用哈希值判断内容是否真正改变
            String oldHash = fileHashCache.get(fileName);
            String newHash = FileHashUtil.calculateMD5(filePath);

            if (newHash == null) {
                log.warn("⚠️ 无法计算文件哈希: {}", fileName);
                return;
            }

            // 如果哈希值相同，说明内容没变，忽略此次 MODIFY 事件
            if (FileHashUtil.isSameHash(oldHash, newHash)) {
                log.debug("⏭️ 文件内容未改变，忽略: {}", fileName);
                return;  // ⭐ 过滤掉虚假的 MODIFY 事件
            }

            changeType = ChangeType.MODIFY;
            log.info("✏️ 检测到文件内容修改: {} (哈希变化: {} -> {})",
                    fileName,
                    oldHash != null ? oldHash.substring(0, 8) + "..." : "null",
                    newHash.substring(0, 8) + "...");

            // 更新哈希缓存
            fileHashCache.put(fileName, newHash);

        } else if (kind == StandardWatchEventKinds.ENTRY_DELETE) {
            changeType = ChangeType.DELETE;
            log.info("🗑️ 检测到文件删除: {}", fileName);

            // 移除哈希缓存
            fileHashCache.remove(fileName);

        } else {
            return;
        }

        // 记录变化
        FileChangeRecord record = recordFileChange(fileName, changeType, filePath);

        // 如果启用自动索引，则自动处理
        if (Boolean.TRUE.equals(currentConfig.getAutoIndex())) {
            processFileChange(record);
        }
    }

    /**
     * 记录文件变化
     */
    private FileChangeRecord recordFileChange(String fileName, ChangeType changeType, Path filePath) {
        String recordId = UUID.randomUUID().toString();

        Long fileSize = null;
        Long fileModifiedTime = null;
        String fileHash = null;
        String oldFileHash = null;

        try {
            if (Files.exists(filePath)) {
                fileSize = Files.size(filePath);
                fileModifiedTime = Files.getLastModifiedTime(filePath).toMillis();

                // 计算文件哈希
                if (changeType != ChangeType.DELETE) {
                    fileHash = FileHashUtil.calculateMD5(filePath);
                    oldFileHash = fileHashCache.get(fileName);
                }
            }
        } catch (IOException e) {
            log.warn("⚠️ 无法获取文件属性: {}", fileName, e);
        }

        // 尝试从文件名提取 documentId
        String documentId = extractDocumentId(fileName);

        FileChangeRecord record = FileChangeRecord.builder()
                .id(recordId)
                .filePath(filePath.toString())
                .fileName(fileName)
                .documentId(documentId)
                .changeType(changeType)
                .fileSize(fileSize)
                .fileModifiedTime(fileModifiedTime)
                .fileHash(fileHash)
                .oldFileHash(oldFileHash)
                .changedAt(System.currentTimeMillis())
                .processed(false)
                .build();

        changeRecords.put(recordId, record);

        log.debug("📝 记录文件变化: id={}, type={}, file={}, hash={}",
                recordId, changeType, fileName,
                fileHash != null ? fileHash.substring(0, 8) + "..." : "null");

        return record;
    }

    /**
     * 处理文件变化（自动索引）
     */
    private void processFileChange(FileChangeRecord record) {
        try {
            log.info("🔄 自动处理文件变化: {}", record.getFileName());

            Path filePath = Paths.get(record.getFilePath());

            switch (record.getChangeType()) {
                case CREATE, MODIFY -> {
                    // 重新索引文件
                    if (Files.exists(filePath)) {
                        String content = DocumentParserUtil.parseDocument(filePath.toFile());

                        String docId = record.getDocumentId();
                        if (docId == null) {
                            docId = "doc_" + System.currentTimeMillis();
                        }

                        Document document = Document.builder()
                                .id(docId)
                                .title(record.getFileName())
                                .content(content)
                                .source("file-watcher")
                                .type("document")
                                .build();

                        ragService.indexDocument(document);
                        log.info("✅ 自动索引成功: {}", record.getFileName());
                    }
                }
                case DELETE -> {
                    // 删除索引
                    if (record.getDocumentId() != null) {
                        ragService.deleteDocument(record.getDocumentId());
                        log.info("✅ 自动删除索引: {}", record.getDocumentId());
                    }
                }
            }

            // 标记为已处理
            record.setProcessed(true);
            record.setProcessedAt(System.currentTimeMillis());

        } catch (Exception e) {
            log.error("❌ 处理文件变化失败: {}", record.getFileName(), e);
            record.setNote("处理失败: " + e.getMessage());
        }
    }

    /**
     * 从文件名提取 documentId
     * 文件名格式: {documentId}_{originalName}
     */
    private String extractDocumentId(String fileName) {
        if (fileName.startsWith("doc_")) {
            int underscoreIndex = fileName.indexOf('_', 4);
            if (underscoreIndex > 0) {
                return fileName.substring(0, underscoreIndex);
            }
        }
        return null;
    }

    /**
     * 获取未处理的文件变化
     */
    public List<FileChangeRecord> getUnprocessedChanges() {
        return changeRecords.values().stream()
                .filter(record -> !record.getProcessed())
                .sorted(Comparator.comparing(FileChangeRecord::getChangedAt).reversed())
                .toList();
    }

    /**
     * 获取所有文件变化
     */
    public List<FileChangeRecord> getAllChanges() {
        return changeRecords.values().stream()
                .sorted(Comparator.comparing(FileChangeRecord::getChangedAt).reversed())
                .toList();
    }

    /**
     * 手动处理文件变化
     */
    public boolean processChange(String recordId) {
        FileChangeRecord record = changeRecords.get(recordId);
        if (record == null) {
            log.warn("⚠️ 记录不存在: {}", recordId);
            return false;
        }

        processFileChange(record);
        return record.getProcessed();
    }

    /**
     * 批量处理文件变化
     */
    public int processAllUnprocessed() {
        List<FileChangeRecord> unprocessed = getUnprocessedChanges();
        int successCount = 0;

        for (FileChangeRecord record : unprocessed) {
            processFileChange(record);
            if (record.getProcessed()) {
                successCount++;
            }
        }

        log.info("✅ 批量处理完成: 成功 {}/{}", successCount, unprocessed.size());
        return successCount;
    }

    /**
     * 清除已处理的记录
     */
    public int clearProcessedRecords() {
        int count = 0;
        Iterator<Map.Entry<String, FileChangeRecord>> iterator = changeRecords.entrySet().iterator();

        while (iterator.hasNext()) {
            Map.Entry<String, FileChangeRecord> entry = iterator.next();
            if (entry.getValue().getProcessed()) {
                iterator.remove();
                count++;
            }
        }

        log.info("🧹 清除已处理记录: {} 条", count);
        return count;
    }

    /**
     * 获取当前配置
     */
    public FileWatcherConfig getCurrentConfig() {
        return currentConfig;
    }

    /**
     * 更新配置
     */
    public boolean updateConfig(FileWatcherConfig newConfig) {
        // 保存配置
        if (!configService.saveFileWatcherConfig(newConfig)) {
            return false;
        }

        boolean wasRunning = running;
        boolean wasEnabled = currentConfig.getEnabled();

        // 更新当前配置
        currentConfig = newConfig;

        // 如果启用状态改变，重启或停止监听
        if (Boolean.TRUE.equals(newConfig.getEnabled()) && !wasRunning) {
            startWatching();
        } else if (Boolean.FALSE.equals(newConfig.getEnabled()) && wasRunning) {
            stopWatching();
        }

        log.info("✅ 配置已更新: enabled={}, autoIndex={}",
                newConfig.getEnabled(), newConfig.getAutoIndex());

        return true;
    }

    /**
     * 关闭时清理
     */
    @PreDestroy
    public void destroy() {
        stopWatching();
    }
}

