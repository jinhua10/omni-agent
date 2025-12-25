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

import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

/**
 * 文件监听服务（重构版）
 *
 * 职责简化：
 * 1. 监听 data/documents 目录的文件变化
 * 2. 定期触发文档注册服务扫描新文件
 * 3. 不负责注册和处理逻辑
 *
 * @author OmniAgent Team
 * @since 3.0.0 (Refactored)
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class FileWatcherService {

    private final ConfigPersistenceService configService;
    private final DocumentRegistrationService registrationService;  // ⭐ 注册服务

    private WatchService watchService;
    private ExecutorService executorService;
    private ScheduledExecutorService scanExecutor;
    private volatile boolean running = false;

    // 文件处理记录（相对路径 -> 记录）
    private final ConcurrentHashMap<String, FileChangeRecord> processingRecords = new ConcurrentHashMap<>();


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

        // 递归注册子目录（使用 try-with-resources 避免资源泄漏）
        try (var pathStream = Files.walk(dir, 1)) {
            pathStream
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
    }

    /**
     * 停止文件监听
     */
    public synchronized void stopWatching() {
        if (!running) {
            return;
        }

        log.info("🛑 正在停止文件监听...");
        running = false;

        try {
            // 1️⃣ 先关闭线程池，等待线程完成
            if (executorService != null) {
                executorService.shutdown();
                if (!executorService.awaitTermination(5, TimeUnit.SECONDS)) {
                    log.warn("⚠️ 监听线程未能在5秒内正常结束，强制关闭");
                    executorService.shutdownNow();
                }
            }

            if (scanExecutor != null) {
                scanExecutor.shutdown();
                if (!scanExecutor.awaitTermination(5, TimeUnit.SECONDS)) {
                    log.warn("⚠️ 扫描线程未能在5秒内正常结束，强制关闭");
                    scanExecutor.shutdownNow();
                }
            }

            // 2️⃣ 再关闭 WatchService（此时所有使用它的线程已停止）
            if (watchService != null) {
                watchService.close();
            }

            log.info("✅ 文件监听已停止");
        } catch (Exception e) {
            log.error("❌ 停止文件监听失败", e);
        }
    }

    /**
     * 扫描并注册新文档（定期任务）⭐ 重构后的核心方法
     *
     * 职责简化：
     * 1. 只负责触发注册服务
     * 2. 不处理具体的注册逻辑
     */
    private void scanAndProcessUnindexedFiles() {
        try {
            String watchDirectory = currentConfig.getWatchDirectory();
            log.debug("🔍 触发文档注册扫描: {}", watchDirectory);

            // 委托给注册服务处理
            int registeredCount = registrationService.scanAndRegisterDocuments(watchDirectory);

            if (registeredCount > 0) {
                log.info("✅ 扫描完成，新注册 {} 个文档", registeredCount);
            }

        } catch (Exception e) {
            log.error("❌ 扫描文档失败", e);
        }
    }

    /**
     * 监听循环（简化版）
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
                log.debug("监听线程被中断，准备退出");
                break;
            } catch (ClosedWatchServiceException e) {
                // WatchService 已关闭，正常退出（应用关闭时会发生）
                log.debug("WatchService 已关闭，监听循环退出");
                break;
            } catch (Exception e) {
                // 只有在服务仍在运行时才记录错误
                if (running) {
                    log.error("❌ 处理文件变化失败", e);
                } else {
                    log.debug("监听服务已停止，忽略后续错误");
                    break;
                }
            }
        }

        log.info("🛑 文件监听循环结束");
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

