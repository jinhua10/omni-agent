package top.yumbo.ai.omni.web.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

/**
 * 归档清理任务服务
 * (Archive Cleanup Task Service)
 *
 * 定时清理中转站中失败的归档文件
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class ArchiveCleanupTaskService {

    private final SystemRAGConfigService ragConfigService;
    private final DocumentStorageService storageService;

    @Value("${omni-agent.file-watcher.watch-directory:./data/documents}")
    private String watchDirectory;

    @Value("${omni-agent.archive.cleanup.enabled:true}")
    private boolean cleanupEnabled;

    @Value("${omni-agent.archive.cleanup.retention-days:7}")
    private int retentionDays;

    /**
     * 定时清理任务
     * 每天凌晨2点执行
     */
    @Scheduled(cron = "${omni-agent.archive.cleanup.cron:0 0 2 * * ?}")
    public void cleanupFailedArchives() {
        if (!cleanupEnabled) {
            log.debug("🔕 归档清理任务已禁用");
            return;
        }

        log.info("🧹 开始归档清理任务: retentionDays={}", retentionDays);

        try {
            Path watchPath = Paths.get(watchDirectory);
            if (!Files.exists(watchPath)) {
                log.warn("⚠️ 中转站目录不存在: {}", watchDirectory);
                return;
            }

            // 扫描中转站文件
            List<FileCleanupCandidate> candidates = scanCleanupCandidates(watchPath);
            log.info("📋 发现 {} 个潜在清理候选文件", candidates.size());

            int retryCount = 0;
            int deleteCount = 0;
            int errorCount = 0;

            for (FileCleanupCandidate candidate : candidates) {
                try {
                    CleanupAction action = determineCleanupAction(candidate);

                    switch (action) {
                        case RETRY_ARCHIVE:
                            if (retryArchive(candidate)) {
                                retryCount++;
                            } else {
                                errorCount++;
                            }
                            break;

                        case DELETE:
                            if (deleteFile(candidate)) {
                                deleteCount++;
                            } else {
                                errorCount++;
                            }
                            break;

                        case SKIP:
                            log.debug("⏭️ 跳过文件: {}", candidate.getFileName());
                            break;
                    }
                } catch (Exception e) {
                    log.error("❌ 处理清理候选文件失败: {}", candidate.getFileName(), e);
                    errorCount++;
                }
            }

            log.info("✅ 归档清理任务完成: 重试归档={}, 删除={}, 错误={}", retryCount, deleteCount, errorCount);

        } catch (Exception e) {
            log.error("❌ 归档清理任务失败", e);
        }
    }

    /**
     * 扫描中转站，找出清理候选文件
     */
    private List<FileCleanupCandidate> scanCleanupCandidates(Path watchPath) throws IOException {
        List<FileCleanupCandidate> candidates = new ArrayList<>();
        long retentionMillis = TimeUnit.DAYS.toMillis(retentionDays);
        long threshold = System.currentTimeMillis() - retentionMillis;

        try (Stream<Path> pathStream = Files.walk(watchPath)) {
            pathStream
                .filter(Files::isRegularFile)
                .forEach(filePath -> {
                    try {
                        BasicFileAttributes attrs = Files.readAttributes(filePath, BasicFileAttributes.class);
                        long fileTime = attrs.creationTime().toMillis();

                        // 只处理超过保留期的文件
                        if (fileTime < threshold) {
                            String fileName = watchPath.relativize(filePath).toString();
                            candidates.add(new FileCleanupCandidate(
                                fileName,
                                filePath,
                                fileTime,
                                attrs.size()
                            ));
                        }
                    } catch (IOException e) {
                        log.warn("⚠️ 读取文件属性失败: {}", filePath, e);
                    }
                });
        }

        return candidates;
    }

    /**
     * 确定清理动作
     */
    private CleanupAction determineCleanupAction(FileCleanupCandidate candidate) {
        // 检查文档配置状态
        try {
            SystemRAGConfigService.DocumentRAGConfig config =
                ragConfigService.getDocumentConfig(candidate.getFileName());

            if (config == null || config.getCreatedAt() == 0) {
                // 未注册的文件，直接删除
                return CleanupAction.DELETE;
            }

            String status = config.getStatus();
            if ("COMPLETED".equals(status)) {
                // 状态是COMPLETED但文件还在，说明归档失败
                // 尝试重新归档
                return CleanupAction.RETRY_ARCHIVE;
            } else if ("FAILED".equals(status)) {
                // 处理失败的文件，直接删除
                return CleanupAction.DELETE;
            } else {
                // 其他状态（PENDING、EXTRACTING等），跳过
                return CleanupAction.SKIP;
            }
        } catch (Exception e) {
            log.warn("⚠️ 检查文档配置失败: {}", candidate.getFileName(), e);
            // 保守策略：跳过
            return CleanupAction.SKIP;
        }
    }

    /**
     * 重试归档
     */
    private boolean retryArchive(FileCleanupCandidate candidate) {
        try {
            log.info("🔄 重试归档: {}", candidate.getFileName());

            // 读取文件内容
            byte[] content = Files.readAllBytes(candidate.getFilePath());

            // 尝试归档
            String savedId = storageService.saveDocument(
                candidate.getFileName(),
                candidate.getFileName(),
                content
            );

            if (savedId != null) {
                log.info("✅ 重试归档成功: {}", candidate.getFileName());

                // 删除中转站文件
                Files.delete(candidate.getFilePath());
                log.info("🗑️ 已清理中转站文件: {}", candidate.getFileName());

                return true;
            } else {
                log.warn("⚠️ 重试归档返回null: {}", candidate.getFileName());
                return false;
            }
        } catch (Exception e) {
            log.error("❌ 重试归档失败: {}", candidate.getFileName(), e);
            return false;
        }
    }

    /**
     * 删除文件
     */
    private boolean deleteFile(FileCleanupCandidate candidate) {
        try {
            Files.delete(candidate.getFilePath());
            log.info("🗑️ 已删除文件: {}", candidate.getFileName());
            return true;
        } catch (IOException e) {
            log.error("❌ 删除文件失败: {}", candidate.getFileName(), e);
            return false;
        }
    }

    /**
     * 手动触发清理任务（用于测试或紧急情况）
     */
    public void manualCleanup() {
        log.info("🔧 手动触发归档清理任务");
        cleanupFailedArchives();
    }

    // ========== 内部类 ==========

    /**
     * 清理候选文件
     */
    private static class FileCleanupCandidate {
        private final String fileName;
        private final Path filePath;
        private final long creationTime;
        private final long fileSize;

        public FileCleanupCandidate(String fileName, Path filePath, long creationTime, long fileSize) {
            this.fileName = fileName;
            this.filePath = filePath;
            this.creationTime = creationTime;
            this.fileSize = fileSize;
        }

        public String getFileName() {
            return fileName;
        }

        public Path getFilePath() {
            return filePath;
        }

        public long getCreationTime() {
            return creationTime;
        }

        public long getFileSize() {
            return fileSize;
        }
    }

    /**
     * 清理动作
     */
    private enum CleanupAction {
        RETRY_ARCHIVE,  // 重试归档
        DELETE,         // 直接删除
        SKIP            // 跳过
    }
}






