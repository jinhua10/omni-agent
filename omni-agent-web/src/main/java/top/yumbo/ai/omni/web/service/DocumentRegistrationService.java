package top.yumbo.ai.omni.web.service;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * 文档注册服务
 * (Document Registration Service)
 *
 * 职责：
 * - 扫描中转站目录
 * - 注册新文档到配置服务
 * - 不处理文档，只负责注册
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class DocumentRegistrationService {

    private final SystemRAGConfigService ragConfigService;

    /**
     * 扫描并注册指定目录下的文档
     *
     * @param watchDirectory 监听目录
     * @return 新注册的文档数量
     */
    public int scanAndRegisterDocuments(String watchDirectory) {
        try {
            Path watchPath = Path.of(watchDirectory);
            if (!Files.exists(watchPath)) {
                log.warn("⚠️ 监听目录不存在: {}", watchDirectory);
                return 0;
            }

            log.debug("🔍 扫描待注册文档: {}", watchDirectory);

            // 递归扫描所有文件（使用 try-with-resources 避免资源泄漏）
            List<Path> unregisteredFiles;
            try (var pathStream = Files.walk(watchPath)) {
                unregisteredFiles = pathStream
                        .filter(Files::isRegularFile)
                        .filter(this::isSupportedFile)
                        .filter(path -> !isRegistered(watchPath.relativize(path).toString()))
                        .toList();
            }

            int registeredCount = 0;
            for (Path file : unregisteredFiles) {
                String relativePath = watchPath.relativize(file).toString();
                if (registerDocument(relativePath, file)) {
                    registeredCount++;
                }
            }

            if (registeredCount > 0) {
                log.info("✅ 扫描完成，新注册 {} 个文档", registeredCount);
            }

            return registeredCount;

        } catch (IOException e) {
            log.error("❌ 扫描文档失败", e);
            return 0;
        }
    }

    /**
     * 注册单个文档
     *
     * @param documentId 文档ID（通常是文件名）
     * @param filePath 文件路径（保留用于未来扩展）
     * @return 是否注册成功
     */
    public boolean registerDocument(String documentId, Path filePath) {
        try {
            // 检查是否已注册
            if (isRegistered(documentId)) {
                log.debug("⏭️ 文档已注册，跳过: {}", documentId);
                return false;
            }

            // 创建文档配置
            SystemRAGConfigService.DocumentRAGConfig config = new SystemRAGConfigService.DocumentRAGConfig();
            config.setDocumentId(documentId);
            config.setStatus("PENDING");
            config.setCreatedAt(System.currentTimeMillis());
            config.setUpdatedAt(System.currentTimeMillis());

            // 使用系统默认配置
            SystemRAGConfigService.SystemRAGConfig systemConfig = ragConfigService.getSystemConfig();
            config.setTextExtractionModel(systemConfig.getDefaultTextExtractionModel());
            config.setChunkingStrategy(systemConfig.getDefaultChunkingStrategy());
            config.setChunkingParams(new java.util.HashMap<>());

            // 注册到配置服务
            ragConfigService.setDocumentConfig(documentId, config);

            log.info("📝 文档已注册: {} (PENDING)", documentId);
            return true;

        } catch (Exception e) {
            log.error("❌ 注册文档失败: {}", documentId, e);
            return false;
        }
    }

    /**
     * 检查文档是否已注册
     */
    private boolean isRegistered(String documentId) {
        try {
            SystemRAGConfigService.DocumentRAGConfig config = ragConfigService.getDocumentConfig(documentId);
            // 如果配置存在且已创建，则认为已注册
            return config != null && config.getCreatedAt() > 0;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * 判断是否为支持的文件类型
     */
    private boolean isSupportedFile(Path path) {
        String fileName = path.getFileName().toString().toLowerCase();
        return fileName.endsWith(".pdf") ||
               fileName.endsWith(".docx") ||
               fileName.endsWith(".doc") ||
               fileName.endsWith(".pptx") ||
               fileName.endsWith(".ppt") ||
               fileName.endsWith(".xlsx") ||
               fileName.endsWith(".xls") ||
               fileName.endsWith(".txt") ||
               fileName.endsWith(".md");
    }

    /**
     * 获取待注册文档数量
     */
    public int getUnregisteredDocumentCount(String watchDirectory) {
        try {
            Path watchPath = Path.of(watchDirectory);
            if (!Files.exists(watchPath)) {
                return 0;
            }

            // 使用 try-with-resources 避免资源泄漏
            try (var pathStream = Files.walk(watchPath)) {
                return (int) pathStream
                        .filter(Files::isRegularFile)
                        .filter(this::isSupportedFile)
                        .filter(path -> !isRegistered(watchPath.relativize(path).toString()))
                        .count();
            }

        } catch (IOException e) {
            log.error("❌ 统计待注册文档失败", e);
            return 0;
        }
    }
}



