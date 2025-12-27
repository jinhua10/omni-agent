package top.yumbo.ai.omni.web.util;

import lombok.extern.slf4j.Slf4j;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.List;

/**
 * 文件存储工具类
 * (File Storage Utility)
 *
 * <p>负责原始文件的存储和管理</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class FileStorageUtil {

    private static final String DEFAULT_UPLOAD_DIR = "./data/documents";
    private static Path uploadDir;

    static {
        initializeUploadDirectory(DEFAULT_UPLOAD_DIR);
    }

    /**
     * 初始化上传目录
     */
    public static void initializeUploadDirectory(String directory) {
        try {
            uploadDir = Paths.get(directory).toAbsolutePath().normalize();
            Files.createDirectories(uploadDir);
            log.info("文件上传目录初始化成功: {}", uploadDir);
        } catch (IOException e) {
            log.error("创建上传目录失败: {}", directory, e);
            throw new RuntimeException("无法创建上传目录", e);
        }
    }

    /**
     * 保存上传的文件
     *
     * @param file       上传的文件
     * @param documentId 文档ID
     * @return 保存后的文件路径
     * @throws IOException 如果保存失败
     */
    public static Path saveFile(MultipartFile file, String documentId) throws IOException {
        String originalFilename = file.getOriginalFilename();
        if (originalFilename == null || originalFilename.isEmpty()) {
            throw new IOException("文件名为空");
        }

        // 清理文件名，防止路径遍历攻击
        String sanitizedFilename = sanitizeFilename(originalFilename);

        // 使用文档ID作为文件名前缀，确保唯一性
        String filename = documentId + "_" + sanitizedFilename;
        Path targetPath = uploadDir.resolve(filename);

        // 保存文件
        Files.copy(file.getInputStream(), targetPath, StandardCopyOption.REPLACE_EXISTING);

        log.info("文件保存成功: {}", targetPath);
        return targetPath;
    }

    /**
     * 获取文件
     *
     * @param documentId 文档ID
     * @param filename   原始文件名
     * @return 文件路径
     */
    public static Path getFile(String documentId, String filename) {
        String sanitizedFilename = sanitizeFilename(filename);
        String storedFilename = documentId + "_" + sanitizedFilename;
        return uploadDir.resolve(storedFilename);
    }

    /**
     * 通过文件名查找文件
     *
     * @param filename 原始文件名
     * @return 文件路径，如果不存在返回 null
     */
    public static Path findFileByName(String filename) throws IOException {
        String sanitizedFilename = sanitizeFilename(filename);

        // 在上传目录中搜索匹配的文件
        return Files.list(uploadDir)
                .filter(path -> path.getFileName().toString().endsWith("_" + sanitizedFilename))
                .findFirst()
                .orElse(null);
    }

    /**
     * 删除文件
     *
     * @param documentId 文档ID
     * @param filename   原始文件名
     * @return 是否删除成功
     */
    public static boolean deleteFile(String documentId, String filename) {
        try {
            Path filePath = getFile(documentId, filename);
            if (Files.exists(filePath)) {
                Files.delete(filePath);
                log.info("文件删除成功: {}", filePath);
                return true;
            } else {
                log.warn("文件不存在: {}", filePath);
                return false;
            }
        } catch (IOException e) {
            log.error("删除文件失败: documentId={}, filename={}", documentId, filename, e);
            return false;
        }
    }

    /**
     * 通过文档ID删除文件（搜索以documentId开头的文件）
     *
     * @param documentId 文档ID
     * @return 是否删除成功
     */
    public static boolean deleteFileByDocumentId(String documentId) {
        try {
            // 在上传目录中搜索以 documentId_ 开头的文件
            List<Path> matchingFiles = Files.list(uploadDir)
                    .filter(path -> path.getFileName().toString().startsWith(documentId + "_"))
                    .collect(java.util.stream.Collectors.toList());

            if (matchingFiles.isEmpty()) {
                log.warn("未找到文档ID对应的文件: {}", documentId);
                return false;
            }

            // 删除所有匹配的文件
            boolean allDeleted = true;
            for (Path filePath : matchingFiles) {
                try {
                    Files.delete(filePath);
                    log.info("✅ 物理文件删除成功: {}", filePath);
                } catch (IOException e) {
                    log.error("❌ 物理文件删除失败: {}", filePath, e);
                    allDeleted = false;
                }
            }

            return allDeleted;

        } catch (IOException e) {
            log.error("搜索文件失败: documentId={}", documentId, e);
            return false;
        }
    }

    /**
     * 清理文件名，防止路径遍历攻击
     */
    private static String sanitizeFilename(String filename) {
        // 移除路径分隔符和特殊字符
        return filename.replaceAll("[/\\\\]", "")
                .replaceAll("[^a-zA-Z0-9._\\-\u4e00-\u9fa5]", "_");
    }

    /**
     * 检查文件是否存在
     */
    public static boolean fileExists(String documentId, String filename) {
        Path filePath = getFile(documentId, filename);
        return Files.exists(filePath);
    }

    /**
     * 获取上传目录
     */
    public static Path getUploadDir() {
        return uploadDir;
    }
}



