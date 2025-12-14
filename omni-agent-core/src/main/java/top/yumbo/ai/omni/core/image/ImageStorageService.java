package top.yumbo.ai.omni.core.image;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Image;

import java.util.List;
import java.util.Optional;

/**
 * 图像存储服务 - 负责图像的存储和管理
 * (Image Storage Service - Responsible for image storage and management)
 *
 * <p>
 * 重构说明 (Refactoring Notes):
 * - 使用 DocumentStorageService 接口存储图像
 * - 删除硬编码的文件存储
 * - 支持多种存储后端（File/MongoDB/S3/MinIO/Redis等）
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0 (Refactored)
 * @version 3.0.0 - 重构为可插拔架构
 */
@Slf4j
@Service
public class ImageStorageService {

    private final DocumentStorageService storageService;

    @Autowired
    public ImageStorageService(DocumentStorageService storageService) {
        this.storageService = storageService;
        log.info("ImageStorageService initialized with storage: {}",
                 storageService.getClass().getSimpleName());
    }

    /**
     * 保存图像
     */
    public String saveImage(String documentId, byte[] imageData, String format) {
        if (imageData == null || imageData.length == 0) {
            log.warn("Empty image data for document: {}", documentId);
            return null;
        }

        try {
            Image image = Image.builder()
                .documentId(documentId)
                .data(imageData)
                .format(format)
                .createdAt(System.currentTimeMillis())
                .build();

            String imageId = storageService.saveImage(documentId, image);
            log.info("Saved image for document {}: {} bytes", documentId, imageData.length);
            return imageId;
        } catch (Exception e) {
            log.error("Failed to save image for document: {}", documentId, e);
            return null;
        }
    }

    /**
     * 获取图像
     */
    public Optional<Image> getImage(String imageId) {
        try {
            return storageService.getImage(imageId);
        } catch (Exception e) {
            log.error("Failed to get image: {}", imageId, e);
            return Optional.empty();
        }
    }

    /**
     * 获取文档的所有图像
     */
    public List<Image> getImagesByDocument(String documentId) {
        try {
            return storageService.getImagesByDocument(documentId);
        } catch (Exception e) {
            log.error("Failed to get images for document: {}", documentId, e);
            return List.of();
        }
    }

    /**
     * 删除图像
     */
    public void deleteImage(String imageId) {
        try {
            storageService.deleteImage(imageId);
            log.info("Deleted image: {}", imageId);
        } catch (Exception e) {
            log.error("Failed to delete image: {}", imageId, e);
        }
    }

    /**
     * 删除文档的所有图像
     */
    public void deleteImagesByDocument(String documentId) {
        try {
            storageService.deleteImagesByDocument(documentId);
            log.info("Deleted all images for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete images for document: {}", documentId, e);
        }
    }
}

