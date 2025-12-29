package top.yumbo.ai.omni.storage.example;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.storage.DocumentStorageRegistry;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.model.Image;

import java.util.List;

/**
 * 文档存储多实例使用示例
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class DocumentStorageMultiInstanceExample {

    /**
     * 主存储服务（自动注入 primary 实例）
     */
    private final DocumentStorageService primaryStorageService;

    /**
     * 存储注册表（管理所有实例）
     */
    private final DocumentStorageRegistry storageRegistry;

    /**
     * 示例 1: 使用主实例保存文档
     */
    public void example1_SaveWithPrimaryInstance(String documentId, byte[] fileData) {
        log.info("=== 示例 1: 使用主实例 ===");

        // 保存原始文档
        primaryStorageService.saveDocument(documentId, "document.pdf", fileData);
        log.info("✅ 文档已保存到主存储");

        // 保存提取的文本
        String extractedText = "这是提取的文本内容...";
        primaryStorageService.saveExtractedText(documentId, extractedText);
        log.info("✅ 提取文本已保存");
    }

    /**
     * 示例 2: 使用指定实例
     */
    public void example2_UseSpecificInstance(String documentId, List<Chunk> chunks) {
        log.info("=== 示例 2: 使用指定实例 ===");

        // 使用 MongoDB 存储块数据
        DocumentStorageService mongoStorage = storageRegistry.getServiceOrThrow("prod-storage");
        mongoStorage.saveChunks(documentId, chunks);
        log.info("✅ 已保存到 MongoDB 存储");

        // 使用 Redis 缓存
        DocumentStorageService redisStorage = storageRegistry.getServiceOrThrow("cache-storage");
        redisStorage.saveChunks(documentId, chunks);
        log.info("✅ 已缓存到 Redis");
    }

    /**
     * 示例 3: 多存储备份
     */
    public void example3_MultiStorageBackup(String documentId, byte[] fileData) {
        log.info("=== 示例 3: 多存储备份 ===");

        // 保存到本地 File 存储
        DocumentStorageService fileStorage = storageRegistry.getServiceOrThrow("dev-storage");
        fileStorage.saveDocument(documentId, "document.pdf", fileData);

        // 同时备份到云端 S3
        if (storageRegistry.hasInstance("cloud-storage")) {
            DocumentStorageService s3Storage = storageRegistry.getServiceOrThrow("cloud-storage");
            s3Storage.saveDocument(documentId, "document.pdf", fileData);
            log.info("✅ 已备份到云端");
        }
    }

    /**
     * 示例 4: 遍历所有实例
     */
    public void example4_IterateAllInstances() {
        log.info("=== 示例 4: 遍历所有实例 ===");

        log.info("当前活跃的存储实例: {}", storageRegistry.getInstanceIds());
        log.info("实例总数: {}", storageRegistry.size());

        // 检查每个实例的状态
        for (String instanceId : storageRegistry.getInstanceIds()) {
            DocumentStorageService service = storageRegistry.getServiceOrThrow(instanceId);
            // 可以获取统计信息等
            log.info("实例 [{}] 状态正常", instanceId);
        }
    }

    /**
     * 示例 5: 图像存储
     */
    public void example5_ImageStorage(String documentId, List<Image> images) {
        log.info("=== 示例 5: 图像存储 ===");

        // 保存到主存储
        primaryStorageService.saveImages(documentId, images);
        log.info("✅ 图像已保存，共 {} 张", images.size());

        // 获取图像
        List<Image> retrievedImages = primaryStorageService.getImagesByDocument(documentId);
        log.info("✅ 获取图像成功，共 {} 张", retrievedImages.size());
    }

    /**
     * 示例 6: 文档清理
     */
    public void example6_CleanupDocument(String documentId) {
        log.info("=== 示例 6: 文档清理 ===");

        // 从所有存储中清理文档
        for (String instanceId : storageRegistry.getInstanceIds()) {
            DocumentStorageService service = storageRegistry.getServiceOrThrow(instanceId);
            service.cleanupDocument(documentId);
            log.info("✅ 已从 {} 清理文档", instanceId);
        }
    }

    /**
     * 示例 7: 读取文档
     */
    public void example7_ReadDocument(String documentId) {
        log.info("=== 示例 7: 读取文档 ===");

        // 先尝试从缓存读取
        if (storageRegistry.hasInstance("cache-storage")) {
            DocumentStorageService cache = storageRegistry.getServiceOrThrow("cache-storage");
            var cachedDoc = cache.getDocument(documentId);
            if (cachedDoc.isPresent()) {
                log.info("✅ 从缓存读取成功");
                return;
            }
        }

        // 缓存未命中，从主存储读取
        var doc = primaryStorageService.getDocument(documentId);
        if (doc.isPresent()) {
            log.info("✅ 从主存储读取成功");
        } else {
            log.warn("⚠️ 文档不存在");
        }
    }
}

