package top.yumbo.ai.storage.redis;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;
import top.yumbo.ai.storage.api.model.Image;
import top.yumbo.ai.storage.api.model.PPLData;
import top.yumbo.ai.storage.api.model.StorageStatistics;

import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * Redis 文档存储实现 - 高性能缓存存储
 * (Redis Document Storage Implementation - High-performance Cache Storage)
 *
 * <p>
 * 特点 (Features):
 * - 高性能读写
 * - 支持数据过期
 * - 适合临时/缓存数据
 * - 支持主从复制和集群
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - Redis Starter 实现
 */
@Slf4j
public class RedisDocumentStorage implements DocumentStorageService {

    private final RedisTemplate<String, Object> redisTemplate;
    private final RedisStorageProperties properties;
    private final ObjectMapper objectMapper;

    public RedisDocumentStorage(RedisTemplate<String, Object> redisTemplate,
                               RedisStorageProperties properties) {
        this.redisTemplate = redisTemplate;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();
        log.info("RedisDocumentStorage initialized with prefix: {}", properties.getKeyPrefix());
    }

    // ========== Key 生成 ==========

    private String getChunkKey(String chunkId) {
        return properties.getKeyPrefix() + "chunk:" + chunkId;
    }

    private String getDocumentChunksKey(String documentId) {
        return properties.getKeyPrefix() + "doc:" + documentId + ":chunks";
    }

    private String getImageKey(String imageId) {
        return properties.getKeyPrefix() + "image:" + imageId;
    }

    private String getDocumentImagesKey(String documentId) {
        return properties.getKeyPrefix() + "doc:" + documentId + ":images";
    }

    private String getPPLKey(String documentId) {
        return properties.getKeyPrefix() + "ppl:" + documentId;
    }

    private String getOptimizationKey(String documentId, String optimizationType) {
        return properties.getKeyPrefix() + "opt:" + documentId + ":" + optimizationType;
    }

    private String getDocumentOptimizationsKey(String documentId) {
        return properties.getKeyPrefix() + "doc:" + documentId + ":optimizations";
    }

    private String getDocumentKey(String documentId) {
        return properties.getKeyPrefix() + "doc:" + documentId;
    }

    // ========== Chunk Storage ==========

    @Override
    public String saveChunk(String documentId, Chunk chunk) {
        try {
            String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();
            chunk.setId(chunkId);

            // 保存 chunk 对象
            String chunkKey = getChunkKey(chunkId);
            redisTemplate.opsForValue().set(chunkKey, chunk);

            // 添加到文档的 chunks 集合
            String docChunksKey = getDocumentChunksKey(documentId);
            redisTemplate.opsForSet().add(docChunksKey, chunkId);

            // 设置过期时间
            if (properties.getTtl() > 0) {
                redisTemplate.expire(chunkKey, properties.getTtl(), TimeUnit.SECONDS);
                redisTemplate.expire(docChunksKey, properties.getTtl(), TimeUnit.SECONDS);
            }

            log.debug("Saved chunk: {}", chunkId);
            return chunkId;
        } catch (Exception e) {
            log.error("Failed to save chunk", e);
            return null;
        }
    }

    @Override
    public List<String> saveChunks(String documentId, List<Chunk> chunks) {
        List<String> chunkIds = new ArrayList<>();
        for (Chunk chunk : chunks) {
            String chunkId = saveChunk(documentId, chunk);
            if (chunkId != null) {
                chunkIds.add(chunkId);
            }
        }
        return chunkIds;
    }

    @Override
    public Optional<Chunk> getChunk(String chunkId) {
        try {
            String chunkKey = getChunkKey(chunkId);
            Object obj = redisTemplate.opsForValue().get(chunkKey);
            if (obj instanceof Chunk) {
                return Optional.of((Chunk) obj);
            }
            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get chunk: {}", chunkId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<Chunk> getChunksByDocument(String documentId) {
        try {
            String docChunksKey = getDocumentChunksKey(documentId);
            Set<Object> chunkIds = redisTemplate.opsForSet().members(docChunksKey);

            if (chunkIds == null || chunkIds.isEmpty()) {
                return new ArrayList<>();
            }

            return chunkIds.stream()
                    .map(id -> getChunk(id.toString()))
                    .filter(Optional::isPresent)
                    .map(Optional::get)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get chunks for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteChunk(String chunkId) {
        try {
            String chunkKey = getChunkKey(chunkId);
            redisTemplate.delete(chunkKey);
            log.debug("Deleted chunk: {}", chunkId);
        } catch (Exception e) {
            log.error("Failed to delete chunk: {}", chunkId, e);
        }
    }

    @Override
    public void deleteChunksByDocument(String documentId) {
        try {
            String docChunksKey = getDocumentChunksKey(documentId);
            Set<Object> chunkIds = redisTemplate.opsForSet().members(docChunksKey);

            if (chunkIds != null) {
                for (Object chunkId : chunkIds) {
                    deleteChunk(chunkId.toString());
                }
            }

            redisTemplate.delete(docChunksKey);
            log.info("Deleted all chunks for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete chunks for document: {}", documentId, e);
        }
    }

    // ========== Image Storage ==========

    @Override
    public String saveImage(String documentId, Image image) {
        try {
            String imageId = image.getId() != null ? image.getId() : UUID.randomUUID().toString();
            image.setId(imageId);

            // 保存 image 对象
            String imageKey = getImageKey(imageId);
            redisTemplate.opsForValue().set(imageKey, image);

            // 添加到文档的 images 集合
            String docImagesKey = getDocumentImagesKey(documentId);
            redisTemplate.opsForSet().add(docImagesKey, imageId);

            // 设置过期时间
            if (properties.getTtl() > 0) {
                redisTemplate.expire(imageKey, properties.getTtl(), TimeUnit.SECONDS);
                redisTemplate.expire(docImagesKey, properties.getTtl(), TimeUnit.SECONDS);
            }

            log.debug("Saved image: {}", imageId);
            return imageId;
        } catch (Exception e) {
            log.error("Failed to save image", e);
            return null;
        }
    }

    @Override
    public Optional<Image> getImage(String imageId) {
        try {
            String imageKey = getImageKey(imageId);
            Object obj = redisTemplate.opsForValue().get(imageKey);
            if (obj instanceof Image) {
                return Optional.of((Image) obj);
            }
            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get image: {}", imageId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<Image> getImagesByDocument(String documentId) {
        try {
            String docImagesKey = getDocumentImagesKey(documentId);
            Set<Object> imageIds = redisTemplate.opsForSet().members(docImagesKey);

            if (imageIds == null || imageIds.isEmpty()) {
                return new ArrayList<>();
            }

            return imageIds.stream()
                    .map(id -> getImage(id.toString()))
                    .filter(Optional::isPresent)
                    .map(Optional::get)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get images for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteImage(String imageId) {
        try {
            String imageKey = getImageKey(imageId);
            redisTemplate.delete(imageKey);
            log.debug("Deleted image: {}", imageId);
        } catch (Exception e) {
            log.error("Failed to delete image: {}", imageId, e);
        }
    }

    @Override
    public void deleteImagesByDocument(String documentId) {
        try {
            String docImagesKey = getDocumentImagesKey(documentId);
            Set<Object> imageIds = redisTemplate.opsForSet().members(docImagesKey);

            if (imageIds != null) {
                for (Object imageId : imageIds) {
                    deleteImage(imageId.toString());
                }
            }

            redisTemplate.delete(docImagesKey);
            log.info("Deleted all images for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete images for document: {}", documentId, e);
        }
    }

    // ========== PPL Data Storage ==========

    @Override
    public String savePPLData(String documentId, PPLData data) {
        try {
            String pplKey = getPPLKey(documentId);
            redisTemplate.opsForValue().set(pplKey, data);

            // 设置过期时间
            if (properties.getTtl() > 0) {
                redisTemplate.expire(pplKey, properties.getTtl(), TimeUnit.SECONDS);
            }

            log.debug("Saved PPL data for document: {}", documentId);
            return documentId;
        } catch (Exception e) {
            log.error("Failed to save PPL data", e);
            return null;
        }
    }

    @Override
    public Optional<PPLData> getPPLData(String documentId) {
        try {
            String pplKey = getPPLKey(documentId);
            Object obj = redisTemplate.opsForValue().get(pplKey);
            if (obj instanceof PPLData) {
                return Optional.of((PPLData) obj);
            }
            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get PPL data for document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deletePPLData(String documentId) {
        try {
            String pplKey = getPPLKey(documentId);
            redisTemplate.delete(pplKey);
            log.info("Deleted PPL data for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete PPL data for document: {}", documentId, e);
        }
    }

    // ========== Optimization Data Storage ==========

    @Override
    public String saveOptimizationData(String documentId, top.yumbo.ai.storage.api.model.OptimizationData data) {
        try {
            String optKey = getOptimizationKey(documentId, data.getOptimizationType());
            redisTemplate.opsForValue().set(optKey, data);

            // 添加到文档的优化类型集合
            String docOptsKey = getDocumentOptimizationsKey(documentId);
            redisTemplate.opsForSet().add(docOptsKey, data.getOptimizationType());

            // 设置过期时间
            if (properties.getTtl() > 0) {
                redisTemplate.expire(optKey, properties.getTtl(), java.util.concurrent.TimeUnit.SECONDS);
                redisTemplate.expire(docOptsKey, properties.getTtl(), java.util.concurrent.TimeUnit.SECONDS);
            }

            log.debug("Saved {} optimization data for document: {}", data.getOptimizationType(), documentId);
            return documentId + ":" + data.getOptimizationType();
        } catch (Exception e) {
            log.error("Failed to save optimization data", e);
            return null;
        }
    }

    @Override
    public Optional<top.yumbo.ai.storage.api.model.OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        try {
            String optKey = getOptimizationKey(documentId, optimizationType);
            Object obj = redisTemplate.opsForValue().get(optKey);
            if (obj instanceof top.yumbo.ai.storage.api.model.OptimizationData) {
                return Optional.of((top.yumbo.ai.storage.api.model.OptimizationData) obj);
            }
            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get {} optimization data for document: {}", optimizationType, documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<top.yumbo.ai.storage.api.model.OptimizationData> getAllOptimizationData(String documentId) {
        try {
            String docOptsKey = getDocumentOptimizationsKey(documentId);
            Set<Object> optimizationTypes = redisTemplate.opsForSet().members(docOptsKey);

            if (optimizationTypes == null || optimizationTypes.isEmpty()) {
                return new ArrayList<>();
            }

            return optimizationTypes.stream()
                    .map(type -> getOptimizationData(documentId, type.toString()))
                    .filter(Optional::isPresent)
                    .map(Optional::get)
                    .collect(java.util.stream.Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get all optimization data for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteOptimizationData(String documentId, String optimizationType) {
        try {
            String optKey = getOptimizationKey(documentId, optimizationType);
            redisTemplate.delete(optKey);

            // 从文档的优化类型集合中移除
            String docOptsKey = getDocumentOptimizationsKey(documentId);
            redisTemplate.opsForSet().remove(docOptsKey, optimizationType);

            log.info("Deleted {} optimization data for document: {}", optimizationType, documentId);
        } catch (Exception e) {
            log.error("Failed to delete {} optimization data for document: {}", optimizationType, documentId, e);
        }
    }

    @Override
    public void deleteAllOptimizationData(String documentId) {
        try {
            String docOptsKey = getDocumentOptimizationsKey(documentId);
            Set<Object> optimizationTypes = redisTemplate.opsForSet().members(docOptsKey);

            if (optimizationTypes != null) {
                for (Object type : optimizationTypes) {
                    String optKey = getOptimizationKey(documentId, type.toString());
                    redisTemplate.delete(optKey);
                }
            }

            redisTemplate.delete(docOptsKey);
            log.info("Deleted all optimization data for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete all optimization data for document: {}", documentId, e);
        }
    }

    // ========== Document Management ==========

    @Override
    public void cleanupDocument(String documentId) {
        deleteChunksByDocument(documentId);
        deleteImagesByDocument(documentId);
        deletePPLData(documentId);
        deleteAllOptimizationData(documentId);
        deleteExtractedText(documentId);  // ⭐ 新增

        // 删除文档元数据
        String docKey = getDocumentKey(documentId);
        redisTemplate.delete(docKey);

        log.info("Cleaned up all data for document: {}", documentId);
    }

    @Override
    public boolean documentExists(String documentId) {
        try {
            String docChunksKey = getDocumentChunksKey(documentId);
            String docImagesKey = getDocumentImagesKey(documentId);
            String pplKey = getPPLKey(documentId);

            return Boolean.TRUE.equals(redisTemplate.hasKey(docChunksKey)) ||
                   Boolean.TRUE.equals(redisTemplate.hasKey(docImagesKey)) ||
                   Boolean.TRUE.equals(redisTemplate.hasKey(pplKey));
        } catch (Exception e) {
            log.error("Failed to check document existence: {}", documentId, e);
            return false;
        }
    }

    @Override
    public long getDocumentSize(String documentId) {
        try {
            long size = 0;

            // 计算 chunks 大小（估算）
            List<Chunk> chunks = getChunksByDocument(documentId);
            for (Chunk chunk : chunks) {
                if (chunk.getContent() != null) {
                    size += chunk.getContent().getBytes().length;
                }
            }

            // 计算 images 大小
            List<Image> images = getImagesByDocument(documentId);
            for (Image image : images) {
                if (image.getData() != null) {
                    size += image.getData().length;
                }
            }

            // 计算 PPL 大小（估算）
            Optional<PPLData> pplData = getPPLData(documentId);
            if (pplData.isPresent()) {
                size += 1024; // 估算 PPL 数据大小
            }

            return size;
        } catch (Exception e) {
            log.error("Failed to calculate document size for: {}", documentId, e);
            return 0;
        }
    }

    // ========== Document Management ==========

    @Override
    public String saveDocument(String documentId, String filename, byte[] fileData) {
        try {
            String documentKey = getDocumentKey(documentId);

            Map<String, Object> docData = new HashMap<>();
            docData.put("documentId", documentId);
            docData.put("filename", filename);
            docData.put("data", fileData);
            docData.put("createdAt", System.currentTimeMillis());

            redisTemplate.opsForHash().putAll(documentKey, docData);

            if (properties.getTtl() > 0) {
                redisTemplate.expire(documentKey, properties.getTtl(), TimeUnit.SECONDS);
            }

            log.debug("Saved document: {}", documentId);
            return documentId;
        } catch (Exception e) {
            log.error("Failed to save document: {}", documentId, e);
            return null;
        }
    }

    @Override
    public Optional<byte[]> getDocument(String documentId) {
        try {
            String documentKey = getDocumentKey(documentId);
            Object data = redisTemplate.opsForHash().get(documentKey, "data");

            if (data instanceof byte[]) {
                return Optional.of((byte[]) data);
            }
            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deleteDocument(String documentId) {
        try {
            String documentKey = getDocumentKey(documentId);
            redisTemplate.delete(documentKey);
            log.debug("Deleted document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete document: {}", documentId, e);
        }
    }

    // ========== Extracted Text Storage ⭐ NEW ==========

    @Override
    public String saveExtractedText(String documentId, String text) {
        try {
            String key = properties.getKeyPrefix() + "extracted:" + documentId;
            redisTemplate.opsForValue().set(key, text);
            log.debug("✅ Saved extracted text: {}, length={}", documentId, text.length());
            return documentId;
        } catch (Exception e) {
            log.error("❌ Failed to save extracted text: {}", documentId, e);
            return null;
        }
    }

    @Override
    public Optional<String> getExtractedText(String documentId) {
        try {
            String key = properties.getKeyPrefix() + "extracted:" + documentId;
            Object value = redisTemplate.opsForValue().get(key);

            if (value != null) {
                String text = value.toString();
                log.debug("✅ Retrieved extracted text: {}, length={}", documentId, text.length());
                return Optional.of(text);
            }

            log.debug("⚠️ Extracted text not found: {}", documentId);
            return Optional.empty();
        } catch (Exception e) {
            log.error("❌ Failed to get extracted text: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deleteExtractedText(String documentId) {
        try {
            String key = properties.getKeyPrefix() + "extracted:" + documentId;
            redisTemplate.delete(key);
            log.debug("🗑️ Deleted extracted text: {}", documentId);
        } catch (Exception e) {
            log.error("❌ Failed to delete extracted text: {}", documentId, e);
        }
    }

    @Override
    public List<top.yumbo.ai.storage.api.model.DocumentMetadata> listAllDocuments() {
        try {
            Set<String> keys = redisTemplate.keys(properties.getKeyPrefix() + "doc:*");
            if (keys == null || keys.isEmpty()) {
                return new ArrayList<>();
            }

            return keys.stream()
                    .filter(key -> !key.contains(":chunks") && !key.contains(":images") && !key.contains(":optimizations"))
                    .map(this::convertToDocumentMetadata)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to list all documents", e);
            return new ArrayList<>();
        }
    }

    @Override
    public List<top.yumbo.ai.storage.api.model.DocumentMetadata> listDocuments(int offset, int limit) {
        try {
            List<top.yumbo.ai.storage.api.model.DocumentMetadata> allDocs = listAllDocuments();
            return allDocs.stream()
                    .skip(offset)
                    .limit(limit)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to list documents with pagination", e);
            return new ArrayList<>();
        }
    }

    @Override
    public List<top.yumbo.ai.storage.api.model.DocumentMetadata> searchDocuments(String keyword) {
        try {
            return listAllDocuments().stream()
                    .filter(doc -> doc.getFilename() != null && doc.getFilename().contains(keyword))
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to search documents with keyword: {}", keyword, e);
            return new ArrayList<>();
        }
    }

    @Override
    public long getDocumentCount() {
        try {
            return listAllDocuments().size();
        } catch (Exception e) {
            log.error("Failed to get document count", e);
            return 0;
        }
    }

    private top.yumbo.ai.storage.api.model.DocumentMetadata convertToDocumentMetadata(String key) {
        try {
            Map<Object, Object> data = redisTemplate.opsForHash().entries(key);
            if (data == null || data.isEmpty()) {
                return null;
            }

            String documentId = (String) data.get("documentId");
            String filename = (String) data.get("filename");
            Long createdAt = data.get("createdAt") instanceof Long ?
                    (Long) data.get("createdAt") : System.currentTimeMillis();

            byte[] fileData = data.get("data") instanceof byte[] ? (byte[]) data.get("data") : null;
            long fileSize = fileData != null ? fileData.length : 0;

            return top.yumbo.ai.storage.api.model.DocumentMetadata.builder()
                    .documentId(documentId)
                    .filename(filename)
                    .fileSize(fileSize)
                    .uploadTime(new java.util.Date(createdAt))
                    .lastModified(new java.util.Date(createdAt))
                    .build();
        } catch (Exception e) {
            log.error("Failed to convert key to DocumentMetadata: {}", key, e);
            return null;
        }
    }

    // ========== Statistics ==========

    @Override
    public StorageStatistics getStatistics() {
        try {
            // Redis 无法高效统计所有 key，使用模式匹配（注意：生产环境慎用 KEYS 命令）
            Set<String> allKeys = redisTemplate.keys(properties.getKeyPrefix() + "*");

            long totalChunks = 0;
            long totalImages = 0;
            long totalPPLData = 0;
            Set<String> documentIds = new HashSet<>();

            if (allKeys != null) {
                for (String key : allKeys) {
                    if (key.contains(":chunk:")) {
                        totalChunks++;
                    } else if (key.contains(":image:")) {
                        totalImages++;
                    } else if (key.contains(":ppl:")) {
                        totalPPLData++;
                    }

                    // 提取 documentId
                    if (key.contains(":doc:")) {
                        String[] parts = key.split(":doc:");
                        if (parts.length > 1) {
                            String docPart = parts[1].split(":")[0];
                            documentIds.add(docPart);
                        }
                    }
                }
            }

            return StorageStatistics.builder()
                    .totalDocuments(documentIds.size())
                    .totalChunks(totalChunks)
                    .totalImages(totalImages)
                    .totalPPLData(totalPPLData)
                    .totalSize(0) // Redis 难以准确计算总大小
                    .storageType("redis")
                    .healthy(isHealthy())
                    .timestamp(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.error("Failed to get statistics", e);
            return StorageStatistics.builder()
                    .storageType("redis")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try {
            // 测试 Redis 连接
            redisTemplate.opsForValue().get("health-check");
            return true;
        } catch (Exception e) {
            log.error("Health check failed", e);
            return false;
        }
    }

    // ========== 文件系统浏览实现 (File System Browse Implementation) =========
    // Redis通过Key命名实现虚拟文件系统，使用:分隔路径层级

    @Override
    public List<Map<String, Object>> listFiles(String virtualPath) {
        try {
            List<Map<String, Object>> items = new ArrayList<>();
            String searchPattern = virtualPath.isEmpty() ? "*" : virtualPath.replace("/", ":") + ":*";
            Set<String> directories = new HashSet<>();

            // 使用SCAN命令扫描匹配的键
            Set<String> keys = redisTemplate.keys(searchPattern);
            if (keys == null) {
                return items;
            }

            for (String key : keys) {
                String relativePath = key.substring((virtualPath.isEmpty() ? "" : virtualPath + "/").length());
                int colonIndex = relativePath.indexOf(':');

                if (colonIndex > 0) {
                    // 子目录
                    String dirName = relativePath.substring(0, colonIndex);
                    if (!directories.contains(dirName)) {
                        directories.add(dirName);
                        Map<String, Object> dirItem = new HashMap<>();
                        dirItem.put("name", dirName);
                        dirItem.put("type", "directory");
                        dirItem.put("path", virtualPath.isEmpty() ? dirName : virtualPath + "/" + dirName);
                        items.add(dirItem);
                    }
                } else {
                    // 文件
                    Object dataObj = redisTemplate.opsForValue().get(key);
                    byte[] data = dataObj instanceof byte[] ? (byte[]) dataObj : null;
                    Map<String, Object> fileItem = new HashMap<>();
                    fileItem.put("name", relativePath);
                    fileItem.put("type", "file");
                    fileItem.put("path", key.replace(":", "/"));
                    fileItem.put("size", data != null ? data.length : 0L);
                    fileItem.put("modified", System.currentTimeMillis());
                    items.add(fileItem);
                }
            }

            return items;
        } catch (Exception e) {
            log.error("列出文件失败: {}", virtualPath, e);
            throw new RuntimeException("列出文件失败: " + e.getMessage(), e);
        }
    }

    @Override
    public byte[] readFile(String virtualPath) {
        try {
            String key = virtualPath.replace("/", ":");
            Object dataObj = redisTemplate.opsForValue().get(key);

            if (dataObj == null) {
                log.warn("文件不存在: {}", virtualPath);
                return null;
            }

            return dataObj instanceof byte[] ? (byte[]) dataObj : null;
        } catch (Exception e) {
            log.error("读取文件失败: {}", virtualPath, e);
            throw new RuntimeException("读取文件失败: " + e.getMessage(), e);
        }
    }

    @Override
    public boolean deleteFile(String virtualPath) {
        try {
            String keyPattern = virtualPath.replace("/", ":") + "*";
            Set<String> keys = redisTemplate.keys(keyPattern);

            if (keys != null && !keys.isEmpty()) {
                redisTemplate.delete(keys);
                log.info("✅ 删除成功: {} (删除了{}个键)", virtualPath, keys.size());
                return true;
            }

            return false;
        } catch (Exception e) {
            log.error("删除失败: {}", virtualPath, e);
            return false;
        }
    }

    @Override
    public boolean createDirectory(String virtualPath) {
        try {
            // Redis中创建"目录"只是一个标记
            String key = virtualPath.replace("/", ":") + ":_dir";
            redisTemplate.opsForValue().set(key, new byte[0]);

            log.info("✅ 创建目录成功: {}", virtualPath);
            return true;
        } catch (Exception e) {
            log.error("创建目录失败: {}", virtualPath, e);
            return false;
        }
    }

    @Override
    public Map<String, Object> getStorageStats(String virtualPath) {
        try {
            String searchPattern = virtualPath.isEmpty() ? "*" : virtualPath.replace("/", ":") + ":*";
            Set<String> keys = redisTemplate.keys(searchPattern);

            long[] stats = {0, 0, 0}; // [files, folders, size]

            if (keys != null) {
                for (String key : keys) {
                    if (key.endsWith(":_dir")) {
                        stats[1]++;
                    } else {
                        stats[0]++;
                        Object dataObj = redisTemplate.opsForValue().get(key);
                        byte[] data = dataObj instanceof byte[] ? (byte[]) dataObj : null;
                        stats[2] += data != null ? data.length : 0;
                    }
                }
            }

            return Map.of(
                "totalFiles", stats[0],
                "totalFolders", stats[1],
                "totalSize", stats[2]
            );
        } catch (Exception e) {
            log.error("获取存储统计失败: {}", virtualPath, e);
            return Map.of(
                "totalFiles", 0L,
                "totalFolders", 0L,
                "totalSize", 0L
            );
        }
    }
}
