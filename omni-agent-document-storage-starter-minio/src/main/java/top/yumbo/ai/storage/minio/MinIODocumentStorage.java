package top.yumbo.ai.storage.minio;

import com.fasterxml.jackson.databind.ObjectMapper;
import io.minio.*;
import io.minio.messages.Item;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;
import top.yumbo.ai.storage.api.model.Image;
import top.yumbo.ai.storage.api.model.PPLData;
import top.yumbo.ai.storage.api.model.StorageStatistics;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.*;
import java.util.stream.Collectors;

/**
 * MinIO 对象存储实现 - 私有云对象存储方案
 * (MinIO Object Storage Implementation - Private Cloud Object Storage)
 *
 * <p>
 * 特点 (Features):
 * - 兼容 S3 API
 * - 私有云部署，数据完全自主可控
 * - 高性能、可扩展
 * - 支持分布式部署
 * - 适合企业内部大规模文档存储
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - MinIO Starter 实现
 */
@Slf4j
public class MinIODocumentStorage implements DocumentStorageService {

    private final MinioClient minioClient;
    private final MinIOStorageProperties properties;
    private final ObjectMapper objectMapper;

    public MinIODocumentStorage(MinioClient minioClient, MinIOStorageProperties properties) {
        this.minioClient = minioClient;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();

        ensureBucketExists();
        log.info("MinIODocumentStorage initialized - bucket: {}", properties.getBucketName());
    }

    private void ensureBucketExists() {
        try {
            boolean exists = minioClient.bucketExists(
                BucketExistsArgs.builder()
                    .bucket(properties.getBucketName())
                    .build()
            );

            if (!exists) {
                minioClient.makeBucket(
                    MakeBucketArgs.builder()
                        .bucket(properties.getBucketName())
                        .build()
                );
                log.info("Created bucket: {}", properties.getBucketName());
            }
        } catch (Exception e) {
            log.error("Failed to ensure bucket exists", e);
        }
    }

    // ========== Key 生成 ==========

    private String getChunkKey(String documentId, String chunkId) {
        return "chunks/" + documentId + "/" + chunkId + ".json";
    }

    private String getImageKey(String documentId, String imageId) {
        return "images/" + documentId + "/" + imageId + ".bin";
    }

    private String getPPLKey(String documentId) {
        return "ppl/" + documentId + "/ppl.json";
    }

    private String getOptimizationKey(String documentId, String optimizationType) {
        return "optimizations/" + documentId + "/" + optimizationType + ".json";
    }

    private String getDocumentPrefix(String documentId) {
        return documentId + "/";
    }

    // ========== Chunk Storage ==========

    @Override
    public String saveChunk(String documentId, Chunk chunk) {
        try {
            String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();
            chunk.setId(chunkId);

            String key = getChunkKey(documentId, chunkId);
            byte[] data = objectMapper.writeValueAsBytes(chunk);

            minioClient.putObject(
                PutObjectArgs.builder()
                    .bucket(properties.getBucketName())
                    .object(key)
                    .stream(new ByteArrayInputStream(data), data.length, -1)
                    .contentType("application/json")
                    .build()
            );

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
            // 需要遍历查找（MinIO 不支持按 ID 直接查询）
            // 简化实现：假设知道 documentId
            log.warn("getChunk by ID only is not efficient in MinIO, consider using getChunksByDocument");
            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get chunk: {}", chunkId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<Chunk> getChunksByDocument(String documentId) {
        try {
            String prefix = "chunks/" + documentId + "/";
            List<Chunk> chunks = new ArrayList<>();

            Iterable<Result<Item>> results = minioClient.listObjects(
                ListObjectsArgs.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .recursive(true)
                    .build()
            );

            for (Result<Item> result : results) {
                Item item = result.get();

                try (InputStream stream = minioClient.getObject(
                    GetObjectArgs.builder()
                        .bucket(properties.getBucketName())
                        .object(item.objectName())
                        .build()
                )) {
                    Chunk chunk = objectMapper.readValue(stream, Chunk.class);
                    chunks.add(chunk);
                }
            }

            return chunks;
        } catch (Exception e) {
            log.error("Failed to get chunks for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteChunk(String chunkId) {
        log.warn("deleteChunk by ID only is not efficient in MinIO");
    }

    @Override
    public void deleteChunksByDocument(String documentId) {
        try {
            String prefix = "chunks/" + documentId + "/";

            Iterable<Result<Item>> results = minioClient.listObjects(
                ListObjectsArgs.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .recursive(true)
                    .build()
            );

            for (Result<Item> result : results) {
                Item item = result.get();
                minioClient.removeObject(
                    RemoveObjectArgs.builder()
                        .bucket(properties.getBucketName())
                        .object(item.objectName())
                        .build()
                );
            }

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

            String key = getImageKey(documentId, imageId);
            byte[] data = objectMapper.writeValueAsBytes(image);

            minioClient.putObject(
                PutObjectArgs.builder()
                    .bucket(properties.getBucketName())
                    .object(key)
                    .stream(new ByteArrayInputStream(data), data.length, -1)
                    .contentType("application/octet-stream")
                    .build()
            );

            log.debug("Saved image: {}", imageId);
            return imageId;
        } catch (Exception e) {
            log.error("Failed to save image", e);
            return null;
        }
    }

    @Override
    public Optional<Image> getImage(String imageId) {
        log.warn("getImage by ID only is not efficient in MinIO");
        return Optional.empty();
    }

    @Override
    public List<Image> getImagesByDocument(String documentId) {
        try {
            String prefix = "images/" + documentId + "/";
            List<Image> images = new ArrayList<>();

            Iterable<Result<Item>> results = minioClient.listObjects(
                ListObjectsArgs.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .recursive(true)
                    .build()
            );

            for (Result<Item> result : results) {
                Item item = result.get();

                try (InputStream stream = minioClient.getObject(
                    GetObjectArgs.builder()
                        .bucket(properties.getBucketName())
                        .object(item.objectName())
                        .build()
                )) {
                    Image image = objectMapper.readValue(stream, Image.class);
                    images.add(image);
                }
            }

            return images;
        } catch (Exception e) {
            log.error("Failed to get images for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteImage(String imageId) {
        log.warn("deleteImage by ID only is not efficient in MinIO");
    }

    @Override
    public void deleteImagesByDocument(String documentId) {
        try {
            String prefix = "images/" + documentId + "/";

            Iterable<Result<Item>> results = minioClient.listObjects(
                ListObjectsArgs.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .recursive(true)
                    .build()
            );

            for (Result<Item> result : results) {
                Item item = result.get();
                minioClient.removeObject(
                    RemoveObjectArgs.builder()
                        .bucket(properties.getBucketName())
                        .object(item.objectName())
                        .build()
                );
            }

            log.info("Deleted all images for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete images for document: {}", documentId, e);
        }
    }

    // ========== PPL Data Storage ==========

    @Override
    public String savePPLData(String documentId, PPLData data) {
        try {
            String key = getPPLKey(documentId);
            byte[] jsonData = objectMapper.writeValueAsBytes(data);

            minioClient.putObject(
                PutObjectArgs.builder()
                    .bucket(properties.getBucketName())
                    .object(key)
                    .stream(new ByteArrayInputStream(jsonData), jsonData.length, -1)
                    .contentType("application/json")
                    .build()
            );

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
            String key = getPPLKey(documentId);

            try (InputStream stream = minioClient.getObject(
                GetObjectArgs.builder()
                    .bucket(properties.getBucketName())
                    .object(key)
                    .build()
            )) {
                PPLData data = objectMapper.readValue(stream, PPLData.class);
                return Optional.of(data);
            }
        } catch (Exception e) {
            log.error("Failed to get PPL data for document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deletePPLData(String documentId) {
        try {
            String key = getPPLKey(documentId);

            minioClient.removeObject(
                RemoveObjectArgs.builder()
                    .bucket(properties.getBucketName())
                    .object(key)
                    .build()
            );

            log.info("Deleted PPL data for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete PPL data for document: {}", documentId, e);
        }
    }

    // ========== Optimization Data Storage ==========

    @Override
    public String saveOptimizationData(String documentId, top.yumbo.ai.storage.api.model.OptimizationData data) {
        try {
            String key = getOptimizationKey(documentId, data.getOptimizationType());
            byte[] jsonData = objectMapper.writeValueAsBytes(data);

            minioClient.putObject(
                io.minio.PutObjectArgs.builder()
                    .bucket(properties.getBucketName())
                    .object(key)
                    .stream(new java.io.ByteArrayInputStream(jsonData), jsonData.length, -1)
                    .contentType("application/json")
                    .build()
            );

            log.debug("Saved {} optimization data for document: {}", data.getOptimizationType(), documentId);
            return key;
        } catch (Exception e) {
            log.error("Failed to save optimization data", e);
            return null;
        }
    }

    @Override
    public java.util.Optional<top.yumbo.ai.storage.api.model.OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        try {
            String key = getOptimizationKey(documentId, optimizationType);

            java.io.InputStream stream = minioClient.getObject(
                io.minio.GetObjectArgs.builder()
                    .bucket(properties.getBucketName())
                    .object(key)
                    .build()
            );

            top.yumbo.ai.storage.api.model.OptimizationData optData =
                objectMapper.readValue(stream, top.yumbo.ai.storage.api.model.OptimizationData.class);
            return java.util.Optional.of(optData);
        } catch (io.minio.errors.ErrorResponseException e) {
            if ("NoSuchKey".equals(e.errorResponse().code())) {
                return java.util.Optional.empty();
            }
            log.error("Failed to get {} optimization data for document: {}", optimizationType, documentId, e);
            return java.util.Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get {} optimization data for document: {}", optimizationType, documentId, e);
            return java.util.Optional.empty();
        }
    }

    @Override
    public java.util.List<top.yumbo.ai.storage.api.model.OptimizationData> getAllOptimizationData(String documentId) {
        try {
            String prefix = "optimizations/" + documentId + "/";
            Iterable<io.minio.Result<io.minio.messages.Item>> results = minioClient.listObjects(
                io.minio.ListObjectsArgs.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build()
            );

            java.util.List<top.yumbo.ai.storage.api.model.OptimizationData> dataList = new java.util.ArrayList<>();
            for (io.minio.Result<io.minio.messages.Item> result : results) {
                try {
                    io.minio.messages.Item item = result.get();
                    java.io.InputStream stream = minioClient.getObject(
                        io.minio.GetObjectArgs.builder()
                            .bucket(properties.getBucketName())
                            .object(item.objectName())
                            .build()
                    );
                    top.yumbo.ai.storage.api.model.OptimizationData data =
                        objectMapper.readValue(stream, top.yumbo.ai.storage.api.model.OptimizationData.class);
                    dataList.add(data);
                } catch (Exception e) {
                    log.error("Failed to read optimization data", e);
                }
            }
            return dataList;
        } catch (Exception e) {
            log.error("Failed to get all optimization data for document: {}", documentId, e);
            return new java.util.ArrayList<>();
        }
    }

    @Override
    public void deleteOptimizationData(String documentId, String optimizationType) {
        try {
            String key = getOptimizationKey(documentId, optimizationType);
            minioClient.removeObject(
                io.minio.RemoveObjectArgs.builder()
                    .bucket(properties.getBucketName())
                    .object(key)
                    .build()
            );
            log.info("Deleted {} optimization data for document: {}", optimizationType, documentId);
        } catch (Exception e) {
            log.error("Failed to delete {} optimization data for document: {}", optimizationType, documentId, e);
        }
    }

    @Override
    public void deleteAllOptimizationData(String documentId) {
        try {
            String prefix = "optimizations/" + documentId + "/";
            Iterable<io.minio.Result<io.minio.messages.Item>> results = minioClient.listObjects(
                io.minio.ListObjectsArgs.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build()
            );

            for (io.minio.Result<io.minio.messages.Item> result : results) {
                io.minio.messages.Item item = result.get();
                minioClient.removeObject(
                    io.minio.RemoveObjectArgs.builder()
                        .bucket(properties.getBucketName())
                        .object(item.objectName())
                        .build()
                );
            }
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
        log.info("Cleaned up all data for document: {}", documentId);
    }

    @Override
    public boolean documentExists(String documentId) {
        try {
            // 检查是否有任何对象存在
            Iterable<Result<Item>> results = minioClient.listObjects(
                ListObjectsArgs.builder()
                    .bucket(properties.getBucketName())
                    .prefix(documentId + "/")
                    .maxKeys(1)
                    .build()
            );

            return results.iterator().hasNext();
        } catch (Exception e) {
            log.error("Failed to check document existence: {}", documentId, e);
            return false;
        }
    }

    @Override
    public long getDocumentSize(String documentId) {
        try {
            long totalSize = 0;

            Iterable<Result<Item>> results = minioClient.listObjects(
                ListObjectsArgs.builder()
                    .bucket(properties.getBucketName())
                    .prefix(documentId + "/")
                    .recursive(true)
                    .build()
            );

            for (Result<Item> result : results) {
                Item item = result.get();
                totalSize += item.size();
            }

            return totalSize;
        } catch (Exception e) {
            log.error("Failed to calculate document size for: {}", documentId, e);
            return 0;
        }
    }

    // ========== Statistics ==========

    @Override
    public StorageStatistics getStatistics() {
        try {
            long totalChunks = 0;
            long totalImages = 0;
            long totalPPLData = 0;
            long totalSize = 0;
            Set<String> documentIds = new HashSet<>();

            Iterable<Result<Item>> results = minioClient.listObjects(
                ListObjectsArgs.builder()
                    .bucket(properties.getBucketName())
                    .recursive(true)
                    .build()
            );

            for (Result<Item> result : results) {
                Item item = result.get();
                String objectName = item.objectName();
                totalSize += item.size();

                if (objectName.startsWith("chunks/")) {
                    totalChunks++;
                    String docId = objectName.split("/")[1];
                    documentIds.add(docId);
                } else if (objectName.startsWith("images/")) {
                    totalImages++;
                    String docId = objectName.split("/")[1];
                    documentIds.add(docId);
                } else if (objectName.startsWith("ppl/")) {
                    totalPPLData++;
                    String docId = objectName.split("/")[1];
                    documentIds.add(docId);
                }
            }

            return StorageStatistics.builder()
                    .totalDocuments((long) documentIds.size())
                    .totalChunks(totalChunks)
                    .totalImages(totalImages)
                    .totalPPLData(totalPPLData)
                    .totalSize(totalSize)
                    .storageType("minio")
                    .healthy(isHealthy())
                    .timestamp(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.error("Failed to get statistics", e);
            return StorageStatistics.builder()
                    .storageType("minio")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try {
            minioClient.bucketExists(
                BucketExistsArgs.builder()
                    .bucket(properties.getBucketName())
                    .build()
            );
            return true;
        } catch (Exception e) {
            log.error("Health check failed", e);
            return false;
        }
    }
}

