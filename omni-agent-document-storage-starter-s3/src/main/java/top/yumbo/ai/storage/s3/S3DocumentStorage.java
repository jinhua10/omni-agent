package top.yumbo.ai.storage.s3;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.*;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;
import top.yumbo.ai.storage.api.model.Image;
import top.yumbo.ai.storage.api.model.PPLData;
import top.yumbo.ai.storage.api.model.StorageStatistics;

import java.util.*;
import java.util.stream.Collectors;

/**
 * AWS S3 对象存储实现 - 公有云对象存储方案
 * (AWS S3 Object Storage Implementation - Public Cloud Object Storage)
 *
 * <p>
 * 特点 (Features):
 * - AWS 官方云存储服务
 * - 全球可用，高可靠性（99.999999999%）
 * - 无限扩展能力
 * - 按量付费
 * - 适合全球化大规模部署
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - S3 Starter 实现
 */
@Slf4j
public class S3DocumentStorage implements DocumentStorageService {

    private final S3Client s3Client;
    private final S3StorageProperties properties;
    private final ObjectMapper objectMapper;

    public S3DocumentStorage(S3Client s3Client, S3StorageProperties properties) {
        this.s3Client = s3Client;
        this.properties = properties;
        this.objectMapper = new ObjectMapper();

        ensureBucketExists();
        log.info("S3DocumentStorage initialized - bucket: {}, region: {}",
                properties.getBucketName(), properties.getRegion());
    }

    private void ensureBucketExists() {
        try {
            HeadBucketRequest headBucketRequest = HeadBucketRequest.builder()
                    .bucket(properties.getBucketName())
                    .build();

            s3Client.headBucket(headBucketRequest);
            log.info("Bucket exists: {}", properties.getBucketName());
        } catch (NoSuchBucketException e) {
            try {
                CreateBucketRequest createBucketRequest = CreateBucketRequest.builder()
                        .bucket(properties.getBucketName())
                        .build();

                s3Client.createBucket(createBucketRequest);
                log.info("Created bucket: {}", properties.getBucketName());
            } catch (Exception ex) {
                log.error("Failed to create bucket", ex);
            }
        } catch (Exception e) {
            log.error("Failed to check bucket", e);
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

    // ========== Chunk Storage ==========

    @Override
    public String saveChunk(String documentId, Chunk chunk) {
        try {
            String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();
            chunk.setId(chunkId);

            String key = getChunkKey(documentId, chunkId);
            byte[] data = objectMapper.writeValueAsBytes(chunk);

            PutObjectRequest putObjectRequest = PutObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .contentType("application/json")
                    .build();

            s3Client.putObject(putObjectRequest, RequestBody.fromBytes(data));

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
        log.warn("getChunk by ID only is not efficient in S3");
        return Optional.empty();
    }

    @Override
    public List<Chunk> getChunksByDocument(String documentId) {
        try {
            String prefix = "chunks/" + documentId + "/";
            List<Chunk> chunks = new ArrayList<>();

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            for (S3Object s3Object : listResponse.contents()) {
                try {
                    GetObjectRequest getRequest = GetObjectRequest.builder()
                            .bucket(properties.getBucketName())
                            .key(s3Object.key())
                            .build();

                    byte[] data = s3Client.getObjectAsBytes(getRequest).asByteArray();
                    Chunk chunk = objectMapper.readValue(data, Chunk.class);
                    chunks.add(chunk);
                } catch (Exception e) {
                    log.error("Failed to read chunk: {}", s3Object.key(), e);
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
        log.warn("deleteChunk by ID only is not efficient in S3");
    }

    @Override
    public void deleteChunksByDocument(String documentId) {
        try {
            String prefix = "chunks/" + documentId + "/";

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            for (S3Object s3Object : listResponse.contents()) {
                DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                        .bucket(properties.getBucketName())
                        .key(s3Object.key())
                        .build();

                s3Client.deleteObject(deleteRequest);
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

            PutObjectRequest putObjectRequest = PutObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .contentType("application/octet-stream")
                    .build();

            s3Client.putObject(putObjectRequest, RequestBody.fromBytes(data));

            log.debug("Saved image: {}", imageId);
            return imageId;
        } catch (Exception e) {
            log.error("Failed to save image", e);
            return null;
        }
    }

    @Override
    public Optional<Image> getImage(String imageId) {
        log.warn("getImage by ID only is not efficient in S3");
        return Optional.empty();
    }

    @Override
    public List<Image> getImagesByDocument(String documentId) {
        try {
            String prefix = "images/" + documentId + "/";
            List<Image> images = new ArrayList<>();

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            for (S3Object s3Object : listResponse.contents()) {
                try {
                    GetObjectRequest getRequest = GetObjectRequest.builder()
                            .bucket(properties.getBucketName())
                            .key(s3Object.key())
                            .build();

                    byte[] data = s3Client.getObjectAsBytes(getRequest).asByteArray();
                    Image image = objectMapper.readValue(data, Image.class);
                    images.add(image);
                } catch (Exception e) {
                    log.error("Failed to read image: {}", s3Object.key(), e);
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
        log.warn("deleteImage by ID only is not efficient in S3");
    }

    @Override
    public void deleteImagesByDocument(String documentId) {
        try {
            String prefix = "images/" + documentId + "/";

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            for (S3Object s3Object : listResponse.contents()) {
                DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                        .bucket(properties.getBucketName())
                        .key(s3Object.key())
                        .build();

                s3Client.deleteObject(deleteRequest);
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

            PutObjectRequest putObjectRequest = PutObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .contentType("application/json")
                    .build();

            s3Client.putObject(putObjectRequest, RequestBody.fromBytes(jsonData));

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

            GetObjectRequest getRequest = GetObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            byte[] data = s3Client.getObjectAsBytes(getRequest).asByteArray();
            PPLData pplData = objectMapper.readValue(data, PPLData.class);

            return Optional.of(pplData);
        } catch (NoSuchKeyException e) {
            log.debug("PPL data not found for document: {}", documentId);
            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get PPL data for document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deletePPLData(String documentId) {
        try {
            String key = getPPLKey(documentId);

            DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            s3Client.deleteObject(deleteRequest);

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

            PutObjectRequest putRequest = PutObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .contentType("application/json")
                    .metadata(java.util.Map.of(
                        "documentId", documentId,
                        "optimizationType", data.getOptimizationType()
                    ))
                    .build();

            s3Client.putObject(putRequest, software.amazon.awssdk.core.sync.RequestBody.fromBytes(jsonData));
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
            GetObjectRequest getRequest = GetObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            byte[] data = s3Client.getObjectAsBytes(getRequest).asByteArray();
            top.yumbo.ai.storage.api.model.OptimizationData optData =
                objectMapper.readValue(data, top.yumbo.ai.storage.api.model.OptimizationData.class);
            return java.util.Optional.of(optData);
        } catch (software.amazon.awssdk.services.s3.model.NoSuchKeyException e) {
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
            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            return listResponse.contents().stream()
                    .map(s3Object -> {
                        try {
                            GetObjectRequest getRequest = GetObjectRequest.builder()
                                    .bucket(properties.getBucketName())
                                    .key(s3Object.key())
                                    .build();
                            byte[] data = s3Client.getObjectAsBytes(getRequest).asByteArray();
                            return objectMapper.readValue(data, top.yumbo.ai.storage.api.model.OptimizationData.class);
                        } catch (Exception e) {
                            log.error("Failed to read optimization data", e);
                            return null;
                        }
                    })
                    .filter(java.util.Objects::nonNull)
                    .collect(java.util.stream.Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get all optimization data for document: {}", documentId, e);
            return new java.util.ArrayList<>();
        }
    }

    @Override
    public void deleteOptimizationData(String documentId, String optimizationType) {
        try {
            String key = getOptimizationKey(documentId, optimizationType);
            DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            s3Client.deleteObject(deleteRequest);
            log.info("Deleted {} optimization data for document: {}", optimizationType, documentId);
        } catch (Exception e) {
            log.error("Failed to delete {} optimization data for document: {}", optimizationType, documentId, e);
        }
    }

    @Override
    public void deleteAllOptimizationData(String documentId) {
        try {
            String prefix = "optimizations/" + documentId + "/";
            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            for (software.amazon.awssdk.services.s3.model.S3Object s3Object : listResponse.contents()) {
                DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                        .bucket(properties.getBucketName())
                        .key(s3Object.key())
                        .build();
                s3Client.deleteObject(deleteRequest);
            }
            log.info("Deleted all optimization data for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete all optimization data for document: {}", documentId, e);
        }
    }

    // ========== Raw Document Storage ==========

    private String getDocumentKey(String documentId) {
        return "documents/" + documentId + "/document.bin";
    }

    @Override
    public String saveDocument(String documentId, String filename, byte[] fileData) {
        try {
            String key = getDocumentKey(documentId);

            PutObjectRequest putRequest = PutObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .metadata(Map.of(
                            "documentId", documentId,
                            "filename", filename,
                            "uploadTime", String.valueOf(System.currentTimeMillis())
                    ))
                    .build();

            s3Client.putObject(putRequest, RequestBody.fromBytes(fileData));
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
            String key = getDocumentKey(documentId);

            GetObjectRequest getRequest = GetObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            byte[] data = s3Client.getObjectAsBytes(getRequest).asByteArray();
            return Optional.of(data);
        } catch (NoSuchKeyException e) {
            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deleteDocument(String documentId) {
        try {
            String key = getDocumentKey(documentId);

            DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            s3Client.deleteObject(deleteRequest);
            log.debug("Deleted document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete document: {}", documentId, e);
        }
    }

    // ========== Document Management ==========

    @Override
    public List<top.yumbo.ai.storage.api.model.DocumentMetadata> listAllDocuments() {
        try {
            String prefix = "documents/";
            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            return listResponse.contents().stream()
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
            return listAllDocuments().stream()
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

    private top.yumbo.ai.storage.api.model.DocumentMetadata convertToDocumentMetadata(S3Object s3Object) {
        try {
            String key = s3Object.key();
            String documentId = key.split("/")[1];

            HeadObjectRequest headRequest = HeadObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            HeadObjectResponse headResponse = s3Client.headObject(headRequest);
            Map<String, String> metadata = headResponse.metadata();

            String filename = metadata.getOrDefault("filename", documentId);
            long uploadTime = metadata.containsKey("uploadtime") ?
                    Long.parseLong(metadata.get("uploadtime")) : s3Object.lastModified().toEpochMilli();

            return top.yumbo.ai.storage.api.model.DocumentMetadata.builder()
                    .documentId(documentId)
                    .filename(filename)
                    .fileSize(s3Object.size())
                    .uploadTime(new java.util.Date(uploadTime))
                    .lastModified(java.util.Date.from(s3Object.lastModified()))
                    .build();
        } catch (Exception e) {
            log.error("Failed to convert S3Object to DocumentMetadata: {}", s3Object.key(), e);
            return null;
        }
    }

    @Override
    public void cleanupDocument(String documentId) {
        deleteDocument(documentId);
        deleteChunksByDocument(documentId);
        deleteImagesByDocument(documentId);
        deletePPLData(documentId);
        deleteAllOptimizationData(documentId);
        log.info("Cleaned up all data for document: {}", documentId);
    }

    @Override
    public boolean documentExists(String documentId) {
        try {
            String prefix = documentId + "/";

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .maxKeys(1)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            return !listResponse.contents().isEmpty();
        } catch (Exception e) {
            log.error("Failed to check document existence: {}", documentId, e);
            return false;
        }
    }

    @Override
    public long getDocumentSize(String documentId) {
        try {
            long totalSize = 0;
            String prefix = documentId + "/";

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            for (S3Object s3Object : listResponse.contents()) {
                totalSize += s3Object.size();
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

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            for (S3Object s3Object : listResponse.contents()) {
                String key = s3Object.key();
                totalSize += s3Object.size();

                if (key.startsWith("chunks/")) {
                    totalChunks++;
                    String docId = key.split("/")[1];
                    documentIds.add(docId);
                } else if (key.startsWith("images/")) {
                    totalImages++;
                    String docId = key.split("/")[1];
                    documentIds.add(docId);
                } else if (key.startsWith("ppl/")) {
                    totalPPLData++;
                    String docId = key.split("/")[1];
                    documentIds.add(docId);
                }
            }

            return StorageStatistics.builder()
                    .totalDocuments((long) documentIds.size())
                    .totalChunks(totalChunks)
                    .totalImages(totalImages)
                    .totalPPLData(totalPPLData)
                    .totalSize(totalSize)
                    .storageType("s3")
                    .healthy(isHealthy())
                    .timestamp(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.error("Failed to get statistics", e);
            return StorageStatistics.builder()
                    .storageType("s3")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try {
            HeadBucketRequest headBucketRequest = HeadBucketRequest.builder()
                    .bucket(properties.getBucketName())
                    .build();

            s3Client.headBucket(headBucketRequest);
            return true;
        } catch (Exception e) {
            log.error("Health check failed", e);
            return false;
        }
    }
}

