package top.yumbo.ai.omni.storage.impl.s3;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import software.amazon.awssdk.core.ResponseInputStream;
import software.amazon.awssdk.core.sync.RequestBody;
import software.amazon.awssdk.services.s3.S3Client;
import software.amazon.awssdk.services.s3.model.*;
import top.yumbo.ai.omni.storage.api.model.DocumentMetadata;
import top.yumbo.ai.omni.storage.api.model.OptimizationData;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.model.Chunk;
import top.yumbo.ai.omni.storage.api.model.Image;
import top.yumbo.ai.omni.storage.api.model.PPLData;
import top.yumbo.ai.omni.storage.api.model.StorageStatistics;

import java.io.ByteArrayOutputStream;
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
        // ⭐ 从 imageId 提取简洁文件名
        // imageId 格式: 文档名_p001_i000
        // 提取: p001_i000
        String simpleFilename = imageId;
        int lastUnderscore = imageId.lastIndexOf("_p");
        if (lastUnderscore > 0) {
            simpleFilename = imageId.substring(lastUnderscore + 1); // 去掉 "_"，保留 "p001_i000"
        }
        return "images/" + documentId + "/" + simpleFilename + ".bin";
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
            // ⭐ 强制要求页码信息
            Integer pageNum = image.getPageNumber();
            if (pageNum == null || pageNum <= 0) {
                throw new IllegalArgumentException(
                    String.format("Image must have valid pageNumber (got: %s, documentId: %s). " +
                                "All images must be assigned a page number.",
                                pageNum, documentId));
            }

            // 从 metadata 中获取图片序号和基础文件名
            Integer imageIndex = null;
            String baseName = documentId;  // 默认使用documentId
            if (image.getMetadata() != null) {
                if (image.getMetadata().containsKey("imageIndex")) {
                    imageIndex = ((Number) image.getMetadata().get("imageIndex")).intValue();
                }
                if (image.getMetadata().containsKey("baseName")) {
                    baseName = (String) image.getMetadata().get("baseName");
                }
            }

            // ⭐ 生成简洁的图片ID：baseName_p001_i000
            String imageId = String.format("%s_p%03d_i%03d",
                    baseName, pageNum, imageIndex != null ? imageIndex : 0);
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
    public String saveOptimizationData(String documentId, OptimizationData data) {
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
    public java.util.Optional<OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        try {
            String key = getOptimizationKey(documentId, optimizationType);
            GetObjectRequest getRequest = GetObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            byte[] data = s3Client.getObjectAsBytes(getRequest).asByteArray();
            OptimizationData optData =
                objectMapper.readValue(data, OptimizationData.class);
            return java.util.Optional.of(optData);
        } catch (software.amazon.awssdk.services.s3.model.NoSuchKeyException e) {
            return java.util.Optional.empty();
        } catch (Exception e) {
            log.error("Failed to get {} optimization data for document: {}", optimizationType, documentId, e);
            return java.util.Optional.empty();
        }
    }

    @Override
    public java.util.List<OptimizationData> getAllOptimizationData(String documentId) {
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
                            return objectMapper.readValue(data, OptimizationData.class);
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

    // ========== Extracted Text Storage ⭐ NEW ==========

    @Override
    public String saveExtractedText(String documentId, String text) {
        try {
            String key = "extracted/" + documentId + ".txt";
            byte[] data = text.getBytes(java.nio.charset.StandardCharsets.UTF_8);

            PutObjectRequest putRequest = PutObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .contentType("text/plain; charset=utf-8")
                    .build();

            s3Client.putObject(putRequest,
                software.amazon.awssdk.core.sync.RequestBody.fromBytes(data));

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
            String key = "extracted/" + documentId + ".txt";

            GetObjectRequest getRequest = GetObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            software.amazon.awssdk.core.ResponseInputStream<?> response =
                s3Client.getObject(getRequest);

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            byte[] buffer = new byte[8192];
            int bytesRead;
            while ((bytesRead = response.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
            response.close();

            String text = outputStream.toString(java.nio.charset.StandardCharsets.UTF_8);
            log.debug("✅ Retrieved extracted text: {}, length={}", documentId, text.length());
            return Optional.of(text);
        } catch (Exception e) {
            log.debug("⚠️ Extracted text not found: {}", documentId);
            return Optional.empty();
        }
    }

    @Override
    public void deleteExtractedText(String documentId) {
        try {
            String key = "extracted/" + documentId + ".txt";

            DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(key)
                    .build();

            s3Client.deleteObject(deleteRequest);
            log.debug("🗑️ Deleted extracted text: {}", documentId);
        } catch (Exception e) {
            log.error("❌ Failed to delete extracted text: {}", documentId, e);
        }
    }

    // ========== Document Management ==========

    @Override
    public List<DocumentMetadata> listAllDocuments() {
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
    public List<DocumentMetadata> listDocuments(int offset, int limit) {
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
    public List<DocumentMetadata> searchDocuments(String keyword) {
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

    private DocumentMetadata convertToDocumentMetadata(S3Object s3Object) {
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

            return DocumentMetadata.builder()
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
        deleteExtractedText(documentId);  // ⭐ 新增
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

    // ========== 文件系统浏览实现 (File System Browse Implementation) ==========
    // S3通过对象键(Object Key)实现虚拟文件系统，使用/分隔路径

    @Override
    public List<Map<String, Object>> listFiles(String virtualPath) {
        try {
            List<Map<String, Object>> items = new ArrayList<>();
            String prefix = virtualPath.isEmpty() ? "" : virtualPath + "/";
            Set<String> directories = new HashSet<>();

            // 列出指定前缀的所有对象
            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .delimiter("/")  // 使用分隔符模拟目录结构
                    .build();

            ListObjectsV2Response response = s3Client.listObjectsV2(listRequest);

            // 处理公共前缀（目录）
            for (CommonPrefix commonPrefix : response.commonPrefixes()) {
                String dirPath = commonPrefix.prefix();
                String dirName = dirPath.substring(prefix.length());
                dirName = dirName.endsWith("/") ? dirName.substring(0, dirName.length() - 1) : dirName;

                Map<String, Object> dirItem = new HashMap<>();
                dirItem.put("name", dirName);
                dirItem.put("type", "directory");
                dirItem.put("path", virtualPath.isEmpty() ? dirName : virtualPath + "/" + dirName);
                items.add(dirItem);
            }

            // 处理对象（文件）
            for (S3Object s3Object : response.contents()) {
                String objectKey = s3Object.key();
                // 跳过目录本身
                if (objectKey.equals(prefix)) {
                    continue;
                }

                String fileName = objectKey.substring(prefix.length());

                Map<String, Object> fileItem = new HashMap<>();
                fileItem.put("name", fileName);
                fileItem.put("type", "file");
                fileItem.put("path", objectKey);
                fileItem.put("size", s3Object.size());
                fileItem.put("modified", s3Object.lastModified() != null ?
                        s3Object.lastModified().toEpochMilli() : System.currentTimeMillis());
                items.add(fileItem);
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
            GetObjectRequest getRequest = GetObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(virtualPath)
                    .build();

            ResponseInputStream<GetObjectResponse> response = s3Client.getObject(getRequest);

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            byte[] buffer = new byte[8192];
            int bytesRead;
            while ((bytesRead = response.read(buffer)) != -1) {
                outputStream.write(buffer, 0, bytesRead);
            }
            response.close();

            return outputStream.toByteArray();
        } catch (NoSuchKeyException e) {
            log.warn("文件不存在: {}", virtualPath);
            return null;
        } catch (Exception e) {
            log.error("读取文件失败: {}", virtualPath, e);
            throw new RuntimeException("读取文件失败: " + e.getMessage(), e);
        }
    }

    @Override
    public boolean deleteFile(String virtualPath) {
        try {
            // 删除单个文件或递归删除目录
            String prefix = virtualPath.endsWith("/") ? virtualPath : virtualPath + "/";

            // 先尝试删除作为文件
            try {
                DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                        .bucket(properties.getBucketName())
                        .key(virtualPath)
                        .build();

                s3Client.deleteObject(deleteRequest);
                log.info("✅ 删除文件成功: {}", virtualPath);
                return true;
            } catch (Exception e) {
                // 可能是目录，继续尝试删除目录内容
            }

            // 列出并删除所有匹配的对象
            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response listResponse = s3Client.listObjectsV2(listRequest);

            boolean deleted = false;
            for (S3Object s3Object : listResponse.contents()) {
                DeleteObjectRequest deleteRequest = DeleteObjectRequest.builder()
                        .bucket(properties.getBucketName())
                        .key(s3Object.key())
                        .build();

                s3Client.deleteObject(deleteRequest);
                deleted = true;
            }

            if (deleted) {
                log.info("✅ 删除目录成功: {}", virtualPath);
            }
            return deleted;
        } catch (Exception e) {
            log.error("删除失败: {}", virtualPath, e);
            return false;
        }
    }

    @Override
    public boolean createDirectory(String virtualPath) {
        try {
            // S3中创建"目录"只需上传一个空对象，对象名以/结尾
            String dirPath = virtualPath.endsWith("/") ? virtualPath : virtualPath + "/";

            PutObjectRequest putRequest = PutObjectRequest.builder()
                    .bucket(properties.getBucketName())
                    .key(dirPath)
                    .build();

            s3Client.putObject(putRequest, RequestBody.fromBytes(new byte[0]));

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
            String prefix = virtualPath.isEmpty() ? "" : virtualPath + "/";
            long[] stats = {0, 0, 0}; // [files, folders, size]

            ListObjectsV2Request listRequest = ListObjectsV2Request.builder()
                    .bucket(properties.getBucketName())
                    .prefix(prefix)
                    .build();

            ListObjectsV2Response response;
            String continuationToken = null;

            do {
                if (continuationToken != null) {
                    listRequest = listRequest.toBuilder()
                            .continuationToken(continuationToken)
                            .build();
                }

                response = s3Client.listObjectsV2(listRequest);

                for (S3Object s3Object : response.contents()) {
                    if (s3Object.key().endsWith("/")) {
                        stats[1]++; // 目录
                    } else {
                        stats[0]++; // 文件
                        stats[2] += s3Object.size();
                    }
                }

                continuationToken = response.nextContinuationToken();
            } while (response.isTruncated());

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
