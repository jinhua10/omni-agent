package top.yumbo.ai.omni.storage.impl.mongodb;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mongodb.client.gridfs.GridFSBucket;
import com.mongodb.client.gridfs.GridFSBuckets;
import com.mongodb.client.gridfs.model.GridFSFile;
import com.mongodb.client.gridfs.model.GridFSUploadOptions;
import lombok.extern.slf4j.Slf4j;
import org.bson.Document;
import org.bson.types.ObjectId;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.omni.storage.api.model.DocumentMetadata;
import top.yumbo.ai.omni.storage.api.model.OptimizationData;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.model.Chunk;
import top.yumbo.ai.omni.storage.api.model.Image;
import top.yumbo.ai.omni.storage.api.model.PPLData;
import top.yumbo.ai.omni.storage.api.model.StorageStatistics;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.*;
import java.util.stream.Collectors;

/**
 * MongoDB GridFS 文档存储实现
 * (MongoDB GridFS Document Storage Implementation)
 *
 * <p>
 * 特点 (Features):
 * - 使用 GridFS 存储大文件
 * - 支持分布式部署
 * - 支持副本集和分片
 * - 适合大规模文档存储
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @version 1.0.0 - MongoDB Starter 实现
 */
@Slf4j
public class MongoDBDocumentStorage implements DocumentStorageService {

    private final MongoTemplate mongoTemplate;
    private final GridFSBucket gridFSBucket;
    private final ObjectMapper objectMapper;

    public MongoDBDocumentStorage(MongoTemplate mongoTemplate, String bucketName) {
        this.mongoTemplate = mongoTemplate;
        this.gridFSBucket = GridFSBuckets.create(mongoTemplate.getDb(), bucketName);
        this.objectMapper = new ObjectMapper();
        log.info("MongoDBDocumentStorage initialized with bucket: {}", bucketName);
    }

    // ========== Raw Document Storage ==========

    @Override
    public String saveDocument(String documentId, String filename, byte[] fileData) {
        try {
            Document metadata = new Document()
                    .append("documentId", documentId)
                    .append("filename", filename)
                    .append("type", "document");

            GridFSUploadOptions options = new GridFSUploadOptions()
                    .metadata(metadata);

            ObjectId fileId = gridFSBucket.uploadFromStream(
                    documentId,
                    new ByteArrayInputStream(fileData),
                    options
            );

            log.debug("Saved document: {} with GridFS ID: {}", documentId, fileId);
            return documentId;
        } catch (Exception e) {
            log.error("Failed to save document: {}", documentId, e);
            return null;
        }
    }

    @Override
    public Optional<byte[]> getDocument(String documentId) {
        try {
            GridFSFile file = gridFSBucket.find(new Document("filename", documentId)).first();
            if (file == null) {
                return Optional.empty();
            }

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            gridFSBucket.downloadToStream(file.getObjectId(), outputStream);
            return Optional.of(outputStream.toByteArray());
        } catch (Exception e) {
            log.error("Failed to get document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deleteDocument(String documentId) {
        try {
            GridFSFile file = gridFSBucket.find(new Document("filename", documentId)).first();
            if (file != null) {
                gridFSBucket.delete(file.getObjectId());
                log.debug("Deleted document: {}", documentId);
            }
        } catch (Exception e) {
            log.error("Failed to delete document: {}", documentId, e);
        }
    }

    // ========== Extracted Text Storage ⭐ NEW ==========

    @Override
    public String saveExtractedText(String documentId, String text) {
        try {
            Document metadata = new Document()
                    .append("documentId", documentId)
                    .append("type", "extracted-text")
                    .append("createdAt", System.currentTimeMillis());

            GridFSUploadOptions options = new GridFSUploadOptions()
                    .metadata(metadata);

            // 删除旧的提取文本（如果存在）
            deleteExtractedText(documentId);

            ObjectId fileId = gridFSBucket.uploadFromStream(
                    "extracted-" + documentId,
                    new ByteArrayInputStream(text.getBytes(java.nio.charset.StandardCharsets.UTF_8)),
                    options
            );

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
            GridFSFile file = gridFSBucket.find(
                    new Document("filename", "extracted-" + documentId)
            ).first();

            if (file == null) {
                log.debug("⚠️ Extracted text not found: {}", documentId);
                return Optional.empty();
            }

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            gridFSBucket.downloadToStream(file.getObjectId(), outputStream);
            String text = outputStream.toString(java.nio.charset.StandardCharsets.UTF_8);

            log.debug("✅ Retrieved extracted text: {}, length={}", documentId, text.length());
            return Optional.of(text);
        } catch (Exception e) {
            log.error("❌ Failed to get extracted text: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deleteExtractedText(String documentId) {
        try {
            GridFSFile file = gridFSBucket.find(
                    new Document("filename", "extracted-" + documentId)
            ).first();

            if (file != null) {
                gridFSBucket.delete(file.getObjectId());
                log.debug("🗑️ Deleted extracted text: {}", documentId);
            }
        } catch (Exception e) {
            log.error("❌ Failed to delete extracted text: {}", documentId, e);
        }
    }

    // ========== Chunk Storage ==========

    @Override
    public String saveChunk(String documentId, Chunk chunk) {
        try {
            String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();

            Document metadata = new Document()
                    .append("documentId", documentId)
                    .append("chunkId", chunkId)
                    .append("type", "chunk")
                    .append("sequence", chunk.getSequence());

            GridFSUploadOptions options = new GridFSUploadOptions()
                    .metadata(metadata);

            byte[] data = objectMapper.writeValueAsBytes(chunk);
            ObjectId fileId = gridFSBucket.uploadFromStream(
                    chunkId,
                    new ByteArrayInputStream(data),
                    options
            );

            log.debug("Saved chunk: {} with GridFS ID: {}", chunkId, fileId);
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
            GridFSFile file = gridFSBucket.find(new Document("filename", chunkId)).first();
            if (file == null) {
                return Optional.empty();
            }

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            gridFSBucket.downloadToStream(file.getObjectId(), outputStream);

            Chunk chunk = objectMapper.readValue(outputStream.toByteArray(), Chunk.class);
            return Optional.of(chunk);
        } catch (Exception e) {
            log.error("Failed to get chunk: {}", chunkId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<Chunk> getChunksByDocument(String documentId) {
        try {
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.documentId", documentId)
                            .append("metadata.type", "chunk")
            ).into(new ArrayList<>());

            return files.stream()
                    .map(file -> {
                        try {
                            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                            gridFSBucket.downloadToStream(file.getObjectId(), outputStream);
                            return objectMapper.readValue(outputStream.toByteArray(), Chunk.class);
                        } catch (Exception e) {
                            log.error("Failed to read chunk file", e);
                            return null;
                        }
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get chunks for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteChunk(String chunkId) {
        try {
            GridFSFile file = gridFSBucket.find(new Document("filename", chunkId)).first();
            if (file != null) {
                gridFSBucket.delete(file.getObjectId());
                log.debug("Deleted chunk: {}", chunkId);
            }
        } catch (Exception e) {
            log.error("Failed to delete chunk: {}", chunkId, e);
        }
    }

    @Override
    public void deleteChunksByDocument(String documentId) {
        try {
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.documentId", documentId)
                            .append("metadata.type", "chunk")
            ).into(new ArrayList<>());

            for (GridFSFile file : files) {
                gridFSBucket.delete(file.getObjectId());
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

            Document metadata = new Document()
                    .append("documentId", documentId)
                    .append("imageId", imageId)
                    .append("type", "image")
                    .append("format", image.getFormat())
                    .append("pageNumber", pageNum)
                    .append("baseName", baseName);

            if (imageIndex != null) {
                metadata.append("imageIndex", imageIndex);
            }

            GridFSUploadOptions options = new GridFSUploadOptions()
                    .metadata(metadata);

            byte[] data = objectMapper.writeValueAsBytes(image);
            ObjectId fileId = gridFSBucket.uploadFromStream(
                    imageId,
                    new ByteArrayInputStream(data),
                    options
            );

            log.debug("Saved image: {} with GridFS ID: {}", imageId, fileId);
            return imageId;
        } catch (Exception e) {
            log.error("Failed to save image", e);
            return null;
        }
    }

    @Override
    public Optional<Image> getImage(String imageId) {
        try {
            GridFSFile file = gridFSBucket.find(new Document("filename", imageId)).first();
            if (file == null) {
                return Optional.empty();
            }

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            gridFSBucket.downloadToStream(file.getObjectId(), outputStream);

            Image image = objectMapper.readValue(outputStream.toByteArray(), Image.class);
            return Optional.of(image);
        } catch (Exception e) {
            log.error("Failed to get image: {}", imageId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<Image> getImagesByDocument(String documentId) {
        try {
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.documentId", documentId)
                            .append("metadata.type", "image")
            ).into(new ArrayList<>());

            return files.stream()
                    .map(file -> {
                        try {
                            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                            gridFSBucket.downloadToStream(file.getObjectId(), outputStream);
                            return objectMapper.readValue(outputStream.toByteArray(), Image.class);
                        } catch (Exception e) {
                            log.error("Failed to read image file", e);
                            return null;
                        }
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get images for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteImage(String imageId) {
        try {
            GridFSFile file = gridFSBucket.find(new Document("filename", imageId)).first();
            if (file != null) {
                gridFSBucket.delete(file.getObjectId());
                log.debug("Deleted image: {}", imageId);
            }
        } catch (Exception e) {
            log.error("Failed to delete image: {}", imageId, e);
        }
    }

    @Override
    public void deleteImagesByDocument(String documentId) {
        try {
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.documentId", documentId)
                            .append("metadata.type", "image")
            ).into(new ArrayList<>());

            for (GridFSFile file : files) {
                gridFSBucket.delete(file.getObjectId());
            }
            log.info("Deleted all images for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete images for document: {}", documentId, e);
        }
    }

    /**
     * 通过哈希值查找图片（用于去重）⭐ NEW
     */
    @Override
    public Optional<String> findImageByHash(String imageHash) {
        try {
            // 查找 metadata.imageHash 匹配的文件
            GridFSFile file = gridFSBucket.find(
                    new Document("metadata.type", "image")
                            .append("metadata.imageHash", imageHash)
            ).first();

            if (file != null && file.getMetadata() != null) {
                String imageId = file.getMetadata().getString("imageId");
                if (imageId != null) {
                    log.debug("🔍 找到重复图片: hash={}, imageId={}",
                            imageHash.substring(0, Math.min(16, imageHash.length())), imageId);
                    return Optional.of(imageId);
                }
            }

            return Optional.empty();
        } catch (Exception e) {
            log.error("Failed to find image by hash", e);
            return Optional.empty();
        }
    }

    // ========== PPL Data Storage ==========

    @Override
    public String savePPLData(String documentId, PPLData data) {
        try {
            Document metadata = new Document()
                    .append("documentId", documentId)
                    .append("type", "ppl");

            GridFSUploadOptions options = new GridFSUploadOptions()
                    .metadata(metadata);

            byte[] jsonData = objectMapper.writeValueAsBytes(data);
            ObjectId fileId = gridFSBucket.uploadFromStream(
                    documentId + "_ppl",
                    new ByteArrayInputStream(jsonData),
                    options
            );

            log.debug("Saved PPL data for document: {} with GridFS ID: {}", documentId, fileId);
            return documentId;
        } catch (Exception e) {
            log.error("Failed to save PPL data", e);
            return null;
        }
    }

    @Override
    public Optional<PPLData> getPPLData(String documentId) {
        try {
            GridFSFile file = gridFSBucket.find(
                    new Document("filename", documentId + "_ppl")
            ).first();

            if (file == null) {
                return Optional.empty();
            }

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            gridFSBucket.downloadToStream(file.getObjectId(), outputStream);

            PPLData pplData = objectMapper.readValue(outputStream.toByteArray(), PPLData.class);
            return Optional.of(pplData);
        } catch (Exception e) {
            log.error("Failed to get PPL data for document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deletePPLData(String documentId) {
        try {
            GridFSFile file = gridFSBucket.find(
                    new Document("filename", documentId + "_ppl")
            ).first();

            if (file != null) {
                gridFSBucket.delete(file.getObjectId());
                log.info("Deleted PPL data for document: {}", documentId);
            }
        } catch (Exception e) {
            log.error("Failed to delete PPL data for document: {}", documentId, e);
        }
    }

    // ========== Optimization Data Storage ==========

    @Override
    public String saveOptimizationData(String documentId, OptimizationData data) {
        try {
            String filename = documentId + "_opt_" + data.getOptimizationType();

            Document metadata = new Document()
                    .append("documentId", documentId)
                    .append("optimizationType", data.getOptimizationType())
                    .append("type", "optimization");

            GridFSUploadOptions options = new GridFSUploadOptions()
                    .metadata(metadata);

            byte[] jsonData = objectMapper.writeValueAsBytes(data);
            ObjectId fileId = gridFSBucket.uploadFromStream(
                    filename,
                    new ByteArrayInputStream(jsonData),
                    options
            );

            log.debug("Saved {} optimization data for document: {} with GridFS ID: {}",
                     data.getOptimizationType(), documentId, fileId);
            return documentId + ":" + data.getOptimizationType();
        } catch (Exception e) {
            log.error("Failed to save optimization data", e);
            return null;
        }
    }

    @Override
    public Optional<OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        try {
            String filename = documentId + "_opt_" + optimizationType;
            GridFSFile file = gridFSBucket.find(
                    new Document("filename", filename)
            ).first();

            if (file == null) {
                return Optional.empty();
            }

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            gridFSBucket.downloadToStream(file.getObjectId(), outputStream);

            OptimizationData optData =
                objectMapper.readValue(outputStream.toByteArray(),
                                     OptimizationData.class);
            return Optional.of(optData);
        } catch (Exception e) {
            log.error("Failed to get {} optimization data for document: {}", optimizationType, documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<OptimizationData> getAllOptimizationData(String documentId) {
        try {
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.documentId", documentId)
                            .append("metadata.type", "optimization")
            ).into(new ArrayList<>());

            return files.stream()
                    .map(file -> {
                        try {
                            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                            gridFSBucket.downloadToStream(file.getObjectId(), outputStream);
                            return objectMapper.readValue(outputStream.toByteArray(),
                                                        OptimizationData.class);
                        } catch (Exception e) {
                            log.error("Failed to read optimization data file", e);
                            return null;
                        }
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to get all optimization data for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteOptimizationData(String documentId, String optimizationType) {
        try {
            String filename = documentId + "_opt_" + optimizationType;
            GridFSFile file = gridFSBucket.find(
                    new Document("filename", filename)
            ).first();

            if (file != null) {
                gridFSBucket.delete(file.getObjectId());
                log.info("Deleted {} optimization data for document: {}", optimizationType, documentId);
            }
        } catch (Exception e) {
            log.error("Failed to delete {} optimization data for document: {}", optimizationType, documentId, e);
        }
    }

    @Override
    public void deleteAllOptimizationData(String documentId) {
        try {
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.documentId", documentId)
                            .append("metadata.type", "optimization")
            ).into(new ArrayList<>());

            for (GridFSFile file : files) {
                gridFSBucket.delete(file.getObjectId());
            }
            log.info("Deleted all optimization data for document: {}", documentId);
        } catch (Exception e) {
            log.error("Failed to delete all optimization data for document: {}", documentId, e);
        }
    }

    // ========== Document Management ==========

    @Override
    public List<DocumentMetadata> listAllDocuments() {
        try {
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.type", "document")
            ).into(new ArrayList<>());

            return files.stream()
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
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.type", "document")
            ).skip(offset).limit(limit).into(new ArrayList<>());

            return files.stream()
                    .map(this::convertToDocumentMetadata)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to list documents with pagination", e);
            return new ArrayList<>();
        }
    }

    @Override
    public List<DocumentMetadata> searchDocuments(String keyword) {
        try {
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.type", "document")
            ).into(new ArrayList<>());

            return files.stream()
                    .filter(file -> {
                        String filename = file.getMetadata() != null ?
                                file.getMetadata().getString("filename") : "";
                        return filename != null && filename.contains(keyword);
                    })
                    .map(this::convertToDocumentMetadata)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("Failed to search documents with keyword: {}", keyword, e);
            return new ArrayList<>();
        }
    }

    @Override
    public long getDocumentCount() {
        try {
            return gridFSBucket.find(
                    new Document("metadata.type", "document")
            ).into(new ArrayList<>()).size();
        } catch (Exception e) {
            log.error("Failed to get document count", e);
            return 0;
        }
    }

    private DocumentMetadata convertToDocumentMetadata(GridFSFile file) {
        try {
            Document metadata = file.getMetadata();
            if (metadata == null) {
                return null;
            }

            return DocumentMetadata.builder()
                    .documentId(metadata.getString("documentId"))
                    .filename(metadata.getString("filename"))
                    .fileSize(file.getLength())
                    .uploadTime(file.getUploadDate())
                    .lastModified(file.getUploadDate())
                    .build();
        } catch (Exception e) {
            log.error("Failed to convert GridFSFile to DocumentMetadata", e);
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
            GridFSFile file = gridFSBucket.find(
                    new Document("metadata.documentId", documentId)
            ).first();
            return file != null;
        } catch (Exception e) {
            log.error("Failed to check document existence: {}", documentId, e);
            return false;
        }
    }

    @Override
    public long getDocumentSize(String documentId) {
        try {
            List<GridFSFile> files = gridFSBucket.find(
                    new Document("metadata.documentId", documentId)
            ).into(new ArrayList<>());

            return files.stream()
                    .mapToLong(GridFSFile::getLength)
                    .sum();
        } catch (Exception e) {
            log.error("Failed to calculate document size for: {}", documentId, e);
            return 0;
        }
    }

    // ========== Statistics ==========

    @Override
    public StorageStatistics getStatistics() {
        try {
            // 统计不同类型的文件
            long totalChunks = gridFSBucket.find(
                    new Document("metadata.type", "chunk")
            ).into(new ArrayList<>()).size();

            long totalImages = gridFSBucket.find(
                    new Document("metadata.type", "image")
            ).into(new ArrayList<>()).size();

            long totalPPLData = gridFSBucket.find(
                    new Document("metadata.type", "ppl")
            ).into(new ArrayList<>()).size();

            // 统计总大小
            List<GridFSFile> allFiles = gridFSBucket.find(new Document()).into(new ArrayList<>());
            long totalSize = allFiles.stream()
                    .mapToLong(GridFSFile::getLength)
                    .sum();

            // 统计文档数（通过 documentId 去重）
            Set<String> documentIds = allFiles.stream()
                    .filter(file -> file.getMetadata() != null)
                    .map(file -> file.getMetadata().getString("documentId"))
                    .filter(Objects::nonNull)
                    .collect(Collectors.toSet());

            return StorageStatistics.builder()
                    .totalDocuments(documentIds.size())
                    .totalChunks(totalChunks)
                    .totalImages(totalImages)
                    .totalPPLData(totalPPLData)
                    .totalSize(totalSize)
                    .storageType("mongodb-gridfs")
                    .healthy(isHealthy())
                    .timestamp(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.error("Failed to get statistics", e);
            return StorageStatistics.builder()
                    .storageType("mongodb-gridfs")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try {
            // 测试 MongoDB 连接
            mongoTemplate.getDb().listCollectionNames().first();
            return true;
        } catch (Exception e) {
            log.error("Health check failed", e);
            return false;
        }
    }

    // ========== 文件系统浏览实现 (File System Browse Implementation) ==========
    // MongoDB通过GridFS和文档的path字段实现虚拟文件系统

    @Override
    public List<Map<String, Object>> listFiles(String virtualPath) {
        try {
            List<Map<String, Object>> items = new ArrayList<>();
            String searchPath = virtualPath.isEmpty() ? "" : virtualPath + "/";

            // 使用GridFSBucket查询文件
            Document query = new Document();
            if (!searchPath.isEmpty()) {
                query.append("metadata.path", new Document("$regex", "^" + searchPath));
            }

            List<GridFSFile> files = gridFSBucket.find(query).into(new ArrayList<>());
            Set<String> directories = new HashSet<>();

            for (GridFSFile gridFSFile : files) {
                Document metadata = gridFSFile.getMetadata();
                String path = metadata != null ? metadata.getString("path") : "";

                if (path != null && path.startsWith(searchPath)) {
                    String relativePath = path.substring(searchPath.length());
                    int slashIndex = relativePath.indexOf('/');

                    if (slashIndex > 0) {
                        // 子目录
                        String dirName = relativePath.substring(0, slashIndex);
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
                        Map<String, Object> fileItem = new HashMap<>();
                        fileItem.put("name", relativePath);
                        fileItem.put("type", "file");
                        fileItem.put("path", path);
                        fileItem.put("size", gridFSFile.getLength());
                        fileItem.put("modified", gridFSFile.getUploadDate() != null ?
                            gridFSFile.getUploadDate().getTime() : System.currentTimeMillis());
                        items.add(fileItem);
                    }
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
            Document query = new Document("metadata.path", virtualPath);
            GridFSFile gridFSFile = gridFSBucket.find(query).first();

            if (gridFSFile == null) {
                log.warn("文件不存在: {}", virtualPath);
                return null;
            }

            ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
            gridFSBucket.downloadToStream(gridFSFile.getObjectId(), outputStream);
            return outputStream.toByteArray();
        } catch (Exception e) {
            log.error("读取文件失败: {}", virtualPath, e);
            throw new RuntimeException("读取文件失败: " + e.getMessage(), e);
        }
    }

    @Override
    public boolean deleteFile(String virtualPath) {
        try {
            // 删除文件或整个目录
            Document query = new Document("metadata.path", new Document("$regex", "^" + virtualPath));

            List<GridFSFile> files = gridFSBucket.find(query).into(new ArrayList<>());
            for (GridFSFile file : files) {
                gridFSBucket.delete(file.getObjectId());
            }

            log.info("✅ 删除成功: {}", virtualPath);
            return !files.isEmpty();
        } catch (Exception e) {
            log.error("删除失败: {}", virtualPath, e);
            return false;
        }
    }

    @Override
    public boolean createDirectory(String virtualPath) {
        try {
            // MongoDB GridFS不需要显式创建目录
            // 创建一个标记文档
            Document metadata = new Document();
            metadata.put("path", virtualPath);
            metadata.put("type", "directory");
            metadata.put("created", System.currentTimeMillis());

            GridFSUploadOptions options = new GridFSUploadOptions().metadata(metadata);

            gridFSBucket.uploadFromStream(
                virtualPath + "/.dir",
                new ByteArrayInputStream(new byte[0]),
                options
            );

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
            String searchPath = virtualPath.isEmpty() ? "" : virtualPath + "/";
            Document query = new Document();

            if (!searchPath.isEmpty()) {
                query.append("metadata.path", new Document("$regex", "^" + searchPath));
            }

            long[] stats = {0, 0, 0}; // [files, folders, size]

            List<GridFSFile> files = gridFSBucket.find(query).into(new ArrayList<>());
            for (GridFSFile gridFSFile : files) {
                Document metadata = gridFSFile.getMetadata();
                String type = metadata != null ? metadata.getString("type") : "file";

                if ("directory".equals(type)) {
                    stats[1]++;
                } else {
                    stats[0]++;
                    stats[2] += gridFSFile.getLength();
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
