package top.yumbo.ai.storage.mongodb;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.mongodb.client.gridfs.GridFSBucket;
import com.mongodb.client.gridfs.GridFSBuckets;
import com.mongodb.client.gridfs.model.GridFSFile;
import com.mongodb.client.gridfs.model.GridFSUploadOptions;
import lombok.extern.slf4j.Slf4j;
import org.bson.Document;
import org.bson.types.ObjectId;
import org.springframework.data.mongodb.core.MongoTemplate;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;
import top.yumbo.ai.storage.api.model.Image;
import top.yumbo.ai.storage.api.model.PPLData;
import top.yumbo.ai.storage.api.model.StorageStatistics;

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
            String imageId = image.getId() != null ? image.getId() : UUID.randomUUID().toString();

            Document metadata = new Document()
                    .append("documentId", documentId)
                    .append("imageId", imageId)
                    .append("type", "image")
                    .append("format", image.getFormat());

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
    public String saveOptimizationData(String documentId, top.yumbo.ai.storage.api.model.OptimizationData data) {
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
    public Optional<top.yumbo.ai.storage.api.model.OptimizationData> getOptimizationData(String documentId, String optimizationType) {
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

            top.yumbo.ai.storage.api.model.OptimizationData optData =
                objectMapper.readValue(outputStream.toByteArray(),
                                     top.yumbo.ai.storage.api.model.OptimizationData.class);
            return Optional.of(optData);
        } catch (Exception e) {
            log.error("Failed to get {} optimization data for document: {}", optimizationType, documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<top.yumbo.ai.storage.api.model.OptimizationData> getAllOptimizationData(String documentId) {
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
                                                        top.yumbo.ai.storage.api.model.OptimizationData.class);
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
}

