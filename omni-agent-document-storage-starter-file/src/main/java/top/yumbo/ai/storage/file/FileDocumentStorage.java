package top.yumbo.ai.storage.file;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;
import top.yumbo.ai.storage.api.model.Image;
import top.yumbo.ai.storage.api.model.PPLData;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Collectors;

/**
 * File 存储实现 - 本地文件系统存储（用于开发和测试）
 * (File Storage Implementation - Local file system storage for development and testing)
 *
 * <p>
 * 特点 (Features):
 * - 本地文件存储，无需外部依赖
 * - 快速启动，适合开发测试
 * - 目录结构清晰
 * - 支持大文件
 * </p>
 *
 * @author OmniAgent Team
 * @version 1.0.0 - File Starter 实现
 * @since 1.0.0
 */
@Slf4j
public class FileDocumentStorage implements DocumentStorageService {

    private final Path basePath;
    private final Path chunksPath;
    private final Path imagesPath;
    private final Path pplPath;
    private final Path documentsPath;

    public FileDocumentStorage(String baseDirectory) {
        this.basePath = Paths.get(baseDirectory);
        this.chunksPath = basePath.resolve("chunks");
        this.imagesPath = basePath.resolve("images");
        this.pplPath = basePath.resolve("ppl");
        this.documentsPath = basePath.resolve("documents");

        initDirectories();
        log.info("FileDocumentStorage initialized at: {}", basePath.toAbsolutePath());
    }

    private void initDirectories() {
        try {
            Files.createDirectories(chunksPath);
            Files.createDirectories(imagesPath);
            Files.createDirectories(pplPath);
            Files.createDirectories(documentsPath);
        } catch (IOException e) {
            log.error("Failed to create storage directories", e);
            throw new RuntimeException("Failed to initialize file storage", e);
        }
    }

    // ========== Chunk Storage ==========

    @Override
    public String saveChunk(String documentId, Chunk chunk) {
        try {
            Path docChunkDir = chunksPath.resolve(documentId);
            Files.createDirectories(docChunkDir);

            String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();
            Path chunkFile = docChunkDir.resolve(chunkId + ".chunk");

            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(chunkFile.toFile()))) {
                oos.writeObject(chunk);
            }

            log.debug("Saved chunk: {}", chunkId);
            return chunkId;
        } catch (IOException e) {
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
            // Search in all document directories
            List<Path> chunkFiles = Files.walk(chunksPath, 2)
                    .filter(p -> p.toString().endsWith(chunkId + ".chunk"))
                    .collect(Collectors.toList());

            if (chunkFiles.isEmpty()) {
                return Optional.empty();
            }

            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(chunkFiles.get(0).toFile()))) {
                return Optional.of((Chunk) ois.readObject());
            }
        } catch (IOException | ClassNotFoundException e) {
            log.error("Failed to get chunk: {}", chunkId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<Chunk> getChunksByDocument(String documentId) {
        try {
            Path docChunkDir = chunksPath.resolve(documentId);
            if (!Files.exists(docChunkDir)) {
                return new ArrayList<>();
            }

            return Files.list(docChunkDir)
                    .filter(p -> p.toString().endsWith(".chunk"))
                    .map(p -> {
                        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(p.toFile()))) {
                            return (Chunk) ois.readObject();
                        } catch (IOException | ClassNotFoundException e) {
                            log.error("Failed to read chunk file: {}", p, e);
                            return null;
                        }
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            log.error("Failed to get chunks for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteChunk(String chunkId) {
        try {
            Files.walk(chunksPath, 2)
                    .filter(p -> p.toString().endsWith(chunkId + ".chunk"))
                    .forEach(p -> {
                        try {
                            Files.delete(p);
                            log.debug("Deleted chunk: {}", chunkId);
                        } catch (IOException e) {
                            log.error("Failed to delete chunk: {}", chunkId, e);
                        }
                    });
        } catch (IOException e) {
            log.error("Failed to delete chunk: {}", chunkId, e);
        }
    }

    @Override
    public void deleteChunksByDocument(String documentId) {
        try {
            Path docChunkDir = chunksPath.resolve(documentId);
            if (Files.exists(docChunkDir)) {
                Files.walk(docChunkDir)
                        .sorted(Comparator.reverseOrder())
                        .forEach(p -> {
                            try {
                                Files.delete(p);
                            } catch (IOException e) {
                                log.error("Failed to delete: {}", p, e);
                            }
                        });
                log.info("Deleted all chunks for document: {}", documentId);
            }
        } catch (IOException e) {
            log.error("Failed to delete chunks for document: {}", documentId, e);
        }
    }

    // ========== Image Storage ==========

    @Override
    public String saveImage(String documentId, Image image) {
        try {
            Path docImageDir = imagesPath.resolve(documentId);
            Files.createDirectories(docImageDir);

            String imageId = image.getId() != null ? image.getId() : UUID.randomUUID().toString();
            Path imageFile = docImageDir.resolve(imageId + ".img");

            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(imageFile.toFile()))) {
                oos.writeObject(image);
            }

            log.debug("Saved image: {}", imageId);
            return imageId;
        } catch (IOException e) {
            log.error("Failed to save image", e);
            return null;
        }
    }

    @Override
    public Optional<Image> getImage(String imageId) {
        try {
            List<Path> imageFiles = Files.walk(imagesPath, 2)
                    .filter(p -> p.toString().endsWith(imageId + ".img"))
                    .collect(Collectors.toList());

            if (imageFiles.isEmpty()) {
                return Optional.empty();
            }

            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(imageFiles.get(0).toFile()))) {
                return Optional.of((Image) ois.readObject());
            }
        } catch (IOException | ClassNotFoundException e) {
            log.error("Failed to get image: {}", imageId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<Image> getImagesByDocument(String documentId) {
        try {
            Path docImageDir = imagesPath.resolve(documentId);
            if (!Files.exists(docImageDir)) {
                return new ArrayList<>();
            }

            return Files.list(docImageDir)
                    .filter(p -> p.toString().endsWith(".img"))
                    .map(p -> {
                        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(p.toFile()))) {
                            return (Image) ois.readObject();
                        } catch (IOException | ClassNotFoundException e) {
                            log.error("Failed to read image file: {}", p, e);
                            return null;
                        }
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            log.error("Failed to get images for document: {}", documentId, e);
            return new ArrayList<>();
        }
    }

    @Override
    public void deleteImage(String imageId) {
        try {
            Files.walk(imagesPath, 2)
                    .filter(p -> p.toString().endsWith(imageId + ".img"))
                    .forEach(p -> {
                        try {
                            Files.delete(p);
                            log.debug("Deleted image: {}", imageId);
                        } catch (IOException e) {
                            log.error("Failed to delete image: {}", imageId, e);
                        }
                    });
        } catch (IOException e) {
            log.error("Failed to delete image: {}", imageId, e);
        }
    }

    @Override
    public void deleteImagesByDocument(String documentId) {
        try {
            Path docImageDir = imagesPath.resolve(documentId);
            if (Files.exists(docImageDir)) {
                Files.walk(docImageDir)
                        .sorted(Comparator.reverseOrder())
                        .forEach(p -> {
                            try {
                                Files.delete(p);
                            } catch (IOException e) {
                                log.error("Failed to delete: {}", p, e);
                            }
                        });
                log.info("Deleted all images for document: {}", documentId);
            }
        } catch (IOException e) {
            log.error("Failed to delete images for document: {}", documentId, e);
        }
    }

    // ========== PPL Data Storage ==========

    @Override
    public String savePPLData(String documentId, PPLData data) {
        try {
            Path docPplDir = pplPath.resolve(documentId);
            Files.createDirectories(docPplDir);

            Path pplFile = docPplDir.resolve("ppl.data");

            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(pplFile.toFile()))) {
                oos.writeObject(data);
            }

            log.debug("Saved PPL data for document: {}", documentId);
            return documentId;
        } catch (IOException e) {
            log.error("Failed to save PPL data", e);
            return null;
        }
    }

    @Override
    public Optional<PPLData> getPPLData(String documentId) {
        try {
            Path pplFile = pplPath.resolve(documentId).resolve("ppl.data");
            if (!Files.exists(pplFile)) {
                return Optional.empty();
            }

            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(pplFile.toFile()))) {
                return Optional.of((PPLData) ois.readObject());
            }
        } catch (IOException | ClassNotFoundException e) {
            log.error("Failed to get PPL data for document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deletePPLData(String documentId) {
        try {
            Path docPplDir = pplPath.resolve(documentId);
            if (Files.exists(docPplDir)) {
                Files.walk(docPplDir)
                        .sorted(Comparator.reverseOrder())
                        .forEach(p -> {
                            try {
                                Files.delete(p);
                            } catch (IOException e) {
                                log.error("Failed to delete: {}", p, e);
                            }
                        });
                log.info("Deleted PPL data for document: {}", documentId);
            }
        } catch (IOException e) {
            log.error("Failed to delete PPL data for document: {}", documentId, e);
        }
    }

    // ========== Document Management ==========

    @Override
    public void cleanupDocument(String documentId) {
        deleteChunksByDocument(documentId);
        deleteImagesByDocument(documentId);
        deletePPLData(documentId);
        log.info("Cleaned up all data for document: {}", documentId);
    }

    @Override
    public boolean documentExists(String documentId) {
        return Files.exists(chunksPath.resolve(documentId)) ||
               Files.exists(imagesPath.resolve(documentId)) ||
               Files.exists(pplPath.resolve(documentId));
    }

    @Override
    public long getDocumentSize(String documentId) {
        try {
            long size = 0;

            // Calculate chunks size
            Path docChunkDir = chunksPath.resolve(documentId);
            if (Files.exists(docChunkDir)) {
                size += Files.walk(docChunkDir)
                        .filter(Files::isRegularFile)
                        .mapToLong(p -> {
                            try {
                                return Files.size(p);
                            } catch (IOException e) {
                                return 0;
                            }
                        })
                        .sum();
            }

            // Calculate images size
            Path docImageDir = imagesPath.resolve(documentId);
            if (Files.exists(docImageDir)) {
                size += Files.walk(docImageDir)
                        .filter(Files::isRegularFile)
                        .mapToLong(p -> {
                            try {
                                return Files.size(p);
                            } catch (IOException e) {
                                return 0;
                            }
                        })
                        .sum();
            }

            // Calculate PPL data size
            Path docPplDir = pplPath.resolve(documentId);
            if (Files.exists(docPplDir)) {
                size += Files.walk(docPplDir)
                        .filter(Files::isRegularFile)
                        .mapToLong(p -> {
                            try {
                                return Files.size(p);
                            } catch (IOException e) {
                                return 0;
                            }
                        })
                        .sum();
            }

            return size;
        } catch (IOException e) {
            log.error("Failed to calculate document size for: {}", documentId, e);
            return 0;
        }
    }

    // ========== Statistics ==========

    @Override
    public top.yumbo.ai.storage.api.model.StorageStatistics getStatistics() {
        try {
            long totalDocuments = 0;
            long totalChunks = 0;
            long totalImages = 0;
            long totalPPLData = 0;
            long totalSize = 0;

            // Count chunks
            if (Files.exists(chunksPath)) {
                totalDocuments = Files.list(chunksPath)
                        .filter(Files::isDirectory)
                        .count();

                totalChunks = Files.walk(chunksPath)
                        .filter(p -> p.toString().endsWith(".chunk"))
                        .count();

                totalSize += Files.walk(chunksPath)
                        .filter(Files::isRegularFile)
                        .mapToLong(p -> {
                            try {
                                return Files.size(p);
                            } catch (IOException e) {
                                return 0;
                            }
                        })
                        .sum();
            }

            // Count images
            if (Files.exists(imagesPath)) {
                totalImages = Files.walk(imagesPath)
                        .filter(p -> p.toString().endsWith(".img"))
                        .count();

                totalSize += Files.walk(imagesPath)
                        .filter(Files::isRegularFile)
                        .mapToLong(p -> {
                            try {
                                return Files.size(p);
                            } catch (IOException e) {
                                return 0;
                            }
                        })
                        .sum();
            }

            // Count PPL data
            if (Files.exists(pplPath)) {
                totalPPLData = Files.list(pplPath)
                        .filter(Files::isDirectory)
                        .count();

                totalSize += Files.walk(pplPath)
                        .filter(Files::isRegularFile)
                        .mapToLong(p -> {
                            try {
                                return Files.size(p);
                            } catch (IOException e) {
                                return 0;
                            }
                        })
                        .sum();
            }

            return top.yumbo.ai.storage.api.model.StorageStatistics.builder()
                    .totalDocuments(totalDocuments)
                    .totalChunks(totalChunks)
                    .totalImages(totalImages)
                    .totalPPLData(totalPPLData)
                    .totalSize(totalSize)
                    .storageType("file")
                    .healthy(isHealthy())
                    .timestamp(System.currentTimeMillis())
                    .build();
        } catch (IOException e) {
            log.error("Failed to get statistics", e);
            return top.yumbo.ai.storage.api.model.StorageStatistics.builder()
                    .storageType("file")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .build();
        }
    }

    @Override
    public boolean isHealthy() {
        return Files.exists(basePath) && Files.isWritable(basePath);
    }
}

