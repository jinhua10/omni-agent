package top.yumbo.ai.storage.file;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.Chunk;
import top.yumbo.ai.storage.api.model.Image;
import top.yumbo.ai.storage.api.model.PPLData;
import top.yumbo.ai.storage.api.model.OptimizationData;

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
    private final Path optimizationPath;  // 新增：优化数据路径
    private final Path documentsPath;

    public FileDocumentStorage(String baseDirectory) {
        this.basePath = Paths.get(baseDirectory);
        this.chunksPath = basePath.resolve("chunks");
        this.imagesPath = basePath.resolve("images");
        this.pplPath = basePath.resolve("ppl");
        this.optimizationPath = basePath.resolve("optimization");  // 新增
        this.documentsPath = basePath.resolve("documents");

        initDirectories();
        log.info("FileDocumentStorage initialized at: {}", basePath.toAbsolutePath());
    }

    private void initDirectories() {
        try {
            Files.createDirectories(chunksPath);
            Files.createDirectories(imagesPath);
            Files.createDirectories(pplPath);
            Files.createDirectories(optimizationPath);  // 新增
            Files.createDirectories(documentsPath);
        } catch (IOException e) {
            log.error("Failed to create storage directories", e);
            throw new RuntimeException("Failed to initialize file storage", e);
        }
    }

    // ========== Raw Document Storage ==========

    @Override
    public String saveDocument(String documentId, String filename, byte[] fileData) {
        try {
            // 使用原文件名直接保存（保留相对路径中的目录结构）⭐
            // 例如: filename = "设计图/架构图.pptx"
            //      保存为: documents/设计图/架构图.pptx
            Path documentFile = documentsPath.resolve(filename);

            // 确保父目录存在
            Path parentDir = documentFile.getParent();
            if (parentDir != null) {
                Files.createDirectories(parentDir);
            }

            Files.write(documentFile, fileData);

            log.debug("Saved document: {}", filename);
            return documentId;
        } catch (IOException e) {
            log.error("Failed to save document: {}", filename, e);
            return null;
        }
    }

    @Override
    public Optional<byte[]> getDocument(String documentId) {
        try {
            // 由于使用原文件名直接存储，需要通过其他方式查找
            // 这里简化处理：遍历查找
            Path[] files = Files.walk(documentsPath, 10)
                    .filter(Files::isRegularFile)
                    .toArray(Path[]::new);

            if (files.length > 0) {
                // 返回第一个找到的文件（需要更好的查找机制）
                byte[] data = Files.readAllBytes(files[0]);
                return Optional.of(data);
            }
            return Optional.empty();
        } catch (IOException e) {
            log.error("Failed to get document: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public void deleteDocument(String documentId) {
        try {
            // 遍历删除（需要更好的删除机制）
            Files.walk(documentsPath, 10)
                    .filter(Files::isRegularFile)
                    .forEach(p -> {
                        try {
                            Files.delete(p);
                            log.debug("Deleted document file: {}", p);
                        } catch (IOException e) {
                            log.error("Failed to delete: {}", p, e);
                        }
                    });
        } catch (IOException e) {
            log.error("Failed to delete document: {}", documentId, e);
        }
    }

    // ========== Chunk Storage ==========

    @Override
    public String saveChunk(String documentId, Chunk chunk) {
        try {
            // documentId 格式: doc_timestamp_filename
            // 提取原文件名（移除前缀）
            String filename = extractFilenameFromDocumentId(documentId);
            Path docChunkDir = chunksPath.resolve(filename);
            Files.createDirectories(docChunkDir);

            // 使用有意义的文件名：chunk_序号（无后缀）⭐
            String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();
            int sequence = chunk.getSequence();
            String chunkFilename = String.format("chunk_%03d", sequence);  // 去掉.chunk后缀
            Path chunkFile = docChunkDir.resolve(chunkFilename);

            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(chunkFile.toFile()))) {
                oos.writeObject(chunk);
            }

            log.debug("Saved chunk: {} -> {}/{}", chunkId, filename, chunkFilename);
            return chunkId;
        } catch (IOException e) {
            log.error("Failed to save chunk", e);
            return null;
        }
    }

    /**
     * 从 documentId 提取原文件名
     * documentId 格式: doc_timestamp_filename
     */
    private String extractFilenameFromDocumentId(String documentId) {
        // doc_1766142807149_倡导节约用水.pptx -> 倡导节约用水.pptx
        if (documentId.startsWith("doc_")) {
            int secondUnderscore = documentId.indexOf('_', 4);
            if (secondUnderscore > 0 && secondUnderscore < documentId.length() - 1) {
                return documentId.substring(secondUnderscore + 1);
            }
        }
        return documentId; // 降级：直接使用
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
            // 在所有文档的分块目录中搜索
            List<Path> chunkFiles = Files.walk(chunksPath, 2)
                    .filter(Files::isRegularFile)
                    .filter(p -> {
                        String name = p.getFileName().toString();
                        // 匹配 chunk_xxx 格式（无后缀）⭐
                        return name.startsWith("chunk_");
                    })
                    .collect(Collectors.toList());

            if (chunkFiles.isEmpty()) {
                return Optional.empty();
            }

            // 如果找到多个，尝试精确匹配chunkId
            for (Path chunkFile : chunkFiles) {
                try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(chunkFile.toFile()))) {
                    Chunk chunk = (Chunk) ois.readObject();
                    if (chunkId.equals(chunk.getId())) {
                        return Optional.of(chunk);
                    }
                } catch (IOException | ClassNotFoundException e) {
                    log.error("Failed to read chunk file: {}", chunkFile, e);
                }
            }

            return Optional.empty();
        } catch (IOException e) {
            log.error("Failed to get chunk: {}", chunkId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<Chunk> getChunksByDocument(String documentId) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path docChunkDir = chunksPath.resolve(filename);
            if (!Files.exists(docChunkDir)) {
                return new ArrayList<>();
            }

            return Files.list(docChunkDir)
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().startsWith("chunk_"))  // 匹配chunk_开头（无后缀）⭐
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
            String filename = extractFilenameFromDocumentId(documentId);
            Path docChunkDir = chunksPath.resolve(filename);
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
                log.info("Deleted all chunks for document: {}", filename);
            }
        } catch (IOException e) {
            log.error("Failed to delete chunks for document: {}", documentId, e);
        }
    }

    // ========== Image Storage ==========

    @Override
    public String saveImage(String documentId, Image image) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path docImageDir = imagesPath.resolve(filename);
            Files.createDirectories(docImageDir);

            // 使用有意义的文件名：page_页码_img（无额外后缀）⭐
            String imageId = image.getId() != null ? image.getId() : UUID.randomUUID().toString();

            // 构建文件名
            String imageFilename;
            if (image.getPageNumber() != null && image.getPageNumber() > 0) {
                // 如果有页码信息，使用 page_001_img 格式（保留原始格式如.png）
                int pageNum = image.getPageNumber();
                String format = image.getFormat() != null ? image.getFormat() : "img";
                imageFilename = String.format("page_%03d_img.%s", pageNum, format);
            } else {
                // 如果没有页码，使用 image_xxx 格式
                String format = image.getFormat() != null ? image.getFormat() : "img";
                imageFilename = String.format("image_%s.%s", imageId.substring(0, Math.min(8, imageId.length())), format);
            }

            Path imageFile = docImageDir.resolve(imageFilename);

            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(imageFile.toFile()))) {
                oos.writeObject(image);
            }

            log.debug("Saved image: {} -> {}/{}", imageId, filename, imageFilename);
            return imageId;
        } catch (IOException e) {
            log.error("Failed to save image", e);
            return null;
        }
    }

    @Override
    public Optional<Image> getImage(String imageId) {
        try {
            // 在所有文档的图片目录中搜索
            List<Path> imageFiles = Files.walk(imagesPath, 2)
                    .filter(Files::isRegularFile)
                    .filter(p -> {
                        String name = p.getFileName().toString();
                        // 匹配 page_xxx_img.xxx 或 image_xxx.xxx 或包含imageId的文件
                        return name.startsWith("page_") || name.startsWith("image_") || name.contains(imageId);
                    })
                    .toList();

            if (imageFiles.isEmpty()) {
                return Optional.empty();
            }

            // 如果找到多个，尝试精确匹配imageId
            for (Path imageFile : imageFiles) {
                try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(imageFile.toFile()))) {
                    Image img = (Image) ois.readObject();
                    if (imageId.equals(img.getId())) {
                        return Optional.of(img);
                    }
                } catch (IOException | ClassNotFoundException e) {
                    log.error("Failed to read image file: {}", imageFile, e);
                }
            }

            return Optional.empty();
        } catch (IOException e) {
            log.error("Failed to get image: {}", imageId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<Image> getImagesByDocument(String documentId) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path docImageDir = imagesPath.resolve(filename);
            if (!Files.exists(docImageDir)) {
                return new ArrayList<>();
            }

            return Files.list(docImageDir)
                    .filter(Files::isRegularFile)
                    .filter(p -> {
                        String name = p.getFileName().toString();
                        // 匹配所有图片文件
                        return name.startsWith("page_") || name.startsWith("image_");
                    })
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
            String filename = extractFilenameFromDocumentId(documentId);
            Path docImageDir = imagesPath.resolve(filename);
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
                log.info("Deleted all images for document: {}", filename);
            }
        } catch (IOException e) {
            log.error("Failed to delete images for document: {}", documentId, e);
        }
    }

    // ========== PPL Data Storage ==========

    @Override
    public String savePPLData(String documentId, PPLData data) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path docPplDir = pplPath.resolve(filename);
            Files.createDirectories(docPplDir);

            Path pplFile = docPplDir.resolve("ppl");  // 无后缀 ⭐

            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(pplFile.toFile()))) {
                oos.writeObject(data);
            }

            log.debug("Saved PPL data for document: {}", filename);
            return documentId;
        } catch (IOException e) {
            log.error("Failed to save PPL data", e);
            return null;
        }
    }

    @Override
    public Optional<PPLData> getPPLData(String documentId) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path pplFile = pplPath.resolve(filename).resolve("ppl");  // 无后缀 ⭐
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
            String filename = extractFilenameFromDocumentId(documentId);
            Path docPplDir = pplPath.resolve(filename);
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
                log.info("Deleted PPL data for document: {}", filename);
            }
        } catch (IOException e) {
            log.error("Failed to delete PPL data for document: {}", documentId, e);
        }
    }

    // ========== Optimization Data Storage (通用RAG优化数据) ==========

    @Override
    public String saveOptimizationData(String documentId, OptimizationData data) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path docOptDir = optimizationPath.resolve(filename);
            Files.createDirectories(docOptDir);

            // 使用优化类型作为文件名（无后缀）⭐
            String fileName = data.getOptimizationType();
            Path optFile = docOptDir.resolve(fileName);

            try (ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(optFile.toFile()))) {
                oos.writeObject(data);
            }

            log.debug("Saved {} optimization data for document: {}", data.getOptimizationType(), filename);
            return documentId + ":" + data.getOptimizationType();
        } catch (IOException e) {
            log.error("Failed to save optimization data", e);
            return null;
        }
    }

    @Override
    public Optional<OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path optFile = optimizationPath.resolve(filename).resolve(optimizationType);  // 无后缀 ⭐
            if (!Files.exists(optFile)) {
                return Optional.empty();
            }

            try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(optFile.toFile()))) {
                return Optional.of((OptimizationData) ois.readObject());
            }
        } catch (IOException | ClassNotFoundException e) {
            log.error("Failed to get {} optimization data for document: {}", optimizationType, documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<OptimizationData> getAllOptimizationData(String documentId) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path docOptDir = optimizationPath.resolve(filename);
            if (!Files.exists(docOptDir)) {
                return Collections.emptyList();
            }

            return Files.list(docOptDir)
                    .filter(Files::isRegularFile)  // 所有文件都是优化数据（无后缀限制）⭐
                    .map(p -> {
                        try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(p.toFile()))) {
                            return (OptimizationData) ois.readObject();
                        } catch (IOException | ClassNotFoundException e) {
                            log.error("Failed to read optimization file: {}", p, e);
                            return null;
                        }
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            log.error("Failed to list optimization data for document: {}", documentId, e);
            return Collections.emptyList();
        }
    }

    @Override
    public void deleteOptimizationData(String documentId, String optimizationType) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path optFile = optimizationPath.resolve(filename).resolve(optimizationType);  // 无后缀 ⭐
            if (Files.exists(optFile)) {
                Files.delete(optFile);
                log.info("Deleted {} optimization data for document: {}", optimizationType, filename);
            }
        } catch (IOException e) {
            log.error("Failed to delete {} optimization data for document: {}", optimizationType, documentId, e);
        }
    }

    @Override
    public void deleteAllOptimizationData(String documentId) {
        try {
            String filename = extractFilenameFromDocumentId(documentId);
            Path docOptDir = optimizationPath.resolve(filename);
            if (Files.exists(docOptDir)) {
                Files.walk(docOptDir)
                        .sorted(Comparator.reverseOrder())
                        .forEach(p -> {
                            try {
                                Files.delete(p);
                            } catch (IOException e) {
                                log.error("Failed to delete: {}", p, e);
                            }
                        });
                log.info("Deleted all optimization data for document: {}", filename);
            }
        } catch (IOException e) {
            log.error("Failed to delete all optimization data for document: {}", documentId, e);
        }
    }

    // ========== Document Management ==========

    @Override
    public void cleanupDocument(String documentId) {
        deleteChunksByDocument(documentId);
        deleteImagesByDocument(documentId);
        deletePPLData(documentId);
        deleteAllOptimizationData(documentId);  // 新增：清理优化数据
        log.info("Cleaned up all data for document: {}", documentId);
    }

    @Override
    public boolean documentExists(String documentId) {
        return Files.exists(chunksPath.resolve(documentId)) ||
               Files.exists(imagesPath.resolve(documentId)) ||
               Files.exists(pplPath.resolve(documentId)) ||
               Files.exists(optimizationPath.resolve(documentId));  // 新增
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

            // Calculate optimization data size (新增)
            Path docOptDir = optimizationPath.resolve(documentId);
            if (Files.exists(docOptDir)) {
                size += Files.walk(docOptDir)
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

