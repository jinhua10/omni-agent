package top.yumbo.ai.storage.file;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.storage.api.model.*;

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
            // documentId 就是原始文件名
            Path documentFile = documentsPath.resolve(documentId);
            if (Files.exists(documentFile)) {
                Files.delete(documentFile);
                log.debug("Deleted document file: {}", documentFile);
            } else {
                log.warn("Document file not found: {}", documentFile);
            }

            // 清理相关的所有数据（chunks、images、ppl、optimization）⭐
            cleanupDocument(documentId);

            log.info("Successfully deleted document and all related data: {}", documentId);
        } catch (IOException e) {
            log.error("Failed to delete document: {}", documentId, e);
        }
    }

    // ========== Chunk Storage ==========

    @Override
    public String saveChunk(String documentId, Chunk chunk) {
        try {
            // 使用原始文件名作为目录（从chunk或documentId获取）
            // documentId 在调用时应该已经是原始文件名
            Path docChunkDir = chunksPath.resolve(documentId);
            Files.createDirectories(docChunkDir);

            // 使用有意义的文件名：chunk_序号.md ⭐
            String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();
            int sequence = chunk.getSequence();
            String chunkFilename = String.format("chunk_%03d.md", sequence);  // 添加.md后缀
            Path chunkFile = docChunkDir.resolve(chunkFilename);

            // 直接保存分块的文本内容，而不是序列化对象 ⭐
            Files.write(chunkFile, chunk.getContent().getBytes(java.nio.charset.StandardCharsets.UTF_8));

            // 同时保存元数据到 JSON 文件，方便查询
            String metadataFilename = chunkFilename + ".meta";
            Path metadataFile = docChunkDir.resolve(metadataFilename);
            String metadataJson = buildChunkMetadataJson(chunk, chunkFilename);
            Files.write(metadataFile, metadataJson.getBytes(java.nio.charset.StandardCharsets.UTF_8));

            log.debug("Saved chunk: {} -> {}/{}", chunkId, documentId, chunkFilename);
            return chunkId;
        } catch (IOException e) {
            log.error("Failed to save chunk for document: {}", documentId, e);
            return null;
        }
    }

    /**
     * 构建分块元数据 JSON
     */
    private String buildChunkMetadataJson(Chunk chunk, String filename) {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        json.append("  \"id\": \"").append(chunk.getId()).append("\",\n");
        json.append("  \"documentId\": \"").append(chunk.getDocumentId()).append("\",\n");
        json.append("  \"filename\": \"").append(filename).append("\",\n");
        json.append("  \"sequence\": ").append(chunk.getSequence()).append(",\n");
        if (chunk.getStartPosition() != null) {
            json.append("  \"startPosition\": ").append(chunk.getStartPosition()).append(",\n");
        }
        if (chunk.getEndPosition() != null) {
            json.append("  \"endPosition\": ").append(chunk.getEndPosition()).append(",\n");
        }
        json.append("  \"size\": ").append(chunk.getSize()).append(",\n");
        if (chunk.getMetadata() != null && !chunk.getMetadata().isEmpty()) {
            json.append("  \"metadata\": ").append(mapToJson(chunk.getMetadata())).append(",\n");
        }
        json.append("  \"createdAt\": ").append(chunk.getCreatedAt() != null ? chunk.getCreatedAt() : System.currentTimeMillis()).append("\n");
        json.append("}");
        return json.toString();
    }

    /**
     * 将 Map 转换为简单的 JSON 字符串
     */
    private String mapToJson(Map<String, Object> map) {
        StringBuilder json = new StringBuilder();
        json.append("{");
        boolean first = true;
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            if (!first) json.append(", ");
            json.append("\"").append(entry.getKey()).append("\": ");
            Object value = entry.getValue();
            if (value instanceof String) {
                json.append("\"").append(value).append("\"");
            } else if (value instanceof Number || value instanceof Boolean) {
                json.append(value);
            } else {
                json.append("\"").append(value).append("\"");
            }
            first = false;
        }
        json.append("}");
        return json.toString();
    }

    /**
     * 从 documentId 提取原文件名
     *
     * @deprecated 不再使用，保留用于向后兼容
     * 新的设计中 documentId 就是原始文件名
     */
    @Deprecated
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
            // 在所有文档的分块目录中搜索元数据文件
            List<Path> metadataFiles = Files.walk(chunksPath, 2)
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().endsWith(".meta"))
                    .collect(Collectors.toList());

            if (metadataFiles.isEmpty()) {
                return Optional.empty();
            }

            // 查找匹配的元数据文件
            for (Path metadataFile : metadataFiles) {
                try {
                    String metadataJson = new String(Files.readAllBytes(metadataFile), java.nio.charset.StandardCharsets.UTF_8);
                    if (metadataJson.contains("\"id\": \"" + chunkId + "\"")) {
                        // 找到匹配的元数据，读取分块
                        String chunkFilename = metadataFile.getFileName().toString().replace(".meta", "");
                        Path chunkFile = metadataFile.getParent().resolve(chunkFilename);

                        if (Files.exists(chunkFile)) {
                            return Optional.of(loadChunkFromFiles(chunkFile, metadataFile));
                        }
                    }
                } catch (IOException e) {
                    log.error("Failed to read metadata file: {}", metadataFile, e);
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
            Path docChunkDir = chunksPath.resolve(documentId);
            if (!Files.exists(docChunkDir)) {
                return new ArrayList<>();
            }

            return Files.list(docChunkDir)
                    .filter(Files::isRegularFile)
                    .filter(p -> !p.getFileName().toString().endsWith(".meta")) // 只处理分块文件
                    .filter(p -> p.getFileName().toString().startsWith("chunk_"))
                    .map(chunkFile -> {
                        try {
                            Path metadataFile = chunkFile.getParent().resolve(chunkFile.getFileName() + ".meta");
                            if (Files.exists(metadataFile)) {
                                return loadChunkFromFiles(chunkFile, metadataFile);
                            } else {
                                // 兼容旧格式：尝试反序列化
                                try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(chunkFile.toFile()))) {
                                    return (Chunk) ois.readObject();
                                } catch (Exception oldFormatEx) {
                                    log.warn("Cannot load chunk (old or new format): {}", chunkFile);
                                    return null;
                                }
                            }
                        } catch (IOException e) {
                            log.error("Failed to read chunk file: {}", chunkFile, e);
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

    /**
     * 从分块文件和元数据文件加载 Chunk 对象
     */
    private Chunk loadChunkFromFiles(Path chunkFile, Path metadataFile) throws IOException {
        // 读取分块文本内容
        String content = new String(Files.readAllBytes(chunkFile), java.nio.charset.StandardCharsets.UTF_8);

        // 读取元数据
        String metadataJson = new String(Files.readAllBytes(metadataFile), java.nio.charset.StandardCharsets.UTF_8);

        // 解析元数据
        String id = extractJsonValue(metadataJson, "id");
        String documentId = extractJsonValue(metadataJson, "documentId");
        Integer sequence = extractJsonIntValue(metadataJson, "sequence");
        Integer startPosition = extractJsonIntValue(metadataJson, "startPosition");
        Integer endPosition = extractJsonIntValue(metadataJson, "endPosition");
        Long createdAt = extractJsonLongValue(metadataJson, "createdAt");

        return Chunk.builder()
                .id(id)
                .documentId(documentId)
                .content(content)
                .sequence(sequence != null ? sequence : 0)
                .startPosition(startPosition)
                .endPosition(endPosition)
                .createdAt(createdAt)
                .build();
    }

    /**
     * 从 JSON 字符串中提取 Long 值
     */
    private Long extractJsonLongValue(String json, String key) {
        String pattern = "\"" + key + "\": ";
        int start = json.indexOf(pattern);
        if (start == -1) return null;
        start += pattern.length();
        int end = json.indexOf(",", start);
        if (end == -1) {
            end = json.indexOf("\n", start);
        }
        if (end == -1) return null;
        try {
            return Long.parseLong(json.substring(start, end).trim());
        } catch (NumberFormatException e) {
            return null;
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

            // 使用有意义的文件名：page_页码_img ⭐
            String imageId = image.getId() != null ? image.getId() : UUID.randomUUID().toString();

            // 构建文件名
            String imageFilename;
            if (image.getPageNumber() != null && image.getPageNumber() > 0) {
                // 如果有页码信息，使用 page_001_img.png 格式
                int pageNum = image.getPageNumber();
                String format = image.getFormat() != null ? image.getFormat() : "png";
                imageFilename = String.format("page_%03d_img.%s", pageNum, format);
            } else {
                // 如果没有页码，使用 image_xxx.png 格式
                String format = image.getFormat() != null ? image.getFormat() : "png";
                imageFilename = String.format("image_%s.%s", imageId.substring(0, Math.min(8, imageId.length())), format);
            }

            Path imageFile = docImageDir.resolve(imageFilename);

            // 直接保存图片的二进制数据，而不是序列化对象 ⭐
            Files.write(imageFile, image.getData());

            // 同时保存元数据到 JSON 文件，方便查询
            String metadataFilename = imageFilename + ".meta";
            Path metadataFile = docImageDir.resolve(metadataFilename);
            String metadataJson = buildImageMetadataJson(image, imageFilename);
            Files.write(metadataFile, metadataJson.getBytes(java.nio.charset.StandardCharsets.UTF_8));

            log.debug("Saved image: {} -> {}/{}", imageId, documentId, imageFilename);
            return imageId;
        } catch (IOException e) {
            log.error("Failed to save image", e);
            return null;
        }
    }

    /**
     * 构建图片元数据 JSON
     */
    private String buildImageMetadataJson(Image image, String filename) {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        json.append("  \"id\": \"").append(image.getId()).append("\",\n");
        json.append("  \"documentId\": \"").append(image.getDocumentId()).append("\",\n");
        json.append("  \"filename\": \"").append(filename).append("\",\n");
        json.append("  \"format\": \"").append(image.getFormat()).append("\",\n");
        if (image.getWidth() != null) {
            json.append("  \"width\": ").append(image.getWidth()).append(",\n");
        }
        if (image.getHeight() != null) {
            json.append("  \"height\": ").append(image.getHeight()).append(",\n");
        }
        if (image.getPageNumber() != null) {
            json.append("  \"pageNumber\": ").append(image.getPageNumber()).append(",\n");
        }
        json.append("  \"size\": ").append(image.getSize()).append(",\n");

        // 添加 metadata 字段（包含图片描述等信息）⭐
        if (image.getMetadata() != null && !image.getMetadata().isEmpty()) {
            json.append("  \"metadata\": ").append(mapToJson(image.getMetadata())).append(",\n");
        }

        json.append("  \"createdAt\": ").append(System.currentTimeMillis()).append("\n");
        json.append("}");
        return json.toString();
    }

    // ...existing code...

    @Override
    public Optional<Image> getImage(String imageId) {
        try {
            // 在所有文档的图片目录中搜索元数据文件
            List<Path> metadataFiles = Files.walk(imagesPath, 2)
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().endsWith(".meta"))
                    .toList();

            if (metadataFiles.isEmpty()) {
                return Optional.empty();
            }

            // 查找匹配的元数据文件
            for (Path metadataFile : metadataFiles) {
                try {
                    String metadataJson = new String(Files.readAllBytes(metadataFile), java.nio.charset.StandardCharsets.UTF_8);
                    if (metadataJson.contains("\"id\": \"" + imageId + "\"")) {
                        // 找到匹配的元数据，读取图片
                        String imageFilename = metadataFile.getFileName().toString().replace(".meta", "");
                        Path imageFile = metadataFile.getParent().resolve(imageFilename);

                        if (Files.exists(imageFile)) {
                            return Optional.of(loadImageFromFiles(imageFile, metadataFile));
                        }
                    }
                } catch (IOException e) {
                    log.error("Failed to read metadata file: {}", metadataFile, e);
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
            Path docImageDir = imagesPath.resolve(documentId);
            if (!Files.exists(docImageDir)) {
                return new ArrayList<>();
            }

            return Files.list(docImageDir)
                    .filter(Files::isRegularFile)
                    .filter(p -> !p.getFileName().toString().endsWith(".meta")) // 只处理图片文件
                    .filter(p -> {
                        String name = p.getFileName().toString();
                        return name.startsWith("page_") || name.startsWith("image_");
                    })
                    .map(imageFile -> {
                        try {
                            Path metadataFile = imageFile.getParent().resolve(imageFile.getFileName() + ".meta");
                            if (Files.exists(metadataFile)) {
                                return loadImageFromFiles(imageFile, metadataFile);
                            } else {
                                // 兼容旧格式：尝试反序列化
                                try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(imageFile.toFile()))) {
                                    return (Image) ois.readObject();
                                } catch (Exception oldFormatEx) {
                                    log.warn("Cannot load image (old or new format): {}", imageFile);
                                    return null;
                                }
                            }
                        } catch (IOException e) {
                            log.error("Failed to read image file: {}", imageFile, e);
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

    /**
     * 从图片文件和元数据文件加载 Image 对象
     */
    private Image loadImageFromFiles(Path imageFile, Path metadataFile) throws IOException {
        // 读取图片二进制数据
        byte[] imageData = Files.readAllBytes(imageFile);

        // 读取元数据
        String metadataJson = new String(Files.readAllBytes(metadataFile), java.nio.charset.StandardCharsets.UTF_8);

        // 解析元数据 (简单的 JSON 解析)
        String id = extractJsonValue(metadataJson, "id");
        String documentId = extractJsonValue(metadataJson, "documentId");
        String format = extractJsonValue(metadataJson, "format");
        Integer width = extractJsonIntValue(metadataJson, "width");
        Integer height = extractJsonIntValue(metadataJson, "height");
        Integer pageNumber = extractJsonIntValue(metadataJson, "pageNumber");

        return Image.builder()
                .id(id)
                .documentId(documentId)
                .data(imageData)
                .format(format)
                .width(width)
                .height(height)
                .pageNumber(pageNumber)
                .build();
    }

    /**
     * 从 JSON 字符串中提取字符串值
     */
    private String extractJsonValue(String json, String key) {
        String pattern = "\"" + key + "\": \"";
        int start = json.indexOf(pattern);
        if (start == -1) return null;
        start += pattern.length();
        int end = json.indexOf("\"", start);
        if (end == -1) return null;
        return json.substring(start, end);
    }

    /**
     * 从 JSON 字符串中提取整数值
     */
    private Integer extractJsonIntValue(String json, String key) {
        String pattern = "\"" + key + "\": ";
        int start = json.indexOf(pattern);
        if (start == -1) return null;
        start += pattern.length();
        int end = json.indexOf(",", start);
        if (end == -1) {
            end = json.indexOf("\n", start);
        }
        if (end == -1) return null;
        try {
            return Integer.parseInt(json.substring(start, end).trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    // ...existing code...

    @Override
    public void deleteImage(String imageId) {
        try {
            Files.walk(imagesPath, 2)
                    .filter(p -> p.toString().contains(imageId))
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

            Path pplFile = docPplDir.resolve("ppl.json");  // 使用 .json 后缀 ⭐

            // 将 PPL 数据转换为 JSON 并保存
            String jsonContent = buildPPLDataJson(data);
            Files.write(pplFile, jsonContent.getBytes(java.nio.charset.StandardCharsets.UTF_8));

            log.debug("Saved PPL data for document: {}", documentId);
            return documentId;
        } catch (IOException e) {
            log.error("Failed to save PPL data", e);
            return null;
        }
    }

    /**
     * 构建 PPL 数据 JSON
     */
    private String buildPPLDataJson(PPLData data) {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        json.append("  \"documentId\": \"").append(data.getDocumentId()).append("\",\n");

        // probablePoints 列表
        if (data.getProbablePoints() != null && !data.getProbablePoints().isEmpty()) {
            json.append("  \"probablePoints\": [");
            for (int i = 0; i < data.getProbablePoints().size(); i++) {
                if (i > 0) json.append(", ");
                json.append("\"").append(escapeJson(data.getProbablePoints().get(i))).append("\"");
            }
            json.append("],\n");
        }

        // scores 映射
        if (data.getScores() != null && !data.getScores().isEmpty()) {
            json.append("  \"scores\": {\n");
            boolean first = true;
            for (Map.Entry<String, Float> entry : data.getScores().entrySet()) {
                if (!first) json.append(",\n");
                json.append("    \"").append(escapeJson(entry.getKey())).append("\": ").append(entry.getValue());
                first = false;
            }
            json.append("\n  },\n");
        }

        if (data.getModelVersion() != null) {
            json.append("  \"modelVersion\": \"").append(data.getModelVersion()).append("\",\n");
        }

        if (data.getAnalyzedAt() != null) {
            json.append("  \"analyzedAt\": ").append(data.getAnalyzedAt()).append(",\n");
        }

        // metadata
        if (data.getMetadata() != null && !data.getMetadata().isEmpty()) {
            json.append("  \"metadata\": ").append(mapToJson(data.getMetadata())).append(",\n");
        }

        json.append("  \"savedAt\": ").append(System.currentTimeMillis()).append("\n");
        json.append("}");
        return json.toString();
    }

    @Override
    public Optional<PPLData> getPPLData(String documentId) {
        try {
            Path pplFile = pplPath.resolve(documentId).resolve("ppl.json");  // 读取 .json 文件 ⭐

            if (!Files.exists(pplFile)) {
                // 尝试读取旧格式（无后缀）
                Path oldPplFile = pplPath.resolve(documentId).resolve("ppl");
                if (Files.exists(oldPplFile)) {
                    try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(oldPplFile.toFile()))) {
                        return Optional.of((PPLData) ois.readObject());
                    } catch (ClassNotFoundException e) {
                        log.error("Failed to deserialize old PPL data for document: {}", documentId, e);
                    }
                }
                return Optional.empty();
            }

            // 读取 JSON 格式的 PPL 数据
            String jsonContent = new String(Files.readAllBytes(pplFile), java.nio.charset.StandardCharsets.UTF_8);
            PPLData data = parsePPLDataJson(jsonContent, documentId);
            return Optional.ofNullable(data);

        } catch (IOException e) {
            log.error("Failed to get PPL data for document: {}", documentId, e);
            return Optional.empty();
        }
    }

    /**
     * 从 JSON 解析 PPL 数据
     */
    private PPLData parsePPLDataJson(String json, String documentId) {
        try {
            PPLData.PPLDataBuilder builder = PPLData.builder();
            builder.documentId(documentId);

            String modelVersion = extractJsonValue(json, "modelVersion");
            if (modelVersion != null) {
                builder.modelVersion(modelVersion);
            }

            Long analyzedAt = extractJsonLongValue(json, "analyzedAt");
            if (analyzedAt != null) {
                builder.analyzedAt(analyzedAt);
            }

            // 解析 probablePoints 数组（简化处理）
            List<String> probablePoints = new ArrayList<>();
            int ppStart = json.indexOf("\"probablePoints\": [");
            if (ppStart != -1) {
                int ppEnd = json.indexOf("]", ppStart);
                if (ppEnd != -1) {
                    String ppArray = json.substring(ppStart + 19, ppEnd);
                    String[] points = ppArray.split("\",\\s*\"");
                    for (String point : points) {
                        String cleaned = point.replace("\"", "").trim();
                        if (!cleaned.isEmpty()) {
                            probablePoints.add(cleaned);
                        }
                    }
                }
            }
            builder.probablePoints(probablePoints);

            // 解析 scores 映射（简化处理）
            Map<String, Float> scores = new HashMap<>();
            int scoresStart = json.indexOf("\"scores\": {");
            if (scoresStart != -1) {
                int scoresEnd = json.indexOf("}", scoresStart);
                if (scoresEnd != -1) {
                    String scoresBlock = json.substring(scoresStart + 11, scoresEnd);
                    String[] entries = scoresBlock.split(",");
                    for (String entry : entries) {
                        String[] kv = entry.split(":");
                        if (kv.length == 2) {
                            String key = kv[0].trim().replace("\"", "");
                            try {
                                Float value = Float.parseFloat(kv[1].trim());
                                scores.put(key, value);
                            } catch (NumberFormatException ignored) {}
                        }
                    }
                }
            }
            builder.scores(scores);

            return builder.build();
        } catch (Exception e) {
            log.error("Failed to parse PPL JSON", e);
            return null;
        }
    }

    /**
     * 转义 JSON 字符串中的特殊字符
     */
    private String escapeJson(String str) {
        if (str == null) return "";
        return str.replace("\\", "\\\\")
                  .replace("\"", "\\\"")
                  .replace("\n", "\\n")
                  .replace("\r", "\\r")
                  .replace("\t", "\\t");
    }

    @Override
    public void deletePPLData(String documentId) {
        try {
            Path docPplDir = pplPath.resolve(documentId);
            if (Files.exists(docPplDir)) {
                // 删除整个目录（包含新旧格式）
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

    // ========== Optimization Data Storage (通用RAG优化数据) ==========

    @Override
    public String saveOptimizationData(String documentId, OptimizationData data) {
        try {
            Path docOptDir = optimizationPath.resolve(documentId);
            Files.createDirectories(docOptDir);

            // 使用优化类型作为文件名，添加 .json 后缀 ⭐
            String fileName = data.getOptimizationType() + ".json";
            Path optFile = docOptDir.resolve(fileName);

            // 将 Optimization 数据转换为 JSON 并保存
            String jsonContent = buildOptimizationDataJson(data);
            Files.write(optFile, jsonContent.getBytes(java.nio.charset.StandardCharsets.UTF_8));

            log.debug("Saved {} optimization data for document: {}", data.getOptimizationType(), documentId);
            return documentId + ":" + data.getOptimizationType();
        } catch (IOException e) {
            log.error("Failed to save optimization data", e);
            return null;
        }
    }

    /**
     * 构建 Optimization 数据 JSON
     */
    private String buildOptimizationDataJson(OptimizationData data) {
        StringBuilder json = new StringBuilder();
        json.append("{\n");
        json.append("  \"documentId\": \"").append(data.getDocumentId()).append("\",\n");
        json.append("  \"optimizationType\": \"").append(data.getOptimizationType()).append("\",\n");

        if (data.getAlgorithmVersion() != null) {
            json.append("  \"algorithmVersion\": \"").append(data.getAlgorithmVersion()).append("\",\n");
        }

        if (data.getProcessedAt() != null) {
            json.append("  \"processedAt\": ").append(data.getProcessedAt()).append(",\n");
        }

        // data 映射
        if (data.getData() != null && !data.getData().isEmpty()) {
            json.append("  \"data\": ").append(mapToJson(data.getData())).append(",\n");
        }

        // metadata 映射
        if (data.getMetadata() != null && !data.getMetadata().isEmpty()) {
            json.append("  \"metadata\": ").append(mapToJson(data.getMetadata())).append(",\n");
        }

        // metrics 映射
        if (data.getMetrics() != null && !data.getMetrics().isEmpty()) {
            json.append("  \"metrics\": {\n");
            boolean first = true;
            for (Map.Entry<String, Double> entry : data.getMetrics().entrySet()) {
                if (!first) json.append(",\n");
                json.append("    \"").append(escapeJson(entry.getKey())).append("\": ").append(entry.getValue());
                first = false;
            }
            json.append("\n  },\n");
        }

        json.append("  \"savedAt\": ").append(System.currentTimeMillis()).append("\n");
        json.append("}");
        return json.toString();
    }

    @Override
    public Optional<OptimizationData> getOptimizationData(String documentId, String optimizationType) {
        try {
            Path optFile = optimizationPath.resolve(documentId).resolve(optimizationType + ".json");  // 读取 .json 文件 ⭐

            if (!Files.exists(optFile)) {
                // 尝试读取旧格式（无后缀）
                Path oldOptFile = optimizationPath.resolve(documentId).resolve(optimizationType);
                if (Files.exists(oldOptFile)) {
                    try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(oldOptFile.toFile()))) {
                        return Optional.of((OptimizationData) ois.readObject());
                    } catch (ClassNotFoundException e) {
                        log.error("Failed to deserialize old optimization data: {}", optimizationType, e);
                    }
                }
                return Optional.empty();
            }

            // 读取 JSON 格式的优化数据
            String jsonContent = new String(Files.readAllBytes(optFile), java.nio.charset.StandardCharsets.UTF_8);
            OptimizationData data = parseOptimizationDataJson(jsonContent);
            return Optional.ofNullable(data);

        } catch (IOException e) {
            log.error("Failed to get {} optimization data for document: {}", optimizationType, documentId, e);
            return Optional.empty();
        }
    }

    /**
     * 从 JSON 解析 Optimization 数据
     */
    private OptimizationData parseOptimizationDataJson(String json) {
        try {
            OptimizationData.OptimizationDataBuilder builder = OptimizationData.builder();

            String documentId = extractJsonValue(json, "documentId");
            String optimizationType = extractJsonValue(json, "optimizationType");
            String algorithmVersion = extractJsonValue(json, "algorithmVersion");
            Long processedAt = extractJsonLongValue(json, "processedAt");

            builder.documentId(documentId);
            builder.optimizationType(optimizationType);
            if (algorithmVersion != null) {
                builder.algorithmVersion(algorithmVersion);
            }
            if (processedAt != null) {
                builder.processedAt(processedAt);
            }

            // 注意：data, metadata, metrics 的完整解析比较复杂
            // 这里简化处理，实际使用时可能需要更完善的 JSON 解析库
            builder.data(new HashMap<>());
            builder.metadata(new HashMap<>());
            builder.metrics(new HashMap<>());

            return builder.build();
        } catch (Exception e) {
            log.error("Failed to parse Optimization JSON", e);
            return null;
        }
    }

    @Override
    public List<OptimizationData> getAllOptimizationData(String documentId) {
        try {
            Path docOptDir = optimizationPath.resolve(documentId);
            if (!Files.exists(docOptDir)) {
                return Collections.emptyList();
            }

            return Files.list(docOptDir)
                    .filter(Files::isRegularFile)
                    .map(p -> {
                        try {
                            // 优先尝试读取 JSON 格式
                            if (p.getFileName().toString().endsWith(".json")) {
                                String jsonContent = new String(Files.readAllBytes(p), java.nio.charset.StandardCharsets.UTF_8);
                                return parseOptimizationDataJson(jsonContent);
                            } else {
                                // 兼容旧格式：尝试反序列化
                                try (ObjectInputStream ois = new ObjectInputStream(new FileInputStream(p.toFile()))) {
                                    return (OptimizationData) ois.readObject();
                                } catch (ClassNotFoundException e) {
                                    log.error("Failed to read optimization file: {}", p, e);
                                    return null;
                                }
                            }
                        } catch (IOException e) {
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
            // 尝试删除 JSON 格式文件
            Path optFile = optimizationPath.resolve(documentId).resolve(optimizationType + ".json");
            if (Files.exists(optFile)) {
                Files.delete(optFile);
                log.info("Deleted {} optimization data for document: {}", optimizationType, documentId);
                return;
            }

            // 尝试删除旧格式文件（无后缀）
            Path oldOptFile = optimizationPath.resolve(documentId).resolve(optimizationType);
            if (Files.exists(oldOptFile)) {
                Files.delete(oldOptFile);
                log.info("Deleted {} optimization data (old format) for document: {}", optimizationType, documentId);
            }
        } catch (IOException e) {
            log.error("Failed to delete {} optimization data for document: {}", optimizationType, documentId, e);
        }
    }

    @Override
    public void deleteAllOptimizationData(String documentId) {
        try {
            Path docOptDir = optimizationPath.resolve(documentId);
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
                log.info("Deleted all optimization data for document: {}", documentId);
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
    public StorageStatistics getStatistics() {
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

    // ========== 文档列表查询方法 ==========

    @Override
    public List<DocumentMetadata> listAllDocuments() {
        try {
            return Files.walk(documentsPath)
                    .filter(Files::isRegularFile)
                    .map(this::buildDocumentMetadata)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            log.error("Failed to list all documents", e);
            return Collections.emptyList();
        }
    }

    @Override
    public List<DocumentMetadata> listDocuments(int offset, int limit) {
        try {
            return Files.walk(documentsPath)
                    .filter(Files::isRegularFile)
                    .skip(offset)
                    .limit(limit)
                    .map(this::buildDocumentMetadata)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            log.error("Failed to list documents with pagination", e);
            return Collections.emptyList();
        }
    }

    @Override
    public List<DocumentMetadata> searchDocuments(String keyword) {
        try {
            String lowerKeyword = keyword.toLowerCase();
            return Files.walk(documentsPath)
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().toLowerCase().contains(lowerKeyword))
                    .map(this::buildDocumentMetadata)
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (IOException e) {
            log.error("Failed to search documents", e);
            return Collections.emptyList();
        }
    }

    @Override
    public long getDocumentCount() {
        try {
            return Files.walk(documentsPath)
                    .filter(Files::isRegularFile)
                    .count();
        } catch (IOException e) {
            log.error("Failed to count documents", e);
            return 0;
        }
    }

    /**
     * 从文件路径构建文档元数据
     */
    private DocumentMetadata buildDocumentMetadata(Path filePath) {
        try {
            Path relativePath = documentsPath.relativize(filePath);
            String relativePathStr = relativePath.toString().replace('\\', '/');
            String filename = filePath.getFileName().toString();

            // 提取文档ID（从文件名推断）
            String documentId = "doc_" + filename;

            // 获取文件属性
            long fileSize = Files.size(filePath);
            long lastModifiedTime = Files.getLastModifiedTime(filePath).toMillis();

            // 获取文件类型
            String fileType = getFileExtension(filename);

            // 统计分块数量
            int chunkCount = countChunks(filename);

            // 统计图片数量
            int imageCount = countImages(filename);

            return top.yumbo.ai.storage.api.model.DocumentMetadata.builder()
                    .documentId(documentId)
                    .filename(filename)
                    .relativePath(relativePathStr)
                    .fileSize(fileSize)
                    .fileType(fileType)
                    .uploadTime(new Date(lastModifiedTime))
                    .lastModified(new Date(lastModifiedTime))
                    .indexed(chunkCount > 0)  // 有分块说明已索引
                    .chunkCount(chunkCount)
                    .imageCount(imageCount)
                    .storagePath(relativePathStr)
                    .build();
        } catch (IOException e) {
            log.error("Failed to build metadata for: {}", filePath, e);
            return null;
        }
    }

    /**
     * 获取文件扩展名
     */
    private String getFileExtension(String filename) {
        int lastDot = filename.lastIndexOf('.');
        if (lastDot > 0 && lastDot < filename.length() - 1) {
            return filename.substring(lastDot + 1);
        }
        return "";
    }

    /**
     * 统计文档的分块数量
     */
    private int countChunks(String filename) {
        try {
            Path docChunkDir = chunksPath.resolve(filename);
            if (!Files.exists(docChunkDir)) {
                return 0;
            }
            return (int) Files.list(docChunkDir)
                    .filter(Files::isRegularFile)
                    .filter(p -> p.getFileName().toString().startsWith("chunk_"))
                    .count();
        } catch (IOException e) {
            return 0;
        }
    }

    /**
     * 统计文档的图片数量
     */
    private int countImages(String filename) {
        try {
            Path docImageDir = imagesPath.resolve(filename);
            if (!Files.exists(docImageDir)) {
                return 0;
            }
            return (int) Files.list(docImageDir)
                    .filter(Files::isRegularFile)
                    .count();
        } catch (IOException e) {
            return 0;
        }
    }
}


