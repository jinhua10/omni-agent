package top.yumbo.ai.omni.storage.api;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.storage.api.exception.*;
import top.yumbo.ai.omni.storage.api.model.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * DocumentStorageService 接口测试
 * 测试接口的默认方法实现
 *
 * @author OmniAgent Team
 */
@DisplayName("DocumentStorageService 接口测试")
class DocumentStorageServiceTest {

    private DocumentStorageService storage;

    @BeforeEach
    void setUp() {
        // 使用简单的内存实现来测试默认方法
        storage = new InMemoryDocumentStorage();
    }

    // ========== 原始文档存储测试 ==========

    @Test
    @DisplayName("测试文档保存和获取")
    void testSaveAndGetDocument() {
        String documentId = "test-doc-001";
        String filename = "test.pdf";
        byte[] data = "Test PDF Content".getBytes(StandardCharsets.UTF_8);

        // 保存文档
        String savedId = storage.saveDocument(documentId, filename, data);
        assertNotNull(savedId);
        assertEquals(documentId, savedId);

        // 获取文档
        Optional<byte[]> retrieved = storage.getDocument(documentId);
        assertTrue(retrieved.isPresent());
        assertArrayEquals(data, retrieved.get());
    }

    @Test
    @DisplayName("测试文档删除")
    void testDeleteDocument() {
        String documentId = "test-doc-002";
        byte[] data = "Test Content".getBytes(StandardCharsets.UTF_8);

        storage.saveDocument(documentId, "test.txt", data);
        assertTrue(storage.getDocument(documentId).isPresent());

        storage.deleteDocument(documentId);
        assertFalse(storage.getDocument(documentId).isPresent());
    }

    @Test
    @DisplayName("测试批量保存文档（非事务）")
    void testBatchSaveDocuments() {
        List<Map<String, Object>> documents = Arrays.asList(
            Map.of("documentId", "batch-1", "filename", "file1.txt", "fileData", "Content 1".getBytes()),
            Map.of("documentId", "batch-2", "filename", "file2.txt", "fileData", "Content 2".getBytes()),
            Map.of("documentId", "batch-3", "filename", "file3.txt", "fileData", "Content 3".getBytes())
        );

        BatchOperationResult result = storage.saveDocuments(documents);

        assertEquals(3, result.getSuccessCount());
        assertEquals(0, result.getFailureCount());
        assertEquals(3, result.getTotalCount());
        assertTrue(result.getSuccessIds().contains("batch-1"));
        assertTrue(result.getSuccessIds().contains("batch-2"));
        assertTrue(result.getSuccessIds().contains("batch-3"));
    }

    @Test
    @DisplayName("测试批量删除文档（非事务）")
    void testBatchDeleteDocuments() {
        // 先保存文档
        storage.saveDocument("del-1", "file1.txt", "Content 1".getBytes());
        storage.saveDocument("del-2", "file2.txt", "Content 2".getBytes());
        storage.saveDocument("del-3", "file3.txt", "Content 3".getBytes());

        // 批量删除
        BatchOperationResult result = storage.deleteDocuments(Arrays.asList("del-1", "del-2", "del-3"));

        assertEquals(3, result.getSuccessCount());
        assertEquals(0, result.getFailureCount());
        assertFalse(storage.getDocument("del-1").isPresent());
        assertFalse(storage.getDocument("del-2").isPresent());
        assertFalse(storage.getDocument("del-3").isPresent());
    }

    @Test
    @DisplayName("测试流式读取文档")
    void testGetDocumentStream() throws Exception {
        String documentId = "stream-doc-001";
        String content = "Test Stream Content";
        storage.saveDocument(documentId, "test.txt", content.getBytes(StandardCharsets.UTF_8));

        try (InputStream stream = storage.getDocumentStream(documentId)) {
            String readContent = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            assertEquals(content, readContent);
        }
    }

    @Test
    @DisplayName("测试流式保存文档")
    void testSaveDocumentStream() throws Exception {
        String documentId = "stream-doc-002";
        String filename = "test.txt";
        String content = "Test Stream Save Content";

        try (InputStream inputStream = new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8))) {
            String savedId = storage.saveDocumentStream(documentId, filename, inputStream);
            assertEquals(documentId, savedId);
        }

        Optional<byte[]> retrieved = storage.getDocument(documentId);
        assertTrue(retrieved.isPresent());
        assertEquals(content, new String(retrieved.get(), StandardCharsets.UTF_8));
    }

    @Test
    @DisplayName("测试复制文档到输出流")
    void testCopyDocumentToStream() throws StorageException {
        String documentId = "copy-doc-001";
        String content = "Test Copy Content";
        storage.saveDocument(documentId, "test.txt", content.getBytes(StandardCharsets.UTF_8));

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        storage.copyDocumentToStream(documentId, outputStream);

        assertEquals(content, outputStream.toString(StandardCharsets.UTF_8));
    }

    @Test
    @DisplayName("测试文档不存在时抛出异常")
    void testDocumentNotFoundExceptionOnStream() {
        assertThrows(DocumentNotFoundException.class, () ->
            storage.getDocumentStream("non-existent-doc")
        );
    }

    // ========== 提取文本存储测试 ==========

    @Test
    @DisplayName("测试保存和获取提取的文本")
    void testSaveAndGetExtractedText() {
        String documentId = "text-doc-001";
        String text = "This is extracted text from PDF";

        String savedId = storage.saveExtractedText(documentId, text);
        assertNotNull(savedId);

        Optional<String> retrieved = storage.getExtractedText(documentId);
        assertTrue(retrieved.isPresent());
        assertEquals(text, retrieved.get());
    }

    @Test
    @DisplayName("测试删除提取的文本")
    void testDeleteExtractedText() {
        String documentId = "text-doc-002";
        storage.saveExtractedText(documentId, "Some text");
        assertTrue(storage.getExtractedText(documentId).isPresent());

        storage.deleteExtractedText(documentId);
        assertFalse(storage.getExtractedText(documentId).isPresent());
    }

    @Test
    @DisplayName("测试流式读取提取的文本")
    void testGetExtractedTextStream() throws Exception {
        String documentId = "text-doc-003";
        String text = "Extracted text content";
        storage.saveExtractedText(documentId, text);

        try (InputStream stream = storage.getExtractedTextStream(documentId)) {
            String readText = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            assertEquals(text, readText);
        }
    }

    @Test
    @DisplayName("测试流式保存提取的文本")
    void testSaveExtractedTextStream() throws Exception {
        String documentId = "text-doc-004";
        String text = "Streamed extracted text";

        try (InputStream inputStream = new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8))) {
            String savedId = storage.saveExtractedTextStream(documentId, inputStream);
            assertEquals(documentId, savedId);
        }

        Optional<String> retrieved = storage.getExtractedText(documentId);
        assertTrue(retrieved.isPresent());
        assertEquals(text, retrieved.get());
    }

    // ========== 分块存储测试 ==========

    @Test
    @DisplayName("测试保存和获取分块")
    void testSaveAndGetChunk() {
        String documentId = "chunk-doc-001";
        Chunk chunk = Chunk.builder()
                .id("chunk-001")
                .documentId(documentId)
                .content("This is chunk 1")
                .sequence(1)
                .build();

        String chunkId = storage.saveChunk(documentId, chunk);
        assertNotNull(chunkId);

        Optional<Chunk> retrieved = storage.getChunk(chunkId);
        assertTrue(retrieved.isPresent());
        assertEquals("This is chunk 1", retrieved.get().getContent());
    }

    @Test
    @DisplayName("测试批量保存分块")
    void testSaveChunks() {
        String documentId = "chunk-doc-002";
        List<Chunk> chunks = Arrays.asList(
                Chunk.builder().id("chunk-1").documentId(documentId).content("Chunk 1").sequence(1).build(),
                Chunk.builder().id("chunk-2").documentId(documentId).content("Chunk 2").sequence(2).build(),
                Chunk.builder().id("chunk-3").documentId(documentId).content("Chunk 3").sequence(3).build()
        );

        List<String> chunkIds = storage.saveChunks(documentId, chunks);
        assertEquals(3, chunkIds.size());

        List<Chunk> retrieved = storage.getChunksByDocument(documentId);
        assertEquals(3, retrieved.size());
    }

    @Test
    @DisplayName("测试删除分块")
    void testDeleteChunk() {
        String documentId = "chunk-doc-003";
        Chunk chunk = Chunk.builder().id("chunk-del-1").documentId(documentId).content("Test").build();

        storage.saveChunk(documentId, chunk);
        assertTrue(storage.getChunk("chunk-del-1").isPresent());

        storage.deleteChunk("chunk-del-1");
        assertFalse(storage.getChunk("chunk-del-1").isPresent());
    }

    @Test
    @DisplayName("测试删除文档的所有分块")
    void testDeleteChunksByDocument() {
        String documentId = "chunk-doc-004";
        storage.saveChunk(documentId, Chunk.builder().id("c1").documentId(documentId).content("C1").build());
        storage.saveChunk(documentId, Chunk.builder().id("c2").documentId(documentId).content("C2").build());

        storage.deleteChunksByDocument(documentId);
        assertTrue(storage.getChunksByDocument(documentId).isEmpty());
    }

    // ========== 图像存储测试 ==========

    @Test
    @DisplayName("测试保存和获取图像")
    void testSaveAndGetImage() {
        String documentId = "img-doc-001";
        Image image = Image.builder()
                .id("img-001")
                .documentId(documentId)
                .data("fake-image-data".getBytes())
                .format("PNG")
                .width(800)
                .height(600)
                .build();

        String imageId = storage.saveImage(documentId, image);
        assertNotNull(imageId);

        Optional<Image> retrieved = storage.getImage(imageId);
        assertTrue(retrieved.isPresent());
        assertEquals("PNG", retrieved.get().getFormat());
        assertEquals(800, retrieved.get().getWidth());
    }

    @Test
    @DisplayName("测试批量保存图像")
    void testSaveImages() {
        String documentId = "img-doc-002";
        List<Image> images = Arrays.asList(
                Image.builder().id("i1").documentId(documentId).data("data1".getBytes()).format("PNG").build(),
                Image.builder().id("i2").documentId(documentId).data("data2".getBytes()).format("JPG").build()
        );

        List<String> imageIds = storage.saveImages(documentId, images);
        assertEquals(2, imageIds.size());

        List<Image> retrieved = storage.getImagesByDocument(documentId);
        assertEquals(2, retrieved.size());
    }

    @Test
    @DisplayName("测试删除图像")
    void testDeleteImage() {
        String documentId = "img-doc-003";
        Image image = Image.builder().id("img-del-1").documentId(documentId).data("data".getBytes()).format("PNG").build();

        storage.saveImage(documentId, image);
        storage.deleteImage("img-del-1");
        assertFalse(storage.getImage("img-del-1").isPresent());
    }

    // ========== 优化数据存储测试 ==========

    @Test
    @DisplayName("测试保存和获取优化数据")
    void testSaveAndGetOptimizationData() {
        String documentId = "opt-doc-001";
        OptimizationData data = OptimizationData.builder()
                .documentId(documentId)
                .optimizationType(OptimizationType.PPL.name())
                .data(Map.of("score", 0.95))
                .build();

        String dataId = storage.saveOptimizationData(documentId, data);
        assertNotNull(dataId);

        Optional<OptimizationData> retrieved = storage.getOptimizationData(documentId, OptimizationType.PPL.name());
        assertTrue(retrieved.isPresent());
        assertEquals(OptimizationType.PPL.name(), retrieved.get().getOptimizationType());
    }

    @Test
    @DisplayName("测试获取文档的所有优化数据")
    void testGetAllOptimizationData() {
        String documentId = "opt-doc-002";
        storage.saveOptimizationData(documentId, OptimizationData.builder()
                .documentId(documentId).optimizationType(OptimizationType.PPL.name()).data(Map.of()).build());
        storage.saveOptimizationData(documentId, OptimizationData.builder()
                .documentId(documentId).optimizationType(OptimizationType.HYDE.name()).data(Map.of()).build());

        List<OptimizationData> allData = storage.getAllOptimizationData(documentId);
        assertEquals(2, allData.size());
    }

    @Test
    @DisplayName("测试删除优化数据")
    void testDeleteOptimizationData() {
        String documentId = "opt-doc-003";
        storage.saveOptimizationData(documentId, OptimizationData.builder()
                .documentId(documentId).optimizationType(OptimizationType.PPL.name()).data(Map.of()).build());

        storage.deleteOptimizationData(documentId, OptimizationType.PPL.name());
        assertFalse(storage.getOptimizationData(documentId, OptimizationType.PPL.name()).isPresent());
    }

    // ========== 元数据管理测试 ==========

    @Test
    @DisplayName("测试保存和获取元数据")
    void testSaveAndGetMetadata() {
        DocumentMetadata metadata = DocumentMetadata.builder()
                .documentId("meta-doc-001")
                .filename("test.pdf")
                .fileSize(1024L)
                .mimeType("application/pdf")
                .build();

        storage.saveMetadata(metadata);

        Optional<DocumentMetadata> retrieved = storage.getMetadata("meta-doc-001");
        assertTrue(retrieved.isPresent());
        assertEquals("test.pdf", retrieved.get().getFilename());
        assertEquals(1024L, retrieved.get().getFileSize());
    }

    @Test
    @DisplayName("测试分页查询元数据")
    void testGetAllMetadataWithPagination() {
        // 保存多个元数据
        for (int i = 1; i <= 10; i++) {
            storage.saveMetadata(DocumentMetadata.builder()
                    .documentId("page-doc-" + i)
                    .filename("file" + i + ".pdf")
                    .build());
        }

        PageRequest pageRequest = PageRequest.of(0, 5);
        PageResult<DocumentMetadata> result = storage.getAllMetadata(pageRequest);

        assertEquals(5, result.getContent().size());
        assertTrue(result.getTotalElements() >= 10);
    }

    // ========== 文档管理测试 ==========

    @Test
    @DisplayName("测试文档存在性检查")
    void testDocumentExists() {
        String documentId = "exists-doc-001";
        storage.saveDocument(documentId, "test.txt", "content".getBytes());

        assertTrue(storage.documentExists(documentId));
        assertFalse(storage.documentExists("non-existent"));
    }

    @Test
    @DisplayName("测试批量检查文档存在性")
    void testCheckDocumentsExist() {
        storage.saveDocument("doc-1", "file1.txt", "content".getBytes());
        storage.saveDocument("doc-2", "file2.txt", "content".getBytes());

        Map<String, List<String>> result = storage.checkDocumentsExist(
                Arrays.asList("doc-1", "doc-2", "doc-3", "doc-4")
        );

        assertEquals(2, result.get("existing").size());
        assertEquals(2, result.get("missing").size());
        assertTrue(result.get("existing").contains("doc-1"));
        assertTrue(result.get("missing").contains("doc-3"));
    }

    @Test
    @DisplayName("测试清理文档")
    void testCleanupDocument() {
        String documentId = "cleanup-doc-001";
        
        // 保存各类数据
        storage.saveDocument(documentId, "test.pdf", "content".getBytes());
        storage.saveChunk(documentId, Chunk.builder().id("c1").documentId(documentId).content("chunk").build());
        storage.saveImage(documentId, Image.builder().id("i1").documentId(documentId).data("img".getBytes()).format("PNG").build());

        // 清理
        storage.cleanupDocument(documentId);

        // 验证都已删除
        assertFalse(storage.getDocument(documentId).isPresent());
        assertTrue(storage.getChunksByDocument(documentId).isEmpty());
        assertTrue(storage.getImagesByDocument(documentId).isEmpty());
    }

    @Test
    @DisplayName("测试统计信息")
    void testGetStatistics() {
        // 保存一些数据
        storage.saveDocument("stat-doc-1", "file1.txt", "content".getBytes());
        storage.saveDocument("stat-doc-2", "file2.txt", "content".getBytes());

        StorageStatistics stats = storage.getStatistics();
        assertNotNull(stats);
        assertTrue(stats.getTotalDocuments() >= 2);
    }

    @Test
    @DisplayName("测试健康检查")
    void testIsHealthy() {
        assertTrue(storage.isHealthy());
    }

    // ========== 内存实现用于测试 ==========

    /**
     * 简单的内存实现，用于测试接口的默认方法
     */
    private static class InMemoryDocumentStorage implements DocumentStorageService {
        private final Map<String, byte[]> documents = new HashMap<>();
        private final Map<String, String> extractedTexts = new HashMap<>();
        private final Map<String, Chunk> chunks = new HashMap<>();
        private final Map<String, List<String>> documentChunks = new HashMap<>();
        private final Map<String, Image> images = new HashMap<>();
        private final Map<String, List<String>> documentImages = new HashMap<>();
        private final Map<String, Map<String, OptimizationData>> optimizationData = new HashMap<>();
        private final Map<String, DocumentMetadata> metadata = new HashMap<>();

        @Override
        public String saveDocument(String documentId, String filename, byte[] fileData) {
            documents.put(documentId, fileData);
            return documentId;
        }

        @Override
        public Optional<byte[]> getDocument(String documentId) {
            return Optional.ofNullable(documents.get(documentId));
        }

        @Override
        public void deleteDocument(String documentId) {
            documents.remove(documentId);
        }

        @Override
        public String saveExtractedText(String documentId, String text) {
            extractedTexts.put(documentId, text);
            return documentId;
        }

        @Override
        public Optional<String> getExtractedText(String documentId) {
            return Optional.ofNullable(extractedTexts.get(documentId));
        }

        @Override
        public void deleteExtractedText(String documentId) {
            extractedTexts.remove(documentId);
        }

        @Override
        public String saveChunk(String documentId, Chunk chunk) {
            String chunkId = chunk.getId() != null ? chunk.getId() : UUID.randomUUID().toString();
            chunk.setId(chunkId);
            chunks.put(chunkId, chunk);
            documentChunks.computeIfAbsent(documentId, k -> new ArrayList<>()).add(chunkId);
            return chunkId;
        }

        @Override
        public List<String> saveChunks(String documentId, List<Chunk> chunksList) {
            return chunksList.stream()
                    .map(chunk -> saveChunk(documentId, chunk))
                    .collect(java.util.stream.Collectors.toList());
        }

        @Override
        public Optional<Chunk> getChunk(String chunkId) {
            return Optional.ofNullable(chunks.get(chunkId));
        }

        @Override
        public List<Chunk> getChunksByDocument(String documentId) {
            return documentChunks.getOrDefault(documentId, Collections.emptyList()).stream()
                    .map(chunks::get)
                    .filter(Objects::nonNull)
                    .collect(java.util.stream.Collectors.toList());
        }

        @Override
        public void deleteChunk(String chunkId) {
            chunks.remove(chunkId);
        }

        @Override
        public void deleteChunksByDocument(String documentId) {
            List<String> chunkIds = documentChunks.remove(documentId);
            if (chunkIds != null) {
                chunkIds.forEach(chunks::remove);
            }
        }

        @Override
        public String saveImage(String documentId, Image image) {
            String imageId = image.getId() != null ? image.getId() : UUID.randomUUID().toString();
            image.setId(imageId);
            images.put(imageId, image);
            documentImages.computeIfAbsent(documentId, k -> new ArrayList<>()).add(imageId);
            return imageId;
        }

        @Override
        public Optional<Image> getImage(String imageId) {
            return Optional.ofNullable(images.get(imageId));
        }

        @Override
        public List<Image> getImagesByDocument(String documentId) {
            return documentImages.getOrDefault(documentId, Collections.emptyList()).stream()
                    .map(images::get)
                    .filter(Objects::nonNull)
                    .collect(java.util.stream.Collectors.toList());
        }

        @Override
        public void deleteImage(String imageId) {
            images.remove(imageId);
        }

        @Override
        public void deleteImagesByDocument(String documentId) {
            List<String> imageIds = documentImages.remove(documentId);
            if (imageIds != null) {
                imageIds.forEach(images::remove);
            }
        }

        @Override
        public String savePPLData(String documentId, PPLData data) {
            return documentId;
        }

        @Override
        public Optional<PPLData> getPPLData(String documentId) {
            return Optional.empty();
        }

        @Override
        public void deletePPLData(String documentId) {
        }

        @Override
        public String saveOptimizationData(String documentId, OptimizationData data) {
            optimizationData.computeIfAbsent(documentId, k -> new HashMap<>())
                    .put(data.getOptimizationType(), data);
            return documentId;
        }

        @Override
        public Optional<OptimizationData> getOptimizationData(String documentId, String optimizationType) {
            return Optional.ofNullable(optimizationData.getOrDefault(documentId, Collections.emptyMap())
                    .get(optimizationType));
        }

        @Override
        public List<OptimizationData> getAllOptimizationData(String documentId) {
            return new ArrayList<>(optimizationData.getOrDefault(documentId, Collections.emptyMap()).values());
        }

        @Override
        public void deleteOptimizationData(String documentId, String optimizationType) {
            Map<String, OptimizationData> docData = optimizationData.get(documentId);
            if (docData != null) {
                docData.remove(optimizationType);
            }
        }

        @Override
        public void deleteAllOptimizationData(String documentId) {
            optimizationData.remove(documentId);
        }

        @Override
        public void saveMetadata(DocumentMetadata documentMetadata) {
            metadata.put(documentMetadata.getDocumentId(), documentMetadata);
        }

        @Override
        public Optional<DocumentMetadata> getMetadata(String documentId) {
            return Optional.ofNullable(metadata.get(documentId));
        }

        @Override
        public List<DocumentMetadata> getAllMetadata() {
            return new ArrayList<>(metadata.values());
        }

        @Override
        public void deleteMetadata(String documentId) {
            metadata.remove(documentId);
        }

        @Override
        public List<DocumentMetadata> listAllDocuments() {
            return new ArrayList<>(metadata.values());
        }

        @Override
        public List<DocumentMetadata> listDocuments(int offset, int limit) {
            return metadata.values().stream()
                    .skip(offset)
                    .limit(limit)
                    .collect(java.util.stream.Collectors.toList());
        }

        @Override
        public List<DocumentMetadata> searchDocuments(String keyword) {
            return metadata.values().stream()
                    .filter(m -> m.getFilename() != null && m.getFilename().contains(keyword))
                    .collect(java.util.stream.Collectors.toList());
        }

        @Override
        public long getDocumentCount() {
            return documents.size();
        }

        @Override
        public void cleanupDocument(String documentId) {
            deleteDocument(documentId);
            deleteExtractedText(documentId);
            deleteChunksByDocument(documentId);
            deleteImagesByDocument(documentId);
            deleteAllOptimizationData(documentId);
            deleteMetadata(documentId);
        }

        @Override
        public boolean documentExists(String documentId) {
            return documents.containsKey(documentId);
        }

        @Override
        public long getDocumentSize(String documentId) {
            byte[] data = documents.get(documentId);
            return data != null ? data.length : 0;
        }

        @Override
        public StorageStatistics getStatistics() {
            return StorageStatistics.builder()
                    .totalDocuments(documents.size())
                    .totalChunks(chunks.size())
                    .totalImages(images.size())
                    .totalSize(documents.values().stream().mapToLong(d -> d.length).sum())
                    .storageType("InMemory")
                    .healthy(true)
                    .build();
        }

        @Override
        public boolean isHealthy() {
            return true;
        }

        @Override
        public List<Map<String, Object>> listFiles(String virtualPath) {
            return Collections.emptyList();
        }

        @Override
        public byte[] readFile(String virtualPath) {
            return new byte[0];
        }

        @Override
        public boolean deleteFile(String virtualPath) {
            return false;
        }

        @Override
        public boolean createDirectory(String virtualPath) {
            return false;
        }

        @Override
        public Map<String, Object> getStorageStats(String virtualPath) {
            return Collections.emptyMap();
        }
    }
}

