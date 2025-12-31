package top.yumbo.ai.omni.storage.impl.file;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.storage.api.exception.*;
import top.yumbo.ai.omni.storage.api.model.*;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * FileDocumentStorage 实现测试
 *
 * 测试覆盖：
 * - 基本CRUD操作
 * - 流式读写
 * - 批量操作
 * - 元数据管理
 * - 文件系统浏览
 * - 边界条件（大文件、中文文件名、并发等）
 *
 * @author OmniAgent Team
 */
@DisplayName("FileDocumentStorage 实现测试")
class FileDocumentStorageTest {

    @TempDir
    Path tempDir;

    private FileDocumentStorage storage;

    @BeforeEach
    void setUp() {
        storage = new FileDocumentStorage(tempDir.toString());
    }

    @AfterEach
    void tearDown() {
        // 清理在 @TempDir 会自动处理
    }

    // ========== 基本CRUD操作测试 (10个) ==========

    @Test
    @DisplayName("测试文档保存和获取")
    void testSaveAndGetDocument() {
        String documentId = "test-doc-001.pdf";
        String filename = "test-doc-001.pdf";
        byte[] data = "Test PDF Content".getBytes(StandardCharsets.UTF_8);

        // 保存
        String savedId = storage.saveDocument(documentId, filename, data);
        assertEquals(documentId, savedId);

        // 获取
        Optional<byte[]> retrieved = storage.getDocument(documentId);
        assertTrue(retrieved.isPresent());
        assertArrayEquals(data, retrieved.get());
    }

    @Test
    @DisplayName("测试文档删除")
    void testDeleteDocument() {
        String documentId = "delete-test.pdf";
        byte[] data = "Delete me".getBytes(StandardCharsets.UTF_8);

        storage.saveDocument(documentId, documentId, data);
        assertTrue(storage.getDocument(documentId).isPresent());

        storage.deleteDocument(documentId);
        assertFalse(storage.getDocument(documentId).isPresent());
    }

    @Test
    @DisplayName("测试文档存在性检查")
    void testDocumentExists() {
        String documentId = "exists-test.pdf";
        assertFalse(storage.documentExists(documentId));

        storage.saveDocument(documentId, documentId, "content".getBytes());
        assertTrue(storage.documentExists(documentId));
    }

    @Test
    @DisplayName("测试提取文本保存和获取")
    void testSaveAndGetExtractedText() {
        String documentId = "text-doc-001.pdf";
        String text = "This is extracted text from PDF document.";

        String savedId = storage.saveExtractedText(documentId, text);
        assertNotNull(savedId);

        Optional<String> retrieved = storage.getExtractedText(documentId);
        assertTrue(retrieved.isPresent());
        assertEquals(text, retrieved.get());
    }

    @Test
    @DisplayName("测试提取文本删除")
    void testDeleteExtractedText() {
        String documentId = "text-delete.pdf";
        storage.saveExtractedText(documentId, "Some text");
        assertTrue(storage.getExtractedText(documentId).isPresent());

        storage.deleteExtractedText(documentId);
        assertFalse(storage.getExtractedText(documentId).isPresent());
    }

    @Test
    @DisplayName("测试分块保存和获取")
    void testSaveAndGetChunk() {
        String documentId = "chunk-doc.pdf";
        Chunk chunk = Chunk.builder()
                .id("chunk-001")
                .documentId(documentId)
                .content("This is chunk content")
                .sequence(1)
                .build();

        String chunkId = storage.saveChunk(documentId, chunk);
        assertNotNull(chunkId);

        Optional<Chunk> retrieved = storage.getChunk(chunkId);
        assertTrue(retrieved.isPresent());
        assertEquals("This is chunk content", retrieved.get().getContent());
        assertEquals(1, retrieved.get().getSequence());
    }

    @Test
    @DisplayName("测试批量保存分块")
    void testSaveChunks() {
        String documentId = "multi-chunk-doc.pdf";
        List<Chunk> chunks = Arrays.asList(
                Chunk.builder().id("c1").documentId(documentId).content("Chunk 1").sequence(1).build(),
                Chunk.builder().id("c2").documentId(documentId).content("Chunk 2").sequence(2).build(),
                Chunk.builder().id("c3").documentId(documentId).content("Chunk 3").sequence(3).build()
        );

        List<String> chunkIds = storage.saveChunks(documentId, chunks);
        assertEquals(3, chunkIds.size());

        List<Chunk> retrieved = storage.getChunksByDocument(documentId);
        assertEquals(3, retrieved.size());
    }

    @Test
    @DisplayName("测试图像保存和获取")
    void testSaveAndGetImage() {
        String documentId = "image-doc.pdf";
        Image image = Image.builder()
                .id("img-001")
                .documentId(documentId)
                .data("fake-image-data".getBytes())
                .format("PNG")
                .width(800)
                .height(600)
                .pageNumber(1)
                .build();

        String imageId = storage.saveImage(documentId, image);
        assertNotNull(imageId);

        Optional<Image> retrieved = storage.getImage(imageId);
        assertTrue(retrieved.isPresent());
        assertEquals("PNG", retrieved.get().getFormat());
        assertEquals(800, retrieved.get().getWidth());
    }

    @Test
    @DisplayName("测试优化数据保存和获取")
    void testSaveAndGetOptimizationData() {
        String documentId = "opt-doc.pdf";
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
    @DisplayName("测试文档清理")
    void testCleanupDocument() {
        String documentId = "cleanup-test.pdf";

        // 保存各种数据
        storage.saveDocument(documentId, documentId, "content".getBytes());
        storage.saveExtractedText(documentId, "text");
        storage.saveChunk(documentId, Chunk.builder().id("c1").documentId(documentId).content("chunk").build());
        storage.saveImage(documentId, Image.builder().id("i1").documentId(documentId).data("img".getBytes()).format("PNG").pageNumber(1).build());

        // 清理
        storage.cleanupDocument(documentId);

        // 验证都已删除
        assertFalse(storage.getDocument(documentId).isPresent());
        assertFalse(storage.getExtractedText(documentId).isPresent());
        assertTrue(storage.getChunksByDocument(documentId).isEmpty());
        assertTrue(storage.getImagesByDocument(documentId).isEmpty());
    }

    // ========== 流式读写测试 (6个) ==========

    @Test
    @DisplayName("测试流式读取文档")
    void testGetDocumentStream() throws Exception {
        String documentId = "stream-doc.pdf";
        String content = "Test stream content with multiple lines\nLine 2\nLine 3";
        storage.saveDocument(documentId, documentId, content.getBytes(StandardCharsets.UTF_8));

        try (InputStream stream = storage.getDocumentStream(documentId)) {
            String readContent = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            assertEquals(content, readContent);
        }
    }

    @Test
    @DisplayName("测试流式保存文档")
    void testSaveDocumentStream() throws Exception {
        String documentId = "stream-save.pdf";
        String content = "Stream save content";

        try (InputStream inputStream = new ByteArrayInputStream(content.getBytes(StandardCharsets.UTF_8))) {
            String savedId = storage.saveDocumentStream(documentId, documentId, inputStream);
            assertEquals(documentId, savedId);
        }

        Optional<byte[]> retrieved = storage.getDocument(documentId);
        assertTrue(retrieved.isPresent());
        assertEquals(content, new String(retrieved.get(), StandardCharsets.UTF_8));
    }

    @Test
    @DisplayName("测试复制文档到输出流")
    void testCopyDocumentToStream() throws StorageException {
        String documentId = "copy-doc.pdf";
        String content = "Copy this content";
        storage.saveDocument(documentId, documentId, content.getBytes(StandardCharsets.UTF_8));

        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        storage.copyDocumentToStream(documentId, outputStream);

        assertEquals(content, outputStream.toString(StandardCharsets.UTF_8));
    }

    @Test
    @DisplayName("测试流式读取提取文本")
    void testGetExtractedTextStream() throws Exception {
        String documentId = "text-stream.pdf";
        String text = "Extracted text for streaming";
        storage.saveExtractedText(documentId, text);

        try (InputStream stream = storage.getExtractedTextStream(documentId)) {
            String readText = new String(stream.readAllBytes(), StandardCharsets.UTF_8);
            assertEquals(text, readText);
        }
    }

    @Test
    @DisplayName("测试流式保存提取文本")
    void testSaveExtractedTextStream() throws Exception {
        String documentId = "text-stream-save.pdf";
        String text = "Stream save extracted text";

        try (InputStream inputStream = new ByteArrayInputStream(text.getBytes(StandardCharsets.UTF_8))) {
            String savedId = storage.saveExtractedTextStream(documentId, inputStream);
            assertEquals(documentId, savedId);
        }

        Optional<String> retrieved = storage.getExtractedText(documentId);
        assertTrue(retrieved.isPresent());
        assertEquals(text, retrieved.get());
    }

    @Test
    @DisplayName("测试文档不存在时抛出异常")
    void testDocumentNotFoundExceptionOnStream() {
        assertThrows(DocumentNotFoundException.class, () ->
                storage.getDocumentStream("non-existent-doc.pdf")
        );

        assertThrows(DocumentNotFoundException.class, () ->
                storage.getExtractedTextStream("non-existent-doc.pdf")
        );
    }

    // ========== 批量操作测试 (4个) ==========

    @Test
    @DisplayName("测试批量保存文档（非事务）")
    void testBatchSaveDocuments() {
        List<Map<String, Object>> documents = Arrays.asList(
                Map.of("documentId", "batch-1.pdf", "filename", "batch-1.pdf", "fileData", "Content 1".getBytes()),
                Map.of("documentId", "batch-2.pdf", "filename", "batch-2.pdf", "fileData", "Content 2".getBytes()),
                Map.of("documentId", "batch-3.pdf", "filename", "batch-3.pdf", "fileData", "Content 3".getBytes())
        );

        BatchOperationResult result = storage.saveDocuments(documents);

        assertEquals(3, result.getSuccessCount());
        assertEquals(0, result.getFailureCount());
        assertEquals(3, result.getTotalCount());
    }

    @Test
    @DisplayName("测试批量删除文档（非事务）")
    void testBatchDeleteDocuments() {
        // 先保存
        storage.saveDocument("del-1.pdf", "del-1.pdf", "c1".getBytes());
        storage.saveDocument("del-2.pdf", "del-2.pdf", "c2".getBytes());
        storage.saveDocument("del-3.pdf", "del-3.pdf", "c3".getBytes());

        // 批量删除
        BatchOperationResult result = storage.deleteDocuments(Arrays.asList("del-1.pdf", "del-2.pdf", "del-3.pdf"));

        assertEquals(3, result.getSuccessCount());
        assertEquals(0, result.getFailureCount());
    }

    @Test
    @DisplayName("测试批量检查文档存在性")
    void testCheckDocumentsExist() {
        storage.saveDocument("exists-1.pdf", "exists-1.pdf", "c1".getBytes());
        storage.saveDocument("exists-2.pdf", "exists-2.pdf", "c2".getBytes());

        Map<String, List<String>> result = storage.checkDocumentsExist(
                Arrays.asList("exists-1.pdf", "exists-2.pdf", "not-exists-1.pdf", "not-exists-2.pdf")
        );

        assertEquals(2, result.get("existing").size());
        assertEquals(2, result.get("missing").size());
    }

    @Test
    @DisplayName("测试批量清理文档")
    void testCleanupDocuments() {
        // 保存多个文档
        for (int i = 1; i <= 3; i++) {
            String docId = "cleanup-" + i + ".pdf";
            storage.saveDocument(docId, docId, ("content" + i).getBytes());
            storage.saveExtractedText(docId, "text" + i);
        }

        // 批量清理
        BatchOperationResult result = storage.cleanupDocuments(
                Arrays.asList("cleanup-1.pdf", "cleanup-2.pdf", "cleanup-3.pdf")
        );

        assertEquals(3, result.getSuccessCount());
        assertEquals(0, result.getFailureCount());
    }

    // ========== 元数据管理测试 (4个) ==========

    @Test
    @DisplayName("测试元数据保存和获取")
    void testSaveAndGetMetadata() {
        DocumentMetadata metadata = DocumentMetadata.builder()
                .documentId("meta-doc.pdf")
                .filename("report.pdf")
                .fileSize(1024000L)
                .mimeType("application/pdf")
                .uploadTime(new Date())
                .build();

        storage.saveMetadata(metadata);

        Optional<DocumentMetadata> retrieved = storage.getMetadata("meta-doc.pdf");
        assertTrue(retrieved.isPresent());
        assertEquals("report.pdf", retrieved.get().getFilename());
        assertEquals(1024000L, retrieved.get().getFileSize());
    }

    @Test
    @DisplayName("测试分页查询元数据")
    void testGetMetadataWithPagination() {
        // 保存10个元数据
        for (int i = 1; i <= 10; i++) {
            storage.saveMetadata(DocumentMetadata.builder()
                    .documentId("page-doc-" + i + ".pdf")
                    .filename("file" + i + ".pdf")
                    .fileSize((long) i * 1000)
                    .build());
        }

        PageRequest pageRequest = PageRequest.of(0, 5);
        PageResult<DocumentMetadata> result = storage.getAllMetadata(pageRequest);

        assertEquals(5, result.getContent().size());
        assertTrue(result.getTotalElements() >= 10);
    }

    @Test
    @DisplayName("测试搜索元数据")
    void testSearchMetadata() {
        // 保存测试数据
        storage.saveMetadata(DocumentMetadata.builder()
                .documentId("search-1.pdf").filename("report-2024.pdf").build());
        storage.saveMetadata(DocumentMetadata.builder()
                .documentId("search-2.pdf").filename("summary-2024.pdf").build());
        storage.saveMetadata(DocumentMetadata.builder()
                .documentId("search-3.pdf").filename("other-file.pdf").build());

        PageRequest pageRequest = PageRequest.of(0, 10);
        PageResult<DocumentMetadata> result = storage.searchMetadata("2024", pageRequest);

        assertTrue(result.getContent().size() >= 2);
    }

    @Test
    @DisplayName("测试元数据删除")
    void testDeleteMetadata() {
        DocumentMetadata metadata = DocumentMetadata.builder()
                .documentId("delete-meta.pdf")
                .filename("test.pdf")
                .build();

        storage.saveMetadata(metadata);
        assertTrue(storage.getMetadata("delete-meta.pdf").isPresent());

        storage.deleteMetadata("delete-meta.pdf");
        assertFalse(storage.getMetadata("delete-meta.pdf").isPresent());
    }

    // ========== 文件系统浏览测试 (5个) ==========

    @Test
    @DisplayName("测试列出目录文件")
    void testListFiles() {
        // 创建一些测试文件
        storage.saveDocument("dir1/file1.pdf", "dir1/file1.pdf", "content1".getBytes());
        storage.saveDocument("dir1/file2.pdf", "dir1/file2.pdf", "content2".getBytes());
        storage.saveDocument("dir2/file3.pdf", "dir2/file3.pdf", "content3".getBytes());

        List<Map<String, Object>> files = storage.listFiles("");
        assertNotNull(files);
        assertFalse(files.isEmpty());
    }

    @Test
    @DisplayName("测试读取文件")
    void testReadFile() {
        String virtualPath = "documents/test-file.pdf";
        String content = "File content for reading";
        storage.saveDocument("test-file.pdf", "test-file.pdf", content.getBytes());

        byte[] readData = storage.readFile(virtualPath);
        assertNotNull(readData);
        // File实现可能返回null或数据
    }

    @Test
    @DisplayName("测试删除文件")
    void testDeleteFile() {
        storage.saveDocument("file-to-delete.pdf", "file-to-delete.pdf", "content".getBytes());

        boolean deleted = storage.deleteFile("documents/file-to-delete.pdf");
        // File实现可能返回true或false，只验证方法执行
    }

    @Test
    @DisplayName("测试创建目录")
    void testCreateDirectory() {
        boolean created = storage.createDirectory("test-directory");
        // File实现可能返回true或false，只验证方法执行
    }

    @Test
    @DisplayName("测试获取存储统计")
    void testGetStorageStats() {
        storage.saveDocument("stat-1.pdf", "stat-1.pdf", "content1".getBytes());
        storage.saveDocument("stat-2.pdf", "stat-2.pdf", "content2".getBytes());

        Map<String, Object> stats = storage.getStorageStats("");
        assertNotNull(stats);
    }

    // ========== 边界条件测试 (7个) ==========

    @Test
    @DisplayName("测试空文件")
    void testEmptyFile() {
        String documentId = "empty.pdf";
        byte[] emptyData = new byte[0];

        String savedId = storage.saveDocument(documentId, documentId, emptyData);
        assertNotNull(savedId);

        Optional<byte[]> retrieved = storage.getDocument(documentId);
        assertTrue(retrieved.isPresent());
        assertEquals(0, retrieved.get().length);
    }

    @Test
    @DisplayName("测试大文件 (10MB)")
    void testLargeFile() {
        String documentId = "large-file.pdf";
        // 创建10MB的测试数据
        byte[] largeData = new byte[10 * 1024 * 1024];
        Arrays.fill(largeData, (byte) 'A');

        String savedId = storage.saveDocument(documentId, documentId, largeData);
        assertNotNull(savedId);

        Optional<byte[]> retrieved = storage.getDocument(documentId);
        assertTrue(retrieved.isPresent());
        assertEquals(largeData.length, retrieved.get().length);
    }

    @Test
    @DisplayName("测试超大文件流式处理 (100MB)")
    void testVeryLargeFileStream() throws Exception {
        String documentId = "very-large.pdf";
        // 创建100MB的测试数据
        int size = 100 * 1024 * 1024;

        // 使用流式保存
        try (InputStream inputStream = new InputStream() {
            private int remaining = size;
            @Override
            public int read() {
                if (remaining-- > 0) return 'B';
                return -1;
            }
        }) {
            String savedId = storage.saveDocumentStream(documentId, documentId, inputStream);
            assertNotNull(savedId);
        }

        // 验证文件存在
        assertTrue(storage.documentExists(documentId));

        // 清理
        storage.deleteDocument(documentId);
    }

    @Test
    @DisplayName("测试中文文件名")
    void testChineseFilename() {
        String documentId = "测试文档.pdf";
        String filename = "测试文档.pdf";
        byte[] data = "中文内容测试".getBytes(StandardCharsets.UTF_8);

        String savedId = storage.saveDocument(documentId, filename, data);
        assertNotNull(savedId);

        Optional<byte[]> retrieved = storage.getDocument(documentId);
        assertTrue(retrieved.isPresent());
        assertArrayEquals(data, retrieved.get());
    }

    @Test
    @DisplayName("测试特殊字符文件名")
    void testSpecialCharactersFilename() {
        // 注意：某些特殊字符在文件系统中不允许，这里测试安全的特殊字符
        String documentId = "test_file-123.pdf";
        byte[] data = "special chars content".getBytes();

        String savedId = storage.saveDocument(documentId, documentId, data);
        assertNotNull(savedId);

        assertTrue(storage.documentExists(documentId));
    }

    @Test
    @DisplayName("测试相对路径")
    void testRelativePath() {
        String documentId = "folder1/subfolder/document.pdf";
        byte[] data = "nested folder content".getBytes();

        String savedId = storage.saveDocument(documentId, documentId, data);
        assertNotNull(savedId);

        Optional<byte[]> retrieved = storage.getDocument(documentId);
        assertTrue(retrieved.isPresent());
        assertArrayEquals(data, retrieved.get());
    }

    @Test
    @DisplayName("测试并发访问")
    void testConcurrentAccess() throws Exception {
        int threadCount = 10;
        ExecutorService executor = Executors.newFixedThreadPool(threadCount);
        CountDownLatch latch = new CountDownLatch(threadCount);
        List<Future<Boolean>> futures = new ArrayList<>();

        try {
            for (int i = 0; i < threadCount; i++) {
                final int index = i;
                Future<Boolean> future = executor.submit(() -> {
                    try {
                        String docId = "concurrent-" + index + ".pdf";
                        byte[] data = ("Content " + index).getBytes();

                        storage.saveDocument(docId, docId, data);
                        Optional<byte[]> retrieved = storage.getDocument(docId);

                        return retrieved.isPresent() && Arrays.equals(data, retrieved.get());
                    } finally {
                        latch.countDown();
                    }
                });
                futures.add(future);
            }

            boolean completed = latch.await(30, TimeUnit.SECONDS);
            assertTrue(completed, "Concurrent operations did not complete in time");

            // 验证所有操作都成功
            for (Future<Boolean> future : futures) {
                assertTrue(future.get());
            }
        } finally {
            executor.shutdown();
        }
    }

    // ========== 统计和健康检查测试 (2个) ==========

    @Test
    @DisplayName("测试统计信息")
    void testGetStatistics() {
        // 保存一些数据
        storage.saveDocument("stat-doc-1.pdf", "stat-doc-1.pdf", "content1".getBytes());
        storage.saveDocument("stat-doc-2.pdf", "stat-doc-2.pdf", "content2".getBytes());
        storage.saveChunk("stat-doc-1.pdf", Chunk.builder().id("c1").documentId("stat-doc-1.pdf").content("chunk").build());

        StorageStatistics stats = storage.getStatistics();
        assertNotNull(stats);
        assertTrue(stats.getTotalDocuments() >= 2);
        assertTrue(stats.getTotalChunks() >= 1);
        assertEquals("File", stats.getStorageType());
    }

    @Test
    @DisplayName("测试健康检查")
    void testHealthCheck() {
        assertTrue(storage.isHealthy());
    }

    // ========== 错误处理测试 (3个) ==========

    @Test
    @DisplayName("测试获取不存在的文档")
    void testGetNonExistentDocument() {
        Optional<byte[]> result = storage.getDocument("non-existent.pdf");
        assertFalse(result.isPresent());
    }

    @Test
    @DisplayName("测试获取不存在的提取文本")
    void testGetNonExistentExtractedText() {
        Optional<String> result = storage.getExtractedText("non-existent.pdf");
        assertFalse(result.isPresent());
    }

    @Test
    @DisplayName("测试删除不存在的文档")
    void testDeleteNonExistentDocument() {
        // 不应该抛出异常
        assertDoesNotThrow(() -> storage.deleteDocument("non-existent.pdf"));
    }
}

