package top.yumbo.ai.omni.storage;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;
import top.yumbo.ai.omni.chunking.Chunk;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.model.*;
import top.yumbo.ai.omni.storage.impl.file.FileDocumentStorage;

import java.nio.charset.StandardCharsets;
import java.nio.file.Path;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * DocumentStorageRegistry 集成测试
 *
 * 测试多实例管理、实例切换、故障转移等场景
 *
 * @author OmniAgent Team
 */
@DisplayName("DocumentStorageRegistry 集成测试")
class DocumentStorageRegistryTest {

    @TempDir
    Path tempDir1;

    @TempDir
    Path tempDir2;

    @TempDir
    Path tempDir3;

    private DocumentStorageRegistry registry;
    private FileDocumentStorage storage1;
    private FileDocumentStorage storage2;
    private FileDocumentStorage storage3;
    private Map<String, DocumentStorageService> servicesMap;

    @BeforeEach
    void setUp() {
        // 创建3个不同的File存储实例
        storage1 = new FileDocumentStorage(tempDir1.toString());
        storage2 = new FileDocumentStorage(tempDir2.toString());
        storage3 = new FileDocumentStorage(tempDir3.toString());

        // 创建服务映射
        servicesMap = new HashMap<>();
        servicesMap.put("storage1", storage1);
        servicesMap.put("storage2", storage2);
        servicesMap.put("storage3", storage3);

        // 创建注册表
        registry = new DocumentStorageRegistry(servicesMap);
    }

    @AfterEach
    void tearDown() {
        // 清理
        registry = null;
    }

    // ========== 基本注册和获取测试 (5个) ==========

    @Test
    @DisplayName("测试注册存储实例")
    void testRegisterStorage() {
        Map<String, DocumentStorageService> newServicesMap = new HashMap<>();
        FileDocumentStorage newStorage = new FileDocumentStorage(tempDir1.toString());
        newServicesMap.put("test-storage", newStorage);

        DocumentStorageRegistry newRegistry = new DocumentStorageRegistry(newServicesMap);

        Optional<DocumentStorageService> retrieved = newRegistry.getService("test-storage");
        assertTrue(retrieved.isPresent());
        assertSame(newStorage, retrieved.get());
    }

    @Test
    @DisplayName("测试获取已注册的实例")
    void testGetRegisteredStorage() {
        Optional<DocumentStorageService> retrieved1 = registry.getService("storage1");
        Optional<DocumentStorageService> retrieved2 = registry.getService("storage2");
        Optional<DocumentStorageService> retrieved3 = registry.getService("storage3");

        assertTrue(retrieved1.isPresent());
        assertTrue(retrieved2.isPresent());
        assertTrue(retrieved3.isPresent());
        assertSame(storage1, retrieved1.get());
        assertSame(storage2, retrieved2.get());
        assertSame(storage3, retrieved3.get());
    }

    @Test
    @DisplayName("测试获取不存在的实例")
    void testGetNonExistentStorage() {
        Optional<DocumentStorageService> result = registry.getService("non-existent");
        assertFalse(result.isPresent());
    }

    @Test
    @DisplayName("测试使用getServiceOrThrow获取实例")
    void testGetServiceOrThrow() {
        // 存在的实例
        DocumentStorageService storage = registry.getServiceOrThrow("storage1");
        assertNotNull(storage);
        assertSame(storage1, storage);

        // 不存在的实例应抛出异常
        assertThrows(IllegalArgumentException.class, () ->
            registry.getServiceOrThrow("non-existent")
        );
    }

    @Test
    @DisplayName("测试获取所有已注册实例ID")
    void testGetAllInstanceIds() {
        Set<String> instanceIds = registry.getInstanceIds();

        assertNotNull(instanceIds);
        assertEquals(3, instanceIds.size());
        assertTrue(instanceIds.contains("storage1"));
        assertTrue(instanceIds.contains("storage2"));
        assertTrue(instanceIds.contains("storage3"));
    }

    @Test
    @DisplayName("测试检查实例是否存在")
    void testHasInstance() {
        assertTrue(registry.hasInstance("storage1"));
        assertTrue(registry.hasInstance("storage2"));
        assertTrue(registry.hasInstance("storage3"));
        assertFalse(registry.hasInstance("non-existent"));
    }

    @Test
    @DisplayName("测试获取实例数量")
    void testSize() {
        assertEquals(3, registry.size());
    }

    @Test
    @DisplayName("测试获取所有服务")
    void testGetAllServices() {
        Map<String, DocumentStorageService> allServices = registry.getAllServices();

        assertNotNull(allServices);
        assertEquals(3, allServices.size());
        assertTrue(allServices.containsKey("storage1"));
        assertTrue(allServices.containsKey("storage2"));
        assertTrue(allServices.containsKey("storage3"));
    }

    // ========== 实例隔离测试 (3个) ==========

    @Test
    @DisplayName("测试不同实例间的数据隔离")
    void testDataIsolationBetweenInstances() {
        String documentId = "test-doc.pdf";
        byte[] data1 = "Data in storage1".getBytes(StandardCharsets.UTF_8);
        byte[] data2 = "Data in storage2".getBytes(StandardCharsets.UTF_8);

        // 在不同实例中保存不同数据
        storage1.saveDocument(documentId, documentId, data1);
        storage2.saveDocument(documentId, documentId, data2);

        // 验证数据隔离
        Optional<byte[]> retrieved1 = storage1.getDocument(documentId);
        Optional<byte[]> retrieved2 = storage2.getDocument(documentId);

        assertTrue(retrieved1.isPresent());
        assertTrue(retrieved2.isPresent());
        assertArrayEquals(data1, retrieved1.get());
        assertArrayEquals(data2, retrieved2.get());

        // 验证storage3中不存在
        assertFalse(storage3.getDocument(documentId).isPresent());
    }

    @Test
    @DisplayName("测试元数据隔离")
    void testMetadataIsolation() {
        DocumentMetadata meta1 = DocumentMetadata.builder()
                .documentId("meta-doc-1")
                .filename("file1.pdf")
                .fileSize(1000L)
                .build();

        DocumentMetadata meta2 = DocumentMetadata.builder()
                .documentId("meta-doc-1") // 同样的ID
                .filename("file2.pdf")
                .fileSize(2000L)
                .build();

        storage1.saveMetadata(meta1);
        storage2.saveMetadata(meta2);

        Optional<DocumentMetadata> retrieved1 = storage1.getMetadata("meta-doc-1");
        Optional<DocumentMetadata> retrieved2 = storage2.getMetadata("meta-doc-1");

        assertTrue(retrieved1.isPresent());
        assertTrue(retrieved2.isPresent());
        assertEquals("file1.pdf", retrieved1.get().getFilename());
        assertEquals("file2.pdf", retrieved2.get().getFilename());
        assertEquals(1000L, retrieved1.get().getFileSize());
        assertEquals(2000L, retrieved2.get().getFileSize());
    }

    @Test
    @DisplayName("测试分块隔离")
    void testChunkIsolation() {
        String documentId = "chunk-doc";

        Chunk chunk1 = Chunk.builder()
                .id("chunk-1")
                .documentId(documentId)
                .content("Content in storage1")
                .build();

        Chunk chunk2 = Chunk.builder()
                .id("chunk-1") // 同样的ID
                .documentId(documentId)
                .content("Content in storage2")
                .build();

        storage1.saveChunk(documentId, chunk1);
        storage2.saveChunk(documentId, chunk2);

        Optional<Chunk> retrieved1 = storage1.getChunk("chunk-1");
        Optional<Chunk> retrieved2 = storage2.getChunk("chunk-1");

        assertTrue(retrieved1.isPresent());
        assertTrue(retrieved2.isPresent());
        assertEquals("Content in storage1", retrieved1.get().getContent());
        assertEquals("Content in storage2", retrieved2.get().getContent());
    }

    // ========== 实例切换测试 (2个) ==========

    @Test
    @DisplayName("测试在不同实例间切换操作")
    void testSwitchBetweenInstances() {
        String documentId = "switch-doc.pdf";

        // 在storage1中保存
        DocumentStorageService current = registry.getServiceOrThrow("storage1");
        current.saveDocument(documentId, documentId, "Data 1".getBytes());
        assertTrue(current.documentExists(documentId));

        // 切换到storage2
        current = registry.getServiceOrThrow("storage2");
        assertFalse(current.documentExists(documentId)); // storage2中不存在

        // 在storage2中保存
        current.saveDocument(documentId, documentId, "Data 2".getBytes());
        assertTrue(current.documentExists(documentId));

        // 切回storage1，验证数据仍然存在
        current = registry.getServiceOrThrow("storage1");
        assertTrue(current.documentExists(documentId));
        Optional<byte[]> data = current.getDocument(documentId);
        assertTrue(data.isPresent());
        assertEquals("Data 1", new String(data.get(), StandardCharsets.UTF_8));
    }

    @Test
    @DisplayName("测试动态切换实例处理不同文档")
    void testDynamicInstanceSwitchForDifferentDocuments() {
        Map<String, String> documentToStorage = Map.of(
            "doc1.pdf", "storage1",
            "doc2.pdf", "storage2",
            "doc3.pdf", "storage3",
            "doc4.pdf", "storage1",
            "doc5.pdf", "storage2"
        );

        // 根据映射保存文档到不同实例
        documentToStorage.forEach((docId, storageName) -> {
            DocumentStorageService storage = registry.getServiceOrThrow(storageName);
            storage.saveDocument(docId, docId, ("Content of " + docId).getBytes());
        });

        // 验证每个文档在正确的实例中
        documentToStorage.forEach((docId, storageName) -> {
            DocumentStorageService storage = registry.getServiceOrThrow(storageName);
            assertTrue(storage.documentExists(docId),
                    "Document " + docId + " should exist in " + storageName);
        });

        // 验证文档不在其他实例中
        assertTrue(storage1.documentExists("doc1.pdf"));
        assertFalse(storage2.documentExists("doc1.pdf"));
        assertFalse(storage3.documentExists("doc1.pdf"));
    }

    // ========== 故障模拟和恢复测试 (2个) ==========

    @Test
    @DisplayName("测试主实例故障时切换到备用实例")
    void testFailoverToBackupInstance() {
        // 模拟主实例（storage1）和备用实例（storage2）
        String documentId = "failover-doc.pdf";
        byte[] data = "Important data".getBytes();

        // 在主实例中保存数据
        storage1.saveDocument(documentId, documentId, data);

        // 同时在备用实例中备份（实际场景中可能通过复制实现）
        storage2.saveDocument(documentId, documentId, data);

        // 模拟主实例故障（这里通过切换到备用实例模拟）
        DocumentStorageService primary = registry.getServiceOrThrow("storage1");
        DocumentStorageService backup = registry.getServiceOrThrow("storage2");

        // 验证备用实例有数据
        assertTrue(backup.documentExists(documentId));
        Optional<byte[]> backupData = backup.getDocument(documentId);
        assertTrue(backupData.isPresent());
        assertArrayEquals(data, backupData.get());
    }

    @Test
    @DisplayName("测试实例不可用时的优雅降级")
    void testGracefulDegradationWhenInstanceUnavailable() {
        // 获取不存在的实例
        Optional<DocumentStorageService> unavailable = registry.getService("unavailable-storage");
        assertFalse(unavailable.isPresent());

        // 应用层应该有降级策略（这里测试检测逻辑）
        if (unavailable.isEmpty()) {
            // 降级到已知可用的实例
            DocumentStorageService fallback = registry.getServiceOrThrow("storage1");
            assertNotNull(fallback);

            // 使用降级实例
            fallback.saveDocument("fallback-doc.pdf", "fallback-doc.pdf", "Fallback data".getBytes());
            assertTrue(fallback.documentExists("fallback-doc.pdf"));
        }
    }

    @Test
    @DisplayName("测试所有实例健康检查")
    void testHealthCheckAllInstances() {
        Map<String, Boolean> healthStatus = new HashMap<>();

        for (String instanceId : registry.getInstanceIds()) {
            DocumentStorageService storage = registry.getServiceOrThrow(instanceId);
            boolean healthy = storage.isHealthy();
            healthStatus.put(instanceId, healthy);
        }

        // 验证所有实例都健康
        assertEquals(3, healthStatus.size());
        assertTrue(healthStatus.get("storage1"));
        assertTrue(healthStatus.get("storage2"));
        assertTrue(healthStatus.get("storage3"));
    }

    // ========== 负载分配测试 (2个) ==========

    @Test
    @DisplayName("测试轮询方式分配文档到不同实例")
    void testRoundRobinDistribution() {
        List<String> instanceIds = List.of("storage1", "storage2", "storage3");
        int documentCount = 15;

        // 轮询分配
        for (int i = 0; i < documentCount; i++) {
            String instanceId = instanceIds.get(i % instanceIds.size());
            DocumentStorageService storage = registry.getServiceOrThrow(instanceId);

            String docId = "round-robin-doc-" + i + ".pdf";
            storage.saveDocument(docId, docId, ("Content " + i).getBytes());
        }

        // 验证分配均匀（每个实例应该有5个文档）
        StorageStatistics stats1 = storage1.getStatistics();
        StorageStatistics stats2 = storage2.getStatistics();
        StorageStatistics stats3 = storage3.getStatistics();

        assertEquals(5, stats1.getTotalDocuments());
        assertEquals(5, stats2.getTotalDocuments());
        assertEquals(5, stats3.getTotalDocuments());
    }

    @Test
    @DisplayName("测试基于文档大小的智能分配")
    void testSizeBasedDistribution() {
        // 小文件分配到storage1，大文件分配到storage2
        List<Map<String, Object>> documents = List.of(
            Map.of("id", "small-1.pdf", "size", 1024),
            Map.of("id", "small-2.pdf", "size", 2048),
            Map.of("id", "large-1.pdf", "size", 1024 * 1024),
            Map.of("id", "large-2.pdf", "size", 2 * 1024 * 1024)
        );

        int sizeThreshold = 100 * 1024; // 100KB

        for (Map<String, Object> doc : documents) {
            String docId = (String) doc.get("id");
            int size = (int) doc.get("size");
            byte[] data = new byte[size];
            Arrays.fill(data, (byte) 'A');

            DocumentStorageService targetStorage;
            if (size < sizeThreshold) {
                targetStorage = registry.getServiceOrThrow("storage1");
            } else {
                targetStorage = registry.getServiceOrThrow("storage2");
            }

            targetStorage.saveDocument(docId, docId, data);
        }

        // 验证分配正确
        assertTrue(storage1.documentExists("small-1.pdf"));
        assertTrue(storage1.documentExists("small-2.pdf"));
        assertTrue(storage2.documentExists("large-1.pdf"));
        assertTrue(storage2.documentExists("large-2.pdf"));

        assertFalse(storage1.documentExists("large-1.pdf"));
        assertFalse(storage2.documentExists("small-1.pdf"));
    }

    // ========== 复杂场景测试 (2个) ==========

    @Test
    @DisplayName("测试多实例协同工作场景")
    void testMultiInstanceCollaboration() {
        String documentId = "collab-doc.pdf";

        // 场景：文档在storage1，分块在storage2，图像在storage3
        // 1. 保存原始文档到storage1
        byte[] docData = "Original document content".getBytes();
        storage1.saveDocument(documentId, documentId, docData);

        // 2. 保存分块到storage2
        List<Chunk> chunks = List.of(
            Chunk.builder().id("chunk-1").documentId(documentId).content("Chunk 1").sequence(1).build(),
            Chunk.builder().id("chunk-2").documentId(documentId).content("Chunk 2").sequence(2).build()
        );
        storage2.saveChunks(documentId, chunks);

        // 3. 保存图像到storage3
        Image image = Image.builder()
                .id("img-1")
                .documentId(documentId)
                .data("image-data".getBytes())
                .format("PNG")
                .pageNumber(1)
                .build();
        storage3.saveImage(documentId, image);

        // 验证数据分布
        assertTrue(storage1.documentExists(documentId));
        assertEquals(2, storage2.getChunksByDocument(documentId).size());
        assertEquals(1, storage3.getImagesByDocument(documentId).size());
    }

    @Test
    @DisplayName("测试跨实例数据迁移")
    void testCrossInstanceDataMigration() {
        String documentId = "migrate-doc.pdf";
        byte[] data = "Data to migrate".getBytes();

        // 1. 在storage1中保存
        storage1.saveDocument(documentId, documentId, data);
        storage1.saveExtractedText(documentId, "Extracted text");

        // 2. 迁移到storage2（读取+保存+删除）
        Optional<byte[]> docData = storage1.getDocument(documentId);
        Optional<String> textData = storage1.getExtractedText(documentId);

        assertTrue(docData.isPresent());
        assertTrue(textData.isPresent());

        storage2.saveDocument(documentId, documentId, docData.get());
        storage2.saveExtractedText(documentId, textData.get());

        // 3. 从storage1删除
        storage1.deleteDocument(documentId);
        storage1.deleteExtractedText(documentId);

        // 4. 验证迁移成功
        assertFalse(storage1.documentExists(documentId));
        assertTrue(storage2.documentExists(documentId));

        Optional<byte[]> migratedDoc = storage2.getDocument(documentId);
        assertTrue(migratedDoc.isPresent());
        assertArrayEquals(data, migratedDoc.get());

        Optional<String> migratedText = storage2.getExtractedText(documentId);
        assertTrue(migratedText.isPresent());
        assertEquals("Extracted text", migratedText.get());
    }

    // ========== 统计和监控测试 (2个) ==========

    @Test
    @DisplayName("测试聚合所有实例的统计信息")
    void testAggregatedStatistics() {
        // 在不同实例中保存数据
        storage1.saveDocument("doc-1.pdf", "doc-1.pdf", "content".getBytes());
        storage1.saveDocument("doc-2.pdf", "doc-2.pdf", "content".getBytes());

        storage2.saveDocument("doc-3.pdf", "doc-3.pdf", "content".getBytes());
        storage2.saveChunk("doc-3.pdf", Chunk.builder().id("c1").documentId("doc-3.pdf").content("chunk").build());

        storage3.saveImage("doc-4.pdf", Image.builder().id("i1").documentId("doc-4.pdf").data("img".getBytes()).format("PNG").pageNumber(1).build());

        // 聚合统计
        long totalDocuments = 0;
        long totalChunks = 0;
        long totalImages = 0;

        for (String instanceId : registry.getInstanceIds()) {
            DocumentStorageService storage = registry.getServiceOrThrow(instanceId);
            StorageStatistics stats = storage.getStatistics();
            totalDocuments += stats.getTotalDocuments();
            totalChunks += stats.getTotalChunks();
            totalImages += stats.getTotalImages();
        }

        assertEquals(3, totalDocuments);
        assertEquals(1, totalChunks);
        assertEquals(1, totalImages);
    }

    @Test
    @DisplayName("测试监控所有实例的健康状态")
    void testMonitorAllInstancesHealth() {
        Map<String, Map<String, Object>> healthReport = new HashMap<>();

        for (String instanceId : registry.getInstanceIds()) {
            DocumentStorageService storage = registry.getServiceOrThrow(instanceId);

            Map<String, Object> instanceReport = new HashMap<>();
            instanceReport.put("healthy", storage.isHealthy());
            instanceReport.put("statistics", storage.getStatistics());

            healthReport.put(instanceId, instanceReport);
        }

        // 验证报告完整
        assertEquals(3, healthReport.size());
        assertTrue((Boolean) healthReport.get("storage1").get("healthy"));
        assertTrue((Boolean) healthReport.get("storage2").get("healthy"));
        assertTrue((Boolean) healthReport.get("storage3").get("healthy"));

        assertNotNull(healthReport.get("storage1").get("statistics"));
        assertNotNull(healthReport.get("storage2").get("statistics"));
        assertNotNull(healthReport.get("storage3").get("statistics"));
    }
}

