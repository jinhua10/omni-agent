package top.yumbo.ai.example;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.web.client.TestRestTemplate;
import org.springframework.boot.test.web.server.LocalServerPort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.storage.api.DocumentStorageService;
import top.yumbo.ai.rag.api.RAGService;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 集成测试 - 组合1: 开发环境
 *
 * 配置: Memory + File + Lucene + Ollama
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@DisplayName("集成测试 - 开发环境配置")
class BasicExampleIntegrationTest {

    @LocalServerPort
    private int port;

    @Autowired
    private TestRestTemplate restTemplate;

    @Autowired(required = false)
    private QuestionClassifierPersistence persistence;

    @Autowired(required = false)
    private DocumentStorageService storageService;

    @Autowired(required = false)
    private RAGService ragService;

    @Test
    @DisplayName("测试应用启动成功")
    void testApplicationStarts() {
        assertNotNull(persistence, "Persistence 服务应该已注入");
        assertNotNull(storageService, "Storage 服务应该已注入");
        assertNotNull(ragService, "RAG 服务应该已注入");
    }

    @Test
    @DisplayName("测试健康检查端点")
    void testHealthEndpoint() {
        String url = "http://localhost:" + port + "/health";
        ResponseEntity<String> response = restTemplate.getForEntity(url, String.class);

        assertEquals(HttpStatus.OK, response.getStatusCode(), "健康检查应该返回200");
        assertNotNull(response.getBody(), "响应体不应该为空");
        assertTrue(response.getBody().contains("status"), "响应应该包含status字段");
    }

    @Test
    @DisplayName("测试存储统计端点")
    void testStorageStatsEndpoint() {
        String url = "http://localhost:" + port + "/storage/stats";
        ResponseEntity<String> response = restTemplate.getForEntity(url, String.class);

        assertEquals(HttpStatus.OK, response.getStatusCode(), "存储统计应该返回200");
        assertNotNull(response.getBody(), "响应体不应该为空");
    }

    @Test
    @DisplayName("测试RAG统计端点")
    void testRagStatsEndpoint() {
        String url = "http://localhost:" + port + "/rag/stats";
        ResponseEntity<String> response = restTemplate.getForEntity(url, String.class);

        assertEquals(HttpStatus.OK, response.getStatusCode(), "RAG统计应该返回200");
        assertNotNull(response.getBody(), "响应体不应该为空");
    }

    @Test
    @DisplayName("测试四维服务都已正确注入")
    void testAllServicesInjected() {
        // 验证四个维度的服务都已注入
        assertNotNull(persistence, "Persistence 服务应该已注入");
        assertNotNull(storageService, "Storage 服务应该已注入");
        assertNotNull(ragService, "RAG 服务应该已注入");

        // 可以添加更多验证
        assertDoesNotThrow(() -> {
            storageService.getStatistics();
        }, "调用 Storage 统计不应该抛出异常");

        assertDoesNotThrow(() -> {
            ragService.getStatistics();
        }, "调用 RAG 统计不应该抛出异常");
    }

    @Test
    @DisplayName("测试Memory Persistence正常工作")
    void testMemoryPersistenceWorks() {
        // 这个测试验证Memory实现已正确加载
        assertNotNull(persistence, "Persistence应该已注入");

        // 简单验证可以调用方法（不抛异常）
        assertDoesNotThrow(() -> {
            persistence.getAllQuestionTypes();
        }, "调用getAllQuestionTypes不应该抛出异常");
    }

    @Test
    @DisplayName("测试File Storage正常工作")
    void testFileStorageWorks() {
        assertNotNull(storageService, "Storage服务应该已注入");

        // 验证可以获取统计信息
        assertDoesNotThrow(() -> {
            storageService.getStatistics();
        }, "调用getStatistics不应该抛出异常");
    }

    @Test
    @DisplayName("测试Lucene RAG正常工作")
    void testLuceneRagWorks() {
        assertNotNull(ragService, "RAG服务应该已注入");

        // 验证可以获取统计信息
        assertDoesNotThrow(() -> {
            ragService.getStatistics();
        }, "调用getStatistics不应该抛出异常");
    }
}

