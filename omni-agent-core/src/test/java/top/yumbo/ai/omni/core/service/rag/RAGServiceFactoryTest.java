package top.yumbo.ai.omni.core.service.rag;

import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import top.yumbo.ai.omni.rag.RagService;

import static org.junit.jupiter.api.Assertions.*;

/**
 * RAG服务工厂测试
 * (RAG Service Factory Tests)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@SpringBootTest
@ActiveProfiles("test")
class RAGServiceFactoryTest {

    @Autowired(required = false)
    private RAGServiceFactory ragServiceFactory;

    @Test
    void testRAGServiceFactoryAvailable() {
        if (ragServiceFactory == null) {
            log.warn("⚠️ RAGServiceFactory 未配置（可能是测试环境）");
            return;
        }

        log.info("✅ RAGServiceFactory 已注入");
        assertTrue(ragServiceFactory.isRAGServiceAvailable(),
                "RAG服务应该可用");
    }

    @Test
    void testGetDefaultRAGService() {
        if (ragServiceFactory == null) {
            log.warn("⚠️ 跳过测试：RAGServiceFactory 未配置");
            return;
        }

        RagService RagService = ragServiceFactory.getDefaultRAGService();
        assertNotNull(RagService, "默认RAG服务不应为null");

        log.info("✅ 获取默认RAG服务成功: {}", RagService.getClass().getSimpleName());
    }

    @Test
    void testGetOrCreateRAGServiceForDomain() {
        if (ragServiceFactory == null) {
            log.warn("⚠️ 跳过测试：RAGServiceFactory 未配置");
            return;
        }

        String testDomainId = "test-domain-001";

        // 第一次获取（创建）
        RagService ragService1 = ragServiceFactory.getOrCreateRAGService(testDomainId);
        assertNotNull(ragService1, "域RAG服务不应为null");

        // 第二次获取（从缓存）
        RagService ragService2 = ragServiceFactory.getOrCreateRAGService(testDomainId);
        assertSame(ragService1, ragService2, "应该返回相同的实例");

        assertTrue(ragServiceFactory.hasDomainRAGService(testDomainId),
                "域应该已经有RAG服务");

        log.info("✅ 域RAG服务创建和缓存成功");
    }

    @Test
    void testRemoveDomainRAGService() {
        if (ragServiceFactory == null) {
            log.warn("⚠️ 跳过测试：RAGServiceFactory 未配置");
            return;
        }

        String testDomainId = "test-domain-002";

        // 创建域RAG服务
        ragServiceFactory.getOrCreateRAGService(testDomainId);
        assertTrue(ragServiceFactory.hasDomainRAGService(testDomainId));

        // 移除域RAG服务
        ragServiceFactory.removeDomainRAGService(testDomainId);
        assertFalse(ragServiceFactory.hasDomainRAGService(testDomainId),
                "域RAG服务应该已被移除");

        log.info("✅ 域RAG服务移除成功");
    }

    @Test
    void testGetDomainCount() {
        if (ragServiceFactory == null) {
            log.warn("⚠️ 跳过测试：RAGServiceFactory 未配置");
            return;
        }

        // 清空所有缓存
        ragServiceFactory.clearAll();
        assertEquals(0, ragServiceFactory.getDomainCount());

        // 创建几个域
        ragServiceFactory.getOrCreateRAGService("domain-1");
        ragServiceFactory.getOrCreateRAGService("domain-2");
        ragServiceFactory.getOrCreateRAGService("domain-3");

        assertEquals(3, ragServiceFactory.getDomainCount(),
                "应该有3个域");

        log.info("✅ 域计数测试成功");
    }

    @Test
    void testNullDomainIdHandling() {
        if (ragServiceFactory == null) {
            log.warn("⚠️ 跳过测试：RAGServiceFactory 未配置");
            return;
        }

        // 测试null域ID
        RagService RagService = ragServiceFactory.getOrCreateRAGService(null);
        assertNotNull(RagService, "null域ID应该返回默认RAG服务");

        // 测试空字符串域ID
        RagService ragService2 = ragServiceFactory.getOrCreateRAGService("");
        assertNotNull(ragService2, "空域ID应该返回默认RAG服务");

        log.info("✅ null/空域ID处理测试成功");
    }
}


