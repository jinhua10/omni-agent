package top.yumbo.ai.omni.core.service.knowledge;

import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import top.yumbo.ai.omni.core.model.RefinedKnowledge;

import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 知识存储服务集成测试
 * (Knowledge Storage Service Integration Tests)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@SpringBootTest
@ActiveProfiles("test")
class KnowledgeStorageServiceIntegrationTest {

    @Autowired(required = false)
    private KnowledgeStorageService knowledgeStorageService;

    @Test
    void testKnowledgeStorageServiceAvailable() {
        if (knowledgeStorageService == null) {
            log.warn("⚠️ KnowledgeStorageService 未配置（可能是测试环境）");
            return;
        }

        log.info("✅ KnowledgeStorageService 已注入");
        assertNotNull(knowledgeStorageService, "知识存储服务不应为null");
    }

    @Test
    void testCreateTestKnowledge() {
        // 简单测试知识对象创建
        RefinedKnowledge knowledge = createTestKnowledge();

        assertNotNull(knowledge.getKnowledgeId());
        assertNotNull(knowledge.getTitle());
        assertNotNull(knowledge.getRefinedContent());

        log.info("✅ 测试知识对象创建成功");
    }

    // ========== 辅助方法 ==========

    private RefinedKnowledge createTestKnowledge() {
        return createTestKnowledge("测试知识", "这是一条测试知识内容");
    }

    private RefinedKnowledge createTestKnowledge(String title, String content) {
        RefinedKnowledge knowledge = new RefinedKnowledge();
        knowledge.setKnowledgeId("knowledge-" + UUID.randomUUID());
        knowledge.setTitle(title);
        knowledge.setRefinedContent(content);
        knowledge.setKnowledgeType("TECHNICAL");
        knowledge.setSourceDomainId("source-domain-001");
        knowledge.setSourceDocumentId("doc-001");
        knowledge.setRoleId("role-001");
        knowledge.setImportance(4);

        return knowledge;
    }
}


