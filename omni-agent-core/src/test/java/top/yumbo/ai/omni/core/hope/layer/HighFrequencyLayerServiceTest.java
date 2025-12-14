package top.yumbo.ai.omni.core.hope.layer;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * HighFrequencyLayerService 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("HighFrequencyLayerService Tests")
class HighFrequencyLayerServiceTest {

    private HighFrequencyLayerService service;

    @BeforeEach
    void setUp() {
        service = new HighFrequencyLayerService();
    }

    @Test
    @DisplayName("查询 - 无上下文")
    void testQuery_NoContext() {
        // Given
        String sessionId = "new-session";
        String question = "测试问题";

        // When
        HighFrequencyLayerService.QueryResult result = service.query(sessionId, question);

        // Then
        assertNotNull(result);
        assertFalse(result.isHasContext());
    }

    @Test
    @DisplayName("查询 - null会话ID")
    void testQuery_NullSessionId() {
        // Given
        String question = "测试问题";

        // When
        HighFrequencyLayerService.QueryResult result = service.query(null, question);

        // Then
        assertNotNull(result);
        assertFalse(result.isHasContext());
    }

    @Test
    @DisplayName("查询 - 空会话ID")
    void testQuery_EmptySessionId() {
        // Given
        String sessionId = "";
        String question = "测试问题";

        // When
        HighFrequencyLayerService.QueryResult result = service.query(sessionId, question);

        // Then
        assertNotNull(result);
        assertFalse(result.isHasContext());
    }

    @Test
    @DisplayName("更新上下文 - 成功")
    void testUpdateContext_Success() {
        // Given
        String sessionId = "session-1";
        String question = "什么是AI？";
        String answer = "AI是人工智能。";

        // When
        service.updateContext(sessionId, question, answer);
        HighFrequencyLayerService.QueryResult result = service.query(sessionId, "下一个问题");

        // Then
        assertTrue(result.isHasContext());
        assertEquals(sessionId, result.getSessionId());
        assertNotNull(result.getContexts());
        assertEquals(2, result.getContexts().size());
        assertTrue(result.getContexts().get(0).contains(question));
        assertTrue(result.getContexts().get(1).contains(answer));
    }

    @Test
    @DisplayName("更新上下文 - null会话ID")
    void testUpdateContext_NullSessionId() {
        // Given
        String question = "测试问题";
        String answer = "测试答案";

        // When & Then - 不应抛出异常
        assertDoesNotThrow(() -> service.updateContext(null, question, answer));
    }

    @Test
    @DisplayName("更新上下文 - 空会话ID")
    void testUpdateContext_EmptySessionId() {
        // Given
        String sessionId = "   ";
        String question = "测试问题";
        String answer = "测试答案";

        // When & Then - 不应抛出异常
        assertDoesNotThrow(() -> service.updateContext(sessionId, question, answer));
    }

    @Test
    @DisplayName("多轮对话上下文")
    void testUpdateContext_MultipleRounds() {
        // Given
        String sessionId = "session-multi";

        // When - 添加3轮对话
        service.updateContext(sessionId, "问题1", "答案1");
        service.updateContext(sessionId, "问题2", "答案2");
        service.updateContext(sessionId, "问题3", "答案3");

        HighFrequencyLayerService.QueryResult result = service.query(sessionId, "问题4");

        // Then
        assertTrue(result.isHasContext());
        List<String> contexts = result.getContexts();
        assertEquals(6, contexts.size()); // 3轮对话 * 2（问+答）
        assertTrue(contexts.get(0).contains("问题1"));
        assertTrue(contexts.get(5).contains("答案3"));
    }

    @Test
    @DisplayName("上下文限制 - 最多10轮")
    void testUpdateContext_MaxContextLimit() {
        // Given
        String sessionId = "session-limit";

        // When - 添加15轮对话（超过10轮限制）
        for (int i = 1; i <= 15; i++) {
            service.updateContext(sessionId, "问题" + i, "答案" + i);
        }

        HighFrequencyLayerService.QueryResult result = service.query(sessionId, "问题16");

        // Then
        assertTrue(result.isHasContext());
        List<String> contexts = result.getContexts();
        assertEquals(20, contexts.size()); // 最多10轮 * 2
        // 应该保留最近的10轮
        assertTrue(contexts.get(0).contains("问题6"));
        assertTrue(contexts.get(19).contains("答案15"));
    }

    @Test
    @DisplayName("设置当前话题")
    void testSetCurrentTopic() {
        // Given
        String sessionId = "session-topic";
        String topic = "人工智能";

        // When
        service.updateContext(sessionId, "什么是AI？", "AI是人工智能");
        service.setCurrentTopic(sessionId, topic);
        HighFrequencyLayerService.QueryResult result = service.query(sessionId, "AI的应用");

        // Then
        assertTrue(result.isHasContext());
        assertEquals(topic, result.getCurrentTopic());
    }

    @Test
    @DisplayName("话题延续检测 - 包含话题关键词")
    void testTopicContinuation_ContainsTopic() {
        // Given
        String sessionId = "session-continuation";
        String topic = "机器学习";

        // When
        service.updateContext(sessionId, "什么是机器学习？", "机器学习是AI的一个分支");
        service.setCurrentTopic(sessionId, topic);
        HighFrequencyLayerService.QueryResult result = service.query(sessionId, "机器学习有哪些算法？");

        // Then
        assertTrue(result.isHasContext());
        assertTrue(result.isTopicContinuation());
    }

    @Test
    @DisplayName("话题延续检测 - 不包含话题关键词")
    void testTopicContinuation_DoesNotContainTopic() {
        // Given
        String sessionId = "session-no-continuation";
        String topic = "深度学习";

        // When
        service.updateContext(sessionId, "什么是深度学习？", "深度学习使用神经网络");
        service.setCurrentTopic(sessionId, topic);
        HighFrequencyLayerService.QueryResult result = service.query(sessionId, "今天天气怎么样？");

        // Then
        assertTrue(result.isHasContext());
        assertFalse(result.isTopicContinuation());
    }

    @Test
    @DisplayName("话题延续检测 - 无话题")
    void testTopicContinuation_NoTopic() {
        // Given
        String sessionId = "session-no-topic";

        // When
        service.updateContext(sessionId, "测试问题", "测试答案");
        HighFrequencyLayerService.QueryResult result = service.query(sessionId, "后续问题");

        // Then
        assertTrue(result.isHasContext());
        assertFalse(result.isTopicContinuation());
    }

    @Test
    @DisplayName("清理会话")
    void testClearSession() {
        // Given
        String sessionId = "session-clear";

        // When
        service.updateContext(sessionId, "问题", "答案");
        HighFrequencyLayerService.QueryResult result1 = service.query(sessionId, "测试");
        assertTrue(result1.isHasContext());

        service.clearSession(sessionId);
        HighFrequencyLayerService.QueryResult result2 = service.query(sessionId, "测试");

        // Then
        assertFalse(result2.isHasContext());
    }

    @Test
    @DisplayName("清理不存在的会话")
    void testClearSession_NonExistent() {
        // Given
        String sessionId = "nonexistent";

        // When & Then - 不应抛出异常
        assertDoesNotThrow(() -> service.clearSession(sessionId));
    }

    @Test
    @DisplayName("多个会话独立管理")
    void testMultipleSessions_Independent() {
        // Given
        String session1 = "session-1";
        String session2 = "session-2";

        // When
        service.updateContext(session1, "问题1-1", "答案1-1");
        service.updateContext(session2, "问题2-1", "答案2-1");
        service.setCurrentTopic(session1, "话题1");
        service.setCurrentTopic(session2, "话题2");

        HighFrequencyLayerService.QueryResult result1 = service.query(session1, "后续");
        HighFrequencyLayerService.QueryResult result2 = service.query(session2, "后续");

        // Then
        assertTrue(result1.isHasContext());
        assertTrue(result2.isHasContext());
        assertEquals("话题1", result1.getCurrentTopic());
        assertEquals("话题2", result2.getCurrentTopic());
        assertTrue(result1.getContexts().get(0).contains("问题1-1"));
        assertTrue(result2.getContexts().get(0).contains("问题2-1"));
    }

    @Test
    @DisplayName("长问题处理")
    void testQuery_LongQuestion() {
        // Given
        String sessionId = "session-long";
        StringBuilder longQuestion = new StringBuilder();
        for (int i = 0; i < 100; i++) {
            longQuestion.append("这是一个很长的问题");
        }

        // When
        service.updateContext(sessionId, "初始问题", "初始答案");
        HighFrequencyLayerService.QueryResult result = service.query(sessionId, longQuestion.toString());

        // Then
        assertTrue(result.isHasContext());
    }

    @Test
    @DisplayName("特殊字符处理")
    void testUpdateContext_SpecialCharacters() {
        // Given
        String sessionId = "session-special";
        String question = "什么是@#$%^&*()？";
        String answer = "这些是特殊字符！";

        // When
        service.updateContext(sessionId, question, answer);
        HighFrequencyLayerService.QueryResult result = service.query(sessionId, "下一个");

        // Then
        assertTrue(result.isHasContext());
        assertTrue(result.getContexts().get(0).contains("@#$%^&*()"));
    }

    @Test
    @DisplayName("查询结果Builder")
    void testQueryResult_Builder() {
        // When
        HighFrequencyLayerService.QueryResult result =
            HighFrequencyLayerService.QueryResult.builder()
                .hasContext(true)
                .sessionId("test-session")
                .currentTopic("test-topic")
                .contexts(List.of("context1", "context2"))
                .isTopicContinuation(true)
                .build();

        // Then
        assertTrue(result.isHasContext());
        assertEquals("test-session", result.getSessionId());
        assertEquals("test-topic", result.getCurrentTopic());
        assertEquals(2, result.getContexts().size());
        assertTrue(result.isTopicContinuation());
    }

    @Test
    @DisplayName("并发访问测试")
    void testConcurrentAccess() throws InterruptedException {
        // Given
        int threadCount = 10;
        Thread[] threads = new Thread[threadCount];

        // When
        for (int i = 0; i < threadCount; i++) {
            final int threadId = i;
            threads[i] = new Thread(() -> {
                String sessionId = "session-" + threadId;
                for (int j = 0; j < 10; j++) {
                    service.updateContext(sessionId, "Q" + j, "A" + j);
                    service.query(sessionId, "test");
                }
            });
            threads[i].start();
        }

        // Wait for all threads
        for (Thread thread : threads) {
            thread.join();
        }

        // Then - 验证每个会话都有正确的数据
        for (int i = 0; i < threadCount; i++) {
            String sessionId = "session-" + i;
            HighFrequencyLayerService.QueryResult result = service.query(sessionId, "test");
            assertTrue(result.isHasContext());
        }
    }
}

