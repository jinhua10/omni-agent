package top.yumbo.ai.omni.core.feedback;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * FeedbackService 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("反馈服务 测试")
class FeedbackServiceTest {

    private FeedbackService feedbackService;

    @BeforeEach
    void setUp() {
        feedbackService = new FeedbackService();
    }

    @Test
    @DisplayName("收集显式正面反馈")
    void testCollectExplicitPositiveFeedback() {
        // Given
        String sessionId = "session-001";
        String userId = "user-001";
        String question = "什么是Java?";
        String answer = "Java是一种编程语言";
        double value = 1.0;
        String[] tags = new String[]{"helpful", "accurate"};
        String comment = "很好的回答";

        // When
        Feedback feedback = feedbackService.collectExplicit(
            sessionId, userId, question, answer, value, tags, comment);

        // Then
        assertNotNull(feedback);
        assertNotNull(feedback.getId());
        assertEquals(sessionId, feedback.getSessionId());
        assertEquals(userId, feedback.getUserId());
        assertEquals(question, feedback.getQuestion());
        assertEquals(answer, feedback.getAnswer());
        assertEquals(value, feedback.getValue());
        assertEquals(Feedback.FeedbackType.EXPLICIT, feedback.getType());
        assertEquals(Feedback.FeedbackSource.USER, feedback.getSource());
        assertArrayEquals(tags, feedback.getTags());
        assertEquals(comment, feedback.getComment());
        assertNotNull(feedback.getCreateTime());
    }

    @Test
    @DisplayName("收集显式负面反馈")
    void testCollectExplicitNegativeFeedback() {
        // Given
        String sessionId = "session-002";
        String userId = "user-002";
        String question = "测试问题";
        String answer = "测试答案";
        double value = -1.0;
        String[] tags = new String[]{"incorrect", "unhelpful"};
        String comment = "答案不准确";

        // When
        Feedback feedback = feedbackService.collectExplicit(
            sessionId, userId, question, answer, value, tags, comment);

        // Then
        assertNotNull(feedback);
        assertEquals(value, feedback.getValue());
        assertTrue(feedback.getValue() < 0);
    }

    @Test
    @DisplayName("收集隐式反馈")
    void testCollectImplicitFeedback() {
        // Given
        String sessionId = "session-003";
        String userId = "user-003";
        String question = "测试问题";
        String answer = "测试答案";
        double value = 0.5;

        // When
        Feedback feedback = feedbackService.collectImplicit(
            sessionId, userId, question, answer, value);

        // Then
        assertNotNull(feedback);
        assertEquals(Feedback.FeedbackType.IMPLICIT, feedback.getType());
        assertEquals(Feedback.FeedbackSource.SYSTEM, feedback.getSource());
        assertEquals(value, feedback.getValue());
        assertNull(feedback.getTags());
        assertNull(feedback.getComment());
    }

    @Test
    @DisplayName("通过ID获取反馈")
    void testGetFeedbackById() {
        // Given
        Feedback created = feedbackService.collectExplicit(
            "session-004", "user-004", "问题", "答案", 1.0, null, null);

        // When
        Feedback retrieved = feedbackService.getFeedback(created.getId());

        // Then
        assertNotNull(retrieved);
        assertEquals(created.getId(), retrieved.getId());
        assertEquals(created.getSessionId(), retrieved.getSessionId());
    }

    @Test
    @DisplayName("获取不存在的反馈返回null")
    void testGetNonExistentFeedback() {
        // When
        Feedback feedback = feedbackService.getFeedback("non-existent-id");

        // Then
        assertNull(feedback);
    }

    @Test
    @DisplayName("获取会话的所有反馈")
    void testGetSessionFeedback() {
        // Given
        String sessionId = "session-005";
        feedbackService.collectExplicit(sessionId, "user-001", "问题1", "答案1", 1.0, null, null);
        feedbackService.collectExplicit(sessionId, "user-002", "问题2", "答案2", -1.0, null, null);
        feedbackService.collectImplicit(sessionId, "user-003", "问题3", "答案3", 0.5);

        // When
        List<Feedback> feedbacks = feedbackService.getSessionFeedback(sessionId);

        // Then
        assertNotNull(feedbacks);
        assertEquals(3, feedbacks.size());
        assertTrue(feedbacks.stream().allMatch(f -> sessionId.equals(f.getSessionId())));
    }

    @Test
    @DisplayName("获取不存在会话的反馈返回空列表")
    void testGetNonExistentSessionFeedback() {
        // When
        List<Feedback> feedbacks = feedbackService.getSessionFeedback("non-existent-session");

        // Then
        assertNotNull(feedbacks);
        assertTrue(feedbacks.isEmpty());
    }

    @Test
    @DisplayName("获取用户的所有反馈")
    void testGetUserFeedback() {
        // Given
        String userId = "user-006";
        feedbackService.collectExplicit("session-1", userId, "问题1", "答案1", 1.0, null, null);
        feedbackService.collectExplicit("session-2", userId, "问题2", "答案2", 0.5, null, null);
        feedbackService.collectImplicit("session-3", userId, "问题3", "答案3", 0.8);

        // When
        List<Feedback> feedbacks = feedbackService.getUserFeedback(userId);

        // Then
        assertNotNull(feedbacks);
        assertEquals(3, feedbacks.size());
        assertTrue(feedbacks.stream().allMatch(f -> userId.equals(f.getUserId())));
    }

    @Test
    @DisplayName("获取反馈统计")
    void testGetStatistics() {
        // Given
        feedbackService.collectExplicit("s1", "u1", "q1", "a1", 1.0, null, null);  // positive
        feedbackService.collectExplicit("s2", "u2", "q2", "a2", -1.0, null, null); // negative
        feedbackService.collectImplicit("s3", "u3", "q3", "a3", 0.5);              // positive
        feedbackService.collectImplicit("s4", "u4", "q4", "a4", 0.0);              // neutral

        // When
        Map<String, Object> stats = feedbackService.getStatistics();

        // Then
        assertNotNull(stats);
        assertEquals(4, ((Number) stats.get("totalCount")).intValue());
        assertEquals(2, ((Number) stats.get("explicitCount")).intValue());
        assertEquals(2, ((Number) stats.get("implicitCount")).intValue());
        assertEquals(2, ((Number) stats.get("positiveCount")).intValue());
        assertEquals(1, ((Number) stats.get("negativeCount")).intValue());
        assertEquals(1, ((Number) stats.get("neutralCount")).intValue());
        assertEquals(4, ((Number) stats.get("sessionCount")).intValue());

        double avgValue = (Double) stats.get("averageValue");
        assertTrue(avgValue >= 0.0);
    }

    @Test
    @DisplayName("空统计")
    void testEmptyStatistics() {
        // When
        Map<String, Object> stats = feedbackService.getStatistics();

        // Then
        assertNotNull(stats);
        assertEquals(0, ((Number) stats.get("totalCount")).intValue());
        assertEquals(0, ((Number) stats.get("explicitCount")).intValue());
        assertEquals(0, ((Number) stats.get("implicitCount")).intValue());
        assertEquals(0.0, stats.get("averageValue"));
    }

    @Test
    @DisplayName("多次收集反馈")
    void testCollectMultipleFeedbacks() {
        // When
        for (int i = 0; i < 10; i++) {
            feedbackService.collectExplicit(
                "session-" + i,
                "user-" + i,
                "问题" + i,
                "答案" + i,
                (i % 2 == 0) ? 1.0 : -1.0,
                null,
                null
            );
        }

        // Then
        Map<String, Object> stats = feedbackService.getStatistics();
        assertEquals(10, ((Number) stats.get("totalCount")).intValue());
        assertEquals(5, ((Number) stats.get("positiveCount")).intValue());
        assertEquals(5, ((Number) stats.get("negativeCount")).intValue());
    }

    @Test
    @DisplayName("处理null标签和评论")
    void testHandleNullTagsAndComment() {
        // When
        Feedback feedback = feedbackService.collectExplicit(
            "session-007", "user-007", "问题", "答案", 1.0, null, null);

        // Then
        assertNotNull(feedback);
        assertNull(feedback.getTags());
        assertNull(feedback.getComment());
    }

    @Test
    @DisplayName("反馈ID唯一性")
    void testFeedbackIdUniqueness() {
        // When
        Feedback f1 = feedbackService.collectExplicit("s1", "u1", "q1", "a1", 1.0, null, null);
        Feedback f2 = feedbackService.collectExplicit("s1", "u1", "q1", "a1", 1.0, null, null);

        // Then
        assertNotNull(f1.getId());
        assertNotNull(f2.getId());
        assertNotEquals(f1.getId(), f2.getId());
    }

    @Test
    @DisplayName("用户反馈按时间倒序排列")
    void testUserFeedbackSortedByTime() throws InterruptedException {
        // Given
        String userId = "user-008";
        feedbackService.collectExplicit("s1", userId, "q1", "a1", 1.0, null, null);
        Thread.sleep(10); // 确保时间不同
        feedbackService.collectExplicit("s2", userId, "q2", "a2", 1.0, null, null);
        Thread.sleep(10);
        feedbackService.collectExplicit("s3", userId, "q3", "a3", 1.0, null, null);

        // When
        List<Feedback> feedbacks = feedbackService.getUserFeedback(userId);

        // Then
        assertEquals(3, feedbacks.size());
        // 验证时间倒序
        for (int i = 0; i < feedbacks.size() - 1; i++) {
            assertTrue(feedbacks.get(i).getCreateTime().compareTo(
                feedbacks.get(i + 1).getCreateTime()) >= 0);
        }
    }
}

