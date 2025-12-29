package top.yumbo.ai.omni.core.old.feedback;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 反馈服务 (Feedback Service)
 * <p>
 * 收集和管理用户反馈
 * (Collects and manages user feedback)
 * <p>
 * 核心功能 (Core Features):
 * - 反馈收集 (Feedback collection)
 * - 反馈存储 (Feedback storage)
 * - 反馈统计 (Feedback statistics)
 * - 反馈查询 (Feedback query)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@Slf4j
@Service
public class FeedbackService {

    /**
     * 反馈存储 (Feedback storage)
     * 使用内存存储，后续可扩展为持久化
     * (Using in-memory storage, can be extended to persistence later)
     */
    private final Map<String, Feedback> feedbackStorage = new ConcurrentHashMap<>();

    /**
     * 会话反馈索引 (Session feedback index)
     * Key: sessionId, Value: List of feedback IDs
     */
    private final Map<String, List<String>> sessionIndex = new ConcurrentHashMap<>();

    /**
     * 收集显式反馈 (Collect explicit feedback)
     *
     * @param sessionId 会话ID (Session ID)
     * @param userId    用户ID (User ID)
     * @param question  问题 (Question)
     * @param answer    答案 (Answer)
     * @param value     反馈值 (Feedback value: 1=good, 0=neutral, -1=bad)
     * @param tags      反馈标签 (Feedback tags)
     * @param comment   评论 (Comment)
     * @return 反馈对象 (Feedback object)
     */
    public Feedback collectExplicit(String sessionId, String userId, String question,
                                    String answer, double value, String[] tags, String comment) {
        long startTime = System.currentTimeMillis();

        // 构建反馈对象 (Build feedback object)
        Feedback feedback = Feedback.builder()
                .id(UUID.randomUUID().toString())
                .sessionId(sessionId)
                .userId(userId)
                .question(question)
                .answer(answer)
                .type(Feedback.FeedbackType.EXPLICIT)
                .source(Feedback.FeedbackSource.USER)
                .value(value)
                .tags(tags)
                .comment(comment)
                .createTime(new Date())
                .status(Feedback.ProcessingStatus.PENDING)
                .build();

        // 存储反馈 (Store feedback)
        feedbackStorage.put(feedback.getId(), feedback);

        // 更新索引 (Update index)
        sessionIndex.computeIfAbsent(sessionId, k -> new ArrayList<>())
                .add(feedback.getId());

        long duration = System.currentTimeMillis() - startTime;
        log.info("Explicit feedback collected: sessionId={}, userId={}, value={}, duration={}ms",
                sessionId, userId, value, duration);

        return feedback;
    }

    /**
     * 收集隐式反馈 (Collect implicit feedback)
     *
     * @param sessionId 会话ID (Session ID)
     * @param userId    用户ID (User ID)
     * @param question  问题 (Question)
     * @param answer    答案 (Answer)
     * @param value     反馈值 (Feedback value)
     * @return 反馈对象 (Feedback object)
     */
    public Feedback collectImplicit(String sessionId, String userId, String question,
                                    String answer, double value) {
        Feedback feedback = Feedback.builder()
                .id(UUID.randomUUID().toString())
                .sessionId(sessionId)
                .userId(userId)
                .question(question)
                .answer(answer)
                .type(Feedback.FeedbackType.IMPLICIT)
                .source(Feedback.FeedbackSource.SYSTEM)
                .value(value)
                .createTime(new Date())
                .status(Feedback.ProcessingStatus.PENDING)
                .build();

        feedbackStorage.put(feedback.getId(), feedback);
        sessionIndex.computeIfAbsent(sessionId, k -> new ArrayList<>())
                .add(feedback.getId());

        log.debug("Implicit feedback collected: sessionId={}, value={}", sessionId, value);

        return feedback;
    }

    /**
     * 获取反馈 (Get feedback by ID)
     *
     * @param feedbackId 反馈ID (Feedback ID)
     * @return 反馈对象 (Feedback object)
     */
    public Feedback getFeedback(String feedbackId) {
        return feedbackStorage.get(feedbackId);
    }

    /**
     * 获取会话的所有反馈 (Get all feedback for a session)
     *
     * @param sessionId 会话ID (Session ID)
     * @return 反馈列表 (List of feedback)
     */
    public List<Feedback> getSessionFeedback(String sessionId) {
        List<String> feedbackIds = sessionIndex.get(sessionId);
        if (feedbackIds == null || feedbackIds.isEmpty()) {
            return Collections.emptyList();
        }

        return feedbackIds.stream()
                .map(feedbackStorage::get)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    /**
     * 获取用户的所有反馈 (Get all feedback for a user)
     *
     * @param userId 用户ID (User ID)
     * @return 反馈列表 (List of feedback)
     */
    public List<Feedback> getUserFeedback(String userId) {
        return feedbackStorage.values().stream()
                .filter(f -> userId.equals(f.getUserId()))
                .sorted(Comparator.comparing(Feedback::getCreateTime).reversed())
                .collect(Collectors.toList());
    }

    /**
     * 获取反馈统计 (Get feedback statistics)
     *
     * @return 统计信息 Map (Statistics map)
     */
    public Map<String, Object> getStatistics() {
        Map<String, Object> stats = new HashMap<>();

        long totalCount = feedbackStorage.size();
        long explicitCount = feedbackStorage.values().stream()
                .filter(f -> f.getType() == Feedback.FeedbackType.EXPLICIT)
                .count();
        long implicitCount = totalCount - explicitCount;

        long positiveCount = feedbackStorage.values().stream()
                .filter(f -> f.getValue() > 0)
                .count();
        long negativeCount = feedbackStorage.values().stream()
                .filter(f -> f.getValue() < 0)
                .count();
        long neutralCount = totalCount - positiveCount - negativeCount;

        double avgValue = feedbackStorage.values().stream()
                .mapToDouble(Feedback::getValue)
                .average()
                .orElse(0.0);

        stats.put("totalCount", totalCount);
        stats.put("explicitCount", explicitCount);
        stats.put("implicitCount", implicitCount);
        stats.put("positiveCount", positiveCount);
        stats.put("negativeCount", negativeCount);
        stats.put("neutralCount", neutralCount);
        stats.put("averageValue", avgValue);
        stats.put("sessionCount", sessionIndex.size());

        return stats;
    }

    /**
     * 更新反馈状态 (Update feedback status)
     *
     * @param feedbackId 反馈ID (Feedback ID)
     * @param status     新状态 (New status)
     */
    public void updateStatus(String feedbackId, Feedback.ProcessingStatus status) {
        Feedback feedback = feedbackStorage.get(feedbackId);
        if (feedback != null) {
            feedback.setStatus(status);
            log.debug("Feedback status updated: id={}, status={}", feedbackId, status);
        }
    }

    /**
     * 清除所有反馈 (Clear all feedback)
     */
    public void clearAll() {
        feedbackStorage.clear();
        sessionIndex.clear();
        log.info("All feedback cleared");
    }

    /**
     * 清除会话反馈 (Clear session feedback)
     *
     * @param sessionId 会话ID (Session ID)
     */
    public void clearSession(String sessionId) {
        List<String> feedbackIds = sessionIndex.remove(sessionId);
        if (feedbackIds != null) {
            feedbackIds.forEach(feedbackStorage::remove);
            log.info("Session feedback cleared: sessionId={}, count={}", sessionId, feedbackIds.size());
        }
    }
}


