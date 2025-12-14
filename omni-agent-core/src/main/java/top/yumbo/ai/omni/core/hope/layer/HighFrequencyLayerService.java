package top.yumbo.ai.omni.core.hope.layer;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 高频层服务 - 管理会话上下文和高频知识
 * (High Frequency Layer Service - Manages session context and high-frequency knowledge)
 *
 * <p>
 * 重构说明 (Refactoring Notes):
 * - 高频层主要存储会话级别的临时数据
 * - 使用内存存储（不需要持久化接口）
 * - 自动过期清理机制
 * </p>
 *
 * 特点 (Features):
 * - 存储当前会话的上下文 (Store current session context)
 * - 话题延续检测 (Topic continuation detection)
 * - 自动过期清理 (Auto expiration cleanup)
 * - 纯内存存储，无需持久化 (Pure memory storage, no persistence needed)
 *
 * @author OmniAgent Team
 * @since 1.0.0 (Refactored)
 * @version 3.0.0 - 重构为可插拔架构
 */
@Slf4j
@Service
public class HighFrequencyLayerService {

    // 会话上下文存储 - 纯内存
    private final Map<String, SessionContext> sessionContexts = new ConcurrentHashMap<>();

    // 会话过期时间（毫秒）- 30分钟
    private static final long SESSION_EXPIRE_TIME = 30 * 60 * 1000;

    /**
     * 构造函数
     */
    public HighFrequencyLayerService() {
        log.info("HighFrequencyLayerService initialized");
        // 启动清理任务
        startCleanupTask();
    }

    /**
     * 查询高频层（会话上下文）
     *
     * @param sessionId 会话ID
     * @param question 问题
     * @return 查询结果
     */
    public QueryResult query(String sessionId, String question) {
        if (sessionId == null || sessionId.trim().isEmpty()) {
            return QueryResult.builder()
                .hasContext(false)
                .build();
        }

        SessionContext context = sessionContexts.get(sessionId);

        if (context == null || isExpired(context)) {
            return QueryResult.builder()
                .hasContext(false)
                .build();
        }

        // 更新最后访问时间
        context.setLastAccessTime(System.currentTimeMillis());

        // 检查话题延续
        boolean isContinuation = isTopicContinuation(context, question);

        return QueryResult.builder()
            .hasContext(true)
            .sessionId(sessionId)
            .currentTopic(context.getCurrentTopic())
            .contexts(context.getContexts())
            .isTopicContinuation(isContinuation)
            .build();
    }

    /**
     * 更新会话上下文
     *
     * @param sessionId 会话ID
     * @param question 问题
     * @param answer 答案
     */
    public void updateContext(String sessionId, String question, String answer) {
        if (sessionId == null || sessionId.trim().isEmpty()) {
            return;
        }

        SessionContext context = sessionContexts.computeIfAbsent(sessionId,
            k -> new SessionContext(sessionId));

        context.addInteraction(question, answer);
        context.setLastAccessTime(System.currentTimeMillis());

        log.debug("Updated session context: {}", sessionId);
    }

    /**
     * 设置当前话题
     *
     * @param sessionId 会话ID
     * @param topic 话题
     */
    public void setCurrentTopic(String sessionId, String topic) {
        SessionContext context = sessionContexts.get(sessionId);
        if (context != null) {
            context.setCurrentTopic(topic);
        }
    }

    /**
     * 清理会话
     *
     * @param sessionId 会话ID
     */
    public void clearSession(String sessionId) {
        sessionContexts.remove(sessionId);
        log.info("Cleared session: {}", sessionId);
    }

    /**
     * 检查会话是否过期
     */
    private boolean isExpired(SessionContext context) {
        long now = System.currentTimeMillis();
        return (now - context.getLastAccessTime()) > SESSION_EXPIRE_TIME;
    }

    /**
     * 检查是否是话题延续
     */
    private boolean isTopicContinuation(SessionContext context, String question) {
        if (context.getCurrentTopic() == null) {
            return false;
        }

        // 简单的话题延续检测
        String normalizedQuestion = question.toLowerCase();
        String normalizedTopic = context.getCurrentTopic().toLowerCase();

        return normalizedQuestion.contains(normalizedTopic) ||
               normalizedTopic.contains(normalizedQuestion.substring(0,
                   Math.min(10, normalizedQuestion.length())));
    }

    /**
     * 启动清理任务
     */
    private void startCleanupTask() {
        Timer timer = new Timer("HighFrequencyLayerCleanup", true);
        timer.scheduleAtFixedRate(new TimerTask() {
            @Override
            public void run() {
                cleanupExpiredSessions();
            }
        }, 60000, 60000); // 每分钟执行一次
    }

    /**
     * 清理过期会话
     */
    private void cleanupExpiredSessions() {
        int removed = 0;
        Iterator<Map.Entry<String, SessionContext>> iterator =
            sessionContexts.entrySet().iterator();

        while (iterator.hasNext()) {
            Map.Entry<String, SessionContext> entry = iterator.next();
            if (isExpired(entry.getValue())) {
                iterator.remove();
                removed++;
            }
        }

        if (removed > 0) {
            log.info("Cleaned up {} expired sessions", removed);
        }
    }

    /**
     * 会话上下文
     */
    private static class SessionContext {
        private final String sessionId;
        private String currentTopic;
        private final List<String> contexts;
        private final List<Interaction> interactions;
        private long lastAccessTime;

        public SessionContext(String sessionId) {
            this.sessionId = sessionId;
            this.contexts = new ArrayList<>();
            this.interactions = new ArrayList<>();
            this.lastAccessTime = System.currentTimeMillis();
        }

        public void addInteraction(String question, String answer) {
            interactions.add(new Interaction(question, answer));

            // 保留最近的上下文
            contexts.add("Q: " + question);
            contexts.add("A: " + answer);

            // 最多保留10轮对话
            if (contexts.size() > 20) {
                contexts.subList(0, contexts.size() - 20).clear();
            }
        }

        public String getCurrentTopic() {
            return currentTopic;
        }

        public void setCurrentTopic(String topic) {
            this.currentTopic = topic;
        }

        public List<String> getContexts() {
            return new ArrayList<>(contexts);
        }

        public long getLastAccessTime() {
            return lastAccessTime;
        }

        public void setLastAccessTime(long time) {
            this.lastAccessTime = time;
        }
    }

    /**
     * 交互记录
     */
    private static class Interaction {
        private final String question;
        private final String answer;
        private final long timestamp;

        public Interaction(String question, String answer) {
            this.question = question;
            this.answer = answer;
            this.timestamp = System.currentTimeMillis();
        }
    }

    /**
     * 查询结果
     */
    @lombok.Data
    @lombok.Builder
    @lombok.NoArgsConstructor
    @lombok.AllArgsConstructor
    public static class QueryResult {
        /** 是否有上下文 */
        private boolean hasContext;

        /** 会话ID */
        private String sessionId;

        /** 当前话题 */
        private String currentTopic;

        /** 上下文列表 */
        private List<String> contexts;

        /** 是否是话题延续 */
        private boolean isTopicContinuation;
    }
}

