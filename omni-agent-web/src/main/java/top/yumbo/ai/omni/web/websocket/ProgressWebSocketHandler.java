package top.yumbo.ai.omni.web.websocket;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;
import top.yumbo.ai.omni.web.model.rag.ProcessingProgress;
import top.yumbo.ai.omni.web.service.rag.ProcessingProgressService;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 进度推送 WebSocket 处理器
 * (Progress Push WebSocket Handler)
 *
 * <p>
 * 处理 WebSocket 连接，实时推送文档处理进度
 * (Handle WebSocket connections and push document processing progress in real-time)
 * </p>
 *
 * @author AI Reviewer Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
public class ProgressWebSocketHandler extends TextWebSocketHandler {

    @Autowired
    private ProcessingProgressService progressService;

    private final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * 存储每个文档ID对应的所有监听会话
     * (Store all listening sessions for each document ID)
     *
     * Key: documentId
     * Value: Set of WebSocketSession
     */
    private final Map<String, java.util.Set<WebSocketSession>> documentSessions = new ConcurrentHashMap<>();

    /**
     * 存储每个会话监听的文档ID
     * (Store document ID for each session)
     *
     * Key: sessionId
     * Value: documentId
     */
    private final Map<String, String> sessionDocuments = new ConcurrentHashMap<>();

    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception {
        log.info("🔌 WebSocket 连接建立: sessionId={}", session.getId());
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) throws Exception {
        String payload = message.getPayload();
        log.debug("📨 收到 WebSocket 消息: sessionId={}, payload={}", session.getId(), payload);

        try {
            // 解析消息 (Parse message)
            @SuppressWarnings("unchecked")
            Map<String, Object> msg = objectMapper.readValue(payload, Map.class);
            String action = (String) msg.get("action");

            if ("subscribe".equals(action)) {
                // 订阅文档进度 (Subscribe to document progress)
                String documentId = (String) msg.get("documentId");
                subscribeToDocument(session, documentId);
            } else if ("unsubscribe".equals(action)) {
                // 取消订阅 (Unsubscribe)
                unsubscribeFromDocument(session);
            }
        } catch (Exception e) {
            log.error("❌ 处理 WebSocket 消息失败: sessionId={}", session.getId(), e);
            sendError(session, "Invalid message format");
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) throws Exception {
        log.info("🔌 WebSocket 连接关闭: sessionId={}, status={}", session.getId(), status);
        unsubscribeFromDocument(session);
    }

    /**
     * 订阅文档进度
     * (Subscribe to document progress)
     */
    private void subscribeToDocument(WebSocketSession session, String documentId) throws IOException {
        if (documentId == null || documentId.isEmpty()) {
            sendError(session, "Document ID is required");
            return;
        }

        // 如果已经订阅了其他文档，先取消订阅 (Unsubscribe from previous document if any)
        unsubscribeFromDocument(session);

        // 添加到订阅列表 (Add to subscription list)
        documentSessions.computeIfAbsent(documentId, k -> ConcurrentHashMap.newKeySet()).add(session);
        sessionDocuments.put(session.getId(), documentId);

        log.info("📌 订阅文档进度: sessionId={}, documentId={}", session.getId(), documentId);

        // 立即发送当前进度 (Send current progress immediately)
        ProcessingProgress progress = progressService.getProgress(documentId);
        if (progress != null) {
            sendProgress(session, progress);
        } else {
            sendMessage(session, Map.of(
                    "type", "info",
                    "message", "Document not found or not processing"
            ));
        }
    }

    /**
     * 取消订阅
     * (Unsubscribe from document)
     */
    private void unsubscribeFromDocument(WebSocketSession session) {
        String documentId = sessionDocuments.remove(session.getId());
        if (documentId != null) {
            java.util.Set<WebSocketSession> sessions = documentSessions.get(documentId);
            if (sessions != null) {
                sessions.remove(session);
                // 如果没有会话监听该文档，移除该文档的订阅记录
                // (Remove document subscription if no sessions listening)
                if (sessions.isEmpty()) {
                    documentSessions.remove(documentId);
                }
            }
            log.info("📌 取消订阅文档进度: sessionId={}, documentId={}", session.getId(), documentId);
        }
    }

    /**
     * 推送进度更新到所有订阅者
     * (Push progress update to all subscribers)
     *
     * @param documentId 文档ID (Document ID)
     * @param progress   进度信息 (Progress information)
     */
    public void broadcastProgress(String documentId, ProcessingProgress progress) {
        java.util.Set<WebSocketSession> sessions = documentSessions.get(documentId);
        if (sessions == null || sessions.isEmpty()) {
            log.debug("📡 没有会话订阅文档: documentId={}", documentId);
            return;
        }

        log.info("📡 广播进度更新: documentId={}, subscribers={}, stage={}, progress={}%",
                documentId, sessions.size(), progress.getStage(), progress.getProgress());

        // 移除已关闭的会话 (Remove closed sessions)
        sessions.removeIf(session -> !session.isOpen());

        // 向所有订阅者发送进度 (Send progress to all subscribers)
        sessions.forEach(session -> {
            try {
                sendProgress(session, progress);
            } catch (Exception e) {
                log.error("❌ 发送进度失败: sessionId={}, documentId={}", session.getId(), documentId, e);
            }
        });
    }

    /**
     * 发送进度消息
     * (Send progress message)
     */
    private void sendProgress(WebSocketSession session, ProcessingProgress progress) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "progress",
                "data", progress
        );
        sendMessage(session, message);
    }

    /**
     * 发送错误消息
     * (Send error message)
     */
    private void sendError(WebSocketSession session, String error) throws IOException {
        Map<String, Object> message = Map.of(
                "type", "error",
                "message", error
        );
        sendMessage(session, message);
    }

    /**
     * 发送消息
     * (Send message)
     */
    private void sendMessage(WebSocketSession session, Map<String, Object> message) throws IOException {
        if (session.isOpen()) {
            String json = objectMapper.writeValueAsString(message);
            session.sendMessage(new TextMessage(json));
        }
    }
}



