package top.yumbo.ai.omni.web.websocket;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import org.springframework.web.socket.CloseStatus;
import org.springframework.web.socket.TextMessage;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.web.socket.handler.TextWebSocketHandler;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 文档处理进度 WebSocket 处理器
 * (Document Processing Progress WebSocket Handler)
 *
 * 实时推送文档处理进度到前端
 * (Real-time push document processing progress to frontend)
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Slf4j
@Component
public class DocumentProcessingWebSocketHandler extends TextWebSocketHandler {

    private final ObjectMapper objectMapper = new ObjectMapper();

    // 存储所有活跃的WebSocket会话
    private final Map<String, WebSocketSession> sessions = new ConcurrentHashMap<>();

    // 存储会话订阅的文档ID
    private final Map<String, String> sessionSubscriptions = new ConcurrentHashMap<>();

    @Override
    public void afterConnectionEstablished(WebSocketSession session) throws Exception {
        String sessionId = session.getId();
        sessions.put(sessionId, session);
        log.info("📡 WebSocket连接建立: sessionId={}", sessionId);

        // 发送欢迎消息
        sendMessage(session, Map.of(
            "type", "connected",
            "message", "WebSocket连接成功"
        ));
    }

    @Override
    protected void handleTextMessage(WebSocketSession session, TextMessage message) throws Exception {
        String sessionId = session.getId();
        String payload = message.getPayload();

        try {
            @SuppressWarnings("unchecked")
            Map<String, Object> data = objectMapper.readValue(payload, Map.class);
            String action = (String) data.get("action");

            if ("subscribe".equals(action)) {
                // 订阅文档进度
                String documentId = (String) data.get("documentId");
                sessionSubscriptions.put(sessionId, documentId);
                log.info("📝 订阅文档进度: sessionId={}, documentId={}", sessionId, documentId);

                // 发送订阅确认
                sendMessage(session, Map.of(
                    "type", "subscribed",
                    "documentId", documentId
                ));
            } else if ("unsubscribe".equals(action)) {
                // 取消订阅
                sessionSubscriptions.remove(sessionId);
                log.info("🚫 取消订阅: sessionId={}", sessionId);
            }
        } catch (Exception e) {
            log.error("❌ 处理WebSocket消息失败: sessionId={}", sessionId, e);
            sendMessage(session, Map.of(
                "type", "error",
                "message", "消息处理失败: " + e.getMessage()
            ));
        }
    }

    @Override
    public void afterConnectionClosed(WebSocketSession session, CloseStatus status) throws Exception {
        String sessionId = session.getId();

        // 清理会话和订阅
        sessions.remove(sessionId);
        String documentId = sessionSubscriptions.remove(sessionId);

        // ⭐ 根据关闭状态码区分正常和异常关闭
        if (status.getCode() == CloseStatus.NORMAL.getCode() ||
            status.getCode() == CloseStatus.GOING_AWAY.getCode()) {
            log.info("🔌 WebSocket正常关闭: sessionId={}, documentId={}, status={}",
                sessionId, documentId, status);
        } else {
            log.debug("🔌 WebSocket异常关闭: sessionId={}, documentId={}, code={}, reason={}",
                sessionId, documentId, status.getCode(), status.getReason());
        }
    }

    @Override
    public void handleTransportError(WebSocketSession session, Throwable exception) throws Exception {
        String sessionId = session.getId();

        // ⭐ 区分不同类型的错误，避免打印正常的连接关闭
        if (exception instanceof java.io.IOException) {
            String message = exception.getMessage();
            if (message != null && (
                message.contains("已建立的连接") ||
                message.contains("Connection reset") ||
                message.contains("Broken pipe"))) {
                // 客户端正常关闭或网络中断，使用 debug 级别
                log.debug("🔌 WebSocket 连接中断: sessionId={}, reason={}", sessionId, message);
            } else {
                log.warn("⚠️ WebSocket IO 错误: sessionId={}, message={}", sessionId, message);
            }
        } else {
            // 其他类型的错误才记录为 error
            log.error("❌ WebSocket传输错误: sessionId={}", sessionId, exception);
        }

        // 安全关闭连接
        try {
            if (session.isOpen()) {
                session.close();
            }
        } catch (Exception e) {
            log.debug("关闭会话时出错（可忽略）: {}", e.getMessage());
        }
    }

    /**
     * 向指定会话发送消息
     */
    private void sendMessage(WebSocketSession session, Object message) {
        try {
            if (session != null && session.isOpen()) {
                String json = objectMapper.writeValueAsString(message);
                session.sendMessage(new TextMessage(json));
            } else {
                log.debug("⚠️ 会话已关闭，跳过消息发送");
            }
        } catch (IOException e) {
            // ⭐ 区分不同的 IO 错误
            String errorMsg = e.getMessage();
            if (errorMsg != null && (
                errorMsg.contains("已建立的连接") ||
                errorMsg.contains("Connection reset") ||
                errorMsg.contains("Broken pipe"))) {
                log.debug("🔌 连接已断开，无法发送消息: {}", errorMsg);
            } else {
                log.error("❌ 发送WebSocket消息失败", e);
            }
        } catch (Exception e) {
            log.error("❌ 发送WebSocket消息时发生未预期的错误", e);
        }
    }

    /**
     * 广播文档处理进度到订阅该文档的所有会话
     */
    public void broadcastProgress(String documentId, Map<String, Object> progress) {
        log.debug("📢 广播进度: documentId={}", documentId);

        sessionSubscriptions.forEach((sessionId, subscribedDocId) -> {
            if (documentId.equals(subscribedDocId)) {
                WebSocketSession session = sessions.get(sessionId);
                if (session != null && session.isOpen()) {
                    sendMessage(session, Map.of(
                        "type", "progress",
                        "data", progress
                    ));
                }
            }
        });
    }

    /**
     * 获取活跃会话数量
     */
    public int getActiveSessionCount() {
        return sessions.size();
    }

    /**
     * 获取订阅数量
     */
    public int getSubscriptionCount() {
        return sessionSubscriptions.size();
    }
}

