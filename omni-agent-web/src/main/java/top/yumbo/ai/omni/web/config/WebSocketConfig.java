package top.yumbo.ai.omni.web.config;

import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;
import top.yumbo.ai.omni.web.websocket.DocumentProcessingWebSocketHandler;

/**
 * WebSocket 配置
 * (WebSocket Configuration)
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */
@Configuration
@EnableWebSocket
@RequiredArgsConstructor
public class WebSocketConfig implements WebSocketConfigurer {

    private final DocumentProcessingWebSocketHandler documentProcessingWebSocketHandler;

    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        registry.addHandler(documentProcessingWebSocketHandler, "/ws/progress")
                .setAllowedOrigins("*"); // 生产环境应该配置具体的域名
    }
}

