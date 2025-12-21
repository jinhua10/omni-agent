package top.yumbo.ai.omni.web.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.socket.config.annotation.EnableWebSocket;
import org.springframework.web.socket.config.annotation.WebSocketConfigurer;
import org.springframework.web.socket.config.annotation.WebSocketHandlerRegistry;
import org.springframework.web.socket.server.standard.ServletServerContainerFactoryBean;
import top.yumbo.ai.omni.web.websocket.ProgressWebSocketHandler;

/**
 * WebSocket 配置类
 * (WebSocket Configuration)
 *
 * <p>
 * 配置 WebSocket 端点和处理器
 * (Configure WebSocket endpoints and handlers)
 * </p>
 *
 * @author AI Reviewer Team
 * @since 2.0.0 (Phase 4)
 */
@Configuration
@EnableWebSocket
public class WebSocketConfig implements WebSocketConfigurer {

    @Override
    public void registerWebSocketHandlers(WebSocketHandlerRegistry registry) {
        // 注册进度推送 WebSocket 端点
        // (Register progress push WebSocket endpoint)
        registry.addHandler(progressWebSocketHandler(), "/ws/progress")
                .setAllowedOrigins("*"); // 允许所有来源（生产环境需要限制）
    }

    /**
     * 创建进度 WebSocket 处理器 Bean
     * (Create progress WebSocket handler bean)
     */
    @Bean
    public ProgressWebSocketHandler progressWebSocketHandler() {
        return new ProgressWebSocketHandler();
    }

    /**
     * 配置 WebSocket 容器
     * (Configure WebSocket container)
     */
    @Bean
    public ServletServerContainerFactoryBean createWebSocketContainer() {
        ServletServerContainerFactoryBean container = new ServletServerContainerFactoryBean();
        container.setMaxTextMessageBufferSize(8192);
        container.setMaxBinaryMessageBufferSize(8192);
        return container;
    }
}

