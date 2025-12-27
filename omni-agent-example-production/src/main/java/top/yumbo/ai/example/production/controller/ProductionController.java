package top.yumbo.ai.example.production.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.model.AIResponse;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.storage.api.model.StorageStatistics;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 生产级 API 控制器
 * (Production API Controller)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/v1")
@RequiredArgsConstructor
public class ProductionController {

    private final AIService aiService;
    private final DocumentStorageService documentStorageService;

    /**
     * 健康检查
     */
    @GetMapping("/health")
    public Map<String, Object> health() {
        Map<String, Object> health = new HashMap<>();
        health.put("status", "UP");
        health.put("ai", aiService.isHealthy());
        health.put("storage", documentStorageService.isHealthy());
        health.put("timestamp", System.currentTimeMillis());
        return health;
    }

    /**
     * 对话接口
     */
    @PostMapping("/chat")
    public Map<String, Object> chat(@RequestBody ChatRequest request) {
        log.info("Chat request: {}", request.getMessage());

        try {
            List<ChatMessage> messages = new ArrayList<>();
            messages.add(ChatMessage.builder()
                    .role("user")
                    .content(request.getMessage())
                    .build());

            AIResponse response = aiService.chat(messages);

            Map<String, Object> result = new HashMap<>();
            result.put("success", true);
            result.put("response", response.getText());
            result.put("model", response.getModel());
            result.put("tokens", response.getTotalTokens());

            return result;
        } catch (Exception e) {
            log.error("Chat failed", e);

            Map<String, Object> result = new HashMap<>();
            result.put("success", false);
            result.put("error", e.getMessage());
            return result;
        }
    }

    /**
     * 存储统计
     */
    @GetMapping("/stats")
    public StorageStatistics stats() {
        return documentStorageService.getStatistics();
    }

    /**
     * AI 状态
     */
    @GetMapping("/ai/status")
    public Map<String, Object> aiStatus() {
        return aiService.getStatus();
    }

    /**
     * 聊天请求对象
     */
    public static class ChatRequest {
        private String message;

        public String getMessage() {
            return message;
        }

        public void setMessage(String message) {
            this.message = message;
        }
    }
}

