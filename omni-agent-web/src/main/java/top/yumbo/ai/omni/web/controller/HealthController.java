package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import top.yumbo.ai.ai.api.AIService;
import top.yumbo.ai.persistence.api.QuestionClassifierPersistence;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.storage.api.DocumentStorageService;

import java.util.HashMap;
import java.util.Map;

/**
 * 健康检查控制器
 *
 * <p>提供系统健康状态检查接口</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api")
@RequiredArgsConstructor
public class HealthController {

    private final QuestionClassifierPersistence persistence;
    private final DocumentStorageService storageService;
    private final RAGService ragService;
    private final AIService aiService;

    /**
     * 健康检查
     *
     * @return 系统健康状态信息
     */
    @GetMapping("/health")
    public Map<String, Object> health() {
        Map<String, Object> result = new HashMap<>();
        result.put("status", "UP");
        result.put("persistence", persistence.getClass().getSimpleName());
        result.put("documentStorage", storageService.getClass().getSimpleName());
        result.put("rag", ragService.getClass().getSimpleName());
        result.put("ai", aiService.getClass().getSimpleName());
        result.put("aiModel", aiService.getCurrentModel());
        result.put("message", "OmniAgent is running with pluggable architecture!");
        return result;
    }
}

