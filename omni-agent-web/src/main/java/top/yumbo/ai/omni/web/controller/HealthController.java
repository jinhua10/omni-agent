package top.yumbo.ai.omni.web.controller;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

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
public class HealthController {

    @Autowired(required = false)
    private KnowledgeRegistry knowledgeRegistry;

    @Autowired
    private DocumentStorageService storageService;

    @Autowired
    private RagService ragService;

    @Autowired
    private AIService aiService;

    /**
     * 健康检查
     *
     * @return 系统健康状态信息
     */
    @GetMapping("/health")
    public Map<String, Object> health() {
        Map<String, Object> result = new HashMap<>();
        result.put("status", "UP");
        result.put("knowledgeRegistry", knowledgeRegistry != null ?
                knowledgeRegistry.getClass().getSimpleName() : "Not Available");
        result.put("documentStorage", storageService.getClass().getSimpleName());
        result.put("rag", ragService.getClass().getSimpleName());
        result.put("ai", aiService.getClass().getSimpleName());
        result.put("aiModel", aiService.getCurrentModel());
        result.put("message", "OmniAgent is running" +
                (knowledgeRegistry != null ? " with Knowledge Network Architecture!" : "!"));
        return result;
    }
}






