package top.yumbo.ai.omni.knowledge.registry.qa.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.knowledge.registry.qa.model.IntentAnalysisResult;

/**
 * 意图分析器
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class IntentAnalyzer {

    @Autowired
    private AIService aiService;

    @Autowired
    private ConversationManager conversationManager;

    private final ObjectMapper objectMapper = new ObjectMapper();

    /**
     * 分析用户意图
     */
    public IntentAnalysisResult analyzeIntent(String userQuery, String conversationId) {
        try {
            // 1. 构建分析提示词
            String prompt = buildIntentAnalysisPrompt(userQuery, conversationId);

            // 2. 调用 AI 分析
            log.info("开始意图分析: query={}", userQuery);
            String aiResponse = aiService.chat(prompt);
            log.debug("AI 意图分析响应: {}", aiResponse);

            // 3. 解析 AI 响应
            return parseIntentResponse(aiResponse);

        } catch (Exception e) {
            log.error("意图分析失败", e);
            return IntentAnalysisResult.createDefault();
        }
    }

    /**
     * 构建意图分析提示词
     */
    private String buildIntentAnalysisPrompt(String query, String conversationId) {
        String conversationHistory = "";
        if (conversationId != null && !conversationId.isEmpty()) {
            conversationHistory = conversationManager.formatConversationHistory(conversationId);
        }

        return String.format("""
                你是一个意图分析专家。请分析用户的问题并识别：
                
                1. 核心意图（what）：用户想要做什么？
                2. 关键实体（who/what）：涉及哪些对象？
                3. 技术上下文（how）：使用什么技术栈？
                4. 约束条件（constraints）：有什么限制？
                5. 缺失信息（missing）：还需要什么信息才能完整回答？
                
                ## 用户问题
                %s
                
                ## 对话历史
                %s
                
                ## 输出格式（JSON）
                请严格按照以下 JSON 格式输出，不要包含任何其他内容：
                {
                  "intent": "用户的核心意图描述",
                  "entities": ["实体1", "实体2"],
                  "techStack": ["技术栈1", "技术栈2"],
                  "constraints": ["约束1", "约束2"],
                  "missingInfo": ["缺失信息1", "缺失信息2"],
                  "confidence": 0.85
                }
                
                如果某个字段没有内容，请使用空数组 []。
                confidence 是你对分析结果的置信度，范围 0-1。
                """,
                query,
                conversationHistory.isEmpty() ? "（首次对话）" : conversationHistory
        );
    }

    /**
     * 解析意图分析响应
     */
    private IntentAnalysisResult parseIntentResponse(String response) {
        try {
            // 提取 JSON 部分（处理可能包含额外文本的情况）
            String jsonPart = extractJson(response);

            // 解析 JSON
            IntentAnalysisResult result = objectMapper.readValue(jsonPart, IntentAnalysisResult.class);

            log.info("✅ 意图分析成功: intent={}, missingInfo={}",
                    result.getIntent(), result.getMissingInfo());

            return result;

        } catch (Exception e) {
            log.error("解析意图分析结果失败: {}", response, e);

            // 降级：创建默认结果
            return IntentAnalysisResult.builder()
                    .intent("回答问题")
                    .confidence(0.5)
                    .build();
        }
    }

    /**
     * 从响应中提取 JSON
     */
    private String extractJson(String response) {
        // 查找 JSON 对象的开始和结束
        int start = response.indexOf('{');
        int end = response.lastIndexOf('}');

        if (start >= 0 && end > start) {
            return response.substring(start, end + 1);
        }

        return response;
    }
}

