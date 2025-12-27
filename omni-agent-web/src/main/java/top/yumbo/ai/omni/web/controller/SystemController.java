package top.yumbo.ai.omni.web.controller;

import jakarta.servlet.http.HttpServletRequest;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.web.dto.ApiDtos.ConversationHistory;

import java.security.MessageDigest;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 系统配置控制器
 *
 * <p>提供系统级配置和工具接口：</p>
 * <ul>
 *   <li>用户 ID 生成</li>
 *   <li>对话历史管理</li>
 *   <li>系统配置</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/system")
@RequiredArgsConstructor
public class SystemController {

    /**
     * 对话历史存储（内存）
     * TODO: 迁移到持久化存储
     */
    private final Map<String, List<ConversationHistory>> conversationHistoryMap = new ConcurrentHashMap<>();

    /**
     * 生成或获取用户 ID
     *
     * @param request HTTP 请求
     * @return 用户信息
     */
    @GetMapping("/user-id")
    public Map<String, Object> getUserId(HttpServletRequest request) {
        Map<String, Object> result = new HashMap<>();

        try {
            String clientIp = getClientIp(request);
            String userId = generateUserId(clientIp);

            result.put("userId", userId);
            result.put("userInfo", Map.of(
                    "ip", clientIp,
                    "createdAt", System.currentTimeMillis()
            ));
            result.put("status", "success");

            log.info("🆔 Generated user ID: {} for IP: {}", userId, clientIp);
        } catch (Exception e) {
            log.error("❌ Failed to generate user ID", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
        }

        return result;
    }

    /**
     * 获取对话历史
     *
     * @param userId   用户 ID
     * @param page     页码
     * @param pageSize 每页大小
     * @param keyword  关键词
     * @return 对话历史列表
     */
    @GetMapping("/history")
    public Map<String, Object> getConversationHistory(
            @RequestParam String userId,
            @RequestParam(defaultValue = "1") int page,
            @RequestParam(defaultValue = "20") int pageSize,
            @RequestParam(required = false) String keyword) {

        Map<String, Object> result = new HashMap<>();

        try {
            List<ConversationHistory> userHistory = conversationHistoryMap.getOrDefault(userId, new ArrayList<>());

            // 过滤关键词
            List<ConversationHistory> filtered = userHistory;
            if (keyword != null && !keyword.trim().isEmpty()) {
                String lowerKeyword = keyword.toLowerCase();
                filtered = userHistory.stream()
                        .filter(h -> h.getQuestion().toLowerCase().contains(lowerKeyword) ||
                                (h.getAnswer() != null && h.getAnswer().toLowerCase().contains(lowerKeyword)))
                        .collect(Collectors.toList());
            }

            // 降序排序（最新的在前）
            filtered.sort((a, b) -> Long.compare(b.getTimestamp(), a.getTimestamp()));

            // 分页
            int total = filtered.size();
            int start = (page - 1) * pageSize;
            int end = Math.min(start + pageSize, total);

            List<ConversationHistory> pageData = start < total ?
                    filtered.subList(start, end) : new ArrayList<>();

            result.put("list", pageData);
            result.put("total", total);
            result.put("page", page);
            result.put("pageSize", pageSize);
            result.put("hasMore", end < total);
            result.put("status", "success");

            log.info("📜 Retrieved {} conversation history items for user: {}", pageData.size(), userId);

        } catch (Exception e) {
            log.error("❌ Failed to get conversation history", e);
            result.put("status", "error");
            result.put("error", e.getMessage());
            result.put("list", new ArrayList<>());
        }

        return result;
    }

    /**
     * 保存对话历史
     *
     * @param userId   用户 ID
     * @param question 问题
     * @param answer   答案
     */
    public void saveConversationHistory(String userId, String question, String answer) {
        ConversationHistory history = new ConversationHistory();
        history.setQuestion(question);
        history.setAnswer(answer);
        history.setTimestamp(System.currentTimeMillis());
        history.setUserId(userId);

        conversationHistoryMap.computeIfAbsent(userId, k -> new ArrayList<>()).add(history);

        log.info("💾 Saved conversation for user: {}, total: {}",
                userId, conversationHistoryMap.get(userId).size());
    }

    // ========== 私有辅助方法 ==========

    /**
     * 获取客户端真实 IP
     */
    private String getClientIp(HttpServletRequest request) {
        String ip = request.getHeader("X-Forwarded-For");
        if (ip == null || ip.isEmpty() || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getHeader("X-Real-IP");
        }
        if (ip == null || ip.isEmpty() || "unknown".equalsIgnoreCase(ip)) {
            ip = request.getRemoteAddr();
        }
        // 如果有多个 IP，取第一个
        if (ip != null && ip.contains(",")) {
            ip = ip.split(",")[0].trim();
        }
        return ip;
    }

    /**
     * 生成用户 ID
     * 使用 IP 地址的哈希值生成用户 ID
     */
    private String generateUserId(String ip) {
        try {
            MessageDigest md = MessageDigest.getInstance("MD5");
            byte[] hashBytes = md.digest(ip.getBytes());
            StringBuilder sb = new StringBuilder();
            for (byte b : hashBytes) {
                sb.append(String.format("%02x", b));
            }
            return "user_" + sb.toString().substring(0, 16);
        } catch (Exception e) {
            // 降级方案：使用 IP 直接编码
            return "user_" + ip.replace(".", "_").replace(":", "_");
        }
    }
}



