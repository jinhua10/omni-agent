package top.yumbo.ai.omni.marketplace.security;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.common.http.HttpClientAdapter;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.HashMap;
import java.util.Map;
import java.util.HashMap;
import java.util.Map;

/**
 * 安全的远程算法调用器
 *
 * 使用 HttpClientAdapter，支持 RestTemplate 或 OkHttp3
 *
 * 安全措施：
 * 1. HTTPS强制（生产环境）
 * 2. 超时控制
 * 3. 请求鉴权
 * 4. URL白名单（禁止访问内网）
 * 5. 防止SSRF攻击
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
public class SecureRemoteExecutor {

    @Autowired(required = false)
    private HttpClientAdapter httpClientAdapter;

    private final ObjectMapper objectMapper = new ObjectMapper();

    // 安全配置
    private static final int DEFAULT_TIMEOUT_MS = 30000;  // 30秒
    private static final boolean REQUIRE_HTTPS = false;   // 开发环境允许HTTP，生产环境改为true

    /**
     * 安全调用远程算法服务
     *
     * @param endpoint 服务端点
     * @param authToken 认证token
     * @param documentId 文档ID
     * @param context 执行上下文
     * @return 执行结果
     * @throws RemoteExecutionException 调用失败
     */
    public Map<String, Object> executeRemote(
            String endpoint,
            String authToken,
            String documentId,
            Map<String, Object> context) throws RemoteExecutionException {

        if (httpClientAdapter == null) {
            throw new RemoteExecutionException("HTTP client not available");
        }

        // 验证端点URL
        validateEndpoint(endpoint);

        try {
            // 1. 构建请求头
            Map<String, String> headers = new HashMap<>();
            headers.put("Content-Type", "application/json");
            headers.put("Accept", "application/json");

            // 添加认证token
            if (authToken != null && !authToken.isEmpty()) {
                headers.put("Authorization", "Bearer " + authToken);
            }

            // 2. 构建请求体
            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("documentId", documentId);
            requestBody.putAll(context);

            String requestJson = objectMapper.writeValueAsString(requestBody);

            // 3. 发送HTTP请求
            String responseJson = httpClientAdapter.post(endpoint + "/execute", headers, requestJson);

            // 4. 解析响应
            return parseResponse(responseJson);

        } catch (Exception e) {
            log.error("Remote algorithm execution failed: endpoint={}, error={}",
                    endpoint, e.getMessage());
            throw new RemoteExecutionException("Remote execution failed: " + e.getMessage(), e);
        }
    }

    /**
     * 验证端点URL（安全检查）
     */
    private void validateEndpoint(String endpoint) throws RemoteExecutionException {
        if (endpoint == null || endpoint.trim().isEmpty()) {
            throw new RemoteExecutionException("Endpoint URL is empty");
        }

        try {
            // 使用 URI 替代已过时的 URL(String) 构造函数
            URI uri = new URI(endpoint);

            // 检查协议（生产环境强制HTTPS）
            String scheme = uri.getScheme();
            if (REQUIRE_HTTPS && !"https".equalsIgnoreCase(scheme)) {
                throw new RemoteExecutionException("HTTPS is required for remote algorithms");
            }

            // 检查主机（防止内网扫描 - SSRF攻击防护）
            String host = uri.getHost();
            if (host == null || host.isEmpty()) {
                throw new RemoteExecutionException("Invalid endpoint URL: missing host");
            }

            if (isInternalHost(host)) {
                log.warn("Attempting to access internal host: {}", host);
                throw new RemoteExecutionException("Access to internal hosts is forbidden");
            }

        } catch (URISyntaxException e) {
            throw new RemoteExecutionException("Invalid endpoint URL: " + endpoint);
        }
    }

    /**
     * 检查是否为内网地址（防止SSRF攻击）
     */
    private boolean isInternalHost(String host) {
        // 检查常见的内网地址
        return host.startsWith("127.") ||
               host.equals("localhost") ||
               host.startsWith("192.168.") ||
               host.startsWith("10.") ||
               host.startsWith("172.16.") ||
               host.startsWith("172.17.") ||
               host.startsWith("172.18.") ||
               host.startsWith("172.19.") ||
               host.startsWith("172.20.") ||
               host.startsWith("172.21.") ||
               host.startsWith("172.22.") ||
               host.startsWith("172.23.") ||
               host.startsWith("172.24.") ||
               host.startsWith("172.25.") ||
               host.startsWith("172.26.") ||
               host.startsWith("172.27.") ||
               host.startsWith("172.28.") ||
               host.startsWith("172.29.") ||
               host.startsWith("172.30.") ||
               host.startsWith("172.31.");
    }

    /**
     * 解析 JSON 响应
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> parseResponse(String json) throws RemoteExecutionException {
        try {
            return objectMapper.readValue(json, Map.class);
        } catch (Exception e) {
            log.error("Failed to parse response JSON: {}", json);
            throw new RemoteExecutionException("Invalid response format: " + e.getMessage());
        }
    }

    /**
     * 远程执行异常
     */
    public static class RemoteExecutionException extends Exception {
        public RemoteExecutionException(String message) {
            super(message);
        }

        public RemoteExecutionException(String message, Throwable cause) {
            super(message, cause);
        }
    }
}

