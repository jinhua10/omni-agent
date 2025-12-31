package top.yumbo.ai.omni.common.http;

import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

import java.util.Map;

/**
 * RestTemplate 适配器
 * <p>
 * 使用 Spring 的 RestTemplate 实现 HTTP 请求
 * <p>
 * 优点：
 * - Spring 自带，无需额外依赖
 * - 与 Spring Boot 自动配置集成
 * - 简单易用
 * <p>
 * 注意：
 * - 超时配置需要在创建RestTemplate时设置
 * - setTimeout方法对已创建的实例无效
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class RestTemplateAdapter implements HttpClientAdapter {

    private final RestTemplate restTemplate;

    public RestTemplateAdapter(RestTemplate restTemplate) {
        this.restTemplate = restTemplate;
    }

    @Override
    public String get(String url, Map<String, String> headers) throws Exception {
        validateUrl(url);
        return executeRequest(url, HttpMethod.GET, headers, null);
    }

    @Override
    public String post(String url, Map<String, String> headers, String body) throws Exception {
        validateUrl(url);
        return executeRequest(url, HttpMethod.POST, headers, body);
    }

    @Override
    public String put(String url, Map<String, String> headers, String body) throws Exception {
        validateUrl(url);
        return executeRequest(url, HttpMethod.PUT, headers, body);
    }

    @Override
    public String delete(String url, Map<String, String> headers) throws Exception {
        validateUrl(url);
        return executeRequest(url, HttpMethod.DELETE, headers, null);
    }

    @Override
    public void setTimeout(int connectTimeoutSeconds, int readTimeoutSeconds) {
        // RestTemplate实例创建后无法动态修改超时时间
        // 需要在创建RestTemplate时配置超时参数
        // 此方法为空实现，保持接口兼容性
    }

    /**
     * 执行HTTP请求
     */
    private String executeRequest(String url, HttpMethod method, Map<String, String> headers, String body) {
        // 构建请求头
        HttpHeaders httpHeaders = new HttpHeaders();
        if (headers != null) {
            headers.forEach(httpHeaders::set);
        }

        // 构建请求实体
        HttpEntity<String> requestEntity = new HttpEntity<>(body, httpHeaders);

        // 发送请求
        ResponseEntity<String> response = restTemplate.exchange(
                url,
                method,
                requestEntity,
                String.class
        );

        if (!response.getStatusCode().is2xxSuccessful()) {
            throw new RuntimeException("HTTP请求失败: " + response.getStatusCode());
        }

        return response.getBody();
    }

    @Override
    public String getName() {
        return "RestTemplate";
    }
}

