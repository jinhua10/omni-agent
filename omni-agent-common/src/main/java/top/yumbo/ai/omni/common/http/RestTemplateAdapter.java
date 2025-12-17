package top.yumbo.ai.omni.common.http;

import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;

import java.util.Map;

/**
 * RestTemplate 适配器
 *
 * 使用 Spring 的 RestTemplate 实现 HTTP 请求
 *
 * 优点：
 * - Spring 自带，无需额外依赖
 * - 与 Spring Boot 自动配置集成
 * - 简单易用
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
    public String post(String url, Map<String, String> headers, String body) throws Exception {
        // 构建请求头
        HttpHeaders httpHeaders = new HttpHeaders();
        headers.forEach(httpHeaders::set);

        // 构建请求实体
        HttpEntity<String> requestEntity = new HttpEntity<>(body, httpHeaders);

        // 发送 POST 请求
        ResponseEntity<String> response = restTemplate.exchange(
                url,
                HttpMethod.POST,
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

