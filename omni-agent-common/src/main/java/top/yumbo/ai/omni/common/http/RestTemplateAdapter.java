package top.yumbo.ai.omni.common.http;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.*;
import org.springframework.web.client.RestTemplate;
import top.yumbo.ai.omni.common.exception.HttpException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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

    private static final Logger log = LoggerFactory.getLogger(RestTemplateAdapter.class);
    private final RestTemplate restTemplate;
    private final List<HttpInterceptor> interceptors = new ArrayList<>();

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

    /**
     * 执行HTTP请求
     */
    private String executeRequest(String url, HttpMethod method, Map<String, String> headers, String body) {
        long startTime = System.currentTimeMillis();

        // 执行拦截器 - beforeRequest
        HttpInterceptor.HttpRequest httpRequest = new HttpInterceptor.HttpRequest(
            url, method.name(), headers, body);
        for (HttpInterceptor interceptor : interceptors) {
            httpRequest = interceptor.beforeRequest(httpRequest);
        }

        try {
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

            long duration = System.currentTimeMillis() - startTime;

            if (!response.getStatusCode().is2xxSuccessful()) {
                HttpException exception = new HttpException(
                    response.getStatusCode().value(),
                    "HTTP请求失败",
                    url,
                    response.getBody(),
                    method.name()
                );

                // 执行拦截器 - onError
                for (HttpInterceptor interceptor : interceptors) {
                    interceptor.onError(httpRequest, exception);
                }

                throw exception;
            }

            // 执行拦截器 - afterResponse
            HttpInterceptor.HttpResponse httpResponse = new HttpInterceptor.HttpResponse(
                response.getStatusCode().value(),
                response.getBody(),
                new HashMap<>(),
                duration
            );

            for (HttpInterceptor interceptor : interceptors) {
                httpResponse = interceptor.afterResponse(httpResponse);
            }

            return httpResponse.getBody();

        } catch (HttpException e) {
            throw e;
        } catch (Exception e) {
            // 执行拦截器 - onError
            for (HttpInterceptor interceptor : interceptors) {
                interceptor.onError(httpRequest, e);
            }
            throw new HttpException(0, "请求执行失败: " + e.getMessage(), url, e);
        }
    }

    @Override
    public void addInterceptor(HttpInterceptor interceptor) {
        if (interceptor != null) {
            interceptors.add(interceptor);
        }
    }

    @Override
    public void clearInterceptors() {
        interceptors.clear();
    }

    @Override
    public String getName() {
        return "RestTemplate";
    }
}

