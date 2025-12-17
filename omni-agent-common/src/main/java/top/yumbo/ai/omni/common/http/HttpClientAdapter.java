package top.yumbo.ai.omni.common.http;

import java.util.Map;

/**
 * HTTP 客户端适配器接口
 *
 * 支持多种 HTTP 客户端实现：
 * - RestTemplate（默认，Spring 自带，零依赖）
 * - OkHttpClient（可选，需引入依赖，高性能）
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface HttpClientAdapter {

    /**
     * 发送 POST 请求
     *
     * @param url 请求URL
     * @param headers 请求头
     * @param body 请求体（JSON字符串）
     * @return 响应体（JSON字符串）
     * @throws Exception 请求失败时抛出异常
     */
    String post(String url, Map<String, String> headers, String body) throws Exception;

    /**
     * 获取适配器名称
     */
    String getName();
}

