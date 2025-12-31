package top.yumbo.ai.omni.common.http;

import lombok.Data;

import java.util.Map;

/**
 * HTTP请求拦截器接口
 * <p>
 * 支持在请求发送前和响应接收后进行拦截处理
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface HttpInterceptor {

    /**
     * 请求拦截
     * <p>
     * 在请求发送前调用，可以修改请求参数
     *
     * @param request 请求对象
     * @return 处理后的请求对象
     */
    default HttpRequest beforeRequest(HttpRequest request) {
        return request;
    }

    /**
     * 响应拦截
     * <p>
     * 在响应接收后调用，可以修改响应内容
     *
     * @param response 响应对象
     * @return 处理后的响应对象
     */
    default HttpResponse afterResponse(HttpResponse response) {
        return response;
    }

    /**
     * 异常拦截
     * <p>
     * 在发生异常时调用
     *
     * @param request   请求对象
     * @param exception 异常对象
     */
    default void onError(HttpRequest request, Exception exception) {
        // 默认不处理
    }

    /**
     * 获取拦截器执行优先级
     * <p>
     * 数值越小优先级越高，越先执行
     * <p>
     * 默认优先级为 0
     *
     * @return 优先级值
     */
    default int getOrder() {
        return 0;
    }

    /**
     * HTTP请求对象
     */
    @Data
    class HttpRequest {
        private String url;
        private String method;
        private Map<String, String> headers;
        private String body;

        public HttpRequest(String url, String method, Map<String, String> headers, String body) {
            this.url = url;
            this.method = method;
            this.headers = headers;
            this.body = body;
        }

    }

    /**
     * HTTP响应对象
     */
    @Data
    class HttpResponse {
        private int statusCode;
        private String body;
        private Map<String, String> headers;
        private long durationMs;

        public HttpResponse(int statusCode, String body, Map<String, String> headers, long durationMs) {
            this.statusCode = statusCode;
            this.body = body;
            this.headers = headers;
            this.durationMs = durationMs;
        }

    }
}

