package top.yumbo.ai.omni.common.http;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * 日志拦截器
 * <p>
 * 记录HTTP请求和响应的详细信息
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class LoggingInterceptor implements HttpInterceptor {

    private static final Logger log = LoggerFactory.getLogger(LoggingInterceptor.class);
    private final boolean logHeaders;
    private final boolean logBody;
    private final int maxBodyLength;

    public LoggingInterceptor() {
        this(true, true, 1000);
    }

    public LoggingInterceptor(boolean logHeaders, boolean logBody, int maxBodyLength) {
        this.logHeaders = logHeaders;
        this.logBody = logBody;
        this.maxBodyLength = maxBodyLength;
    }

    @Override
    public HttpRequest beforeRequest(HttpRequest request) {
        if (log.isDebugEnabled()) {
            StringBuilder sb = new StringBuilder();
            sb.append("HTTP请求: ").append(request.getMethod()).append(" ").append(request.getUrl());

            if (logHeaders && request.getHeaders() != null && !request.getHeaders().isEmpty()) {
                sb.append("\n  Headers: ").append(request.getHeaders());
            }

            if (logBody && request.getBody() != null && !request.getBody().isEmpty()) {
                sb.append("\n  Body: ").append(truncate(request.getBody(), maxBodyLength));
            }

            log.debug(sb.toString());
        }
        return request;
    }

    @Override
    public HttpResponse afterResponse(HttpResponse response) {
        if (log.isDebugEnabled()) {
            StringBuilder sb = new StringBuilder();
            sb.append("HTTP响应: ").append(response.getStatusCode())
              .append(" (").append(response.getDurationMs()).append("ms)");

            if (logHeaders && response.getHeaders() != null && !response.getHeaders().isEmpty()) {
                sb.append("\n  Headers: ").append(response.getHeaders());
            }

            if (logBody && response.getBody() != null && !response.getBody().isEmpty()) {
                sb.append("\n  Body: ").append(truncate(response.getBody(), maxBodyLength));
            }

            log.debug(sb.toString());
        }
        return response;
    }

    @Override
    public void onError(HttpRequest request, Exception exception) {
        log.error("HTTP请求失败: {} {} - {}",
            request.getMethod(), request.getUrl(), exception.getMessage(), exception);
    }

    private String truncate(String text, int maxLength) {
        if (text == null || text.length() <= maxLength) {
            return text;
        }
        return text.substring(0, maxLength) + "... (truncated)";
    }
}

