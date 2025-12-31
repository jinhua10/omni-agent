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

    /**
     * 日志级别枚举
     */
    public enum LogLevel {
        TRACE, DEBUG, INFO, WARN, ERROR, OFF
    }

    private static final Logger log = LoggerFactory.getLogger(LoggingInterceptor.class);
    private final boolean logHeaders;
    private final boolean logBody;
    private final int maxBodyLength;
    private final LogLevel requestLogLevel;
    private final LogLevel responseLogLevel;

    public LoggingInterceptor() {
        this(true, true, 1000, LogLevel.DEBUG, LogLevel.DEBUG);
    }

    public LoggingInterceptor(boolean logHeaders, boolean logBody, int maxBodyLength) {
        this(logHeaders, logBody, maxBodyLength, LogLevel.DEBUG, LogLevel.DEBUG);
    }

    public LoggingInterceptor(boolean logHeaders, boolean logBody, int maxBodyLength,
                              LogLevel requestLogLevel, LogLevel responseLogLevel) {
        this.logHeaders = logHeaders;
        this.logBody = logBody;
        this.maxBodyLength = maxBodyLength;
        this.requestLogLevel = requestLogLevel;
        this.responseLogLevel = responseLogLevel;
    }

    @Override
    public HttpRequest beforeRequest(HttpRequest request) {
        if (shouldLog(requestLogLevel)) {
            StringBuilder sb = new StringBuilder();
            sb.append("HTTP请求: ").append(request.getMethod()).append(" ").append(request.getUrl());

            if (logHeaders && request.getHeaders() != null && !request.getHeaders().isEmpty()) {
                sb.append("\n  Headers: ").append(request.getHeaders());
            }

            if (logBody && request.getBody() != null && !request.getBody().isEmpty()) {
                sb.append("\n  Body: ").append(truncate(request.getBody(), maxBodyLength));
            }

            logAtLevel(requestLogLevel, sb.toString());
        }
        return request;
    }

    @Override
    public HttpResponse afterResponse(HttpResponse response) {
        if (shouldLog(responseLogLevel)) {
            StringBuilder sb = new StringBuilder();
            sb.append("HTTP响应: ").append(response.getStatusCode())
                    .append(" (").append(response.getDurationMs()).append("ms)");

            if (logHeaders && response.getHeaders() != null && !response.getHeaders().isEmpty()) {
                sb.append("\n  Headers: ").append(response.getHeaders());
            }

            if (logBody && response.getBody() != null && !response.getBody().isEmpty()) {
                sb.append("\n  Body: ").append(truncate(response.getBody(), maxBodyLength));
            }

            logAtLevel(responseLogLevel, sb.toString());
        }
        return response;
    }

    @Override
    public void onError(HttpRequest request, Exception exception) {
        log.error("HTTP请求失败: {} {} - {}",
                request.getMethod(), request.getUrl(), exception.getMessage(), exception);
    }

    /**
     * 判断是否应该记录日志
     */
    private boolean shouldLog(LogLevel level) {
        if (level == LogLevel.OFF) {
            return false;
        }
        return switch (level) {
            case TRACE -> log.isTraceEnabled();
            case DEBUG -> log.isDebugEnabled();
            case INFO -> log.isInfoEnabled();
            case WARN -> log.isWarnEnabled();
            case ERROR -> log.isErrorEnabled();
            default -> false;
        };
    }

    /**
     * 按指定级别输出日志
     */
    private void logAtLevel(LogLevel level, String message) {
        switch (level) {
            case TRACE:
                log.trace(message);
                break;
            case DEBUG:
                log.debug(message);
                break;
            case INFO:
                log.info(message);
                break;
            case WARN:
                log.warn(message);
                break;
            case ERROR:
                log.error(message);
                break;
            default:
                break;
        }
    }

    private String truncate(String text, int maxLength) {
        if (text == null || text.length() <= maxLength) {
            return text;
        }
        return text.substring(0, maxLength) + "... (truncated)";
    }
}

