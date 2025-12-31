package top.yumbo.ai.omni.common.exception;

/**
 * HTTP异常
 * <p>
 * 封装HTTP请求过程中的异常信息
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class HttpException extends BaseException {

    private final int statusCode;
    private final String url;
    private final String responseBody;
    private final String method;

    public HttpException(int statusCode, String message, String url) {
        this(statusCode, message, url, null, null);
    }

    public HttpException(int statusCode, String message, String url, String responseBody) {
        this(statusCode, message, url, responseBody, null);
    }

    public HttpException(int statusCode, String message, String url, String responseBody, String method) {
        super("HTTP_ERROR", String.format("[%s] %s - Status: %d, URL: %s",
            method != null ? method : "HTTP", message, statusCode, url));
        this.statusCode = statusCode;
        this.url = url;
        this.responseBody = responseBody;
        this.method = method;
    }

    public HttpException(int statusCode, String message, String url, Throwable cause) {
        super("HTTP_ERROR", String.format("HTTP请求失败 - Status: %d, URL: %s, Message: %s",
            statusCode, url, message), cause);
        this.statusCode = statusCode;
        this.url = url;
        this.responseBody = null;
        this.method = null;
    }

    public int getStatusCode() {
        return statusCode;
    }

    public String getUrl() {
        return url;
    }

    public String getResponseBody() {
        return responseBody;
    }

    public String getMethod() {
        return method;
    }

    /**
     * 判断是否为客户端错误 (4xx)
     */
    public boolean isClientError() {
        return statusCode >= 400 && statusCode < 500;
    }

    /**
     * 判断是否为服务端错误 (5xx)
     */
    public boolean isServerError() {
        return statusCode >= 500 && statusCode < 600;
    }
}

