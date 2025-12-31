package top.yumbo.ai.omni.common.http;

import okhttp3.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import top.yumbo.ai.omni.common.exception.HttpException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;

/**
 * OkHttp3 适配器
 *
 * 使用 OkHttp3 实现 HTTP 请求
 *
 * 优点：
 * - 性能更好，连接池管理更优
 * - 支持 HTTP/2
 * - 内存占用更低
 * - 适合高频调用场景
 *
 * 注意：需要引入依赖
 * <pre>
 * &lt;dependency&gt;
 *     &lt;groupId&gt;com.squareup.okhttp3&lt;/groupId&gt;
 *     &lt;artifactId&gt;okhttp&lt;/artifactId&gt;
 *     &lt;version&gt;4.12.0&lt;/version&gt;
 * &lt;/dependency&gt;
 * </pre>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class OkHttp3Adapter implements HttpClientAdapter {

    private static final Logger log = LoggerFactory.getLogger(OkHttp3Adapter.class);
    private OkHttpClient client;
    private static final MediaType JSON = MediaType.parse("application/json; charset=utf-8");
    private final List<HttpInterceptor> interceptors = new CopyOnWriteArrayList<>();
    private long maxRequestSize = 10 * 1024 * 1024; // 默认10MB
    private long maxResponseSize = 10 * 1024 * 1024; // 默认10MB
    private Executor asyncExecutor; // null表示使用默认ForkJoinPool

    /**
     * 使用默认配置的 OkHttpClient
     */
    public OkHttp3Adapter() {
        this.client = createDefaultClient();
    }

    /**
     * 使用自定义的 OkHttpClient
     */
    public OkHttp3Adapter(OkHttpClient client) {
        this.client = client;
    }

    /**
     * 创建默认的 OkHttpClient
     */
    private static OkHttpClient createDefaultClient() {
        return new OkHttpClient.Builder()
                .connectTimeout(30, TimeUnit.SECONDS)
                .readTimeout(60, TimeUnit.SECONDS)
                .writeTimeout(60, TimeUnit.SECONDS)
                .connectionPool(new ConnectionPool(20, 5, TimeUnit.MINUTES))
                .retryOnConnectionFailure(true)
                .build();
    }

    @Override
    public String get(String url, Map<String, String> headers) throws Exception {
        validateUrl(url);

        Request.Builder requestBuilder = new Request.Builder()
                .url(url)
                .get();

        if (headers != null) {
            headers.forEach(requestBuilder::addHeader);
        }

        return executeRequest(requestBuilder.build(), "GET", url, headers, null);
    }

    @Override
    public String post(String url, Map<String, String> headers, String body) throws Exception {
        validateUrl(url);
        validateRequestSize(body);

        RequestBody requestBody = body != null
            ? RequestBody.create(body, JSON)
            : RequestBody.create("", JSON);

        Request.Builder requestBuilder = new Request.Builder()
                .url(url)
                .post(requestBody);

        if (headers != null) {
            headers.forEach(requestBuilder::addHeader);
        }

        return executeRequest(requestBuilder.build(), "POST", url, headers, body);
    }

    @Override
    public String put(String url, Map<String, String> headers, String body) throws Exception {
        validateUrl(url);
        validateRequestSize(body);

        RequestBody requestBody = body != null
            ? RequestBody.create(body, JSON)
            : RequestBody.create("", JSON);

        Request.Builder requestBuilder = new Request.Builder()
                .url(url)
                .put(requestBody);

        if (headers != null) {
            headers.forEach(requestBuilder::addHeader);
        }

        return executeRequest(requestBuilder.build(), "PUT", url, headers, body);
    }

    @Override
    public String delete(String url, Map<String, String> headers) throws Exception {
        validateUrl(url);

        Request.Builder requestBuilder = new Request.Builder()
                .url(url)
                .delete();

        if (headers != null) {
            headers.forEach(requestBuilder::addHeader);
        }

        return executeRequest(requestBuilder.build(), "DELETE", url, headers, null);
    }

    /**
     * 执行HTTP请求
     */
    private String executeRequest(Request request, String method, String url,
                                   Map<String, String> headers, String body) throws Exception {
        long startTime = System.currentTimeMillis();

        // 执行拦截器 - beforeRequest
        HttpInterceptor.HttpRequest httpRequest = new HttpInterceptor.HttpRequest(url, method, headers, body);
        for (HttpInterceptor interceptor : interceptors) {
            httpRequest = interceptor.beforeRequest(httpRequest);
        }

        try (Response response = client.newCall(request).execute()) {
            long duration = System.currentTimeMillis() - startTime;

            if (!response.isSuccessful()) {
                String errorBody = "";
                ResponseBody responseBody = response.body();
                if (responseBody != null) {
                    errorBody = responseBody.string();
                }

                HttpException exception = new HttpException(
                    response.code(),
                    response.message(),
                    url,
                    errorBody,
                    method
                );

                // 执行拦截器 - onError
                for (HttpInterceptor interceptor : interceptors) {
                    interceptor.onError(httpRequest, exception);
                }

                throw exception;
            }

            ResponseBody responseBody = response.body();
            if (responseBody == null) {
                throw new HttpException(response.code(), "响应体为空", url);
            }

            String responseBodyString = responseBody.string();

            // 验证响应体大小
            validateResponseSize(responseBodyString);

            // 执行拦截器 - afterResponse
            HttpInterceptor.HttpResponse httpResponse = new HttpInterceptor.HttpResponse(
                response.code(),
                responseBodyString,
                new HashMap<>(),
                duration
            );

            for (HttpInterceptor interceptor : interceptors) {
                httpResponse = interceptor.afterResponse(httpResponse);
            }

            return httpResponse.getBody();

        } catch (top.yumbo.ai.omni.common.exception.ValidationException e) {
            // ValidationException直接抛出，不包装
            throw e;
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
    public void setTimeout(int connectTimeoutSeconds, int readTimeoutSeconds) {
        this.client = this.client.newBuilder()
                .connectTimeout(connectTimeoutSeconds, TimeUnit.SECONDS)
                .readTimeout(readTimeoutSeconds, TimeUnit.SECONDS)
                .writeTimeout(readTimeoutSeconds, TimeUnit.SECONDS)
                .build();
    }

    @Override
    public void setMaxRequestSize(long maxBytes) {
        this.maxRequestSize = maxBytes;
    }

    @Override
    public void setMaxResponseSize(long maxBytes) {
        this.maxResponseSize = maxBytes;
    }

    @Override
    public void setAsyncExecutor(Executor executor) {
        this.asyncExecutor = executor;
    }

    @Override
    public Executor getAsyncExecutor() {
        return asyncExecutor != null ? asyncExecutor : HttpClientAdapter.super.getAsyncExecutor();
    }

    /**
     * 验证请求体大小
     */
    private void validateRequestSize(String body) {
        if (body != null && maxRequestSize > 0) {
            long bodySize = body.getBytes().length;
            if (bodySize > maxRequestSize) {
                throw new top.yumbo.ai.omni.common.exception.ValidationException(
                    "body",
                    bodySize,
                    "Request body size " + bodySize + " bytes exceeds maximum allowed size " + maxRequestSize + " bytes"
                );
            }
        }
    }

    /**
     * 验证响应体大小
     */
    private void validateResponseSize(String responseBody) {
        if (responseBody != null && maxResponseSize > 0) {
            long bodySize = responseBody.getBytes().length;
            if (bodySize > maxResponseSize) {
                throw new top.yumbo.ai.omni.common.exception.ValidationException(
                    "responseBody",
                    bodySize,
                    "Response body size " + bodySize + " bytes exceeds maximum allowed size " + maxResponseSize + " bytes"
                );
            }
        }
    }

    @Override
    public String getName() {
        return "OkHttp3";
    }
}

