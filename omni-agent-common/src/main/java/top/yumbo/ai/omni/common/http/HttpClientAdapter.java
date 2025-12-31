package top.yumbo.ai.omni.common.http;

import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.Executor;
import java.util.concurrent.ForkJoinPool;

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
     * 发送 GET 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @return 响应体（JSON字符串）
     * @throws IllegalArgumentException URL格式错误
     * @throws Exception 请求失败时抛出异常
     */
    String get(String url, Map<String, String> headers) throws Exception;

    /**
     * 发送 POST 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @param body 请求体（JSON字符串，可为null）
     * @return 响应体（JSON字符串）
     * @throws IllegalArgumentException URL格式错误
     * @throws Exception 请求失败时抛出异常
     */
    String post(String url, Map<String, String> headers, String body) throws Exception;

    /**
     * 发送 PUT 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @param body 请求体（JSON字符串，可为null）
     * @return 响应体（JSON字符串）
     * @throws IllegalArgumentException URL格式错误
     * @throws Exception 请求失败时抛出异常
     */
    String put(String url, Map<String, String> headers, String body) throws Exception;

    /**
     * 发送 DELETE 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @return 响应体（JSON字符串）
     * @throws IllegalArgumentException URL格式错误
     * @throws Exception 请求失败时抛出异常
     */
    String delete(String url, Map<String, String> headers) throws Exception;

    /**
     * 发送 PATCH 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @param body 请求体（JSON字符串，可为null）
     * @return 响应体（JSON字符串）
     * @throws IllegalArgumentException URL格式错误
     * @throws Exception 请求失败时抛出异常
     */
    String patch(String url, Map<String, String> headers, String body) throws Exception;

    /**
     * 设置超时时间
     *
     * @param connectTimeoutSeconds 连接超时时间（秒）
     * @param readTimeoutSeconds 读取超时时间（秒）
     */
    default void setTimeout(int connectTimeoutSeconds, int readTimeoutSeconds) {
        // 默认实现为空，子类可选择性实现
    }

    /**
     * 设置请求体最大大小
     *
     * @param maxBytes 最大字节数，0或负数表示不限制
     */
    default void setMaxRequestSize(long maxBytes) {
        // 默认实现为空，子类可选择性实现
    }

    /**
     * 设置响应体最大大小
     *
     * @param maxBytes 最大字节数，0或负数表示不限制
     */
    default void setMaxResponseSize(long maxBytes) {
        // 默认实现为空，子类可选择性实现
    }

    /**
     * 设置异步请求执行器
     *
     * @param executor 自定义执行器，null表示使用默认ForkJoinPool
     */
    default void setAsyncExecutor(Executor executor) {
        // 默认实现为空，子类可选择性实现
    }

    /**
     * 获取异步请求执行器
     *
     * @return 当前使用的执行器，默认返回ForkJoinPool.commonPool()
     */
    default Executor getAsyncExecutor() {
        return ForkJoinPool.commonPool();
    }

    /**
     * 获取适配器名称
     *
     * @return 适配器名称
     */
    String getName();

    /**
     * 验证URL格式
     *
     * @param url 请求URL
     * @throws IllegalArgumentException URL格式错误
     */
    default void validateUrl(String url) {
        UrlValidator.validateFull(url);
    }

    /**
     * 发送异步 GET 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @return CompletableFuture包装的响应体
     */
    default CompletableFuture<String> getAsync(String url, Map<String, String> headers) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return get(url, headers);
            } catch (Exception e) {
                throw new java.util.concurrent.CompletionException(e);
            }
        }, getAsyncExecutor());
    }

    /**
     * 发送异步 POST 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @param body 请求体（JSON字符串，可为null）
     * @return CompletableFuture包装的响应体
     */
    default CompletableFuture<String> postAsync(String url, Map<String, String> headers, String body) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return post(url, headers, body);
            } catch (Exception e) {
                throw new java.util.concurrent.CompletionException(e);
            }
        }, getAsyncExecutor());
    }

    /**
     * 发送异步 PUT 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @param body 请求体（JSON字符串，可为null）
     * @return CompletableFuture包装的响应体
     */
    default CompletableFuture<String> putAsync(String url, Map<String, String> headers, String body) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return put(url, headers, body);
            } catch (Exception e) {
                throw new java.util.concurrent.CompletionException(e);
            }
        }, getAsyncExecutor());
    }

    /**
     * 发送异步 DELETE 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @return CompletableFuture包装的响应体
     */
    default CompletableFuture<String> deleteAsync(String url, Map<String, String> headers) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return delete(url, headers);
            } catch (Exception e) {
                throw new java.util.concurrent.CompletionException(e);
            }
        }, getAsyncExecutor());
    }

    /**
     * 发送异步 PATCH 请求
     *
     * @param url 请求URL
     * @param headers 请求头（可为null）
     * @param body 请求体（JSON字符串，可为null）
     * @return CompletableFuture包装的响应体
     */
    default CompletableFuture<String> patchAsync(String url, Map<String, String> headers, String body) {
        return CompletableFuture.supplyAsync(() -> {
            try {
                return patch(url, headers, body);
            } catch (Exception e) {
                throw new java.util.concurrent.CompletionException(e);
            }
        }, getAsyncExecutor());
    }

    /**
     * 添加请求拦截器
     *
     * @param interceptor 拦截器实例
     */
    default void addInterceptor(HttpInterceptor interceptor) {
        // 默认实现为空，子类可选择性实现
    }

    /**
     * 移除所有拦截器
     */
    default void clearInterceptors() {
        // 默认实现为空，子类可选择性实现
    }
}

