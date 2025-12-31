package top.yumbo.ai.omni.common.http;

import okhttp3.ConnectionPool;
import okhttp3.OkHttpClient;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;

/**
 * OkHttp3Adapter构建器
 * <p>
 * 提供流畅的API来配置和创建OkHttp3Adapter实例
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class OkHttp3AdapterBuilder {

    private int connectTimeout = 30;
    private int readTimeout = 60;
    private int writeTimeout = 60;
    private int maxConnections = 20;
    private long keepAliveDuration = 5;
    private TimeUnit keepAliveUnit = TimeUnit.MINUTES;
    private boolean retryOnFailure = true;
    private long maxRequestSize = 10 * 1024 * 1024; // 10MB
    private long maxResponseSize = 10 * 1024 * 1024; // 10MB
    private Executor asyncExecutor;
    private RetryPolicy retryPolicy;
    private final List<HttpInterceptor> interceptors = new ArrayList<>();
    private OkHttpClient customClient;

    /**
     * 设置连接超时时间
     *
     * @param timeout 超时时间
     * @param unit    时间单位
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder connectTimeout(int timeout, TimeUnit unit) {
        this.connectTimeout = (int) unit.toSeconds(timeout);
        return this;
    }

    /**
     * 设置读取超时时间
     *
     * @param timeout 超时时间
     * @param unit    时间单位
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder readTimeout(int timeout, TimeUnit unit) {
        this.readTimeout = (int) unit.toSeconds(timeout);
        return this;
    }

    /**
     * 设置写入超时时间
     *
     * @param timeout 超时时间
     * @param unit    时间单位
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder writeTimeout(int timeout, TimeUnit unit) {
        this.writeTimeout = (int) unit.toSeconds(timeout);
        return this;
    }

    /**
     * 设置连接池最大连接数
     *
     * @param maxConnections 最大连接数
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder maxConnections(int maxConnections) {
        this.maxConnections = maxConnections;
        return this;
    }

    /**
     * 设置连接保活时间
     *
     * @param duration 保活时间
     * @param unit     时间单位
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder keepAlive(long duration, TimeUnit unit) {
        this.keepAliveDuration = duration;
        this.keepAliveUnit = unit;
        return this;
    }

    /**
     * 设置连接失败时是否重试
     *
     * @param retry true-重试，false-不重试
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder retryOnConnectionFailure(boolean retry) {
        this.retryOnFailure = retry;
        return this;
    }

    /**
     * 设置请求体最大大小
     *
     * @param maxSize 最大字节数
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder maxRequestSize(long maxSize) {
        this.maxRequestSize = maxSize;
        return this;
    }

    /**
     * 设置响应体最大大小
     *
     * @param maxSize 最大字节数
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder maxResponseSize(long maxSize) {
        this.maxResponseSize = maxSize;
        return this;
    }

    /**
     * 设置异步执行器
     *
     * @param executor 执行器
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder asyncExecutor(Executor executor) {
        this.asyncExecutor = executor;
        return this;
    }

    /**
     * 设置重试策略
     *
     * @param policy 重试策略
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder retryPolicy(RetryPolicy policy) {
        this.retryPolicy = policy;
        return this;
    }

    /**
     * 添加拦截器
     *
     * @param interceptor 拦截器
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder addInterceptor(HttpInterceptor interceptor) {
        if (interceptor != null) {
            this.interceptors.add(interceptor);
        }
        return this;
    }

    /**
     * 使用自定义的OkHttpClient
     * <p>
     * 注意：使用自定义客户端后，其他配置参数将被忽略
     *
     * @param client 自定义OkHttpClient
     * @return Builder实例
     */
    public OkHttp3AdapterBuilder client(OkHttpClient client) {
        this.customClient = client;
        return this;
    }

    /**
     * 构建OkHttp3Adapter实例
     *
     * @return OkHttp3Adapter实例
     */
    public OkHttp3Adapter build() {
        OkHttp3Adapter adapter;

        if (customClient != null) {
            // 使用自定义客户端
            adapter = new OkHttp3Adapter(customClient);
        } else {
            // 使用配置构建客户端
            OkHttpClient client = new OkHttpClient.Builder()
                    .connectTimeout(connectTimeout, TimeUnit.SECONDS)
                    .readTimeout(readTimeout, TimeUnit.SECONDS)
                    .writeTimeout(writeTimeout, TimeUnit.SECONDS)
                    .connectionPool(new ConnectionPool(maxConnections, keepAliveDuration, keepAliveUnit))
                    .retryOnConnectionFailure(retryOnFailure)
                    .build();
            adapter = new OkHttp3Adapter(client);
        }

        // 应用配置
        adapter.setMaxRequestSize(maxRequestSize);
        adapter.setMaxResponseSize(maxResponseSize);

        if (asyncExecutor != null) {
            adapter.setAsyncExecutor(asyncExecutor);
        }

        if (retryPolicy != null) {
            adapter.setRetryPolicy(retryPolicy);
        }

        // 添加拦截器
        for (HttpInterceptor interceptor : interceptors) {
            adapter.addInterceptor(interceptor);
        }

        return adapter;
    }

    /**
     * 创建默认Builder
     *
     * @return Builder实例
     */
    public static OkHttp3AdapterBuilder builder() {
        return new OkHttp3AdapterBuilder();
    }

    /**
     * 创建生产环境推荐配置的Builder
     *
     * @return Builder实例
     */
    public static OkHttp3AdapterBuilder production() {
        return new OkHttp3AdapterBuilder()
                .connectTimeout(10, TimeUnit.SECONDS)
                .readTimeout(30, TimeUnit.SECONDS)
                .writeTimeout(30, TimeUnit.SECONDS)
                .maxConnections(50)
                .keepAlive(5, TimeUnit.MINUTES)
                .retryOnConnectionFailure(true)
                .maxRequestSize(5 * 1024 * 1024)  // 5MB
                .maxResponseSize(10 * 1024 * 1024); // 10MB
    }

    /**
     * 创建开发环境推荐配置的Builder
     *
     * @return Builder实例
     */
    public static OkHttp3AdapterBuilder development() {
        return new OkHttp3AdapterBuilder()
                .connectTimeout(30, TimeUnit.SECONDS)
                .readTimeout(60, TimeUnit.SECONDS)
                .writeTimeout(60, TimeUnit.SECONDS)
                .maxConnections(10)
                .keepAlive(2, TimeUnit.MINUTES)
                .retryOnConnectionFailure(true)
                .maxRequestSize(50 * 1024 * 1024)  // 50MB
                .maxResponseSize(50 * 1024 * 1024) // 50MB
                .addInterceptor(new LoggingInterceptor()); // 开发环境默认添加日志
    }
}

