package top.yumbo.ai.omni.common.http;

import okhttp3.*;

import java.util.Map;
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

    private final OkHttpClient client;
    private static final MediaType JSON = MediaType.parse("application/json; charset=utf-8");

    /**
     * 使用默认配置的 OkHttpClient
     */
    public OkHttp3Adapter() {
        this(createDefaultClient());
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
                .connectTimeout(120, TimeUnit.SECONDS)
                .readTimeout(120, TimeUnit.SECONDS)
                .writeTimeout(120, TimeUnit.SECONDS)
                .connectionPool(new ConnectionPool(20, 5, TimeUnit.MINUTES))
                .retryOnConnectionFailure(true)
                .build();
    }

    @Override
    public String post(String url, Map<String, String> headers, String body) throws Exception {
        // 构建请求
        Request.Builder requestBuilder = new Request.Builder()
                .url(url)
                .post(RequestBody.create(body, JSON));

        // 添加请求头
        headers.forEach(requestBuilder::addHeader);

        Request request = requestBuilder.build();

        // 发送请求
        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new RuntimeException("HTTP请求失败: " + response.code());
            }

            ResponseBody responseBody = response.body();
            if (responseBody == null) {
                throw new RuntimeException("响应体为空");
            }

            return responseBody.string();
        }
    }

    @Override
    public String getName() {
        return "OkHttp3";
    }
}

