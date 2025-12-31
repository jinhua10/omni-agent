package top.yumbo.ai.omni.common.http;

import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.Executor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * OkHttp3AdapterBuilder测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class OkHttp3AdapterBuilderTest {

    private MockWebServer mockWebServer;
    private String baseUrl;

    @BeforeEach
    void setUp() throws Exception {
        mockWebServer = new MockWebServer();
        mockWebServer.start();
        baseUrl = mockWebServer.url("/").toString();
    }

    @AfterEach
    void tearDown() throws Exception {
        mockWebServer.shutdown();
    }

    @Test
    void testDefaultBuilder_shouldCreateAdapter() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder().build();
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(adapter);
        assertNotNull(response);
        assertTrue(response.contains("ok"));
    }

    @Test
    void testBuilderWithCustomTimeout_shouldApplyTimeout() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .connectTimeout(5, TimeUnit.SECONDS)
                .readTimeout(10, TimeUnit.SECONDS)
                .build();

        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
    }

    @Test
    void testBuilderWithMaxSizes_shouldApplySizeLimits() {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("x".repeat(200)) // 200字节
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .maxRequestSize(1024)
                .maxResponseSize(100) // 限制100字节
                .build();

        // then
        assertThrows(Exception.class, () -> {
            adapter.get(baseUrl + "test", null);
        });
    }

    @Test
    void testBuilderWithInterceptor_shouldAddInterceptor() throws Exception {
        // given
        AtomicInteger interceptorCalled = new AtomicInteger(0);

        HttpInterceptor testInterceptor = new HttpInterceptor() {
            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                interceptorCalled.incrementAndGet();
                return request;
            }
        };

        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .addInterceptor(testInterceptor)
                .build();

        adapter.get(baseUrl + "test", null);

        // then
        assertEquals(1, interceptorCalled.get());
    }

    @Test
    void testBuilderWithMultipleInterceptors_shouldAddAll() throws Exception {
        // given
        AtomicInteger count1 = new AtomicInteger(0);
        AtomicInteger count2 = new AtomicInteger(0);

        HttpInterceptor interceptor1 = new HttpInterceptor() {
            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                count1.incrementAndGet();
                return request;
            }
        };

        HttpInterceptor interceptor2 = new HttpInterceptor() {
            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                count2.incrementAndGet();
                return request;
            }
        };

        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .addInterceptor(interceptor1)
                .addInterceptor(interceptor2)
                .build();

        adapter.get(baseUrl + "test", null);

        // then
        assertEquals(1, count1.get());
        assertEquals(1, count2.get());
    }

    @Test
    void testBuilderWithRetryPolicy_shouldApplyRetry() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .retryPolicy(RetryPolicy.fixedDelay(2, 100))
                .build();

        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
        assertEquals(2, mockWebServer.getRequestCount()); // 初始请求 + 1次重试
    }

    @Test
    void testBuilderWithAsyncExecutor_shouldUseCustomExecutor() throws Exception {
        // given
        AtomicInteger executorCalled = new AtomicInteger(0);

        Executor customExecutor = command -> {
            executorCalled.incrementAndGet();
            command.run();
        };

        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .asyncExecutor(customExecutor)
                .build();

        adapter.getAsync(baseUrl + "test", null).get(5, TimeUnit.SECONDS);

        // then
        assertEquals(1, executorCalled.get());
    }

    @Test
    void testProductionBuilder_shouldUseProductionDefaults() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.production().build();
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(adapter);
        assertNotNull(response);
    }

    @Test
    void testDevelopmentBuilder_shouldUseDevelopmentDefaults() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.development().build();
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(adapter);
        assertNotNull(response);
    }

    @Test
    void testBuilderFluentApi_shouldChainCalls() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .connectTimeout(10, TimeUnit.SECONDS)
                .readTimeout(20, TimeUnit.SECONDS)
                .maxConnections(30)
                .maxRequestSize(1024 * 1024)
                .maxResponseSize(2 * 1024 * 1024)
                .retryOnConnectionFailure(true)
                .build();

        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
    }

    @Test
    void testBuilderWithNullInterceptor_shouldIgnore() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .addInterceptor(null)
                .build();

        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
    }

    @Test
    void testBuilderConnectionPoolSettings_shouldApply() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .maxConnections(50)
                .keepAlive(10, TimeUnit.MINUTES)
                .build();

        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
        ConnectionPoolMonitor.PoolStats stats = adapter.getPoolMonitor().getStats();
        assertNotNull(stats);
    }

    @Test
    void testBuilderRetryOnFailure_shouldApply() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when - 禁用自动重试
        OkHttp3Adapter adapter = OkHttp3AdapterBuilder.builder()
                .retryOnConnectionFailure(false)
                .build();

        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
    }
}

