package top.yumbo.ai.omni.common.http;

import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 重试机制测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class RetryPolicyTest {

    private MockWebServer mockWebServer;
    private OkHttp3Adapter adapter;
    private String baseUrl;

    @BeforeEach
    void setUp() throws Exception {
        mockWebServer = new MockWebServer();
        mockWebServer.start();
        baseUrl = mockWebServer.url("/").toString();
        adapter = new OkHttp3Adapter();
    }

    @AfterEach
    void tearDown() throws Exception {
        mockWebServer.shutdown();
    }

    @Test
    void testNoRetry_shouldNotRetryOnFailure() {
        // given
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        adapter.setRetryPolicy(RetryPolicy.noRetry());

        // when & then
        assertThrows(Exception.class, () -> adapter.get(baseUrl + "test", null));

        // 只有1个请求（没有重试）
        assertEquals(1, mockWebServer.getRequestCount());
    }

    @Test
    void testFixedDelay_shouldRetryWithFixedDelay() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"success\":true}")
                .setResponseCode(200));

        adapter.setRetryPolicy(RetryPolicy.fixedDelay(3, 100));

        long startTime = System.currentTimeMillis();

        // when
        String response = adapter.get(baseUrl + "test", null);

        long duration = System.currentTimeMillis() - startTime;

        // then
        assertNotNull(response);
        assertTrue(response.contains("success"));
        assertEquals(3, mockWebServer.getRequestCount()); // 初始请求 + 2次重试
        assertTrue(duration >= 200); // 至少2次延迟 * 100ms
    }

    @Test
    void testExponentialBackoff_shouldIncreaseDelay() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"success\":true}")
                .setResponseCode(200));

        adapter.setRetryPolicy(RetryPolicy.exponentialBackoff(3, 100));

        long startTime = System.currentTimeMillis();

        // when
        String response = adapter.get(baseUrl + "test", null);

        long duration = System.currentTimeMillis() - startTime;

        // then
        assertNotNull(response);
        assertTrue(response.contains("success"));
        assertEquals(3, mockWebServer.getRequestCount());
        // 延迟应该是 100ms + 200ms = 300ms
        assertTrue(duration >= 300);
    }

    @Test
    void testMaxRetriesExceeded_shouldThrowException() {
        // given
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));

        adapter.setRetryPolicy(RetryPolicy.fixedDelay(2, 50)); // 最多重试2次

        // when & then
        assertThrows(Exception.class, () -> adapter.get(baseUrl + "test", null));

        // 初始请求 + 2次重试 = 3次请求
        assertEquals(3, mockWebServer.getRequestCount());
    }

    @Test
    void testClientError_shouldNotRetry() {
        // given
        mockWebServer.enqueue(new MockResponse().setResponseCode(404)); // 客户端错误
        adapter.setRetryPolicy(RetryPolicy.fixedDelay(3, 100));

        // when & then
        assertThrows(Exception.class, () -> adapter.get(baseUrl + "test", null));

        // 4xx错误不应该重试
        assertEquals(1, mockWebServer.getRequestCount());
    }

    @Test
    void testSuccessOnFirstAttempt_shouldNotRetry() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"success\":true}")
                .setResponseCode(200));

        adapter.setRetryPolicy(RetryPolicy.fixedDelay(3, 100));

        // when
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
        assertEquals(1, mockWebServer.getRequestCount()); // 没有重试
    }

    @Test
    void testExponentialBackoffWithLimit_shouldCapDelay() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"success\":true}")
                .setResponseCode(200));

        // 初始100ms，但最大延迟150ms
        adapter.setRetryPolicy(RetryPolicy.exponentialBackoffWithLimit(5, 100, 150));

        long startTime = System.currentTimeMillis();

        // when
        String response = adapter.get(baseUrl + "test", null);

        long duration = System.currentTimeMillis() - startTime;

        // then
        assertNotNull(response);
        assertEquals(4, mockWebServer.getRequestCount());
        // 延迟应该是 100 + 150 + 150 = 400ms (第2次被cap，第3次被cap)
        assertTrue(duration >= 400);
        assertTrue(duration < 600); // 确保没有超过限制
    }

    @Test
    void testCustomRetryPolicy() throws Exception {
        // given
        AtomicInteger retryCount = new AtomicInteger(0);

        RetryPolicy customPolicy = new RetryPolicy() {
            @Override
            public boolean shouldRetry(int attempt, Exception exception) {
                retryCount.incrementAndGet();
                return attempt <= 2 && isRetriable(exception);
            }

            @Override
            public long getDelayMillis(int attempt) {
                return 50L * attempt; // 递增延迟
            }

            @Override
            public int getMaxRetries() {
                return 2;
            }
        };

        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"success\":true}")
                .setResponseCode(200));

        adapter.setRetryPolicy(customPolicy);

        // when
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
        assertEquals(2, mockWebServer.getRequestCount());
        assertEquals(1, retryCount.get()); // shouldRetry被调用1次（第一次失败后）
    }

    @Test
    void testSetNullRetryPolicy_shouldUseNoRetry() {
        // given
        mockWebServer.enqueue(new MockResponse().setResponseCode(500));
        adapter.setRetryPolicy(null);

        // when & then
        assertThrows(Exception.class, () -> adapter.get(baseUrl + "test", null));
        assertEquals(1, mockWebServer.getRequestCount());
    }

    @Test
    void testGetRetryPolicy_shouldReturnSetPolicy() {
        // given
        RetryPolicy policy = RetryPolicy.fixedDelay(3, 100);

        // when
        adapter.setRetryPolicy(policy);

        // then
        assertEquals(policy, adapter.getRetryPolicy());
    }
}

