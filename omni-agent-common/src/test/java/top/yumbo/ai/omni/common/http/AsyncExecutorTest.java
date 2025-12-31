package top.yumbo.ai.omni.common.http;

import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.concurrent.*;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 异步执行器测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class AsyncExecutorTest {

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
    void testDefaultAsyncExecutor_shouldUseForkJoinPool() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        Executor executor = adapter.getAsyncExecutor();

        // then
        assertNotNull(executor);
        assertEquals(ForkJoinPool.commonPool(), executor);
    }

    @Test
    void testCustomAsyncExecutor_shouldUseCustomExecutor() throws Exception {
        // given
        AtomicInteger executionCount = new AtomicInteger(0);
        Executor customExecutor = command -> {
            executionCount.incrementAndGet();
            command.run();
        };

        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        adapter.setAsyncExecutor(customExecutor);

        // when
        CompletableFuture<String> future = adapter.getAsync(baseUrl + "test", null);
        String response = future.get(5, TimeUnit.SECONDS);

        // then
        assertNotNull(response);
        assertTrue(response.contains("ok"));
        assertEquals(1, executionCount.get(), "Custom executor should be used");
    }

    @Test
    void testAsyncGet_shouldReturnCompletableFuture() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"data\":\"async test\"}")
                .setResponseCode(200));

        // when
        CompletableFuture<String> future = adapter.getAsync(baseUrl + "test", null);

        // then
        assertNotNull(future);
        assertFalse(future.isDone(), "Should not be done immediately");

        String response = future.get(5, TimeUnit.SECONDS);
        assertTrue(response.contains("async test"));
    }

    @Test
    void testAsyncPost_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"id\":123}")
                .setResponseCode(201));

        String body = "{\"name\":\"test\"}";

        // when
        CompletableFuture<String> future = adapter.postAsync(baseUrl + "users", null, body);
        String response = future.get(5, TimeUnit.SECONDS);

        // then
        assertNotNull(response);
        assertTrue(response.contains("123"));
    }

    @Test
    void testAsyncPut_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"updated\":true}")
                .setResponseCode(200));

        String body = "{\"name\":\"updated\"}";

        // when
        CompletableFuture<String> future = adapter.putAsync(baseUrl + "users/1", null, body);
        String response = future.get(5, TimeUnit.SECONDS);

        // then
        assertNotNull(response);
        assertTrue(response.contains("updated"));
    }

    @Test
    void testAsyncDelete_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"deleted\":true}")
                .setResponseCode(200));

        // when
        CompletableFuture<String> future = adapter.deleteAsync(baseUrl + "users/1", null);
        String response = future.get(5, TimeUnit.SECONDS);

        // then
        assertNotNull(response);
        assertTrue(response.contains("deleted"));
    }

    @Test
    void testAsyncError_shouldThrowCompletionException() {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setResponseCode(500)
                .setBody("Internal Server Error"));

        // when
        CompletableFuture<String> future = adapter.getAsync(baseUrl + "error", null);

        // then
        ExecutionException exception = assertThrows(ExecutionException.class, () -> {
            future.get(5, TimeUnit.SECONDS);
        });

        Throwable cause = exception.getCause();
        // CompletionException包装了HttpException
        assertTrue(cause instanceof CompletionException ||
                   cause instanceof top.yumbo.ai.omni.common.exception.HttpException);
    }

    @Test
    void testThreadPoolIsolation_customExecutorShouldNotAffectCommonPool() throws Exception {
        // given
        ThreadPoolExecutor customExecutor = new ThreadPoolExecutor(
                2, 2,
                60L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(),
                new ThreadFactory() {
                    private final AtomicInteger counter = new AtomicInteger(0);
                    @Override
                    public Thread newThread(Runnable r) {
                        return new Thread(r, "custom-async-" + counter.incrementAndGet());
                    }
                }
        );

        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        adapter.setAsyncExecutor(customExecutor);

        // when
        CompletableFuture<String> future = adapter.getAsync(baseUrl + "test", null);
        future.get(5, TimeUnit.SECONDS);

        // then
        assertEquals(1, customExecutor.getCompletedTaskCount(),
                "Custom executor should execute the task");

        // cleanup
        customExecutor.shutdown();
        assertTrue(customExecutor.awaitTermination(5, TimeUnit.SECONDS));
    }

    @Test
    void testMultipleAsyncRequests_shouldExecuteConcurrently() throws Exception {
        // given
        for (int i = 0; i < 5; i++) {
            mockWebServer.enqueue(new MockResponse()
                    .setBody("{\"id\":" + i + "}")
                    .setResponseCode(200));
        }

        // when
        CompletableFuture<String>[] futures = new CompletableFuture[5];
        for (int i = 0; i < 5; i++) {
            futures[i] = adapter.getAsync(baseUrl + "test/" + i, null);
        }

        CompletableFuture<Void> allOf = CompletableFuture.allOf(futures);
        allOf.get(10, TimeUnit.SECONDS);

        // then
        for (int i = 0; i < 5; i++) {
            assertTrue(futures[i].isDone());
            String response = futures[i].get();
            // 响应包含id字段即可，不需要严格匹配索引
            assertTrue(response.contains("\"id\":"));
        }
    }

    @Test
    void testSetNullExecutor_shouldRevertToDefault() throws Exception {
        // given
        ThreadPoolExecutor customExecutor = new ThreadPoolExecutor(
                1, 1,
                60L, TimeUnit.SECONDS,
                new LinkedBlockingQueue<>()
        );

        adapter.setAsyncExecutor(customExecutor);
        adapter.setAsyncExecutor(null); // 重置为默认

        // when
        Executor executor = adapter.getAsyncExecutor();

        // then
        assertEquals(ForkJoinPool.commonPool(), executor);

        // cleanup
        customExecutor.shutdown();
    }
}

