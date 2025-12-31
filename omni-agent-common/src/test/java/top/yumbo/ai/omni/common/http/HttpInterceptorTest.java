package top.yumbo.ai.omni.common.http;

import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * HTTP拦截器和异步功能测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class HttpInterceptorTest {

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
    void testInterceptor_beforeRequest_modifiesHeaders() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"success\"}")
                .setResponseCode(200));

        AtomicBoolean interceptorCalled = new AtomicBoolean(false);

        adapter.addInterceptor(new HttpInterceptor() {
            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                interceptorCalled.set(true);
                Map<String, String> headers = request.getHeaders();
                if (headers == null) {
                    headers = new HashMap<>();
                }
                headers.put("X-Custom-Header", "test-value");
                request.setHeaders(headers);
                return request;
            }
        });

        // when
        adapter.get(baseUrl + "test", null);

        // then
        assertTrue(interceptorCalled.get());
    }

    @Test
    void testInterceptor_afterResponse_modifiesBody() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"success\"}")
                .setResponseCode(200));

        adapter.addInterceptor(new HttpInterceptor() {
            @Override
            public HttpResponse afterResponse(HttpResponse response) {
                response.setBody("{\"status\":\"modified\"}");
                return response;
            }
        });

        // when
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertTrue(response.contains("modified"));
    }

    @Test
    void testInterceptor_onError_handleException() {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setResponseCode(404)
                .setBody("Not Found"));

        AtomicBoolean errorHandled = new AtomicBoolean(false);

        adapter.addInterceptor(new HttpInterceptor() {
            @Override
            public void onError(HttpRequest request, Exception exception) {
                errorHandled.set(true);
            }
        });

        // when & then
        assertThrows(Exception.class, () -> {
            adapter.get(baseUrl + "notfound", null);
        });

        assertTrue(errorHandled.get());
    }

    @Test
    void testLoggingInterceptor_logsRequests() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"data\":\"test\"}")
                .setResponseCode(200));

        LoggingInterceptor loggingInterceptor = new LoggingInterceptor();
        adapter.addInterceptor(loggingInterceptor);

        // when
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
        assertTrue(response.contains("test"));
    }

    @Test
    void testMultipleInterceptors_executedInOrder() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"success\"}")
                .setResponseCode(200));

        AtomicInteger callOrder = new AtomicInteger(0);
        AtomicInteger firstCalled = new AtomicInteger(-1);
        AtomicInteger secondCalled = new AtomicInteger(-1);

        adapter.addInterceptor(new HttpInterceptor() {
            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                firstCalled.set(callOrder.getAndIncrement());
                return request;
            }
        });

        adapter.addInterceptor(new HttpInterceptor() {
            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                secondCalled.set(callOrder.getAndIncrement());
                return request;
            }
        });

        // when
        adapter.get(baseUrl + "test", null);

        // then
        assertEquals(0, firstCalled.get());
        assertEquals(1, secondCalled.get());
    }

    @Test
    void testClearInterceptors_removesAllInterceptors() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"success\"}")
                .setResponseCode(200));

        AtomicBoolean interceptorCalled = new AtomicBoolean(false);

        adapter.addInterceptor(new HttpInterceptor() {
            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                interceptorCalled.set(true);
                return request;
            }
        });

        adapter.clearInterceptors();

        // when
        adapter.get(baseUrl + "test", null);

        // then
        assertFalse(interceptorCalled.get());
    }

    @Test
    void testAsyncGet_returnsCompletableFuture() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"success\"}")
                .setResponseCode(200));

        // when
        CompletableFuture<String> future = adapter.getAsync(baseUrl + "test", null);
        String response = future.get();

        // then
        assertNotNull(response);
        assertTrue(response.contains("success"));
    }

    @Test
    void testAsyncPost_returnsCompletableFuture() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"id\":123}")
                .setResponseCode(201));

        Map<String, String> headers = new HashMap<>();
        headers.put("Content-Type", "application/json");
        String body = "{\"name\":\"John\"}";

        // when
        CompletableFuture<String> future = adapter.postAsync(baseUrl + "users", headers, body);
        String response = future.get();

        // then
        assertNotNull(response);
        assertTrue(response.contains("123"));
    }

    @Test
    void testAsyncGet_withException_throwsException() {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setResponseCode(500)
                .setBody("Internal Server Error"));

        // when
        CompletableFuture<String> future = adapter.getAsync(baseUrl + "error", null);

        // then
        assertThrows(Exception.class, future::get);
    }
}

