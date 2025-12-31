package top.yumbo.ai.omni.common.http;

import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 拦截器优先级测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class InterceptorPriorityTest {

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
    void testInterceptorOrder_shouldExecuteByPriority() throws Exception {
        // given
        List<String> executionOrder = new ArrayList<>();

        // 优先级: 低 -> 高 (order值从大到小)
        HttpInterceptor interceptor1 = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 100;
            }

            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                executionOrder.add("interceptor1");
                return request;
            }
        };

        HttpInterceptor interceptor2 = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 50;
            }

            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                executionOrder.add("interceptor2");
                return request;
            }
        };

        HttpInterceptor interceptor3 = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 10;
            }

            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                executionOrder.add("interceptor3");
                return request;
            }
        };

        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        adapter.addInterceptor(interceptor1);
        adapter.addInterceptor(interceptor2);
        adapter.addInterceptor(interceptor3);

        adapter.get(baseUrl + "test", null);

        // then
        assertEquals(3, executionOrder.size());
        assertEquals("interceptor3", executionOrder.get(0)); // order=10, 最先执行
        assertEquals("interceptor2", executionOrder.get(1)); // order=50
        assertEquals("interceptor1", executionOrder.get(2)); // order=100, 最后执行
    }

    @Test
    void testDefaultOrder_shouldBeZero() throws Exception {
        // given
        List<String> executionOrder = new ArrayList<>();

        HttpInterceptor defaultInterceptor = new HttpInterceptor() {
            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                executionOrder.add("default");
                return request;
            }
        };

        HttpInterceptor highPriorityInterceptor = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return -10;
            }

            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                executionOrder.add("high");
                return request;
            }
        };

        HttpInterceptor lowPriorityInterceptor = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 10;
            }

            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                executionOrder.add("low");
                return request;
            }
        };

        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        adapter.addInterceptor(defaultInterceptor);
        adapter.addInterceptor(highPriorityInterceptor);
        adapter.addInterceptor(lowPriorityInterceptor);

        adapter.get(baseUrl + "test", null);

        // then
        assertEquals(3, executionOrder.size());
        assertEquals("high", executionOrder.get(0));    // order=-10
        assertEquals("default", executionOrder.get(1));  // order=0 (default)
        assertEquals("low", executionOrder.get(2));      // order=10
    }

    @Test
    void testSameOrder_shouldKeepInsertionOrder() throws Exception {
        // given
        List<String> executionOrder = new ArrayList<>();

        HttpInterceptor interceptor1 = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 0;
            }

            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                executionOrder.add("first");
                return request;
            }
        };

        HttpInterceptor interceptor2 = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 0;
            }

            @Override
            public HttpRequest beforeRequest(HttpRequest request) {
                executionOrder.add("second");
                return request;
            }
        };

        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        adapter.addInterceptor(interceptor1);
        adapter.addInterceptor(interceptor2);

        adapter.get(baseUrl + "test", null);

        // then - 相同优先级保持插入顺序
        assertEquals(2, executionOrder.size());
        assertEquals("first", executionOrder.get(0));
        assertEquals("second", executionOrder.get(1));
    }

    @Test
    void testAfterResponse_shouldAlsoRespectOrder() throws Exception {
        // given
        List<String> executionOrder = new ArrayList<>();

        HttpInterceptor interceptor1 = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 100;
            }

            @Override
            public HttpResponse afterResponse(HttpResponse response) {
                executionOrder.add("interceptor1");
                return response;
            }
        };

        HttpInterceptor interceptor2 = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 50;
            }

            @Override
            public HttpResponse afterResponse(HttpResponse response) {
                executionOrder.add("interceptor2");
                return response;
            }
        };

        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        adapter.addInterceptor(interceptor1);
        adapter.addInterceptor(interceptor2);

        adapter.get(baseUrl + "test", null);

        // then
        assertEquals(2, executionOrder.size());
        assertEquals("interceptor2", executionOrder.get(0)); // order=50先执行
        assertEquals("interceptor1", executionOrder.get(1)); // order=100后执行
    }

    @Test
    void testOnError_shouldAlsoRespectOrder() {
        // given
        List<String> executionOrder = new ArrayList<>();

        HttpInterceptor interceptor1 = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 100;
            }

            @Override
            public void onError(HttpRequest request, Exception exception) {
                executionOrder.add("interceptor1");
            }
        };

        HttpInterceptor interceptor2 = new HttpInterceptor() {
            @Override
            public int getOrder() {
                return 50;
            }

            @Override
            public void onError(HttpRequest request, Exception exception) {
                executionOrder.add("interceptor2");
            }
        };

        mockWebServer.enqueue(new MockResponse()
                .setResponseCode(500)
                .setBody("Internal Server Error"));

        // when
        adapter.addInterceptor(interceptor1);
        adapter.addInterceptor(interceptor2);

        try {
            adapter.get(baseUrl + "test", null);
        } catch (Exception e) {
            // Expected exception
        }

        // then
        assertEquals(2, executionOrder.size());
        assertEquals("interceptor2", executionOrder.get(0)); // order=50先执行
        assertEquals("interceptor1", executionOrder.get(1)); // order=100后执行
    }
}

