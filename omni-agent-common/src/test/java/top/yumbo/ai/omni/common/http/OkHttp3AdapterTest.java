package top.yumbo.ai.omni.common.http;

import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import okhttp3.mockwebserver.RecordedRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.common.exception.HttpException;
import top.yumbo.ai.omni.common.exception.ValidationException;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;

/**
 * OkHttp3Adapter 单元测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class OkHttp3AdapterTest {

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
    void testGet_validRequest_returnsResponse() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"success\"}")
                .setResponseCode(200));

        Map<String, String> headers = new HashMap<>();
        headers.put("Accept", "application/json");

        // when
        String response = adapter.get(baseUrl + "test", headers);

        // then
        assertNotNull(response);
        assertTrue(response.contains("success"));

        RecordedRequest request = mockWebServer.takeRequest();
        assertEquals("GET", request.getMethod());
        assertEquals("application/json", request.getHeader("Accept"));
    }

    @Test
    void testGet_nullHeaders_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"data\":\"test\"}")
                .setResponseCode(200));

        // when
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
        assertTrue(response.contains("test"));
    }

    @Test
    void testPost_withBody_sendsCorrectRequest() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"id\":123}")
                .setResponseCode(201));

        Map<String, String> headers = new HashMap<>();
        headers.put("Content-Type", "application/json");

        String body = "{\"name\":\"John\"}";

        // when
        String response = adapter.post(baseUrl + "users", headers, body);

        // then
        assertNotNull(response);
        assertTrue(response.contains("123"));

        RecordedRequest request = mockWebServer.takeRequest();
        assertEquals("POST", request.getMethod());
        assertEquals(body, request.getBody().readUtf8());
    }

    @Test
    void testPost_nullBody_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        String response = adapter.post(baseUrl + "test", null, null);

        // then
        assertNotNull(response);
    }

    @Test
    void testPut_validRequest_returnsResponse() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"updated\":true}")
                .setResponseCode(200));

        String body = "{\"name\":\"Updated\"}";

        // when
        String response = adapter.put(baseUrl + "users/123", null, body);

        // then
        assertNotNull(response);
        assertTrue(response.contains("updated"));

        RecordedRequest request = mockWebServer.takeRequest();
        assertEquals("PUT", request.getMethod());
    }

    @Test
    void testDelete_validRequest_returnsResponse() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"deleted\":true}")
                .setResponseCode(200));

        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token");

        // when
        String response = adapter.delete(baseUrl + "users/123", headers);

        // then
        assertNotNull(response);
        assertTrue(response.contains("deleted"));

        RecordedRequest request = mockWebServer.takeRequest();
        assertEquals("DELETE", request.getMethod());
        assertEquals("Bearer token", request.getHeader("Authorization"));
    }

    @Test
    void testGet_httpError_throwsException() {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setResponseCode(404)
                .setBody("Not Found"));

        // when & then
        HttpException exception = assertThrows(HttpException.class, () -> {
            adapter.get(baseUrl + "notfound", null);
        });

        assertEquals(404, exception.getStatusCode());
    }

    @Test
    void testPost_serverError_throwsException() {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setResponseCode(500)
                .setBody("Internal Server Error"));

        // when & then
        HttpException exception = assertThrows(HttpException.class, () -> {
            adapter.post(baseUrl + "error", null, "{}");
        });

        assertEquals(500, exception.getStatusCode());
    }

    @Test
    void testGet_invalidUrl_throwsException() {
        // when & then
        assertThrows(ValidationException.class, () -> {
            adapter.get("invalid-url", null);
        });
    }

    @Test
    void testPost_invalidUrl_throwsException() {
        // when & then
        assertThrows(ValidationException.class, () -> {
            adapter.post("ftp://example.com", null, "{}");
        });
    }


    @Test
    void testGetName_returnsCorrectName() {
        // when
        String name = adapter.getName();

        // then
        assertEquals("OkHttp3", name);
    }

    @Test
    void testComplexUrl_withQueryParams_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"results\":[]}")
                .setResponseCode(200));

        String complexUrl = baseUrl + "api/users?page=1&size=10&sort=name#section";

        // when
        String response = adapter.get(complexUrl, null);

        // then
        assertNotNull(response);
        assertTrue(response.contains("results"));
    }

    @Test
    void testMultipleHeaders_allHeadersSent() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"ok\":true}")
                .setResponseCode(200));

        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token123");
        headers.put("Content-Type", "application/json");
        headers.put("X-Custom-Header", "custom-value");

        // when
        adapter.post(baseUrl + "test", headers, "{}");

        //  then
        RecordedRequest request = mockWebServer.takeRequest();
        assertEquals("Bearer token123", request.getHeader("Authorization"));
        assertTrue(Objects.requireNonNull(request.getHeader("Content-Type")).contains("application/json")); // OkHttp会自动添加charset
        assertEquals("custom-value", request.getHeader("X-Custom-Header"));
    }
}

