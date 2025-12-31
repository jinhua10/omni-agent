package top.yumbo.ai.omni.common.http;

import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import okhttp3.mockwebserver.RecordedRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * PATCH方法测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class PatchMethodTest {

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
    void testPatch_withBody_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"updated\":true,\"name\":\"Updated Name\"}")
                .setResponseCode(200));

        Map<String, String> headers = new HashMap<>();
        headers.put("Content-Type", "application/json");

        String body = "{\"name\":\"Updated Name\"}";

        // when
        String response = adapter.patch(baseUrl + "users/123", headers, body);

        // then
        assertNotNull(response);
        assertTrue(response.contains("updated"));
        assertTrue(response.contains("Updated Name"));

        RecordedRequest request = mockWebServer.takeRequest();
        assertEquals("PATCH", request.getMethod());
        assertEquals(body, request.getBody().readUtf8());
    }

    @Test
    void testPatch_nullBody_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        String response = adapter.patch(baseUrl + "test", null, null);

        // then
        assertNotNull(response);
        assertTrue(response.contains("ok"));

        RecordedRequest request = mockWebServer.takeRequest();
        assertEquals("PATCH", request.getMethod());
    }

    @Test
    void testPatch_withHeaders_shouldSendHeaders() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"patched\":true}")
                .setResponseCode(200));

        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token123");
        headers.put("X-Custom-Header", "custom-value");

        String body = "{\"field\":\"value\"}";

        // when
        String response = adapter.patch(baseUrl + "resource", headers, body);

        // then
        assertNotNull(response);

        RecordedRequest request = mockWebServer.takeRequest();
        assertEquals("PATCH", request.getMethod());
        assertEquals("Bearer token123", request.getHeader("Authorization"));
        assertEquals("custom-value", request.getHeader("X-Custom-Header"));
    }

    @Test
    void testPatchAsync_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"patched\":true}")
                .setResponseCode(200));

        String body = "{\"update\":\"data\"}";

        // when
        String response = adapter.patchAsync(baseUrl + "test", null, body)
                .get(5, java.util.concurrent.TimeUnit.SECONDS);

        // then
        assertNotNull(response);
        assertTrue(response.contains("patched"));

        RecordedRequest request = mockWebServer.takeRequest();
        assertEquals("PATCH", request.getMethod());
    }

    @Test
    void testPatch_error_shouldThrowException() {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setResponseCode(400)
                .setBody("Bad Request"));

        // when & then
        Exception exception = assertThrows(Exception.class, () -> {
            adapter.patch(baseUrl + "invalid", null, "{}");
        });

        assertTrue(exception.getMessage().contains("400") ||
                   exception.getMessage().contains("Bad Request"));
    }

    @Test
    void testPatch_partialUpdate_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"id\":1,\"name\":\"New Name\",\"email\":\"old@example.com\"}")
                .setResponseCode(200));

        // 只更新name字段
        String partialUpdate = "{\"name\":\"New Name\"}";

        // when
        String response = adapter.patch(baseUrl + "users/1", null, partialUpdate);

        // then
        assertNotNull(response);
        assertTrue(response.contains("New Name"));
        assertTrue(response.contains("old@example.com")); // 其他字段保持不变
    }
}

