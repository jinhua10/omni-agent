package top.yumbo.ai.omni.common.http;

import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.common.exception.ValidationException;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 请求/响应大小限制测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class RequestSizeLimitTest {

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
    void testRequestSizeLimit_withinLimit_shouldSucceed() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        adapter.setMaxRequestSize(1024); // 1KB limit
        String smallBody = "{\"data\":\"test\"}"; // Small body

        // when
        String response = adapter.post(baseUrl + "test", null, smallBody);

        // then
        assertNotNull(response);
        assertTrue(response.contains("ok"));
    }

    @Test
    void testRequestSizeLimit_exceedsLimit_shouldThrowException() {
        // given
        adapter.setMaxRequestSize(100); // 100 bytes limit
        StringBuilder largeBody = new StringBuilder();
        for (int i = 0; i < 200; i++) {
            largeBody.append("x");
        }

        // when & then
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            adapter.post(baseUrl + "test", null, largeBody.toString());
        });

        assertTrue(exception.getMessage().contains("exceeds maximum allowed size"));
        assertEquals("body", exception.getFieldName());
    }

    @Test
    void testResponseSizeLimit_withinLimit_shouldSucceed() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"data\":\"test\"}")
                .setResponseCode(200));

        adapter.setMaxResponseSize(1024); // 1KB limit

        // when
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
    }

    @Test
    void testResponseSizeLimit_exceedsLimit_shouldThrowException() {
        // given
        StringBuilder largeResponse = new StringBuilder("{\"data\":\"");
        for (int i = 0; i < 200; i++) {
            largeResponse.append("xxxxxxxxxx");
        }
        largeResponse.append("\"}");

        mockWebServer.enqueue(new MockResponse()
                .setBody(largeResponse.toString())
                .setResponseCode(200));

        adapter.setMaxResponseSize(100); // 100 bytes limit

        // when & then
        ValidationException exception = assertThrows(ValidationException.class, () -> {
            adapter.get(baseUrl + "test", null);
        });

        assertTrue(exception.getMessage().contains("exceeds maximum allowed size"));
        assertEquals("responseBody", exception.getFieldName());
    }

    @Test
    void testRequestSizeLimit_zeroLimit_shouldNotValidate() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        adapter.setMaxRequestSize(0); // No limit
        StringBuilder largeBody = new StringBuilder();
        for (int i = 0; i < 1000; i++) {
            largeBody.append("x");
        }

        // when
        String response = adapter.post(baseUrl + "test", null, largeBody.toString());

        // then
        assertNotNull(response);
    }

    @Test
    void testResponseSizeLimit_negativeLimit_shouldNotValidate() throws Exception {
        // given
        StringBuilder largeResponse = new StringBuilder("{\"data\":\"");
        for (int i = 0; i < 200; i++) {
            largeResponse.append("xxxxxxxxxx");
        }
        largeResponse.append("\"}");

        mockWebServer.enqueue(new MockResponse()
                .setBody(largeResponse.toString())
                .setResponseCode(200));

        adapter.setMaxResponseSize(-1); // No limit

        // when
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
    }

    @Test
    void testSetTimeout_shouldUpdateTimeout() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{\"status\":\"ok\"}")
                .setResponseCode(200));

        // when
        adapter.setTimeout(5, 10);
        String response = adapter.get(baseUrl + "test", null);

        // then
        assertNotNull(response);
        assertTrue(response.contains("ok"));
    }
}

