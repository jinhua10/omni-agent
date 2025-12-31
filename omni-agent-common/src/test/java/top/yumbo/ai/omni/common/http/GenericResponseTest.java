package top.yumbo.ai.omni.common.http;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * 泛型响应测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class GenericResponseTest {

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

    // 测试用的DTO类
    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class User {
        private Long id;
        private String name;
        private String email;

    }

    @Data
    public static class ApiResponse<T> {
        private int code;
        private String message;
        private T data;
    }

    @Test
    void testGetWithGenericResponse_shouldDeserialize() throws Exception {
        // given
        String jsonResponse = "{\"id\":1,\"name\":\"John Doe\",\"email\":\"john@example.com\"}";
        mockWebServer.enqueue(new MockResponse()
                .setBody(jsonResponse)
                .setResponseCode(200));

        // when
        User user = adapter.get(baseUrl + "users/1", null, User.class);

        // then
        assertNotNull(user);
        assertEquals(1L, user.getId());
        assertEquals("John Doe", user.getName());
        assertEquals("john@example.com", user.getEmail());
    }

    @Test
    void testPostWithGenericResponse_shouldDeserialize() throws Exception {
        // given
        String requestBody = "{\"name\":\"Jane Doe\",\"email\":\"jane@example.com\"}";
        String jsonResponse = "{\"id\":2,\"name\":\"Jane Doe\",\"email\":\"jane@example.com\"}";

        mockWebServer.enqueue(new MockResponse()
                .setBody(jsonResponse)
                .setResponseCode(201));

        // when
        User user = adapter.post(baseUrl + "users", null, requestBody, User.class);

        // then
        assertNotNull(user);
        assertEquals(2L, user.getId());
        assertEquals("Jane Doe", user.getName());
        assertEquals("jane@example.com", user.getEmail());
    }

    @Test
    void testPutWithGenericResponse_shouldDeserialize() throws Exception {
        // given
        String requestBody = "{\"name\":\"John Updated\"}";
        String jsonResponse = "{\"id\":1,\"name\":\"John Updated\",\"email\":\"john@example.com\"}";

        mockWebServer.enqueue(new MockResponse()
                .setBody(jsonResponse)
                .setResponseCode(200));

        // when
        User user = adapter.put(baseUrl + "users/1", null, requestBody, User.class);

        // then
        assertNotNull(user);
        assertEquals(1L, user.getId());
        assertEquals("John Updated", user.getName());
    }

    @Test
    void testDeleteWithGenericResponse_shouldDeserialize() throws Exception {
        // given
        String jsonResponse = "{\"id\":1,\"name\":\"Deleted User\",\"email\":\"deleted@example.com\"}";

        mockWebServer.enqueue(new MockResponse()
                .setBody(jsonResponse)
                .setResponseCode(200));

        // when
        User user = adapter.delete(baseUrl + "users/1", null, User.class);

        // then
        assertNotNull(user);
        assertEquals(1L, user.getId());
        assertEquals("Deleted User", user.getName());
    }

    @Test
    void testPatchWithGenericResponse_shouldDeserialize() throws Exception {
        // given
        String requestBody = "{\"email\":\"newemail@example.com\"}";
        String jsonResponse = "{\"id\":1,\"name\":\"John Doe\",\"email\":\"newemail@example.com\"}";

        mockWebServer.enqueue(new MockResponse()
                .setBody(jsonResponse)
                .setResponseCode(200));

        // when
        User user = adapter.patch(baseUrl + "users/1", null, requestBody, User.class);

        // then
        assertNotNull(user);
        assertEquals(1L, user.getId());
        assertEquals("newemail@example.com", user.getEmail());
    }

    @Test
    void testDeserializeString_shouldWork() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("\"Hello World\"")
                .setResponseCode(200));

        // when
        String result = adapter.get(baseUrl + "test", null, String.class);

        // then
        assertEquals("Hello World", result);
    }

    @Test
    void testInvalidJson_shouldThrowException() {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("invalid json")
                .setResponseCode(200));

        // when & then
        assertThrows(Exception.class, () -> {
            adapter.get(baseUrl + "test", null, User.class);
        });
    }

    @Test
    void testNullResponse_shouldReturnNull() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("null")
                .setResponseCode(200));

        // when
        User user = adapter.get(baseUrl + "test", null, User.class);

        // then
        assertNull(user);
    }

    @Test
    void testEmptyObject_shouldDeserialize() throws Exception {
        // given
        mockWebServer.enqueue(new MockResponse()
                .setBody("{}")
                .setResponseCode(200));

        // when
        User user = adapter.get(baseUrl + "test", null, User.class);

        // then
        assertNotNull(user);
        assertNull(user.getId());
        assertNull(user.getName());
        assertNull(user.getEmail());
    }
}

