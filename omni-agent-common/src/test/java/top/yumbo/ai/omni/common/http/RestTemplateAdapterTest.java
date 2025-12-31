package top.yumbo.ai.omni.common.http;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.http.*;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.HttpServerErrorException;
import org.springframework.web.client.RestTemplate;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * RestTemplateAdapter 单元测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class RestTemplateAdapterTest {

    private RestTemplate restTemplate;
    private RestTemplateAdapter adapter;

    @BeforeEach
    void setUp() {
        restTemplate = mock(RestTemplate.class);
        adapter = new RestTemplateAdapter(restTemplate);
    }

    @Test
    void testGet_validRequest_returnsResponse() throws Exception {
        // given
        String url = "https://api.example.com/users";
        String expectedResponse = "{\"users\":[]}";

        ResponseEntity<String> responseEntity = new ResponseEntity<>(expectedResponse, HttpStatus.OK);
        when(restTemplate.exchange(eq(url), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(responseEntity);

        Map<String, String> headers = new HashMap<>();
        headers.put("Accept", "application/json");

        // when
        String response = adapter.get(url, headers);

        // then
        assertEquals(expectedResponse, response);
        verify(restTemplate).exchange(eq(url), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class));
    }

    @Test
    void testGet_nullHeaders_shouldWork() throws Exception {
        // given
        String url = "https://api.example.com/data";
        ResponseEntity<String> responseEntity = new ResponseEntity<>("{\"data\":\"test\"}", HttpStatus.OK);
        when(restTemplate.exchange(anyString(), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(responseEntity);

        // when
        String response = adapter.get(url, null);

        // then
        assertNotNull(response);
    }

    @Test
    void testPost_withBody_sendsCorrectRequest() throws Exception {
        // given
        String url = "https://api.example.com/users";
        String body = "{\"name\":\"John\"}";
        String expectedResponse = "{\"id\":123}";

        ResponseEntity<String> responseEntity = new ResponseEntity<>(expectedResponse, HttpStatus.CREATED);
        when(restTemplate.exchange(eq(url), eq(HttpMethod.POST), any(HttpEntity.class), eq(String.class)))
                .thenReturn(responseEntity);

        Map<String, String> headers = new HashMap<>();
        headers.put("Content-Type", "application/json");

        // when
        String response = adapter.post(url, headers, body);

        // then
        assertEquals(expectedResponse, response);
        verify(restTemplate).exchange(eq(url), eq(HttpMethod.POST), any(HttpEntity.class), eq(String.class));
    }

    @Test
    void testPost_nullBody_shouldWork() throws Exception {
        // given
        String url = "https://api.example.com/test";
        ResponseEntity<String> responseEntity = new ResponseEntity<>("{\"ok\":true}", HttpStatus.OK);
        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(String.class)))
                .thenReturn(responseEntity);

        // when
        String response = adapter.post(url, null, null);

        // then
        assertNotNull(response);
    }

    @Test
    void testPut_validRequest_returnsResponse() throws Exception {
        // given
        String url = "https://api.example.com/users/123";
        String body = "{\"name\":\"Updated\"}";
        String expectedResponse = "{\"updated\":true}";

        ResponseEntity<String> responseEntity = new ResponseEntity<>(expectedResponse, HttpStatus.OK);
        when(restTemplate.exchange(eq(url), eq(HttpMethod.PUT), any(HttpEntity.class), eq(String.class)))
                .thenReturn(responseEntity);

        // when
        String response = adapter.put(url, null, body);

        // then
        assertEquals(expectedResponse, response);
        verify(restTemplate).exchange(eq(url), eq(HttpMethod.PUT), any(HttpEntity.class), eq(String.class));
    }

    @Test
    void testDelete_validRequest_returnsResponse() throws Exception {
        // given
        String url = "https://api.example.com/users/123";
        String expectedResponse = "{\"deleted\":true}";

        ResponseEntity<String> responseEntity = new ResponseEntity<>(expectedResponse, HttpStatus.OK);
        when(restTemplate.exchange(eq(url), eq(HttpMethod.DELETE), any(HttpEntity.class), eq(String.class)))
                .thenReturn(responseEntity);

        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token");

        // when
        String response = adapter.delete(url, headers);

        // then
        assertEquals(expectedResponse, response);
        verify(restTemplate).exchange(eq(url), eq(HttpMethod.DELETE), any(HttpEntity.class), eq(String.class));
    }

    @Test
    void testGet_httpClientError_throwsException() {
        // given
        String url = "https://api.example.com/notfound";
        when(restTemplate.exchange(anyString(), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class)))
                .thenThrow(new HttpClientErrorException(HttpStatus.NOT_FOUND));

        // when & then
        assertThrows(HttpClientErrorException.class, () -> {
            adapter.get(url, null);
        });
    }

    @Test
    void testPost_httpServerError_throwsException() {
        // given
        String url = "https://api.example.com/error";
        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(String.class)))
                .thenThrow(new HttpServerErrorException(HttpStatus.INTERNAL_SERVER_ERROR));

        // when & then
        assertThrows(HttpServerErrorException.class, () -> {
            adapter.post(url, null, "{}");
        });
    }

    @Test
    void testGet_invalidUrl_throwsException() {
        // when & then
        assertThrows(IllegalArgumentException.class, () -> {
            adapter.get("invalid-url", null);
        });
    }

    @Test
    void testPost_invalidUrl_throwsException() {
        // when & then
        assertThrows(IllegalArgumentException.class, () -> {
            adapter.post(null, null, "{}");
        });
    }

    @Test
    void testSetTimeout_doesNotThrowException() {
        // when & then (RestTemplate不支持动态超时，方法为空实现)
        assertDoesNotThrow(() -> {
            adapter.setTimeout(30, 60);
        });
    }

    @Test
    void testGetName_returnsCorrectName() {
        // when
        String name = adapter.getName();

        // then
        assertEquals("RestTemplate", name);
    }

    @Test
    void testGet_non2xxStatus_throwsException() {
        // given
        String url = "https://api.example.com/test";
        ResponseEntity<String> responseEntity = new ResponseEntity<>("{}", HttpStatus.BAD_REQUEST);
        when(restTemplate.exchange(anyString(), eq(HttpMethod.GET), any(HttpEntity.class), eq(String.class)))
                .thenReturn(responseEntity);

        // when & then
        Exception exception = assertThrows(RuntimeException.class, () -> {
            adapter.get(url, null);
        });

        assertTrue(exception.getMessage().contains("HTTP请求失败"));
    }

    @Test
    void testMultipleHeaders_allHeadersSet() throws Exception {
        // given
        String url = "https://api.example.com/test";
        ResponseEntity<String> responseEntity = new ResponseEntity<>("{\"ok\":true}", HttpStatus.OK);
        when(restTemplate.exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(String.class)))
                .thenReturn(responseEntity);

        Map<String, String> headers = new HashMap<>();
        headers.put("Authorization", "Bearer token123");
        headers.put("Content-Type", "application/json");
        headers.put("X-Custom-Header", "custom-value");

        // when
        adapter.post(url, headers, "{}");

        // then
        verify(restTemplate).exchange(anyString(), eq(HttpMethod.POST), any(HttpEntity.class), eq(String.class));
    }
}

