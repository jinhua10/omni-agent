package top.yumbo.ai.omni.common.http;

import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.common.exception.ValidationException;

import static org.junit.jupiter.api.Assertions.*;

/**
 * URL验证工具类测试
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
class UrlValidatorTest {

    @Test
    void testValidateBasic_validHttpUrl_shouldPass() {
        // 正常的HTTP URL
        assertDoesNotThrow(() -> UrlValidator.validateBasic("http://example.com"));
        assertDoesNotThrow(() -> UrlValidator.validateBasic("http://example.com/path"));
        assertDoesNotThrow(() -> UrlValidator.validateBasic("http://example.com:8080/path"));
    }

    @Test
    void testValidateBasic_validHttpsUrl_shouldPass() {
        // 正常的HTTPS URL
        assertDoesNotThrow(() -> UrlValidator.validateBasic("https://example.com"));
        assertDoesNotThrow(() -> UrlValidator.validateBasic("https://api.example.com/v1/users"));
    }

    @Test
    void testValidateBasic_nullUrl_shouldThrowException() {
        // null URL
        ValidationException exception = assertThrows(
            ValidationException.class,
            () -> UrlValidator.validateBasic(null)
        );
        assertTrue(exception.getMessage().contains("URL cannot be null or empty"));
    }

    @Test
    void testValidateBasic_emptyUrl_shouldThrowException() {
        // 空字符串
        ValidationException exception = assertThrows(
            ValidationException.class,
            () -> UrlValidator.validateBasic("")
        );
        assertTrue(exception.getMessage().contains("URL cannot be null or empty"));
    }

    @Test
    void testValidateBasic_blankUrl_shouldThrowException() {
        // 空白字符串
        ValidationException exception = assertThrows(
            ValidationException.class,
            () -> UrlValidator.validateBasic("   ")
        );
        assertTrue(exception.getMessage().contains("URL cannot be null or empty"));
    }

    @Test
    void testValidateBasic_invalidProtocol_shouldThrowException() {
        // 不支持的协议
        ValidationException exception = assertThrows(
            ValidationException.class,
            () -> UrlValidator.validateBasic("ftp://example.com")
        );
        assertTrue(exception.getMessage().contains("Invalid URL protocol"));
    }

    @Test
    void testValidateBasic_noProtocol_shouldThrowException() {
        // 没有协议
        ValidationException exception = assertThrows(
            ValidationException.class,
            () -> UrlValidator.validateBasic("example.com")
        );
        assertTrue(exception.getMessage().contains("Invalid URL protocol"));
    }

    @Test
    void testValidateFull_validUrl_shouldPass() {
        // 完整的有效URL
        assertDoesNotThrow(() -> UrlValidator.validateFull("https://api.example.com/v1/users?id=123"));
    }

    @Test
    void testValidateFull_malformedUrl_shouldThrowException() {
        // 格式错误的URL
        assertThrows(
            ValidationException.class,
            () -> UrlValidator.validateFull("http://")
        );
    }

    @Test
    void testValidateFull_urlWithSpaceInHost_shouldThrowException() {
        // host包含空格
        assertThrows(
            ValidationException.class,
            () -> UrlValidator.validateFull("http://example .com")
        );
    }

    @Test
    void testValidateStrict_validUrl_shouldPass() {
        // 严格验证通过
        assertDoesNotThrow(() -> UrlValidator.validateStrict("https://example.com:443/path"));
    }

    @Test
    void testValidateStrict_invalidPort_shouldThrowException() {
        // 端口号超出范围（注意：URL类可能在解析时就失败）
        assertThrows(
            ValidationException.class,
            () -> UrlValidator.validateStrict("http://example.com:99999/path")
        );
    }

    @Test
    void testIsValid_validUrl_returnsTrue() {
        // 有效URL返回true
        assertTrue(UrlValidator.isValid("http://example.com"));
        assertTrue(UrlValidator.isValid("https://example.com"));
    }

    @Test
    void testIsValid_invalidUrl_returnsFalse() {
        // 无效URL返回false
        assertFalse(UrlValidator.isValid(null));
        assertFalse(UrlValidator.isValid(""));
        assertFalse(UrlValidator.isValid("invalid-url"));
        assertFalse(UrlValidator.isValid("ftp://example.com"));
    }

    @Test
    void testIsHttps_httpsUrl_returnsTrue() {
        // HTTPS URL
        assertTrue(UrlValidator.isHttps("https://example.com"));
        assertTrue(UrlValidator.isHttps("HTTPS://example.com"));
    }

    @Test
    void testIsHttps_httpUrl_returnsFalse() {
        // HTTP URL
        assertFalse(UrlValidator.isHttps("http://example.com"));
    }

    @Test
    void testIsHttps_nullUrl_returnsFalse() {
        // null URL
        assertFalse(UrlValidator.isHttps(null));
    }

    @Test
    void testNormalize_urlWithSpaces_removesSpaces() {
        // 移除前后空格
        assertEquals("http://example.com", UrlValidator.normalize("  http://example.com  "));
    }

    @Test
    void testNormalize_nullUrl_returnsNull() {
        // null返回null
        assertNull(UrlValidator.normalize(null));
    }

    @Test
    void testNormalize_normalUrl_returnsSame() {
        // 正常URL保持不变
        assertEquals("http://example.com", UrlValidator.normalize("http://example.com"));
    }

    @Test
    void testComplexUrls() {
        // 复杂URL测试
        assertDoesNotThrow(() -> UrlValidator.validateFull("https://api.example.com:8443/v1/users/123?name=John&age=30#section"));
        assertDoesNotThrow(() -> UrlValidator.validateFull("http://localhost:8080/api/data"));
        assertDoesNotThrow(() -> UrlValidator.validateFull("https://192.168.1.1/admin"));
    }
}

