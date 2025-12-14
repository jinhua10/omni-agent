package top.yumbo.ai.omni.core.p2p;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.Set;

import static org.assertj.core.api.Assertions.*;

/**
 * ConnectionCodeGenerator 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("ConnectionCodeGenerator Tests")
class ConnectionCodeGeneratorTest {

    private ConnectionCodeGenerator generator;

    @BeforeEach
    void setUp() {
        generator = new ConnectionCodeGenerator();
    }

    @Test
    @DisplayName("应该生成有效的连接码")
    void shouldGenerateValidCode() {
        // When
        String code = generator.generate("user-001", "Alice", "publicKey123");

        // Then
        assertThat(code)
                .isNotNull()
                .matches("[A-Z]{3}-[A-Z]{3}-\\d{3}");
    }

    @Test
    @DisplayName("应该生成唯一的连接码")
    void shouldGenerateUniqueCodes() {
        // Given
        Set<String> codes = new HashSet<>();

        // When
        for (int i = 0; i < 100; i++) {
            String code = generator.generate("user-" + i, "User" + i, "key" + i);
            codes.add(code);
        }

        // Then
        assertThat(codes).hasSize(100);
    }

    @Test
    @DisplayName("应该验证有效的连接码")
    void shouldValidateValidCode() {
        // Given
        String code = generator.generate("user-001", "Alice", "publicKey123");

        // When
        ConnectionCodeGenerator.CodeInfo info = generator.validate(code);

        // Then
        assertThat(info).isNotNull();
        assertThat(info.userId).isEqualTo("user-001");
        assertThat(info.userName).isEqualTo("Alice");
        assertThat(info.publicKey).isEqualTo("publicKey123");
        assertThat(info.used).isFalse();
        assertThat(info.createTime).isBeforeOrEqualTo(LocalDateTime.now());
        assertThat(info.expiryTime).isAfter(LocalDateTime.now());
    }

    @Test
    @DisplayName("应该拒绝不存在的连接码")
    void shouldRejectNonExistentCode() {
        // When
        ConnectionCodeGenerator.CodeInfo info = generator.validate("AAA-BBB-999");

        // Then
        assertThat(info).isNull();
    }

    @Test
    @DisplayName("应该标记连接码为已使用")
    void shouldMarkCodeAsUsed() {
        // Given
        String code = generator.generate("user-001", "Alice", "publicKey123");

        // When
        generator.markUsed(code);

        // Then
        ConnectionCodeGenerator.CodeInfo info = generator.validate(code);
        assertThat(info).isNull();
    }

    @Test
    @DisplayName("应该拒绝已使用的连接码")
    void shouldRejectUsedCode() {
        // Given
        String code = generator.generate("user-001", "Alice", "publicKey123");
        generator.markUsed(code);

        // When
        ConnectionCodeGenerator.CodeInfo info = generator.validate(code);

        // Then
        assertThat(info).isNull();
    }

    @Test
    @DisplayName("连接码格式应该正确")
    void shouldHaveCorrectFormat() {
        // When
        String code = generator.generate("user-001", "Alice", "publicKey123");

        // Then
        String[] parts = code.split("-");
        assertThat(parts).hasSize(3);
        assertThat(parts[0]).hasSize(3).matches("[A-Z]{3}");
        assertThat(parts[1]).hasSize(3).matches("[A-Z]{3}");
        assertThat(parts[2]).hasSize(3).matches("\\d{3}");
    }

    @Test
    @DisplayName("应该处理多个用户的连接码")
    void shouldHandleMultipleUsers() {
        // When
        String code1 = generator.generate("user-001", "Alice", "key1");
        String code2 = generator.generate("user-002", "Bob", "key2");
        String code3 = generator.generate("user-003", "Charlie", "key3");

        // Then
        assertThat(code1).isNotEqualTo(code2).isNotEqualTo(code3);

        ConnectionCodeGenerator.CodeInfo info1 = generator.validate(code1);
        ConnectionCodeGenerator.CodeInfo info2 = generator.validate(code2);
        ConnectionCodeGenerator.CodeInfo info3 = generator.validate(code3);

        assertThat(info1.userId).isEqualTo("user-001");
        assertThat(info2.userId).isEqualTo("user-002");
        assertThat(info3.userId).isEqualTo("user-003");
    }

    @Test
    @DisplayName("应该存储完整的用户信息")
    void shouldStoreCompleteUserInfo() {
        // Given
        String userId = "user-123";
        String userName = "TestUser";
        String publicKey = "MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA...";

        // When
        String code = generator.generate(userId, userName, publicKey);
        ConnectionCodeGenerator.CodeInfo info = generator.validate(code);

        // Then
        assertThat(info).isNotNull();
        assertThat(info.code).isEqualTo(code);
        assertThat(info.userId).isEqualTo(userId);
        assertThat(info.userName).isEqualTo(userName);
        assertThat(info.publicKey).isEqualTo(publicKey);
    }

    @Test
    @DisplayName("应该处理特殊字符的用户名")
    void shouldHandleSpecialCharactersInUsername() {
        // When
        String code = generator.generate("user-001", "Alice@测试#123", "key");
        ConnectionCodeGenerator.CodeInfo info = generator.validate(code);

        // Then
        assertThat(info).isNotNull();
        assertThat(info.userName).isEqualTo("Alice@测试#123");
    }

    @Test
    @DisplayName("连接码应该包含随机性")
    void shouldProduceRandomCodes() {
        // Given
        Set<String> codes = new HashSet<>();

        // When
        for (int i = 0; i < 10; i++) {
            String code = generator.generate("user-001", "Alice", "key");
            codes.add(code);
        }

        // Then
        // 至少应该有2个不同的码（由于随机性，几乎不可能都相同）
        assertThat(codes.size()).isGreaterThanOrEqualTo(2);
    }
}
