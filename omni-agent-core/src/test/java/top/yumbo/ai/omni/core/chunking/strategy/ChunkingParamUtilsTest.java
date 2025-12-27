package top.yumbo.ai.omni.core.chunking.strategy;

import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * 测试 ChunkingParamUtils 参数类型转换
 */
class ChunkingParamUtilsTest {

    @Test
    void testStringToInteger() {
        Map<String, Object> params = new HashMap<>();
        params.put("size", "100");

        Integer result = ChunkingParamUtils.getParam(params, "size", 50);
        assertEquals(100, result);
    }

    @Test
    void testStringToDouble() {
        Map<String, Object> params = new HashMap<>();
        params.put("threshold", "0.3");

        Double result = ChunkingParamUtils.getParam(params, "threshold", 0.5);
        assertEquals(0.3, result, 0.001);
    }

    @Test
    void testIntegerToDouble() {
        Map<String, Object> params = new HashMap<>();
        params.put("threshold", 1);

        Double result = ChunkingParamUtils.getParam(params, "threshold", 0.5);
        assertEquals(1.0, result, 0.001);
    }

    @Test
    void testDoubleToInteger() {
        Map<String, Object> params = new HashMap<>();
        params.put("size", 100.5);

        Integer result = ChunkingParamUtils.getParam(params, "size", 50);
        assertEquals(100, result);
    }

    @Test
    void testInvalidString() {
        Map<String, Object> params = new HashMap<>();
        params.put("size", "invalid");

        Integer result = ChunkingParamUtils.getParam(params, "size", 50);
        assertEquals(50, result); // 应该返回默认值
    }

    @Test
    void testMissingParam() {
        Map<String, Object> params = new HashMap<>();

        Integer result = ChunkingParamUtils.getParam(params, "size", 50);
        assertEquals(50, result);
    }

    @Test
    void testNullParams() {
        Integer result = ChunkingParamUtils.getParam(null, "size", 50);
        assertEquals(50, result);
    }

    @Test
    void testBoolean() {
        Map<String, Object> params = new HashMap<>();
        params.put("enabled", "true");

        Boolean result = ChunkingParamUtils.getParam(params, "enabled", false);
        assertEquals(true, result);
    }
}


