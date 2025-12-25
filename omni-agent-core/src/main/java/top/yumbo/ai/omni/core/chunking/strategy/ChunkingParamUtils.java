package top.yumbo.ai.omni.core.chunking.strategy;

import lombok.extern.slf4j.Slf4j;

import java.util.Map;

/**
 * 分块策略参数工具类
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public class ChunkingParamUtils {

    /**
     * 从参数 Map 中获取参数，支持类型自动转换
     *
     * @param params 参数 Map
     * @param key 参数键
     * @param defaultValue 默认值
     * @param <T> 参数类型
     * @return 参数值，如果不存在或转换失败则返回默认值
     */
    @SuppressWarnings("unchecked")
    public static <T> T getParam(Map<String, Object> params, String key, T defaultValue) {
        if (params == null || !params.containsKey(key)) {
            return defaultValue;
        }

        Object value = params.get(key);
        if (value == null) {
            return defaultValue;
        }

        // 类型转换处理：处理 String -> Number 的情况
        if (defaultValue instanceof Integer) {
            if (value instanceof String) {
                try {
                    return (T) Integer.valueOf((String) value);
                } catch (NumberFormatException e) {
                    log.warn("参数 {} 转换失败: {}, 使用默认值: {}", key, value, defaultValue);
                    return defaultValue;
                }
            } else if (value instanceof Number) {
                // 处理 Double -> Integer 的情况
                return (T) Integer.valueOf(((Number) value).intValue());
            }
        } else if (defaultValue instanceof Double) {
            if (value instanceof String) {
                try {
                    return (T) Double.valueOf((String) value);
                } catch (NumberFormatException e) {
                    log.warn("参数 {} 转换失败: {}, 使用默认值: {}", key, value, defaultValue);
                    return defaultValue;
                }
            } else if (value instanceof Number) {
                // 处理 Integer -> Double 的情况
                return (T) Double.valueOf(((Number) value).doubleValue());
            }
        } else if (defaultValue instanceof Long) {
            if (value instanceof String) {
                try {
                    return (T) Long.valueOf((String) value);
                } catch (NumberFormatException e) {
                    log.warn("参数 {} 转换失败: {}, 使用默认值: {}", key, value, defaultValue);
                    return defaultValue;
                }
            } else if (value instanceof Number) {
                return (T) Long.valueOf(((Number) value).longValue());
            }
        } else if (defaultValue instanceof Float) {
            if (value instanceof String) {
                try {
                    return (T) Float.valueOf((String) value);
                } catch (NumberFormatException e) {
                    log.warn("参数 {} 转换失败: {}, 使用默认值: {}", key, value, defaultValue);
                    return defaultValue;
                }
            } else if (value instanceof Number) {
                return (T) Float.valueOf(((Number) value).floatValue());
            }
        } else if (defaultValue instanceof Boolean) {
            if (value instanceof String) {
                return (T) Boolean.valueOf((String) value);
            }
        }

        // 直接类型转换
        try {
            return (T) value;
        } catch (ClassCastException e) {
            log.warn("参数 {} 类型转换失败: {} -> {}, 使用默认值: {}",
                     key, value.getClass().getSimpleName(),
                     defaultValue.getClass().getSimpleName(), defaultValue);
            return defaultValue;
        }
    }
}

