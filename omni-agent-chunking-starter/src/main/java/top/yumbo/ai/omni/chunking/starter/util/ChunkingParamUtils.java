package top.yumbo.ai.omni.chunking.starter.util;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.chunking.ChunkingConfig;

import java.util.Map;

/**
 * 分块策略参数工具类
 *
 * <p>提供类型安全的参数获取和自动类型转换</p>
 * <p>从 core/old/chunking 迁移而来，并扩展支持 ChunkingConfig</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class ChunkingParamUtils {

    /**
     * 从 ChunkingConfig 获取固定长度分块的块大小
     *
     * @param config 分块配置
     * @param defaultValue 默认值
     * @return 块大小
     */
    public static int getFixedLengthSize(ChunkingConfig config, int defaultValue) {
        return config.getFixedLengthSize() != null ? config.getFixedLengthSize() : defaultValue;
    }

    /**
     * 从 ChunkingConfig 获取重叠大小
     *
     * @param config 分块配置
     * @param defaultValue 默认值
     * @return 重叠大小
     */
    public static int getOverlap(ChunkingConfig config, int defaultValue) {
        return config.getOverlap() != null ? config.getOverlap() : defaultValue;
    }

    /**
     * 从 ChunkingConfig 获取最大分块大小
     *
     * @param config 分块配置
     * @param defaultValue 默认值
     * @return 最大分块大小
     */
    public static int getMaxChunkSize(ChunkingConfig config, int defaultValue) {
        return config.getMaxChunkSize() != null ? config.getMaxChunkSize() : defaultValue;
    }

    /**
     * 从 ChunkingConfig 获取最小分块大小
     *
     * @param config 分块配置
     * @param defaultValue 默认值
     * @return 最小分块大小
     */
    public static int getMinChunkSize(ChunkingConfig config, int defaultValue) {
        return config.getMinChunkSize() != null ? config.getMinChunkSize() : defaultValue;
    }

    /**
     * 从 ChunkingConfig 获取语义相似度阈值
     *
     * @param config 分块配置
     * @param defaultValue 默认值
     * @return 语义相似度阈值
     */
    public static double getSemanticThreshold(ChunkingConfig config, double defaultValue) {
        return config.getSemanticThreshold() != null ? config.getSemanticThreshold() : defaultValue;
    }

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

