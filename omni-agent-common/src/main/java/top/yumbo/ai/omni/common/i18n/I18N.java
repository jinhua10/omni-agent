package top.yumbo.ai.omni.common.i18n;

import lombok.extern.slf4j.Slf4j;
import org.yaml.snakeyaml.Yaml;

import java.io.InputStream;
import java.text.MessageFormat;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * 提供后端日志国际化支持（静态工具类）
 * (Provides backend log internationalization support - static utility class)
 * <p>
 * 使用静态方法 `get(key, args...)` 在任何场景下获取日志模板（支持非 Spring 启动）。
 * (Use static method `get(key, args...)` to get log templates in any scenario)
 *
 * <p>编码说明 (Encoding Notes):
 * <ul>
 *   <li>所有 messages*.yml 文件统一使用 UTF-8 编码保存 (All files saved in UTF-8)</li>
 *   <li>使用 SnakeYAML 库加载 YAML 格式的国际化文件 (Use SnakeYAML to load YAML i18n files)</li>
 *   <li>支持嵌套的 YAML 结构，自动展平为点号分隔的 key (Support nested YAML structure)</li>
 * </ul>
 * </p>
 *
 * @author AI Reviewer Team
 * @since 2.0.0
 */
@Slf4j
public final class I18N {

    // 静态加载的消息映射表 (Statically loaded message maps)
    private static final Map<String, String> messagesZh = new HashMap<>();
    private static final Map<String, String> messagesEn = new HashMap<>();

    static {
        // 加载中文消息 (Load Chinese messages)
        // 动态扫描 i18n/zh/ 目录下的所有 yml 文件 (Dynamically scan all yml files in i18n/zh/ directory)
        loadMessagesWithPrefix("i18n/zh/", messagesZh, "Chinese");

        // 加载英文消息 (Load English messages)
        // 动态扫描 i18n/en/ 目录下的所有 yml 文件 (Dynamically scan all yml files in i18n/en/ directory)
        loadMessagesWithPrefix("i18n/en/", messagesEn, "English");
    }

    /**
     * 加载指定目录下的所有 yml 文件
     * (Load all yml files in specified directory)
     *
     * @param directory 目录路径，如 "i18n/zh/" (Directory path, e.g. "i18n/zh/")
     * @param target    目标消息Map (Target message map)
     * @param language  语言名称（用于日志） (Language name for logging)
     */
    private static void loadMessagesWithPrefix(String directory, Map<String, String> target, String language) {
        int totalLoaded = 0;

        try {
            // 获取目录下的所有资源 (Get all resources in directory)
            ClassLoader classLoader = I18N.class.getClassLoader();
            java.net.URL resource = classLoader.getResource(directory);

            if (resource == null) {
                log.warn("Directory not found: {}", directory);
                return;
            }

            // 处理不同的资源类型 (Handle different resource types)
            if ("file".equals(resource.getProtocol())) {
                // 文件系统路径 (File system path)
                java.io.File dir = new java.io.File(resource.toURI());
                if (dir.exists() && dir.isDirectory()) {
                    java.io.File[] files = dir.listFiles((d, name) -> name.endsWith(".yml"));
                    if (files != null) {
                        for (java.io.File file : files) {
                            String filename = directory + file.getName();
                            totalLoaded += loadSingleYmlFile(filename, target, language);
                        }
                    }
                }
            } else if ("jar".equals(resource.getProtocol())) {
                // JAR 文件路径 (JAR file path)
                String jarPath = resource.getPath();
                if (jarPath.contains("!")) {
                    String[] parts = jarPath.split("!");
                    String jarFilePath = parts[0].substring(5); // 移除 "file:" 前缀 (Remove "file:" prefix)
                    try (java.util.jar.JarFile jarFile = new java.util.jar.JarFile(jarFilePath)) {
                        java.util.Enumeration<java.util.jar.JarEntry> entries = jarFile.entries();
                        while (entries.hasMoreElements()) {
                            java.util.jar.JarEntry entry = entries.nextElement();
                            String entryName = entry.getName();
                            // 检查是否在目标目录且是 yml 文件 (Check if in target directory and is yml file)
                            if (entryName.startsWith(directory) && entryName.endsWith(".yml") && !entry.isDirectory()) {
                                totalLoaded += loadSingleYmlFile(entryName, target, language);
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            log.error("Failed to scan directory: {}", directory, e);
        }

        if (totalLoaded == 0) {
            log.warn("No {} message keys loaded from directory: {}", language, directory);
        } else {
            log.info("Loaded total {} {} message keys from directory: {}", totalLoaded, language, directory);
        }
    }

    /**
     * 加载单个 yml 文件
     * (Load single yml file)
     *
     * @param filename 文件路径 (File path)
     * @param target   目标消息Map (Target message map)
     * @param language 语言名称 (Language name)
     * @return 加载的键数量 (Number of keys loaded)
     */
    private static int loadSingleYmlFile(String filename, Map<String, String> target, String language) {
        try (InputStream is = I18N.class.getClassLoader().getResourceAsStream(filename)) {
            if (is != null) {
                Yaml yaml = new Yaml();
                Map<String, Object> data = yaml.load(is);
                int beforeSize = target.size();
                flattenYaml("", data, target);
                int loaded = target.size() - beforeSize;
                log.debug("Loaded {} {} keys from {}", loaded, language, filename);
                return loaded;
            } else {
                log.debug("{} not found (optional)", filename);
                return 0;
            }
        } catch (Exception e) {
            log.error("Failed to load {}", filename, e);
            return 0;
        }
    }

    /**
     * 将嵌套的 YAML 结构展平为点号分隔的 key
     * (Flatten nested YAML structure to dot-separated keys)
     */
    @SuppressWarnings("unchecked")
    private static void flattenYaml(String prefix, Map<String, Object> map, Map<String, String> result) {
        // 保护性校验：空 map 直接返回 (Protective check: return directly if map is empty)
        if (map == null || map.isEmpty()) {
            return;
        }

        // 规范化 prefix，避免后续调用 NPE (Normalize prefix to avoid NPE)
        String safePrefix = prefix == null ? "" : prefix;

        // 如果顶层是单个 'lang' 节点（来自非标准挂载），解包它以去掉 'lang.' 前缀
        // If the top-level map contains only a single 'lang' node, unwrap it so keys are not prefixed with 'lang.'
        if (safePrefix.isEmpty() && map.size() == 1 && map.containsKey("lang") && map.get("lang") instanceof Map) {
            flattenYaml("", (Map<String, Object>) map.get("lang"), result);
            return;
        }

        for (Map.Entry<String, Object> entry : map.entrySet()) {
            String key = safePrefix.isEmpty() ? entry.getKey() : safePrefix + "." + entry.getKey();
            Object value = entry.getValue();

            if (value instanceof Map) {
                // 安全处理嵌套的 Map，可能包含非字符串键 (Safely handle nested maps that may contain non-string keys)
                flattenYamlSafe(key, (Map<?, ?>) value, result);
            } else if (value != null) {
                // 叶子节点，存储值 (Leaf node, store value)
                result.put(key, value.toString());
            }
        }
    }

    /**
     * 安全地处理嵌套的 YAML Map，支持非字符串键
     * (Safely handle nested YAML maps with non-string keys)
     */
    @SuppressWarnings("unchecked")
    private static void flattenYamlSafe(String prefix, Map<?, ?> map, Map<String, String> result) {
        if (map == null || map.isEmpty()) {
            return;
        }
        String safePrefix = prefix == null ? "" : prefix;
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            // 检查键是否为null (Check if key is null)
            Object entryKey = entry.getKey();
            if (entryKey == null) {
                // 提供更详细的诊断信息 (Provide more detailed diagnostic information)
                Object value = entry.getValue();
                log.warn("Found null key in YAML map at prefix '{}', value type: {}, value: {}",
                        safePrefix.isEmpty() ? "<root>" : safePrefix,
                        value == null ? "null" : value.getClass().getSimpleName(),
                        value == null ? "null" : (value.toString().length() > 100 ? value.toString().substring(0, 100) + "..." : value.toString()));
                continue;
            }

            String key = safePrefix.isEmpty() ? entryKey.toString() : safePrefix + "." + entryKey.toString();
            Object value = entry.getValue();

            if (value instanceof Map) {
                // 递归处理嵌套的 Map (Recursively process nested maps)
                flattenYamlSafe(key, (Map<?, ?>) value, result);
            } else if (value != null) {
                // 叶子节点，存储值 (Leaf node, store value)
                result.put(key, value.toString());
            }
        }
    }

    private I18N() {
        // utility class
    }

    /**
     * 确定当前语言环境 (Determine current locale)
     */
    private static Locale determineStaticLocale() {
        // 首先检查系统属性 (Check system property first)
        String cfg = System.getProperty("log.locale");
        if (cfg == null || cfg.isEmpty()) {
            cfg = System.getenv("LOG_LOCALE");
        }
        if (cfg != null) {
            if ("zh".equalsIgnoreCase(cfg) || "zh-CN".equalsIgnoreCase(cfg)) {
                return Locale.SIMPLIFIED_CHINESE;
            } else if ("en".equalsIgnoreCase(cfg) || "en-US".equalsIgnoreCase(cfg)) {
                return Locale.ENGLISH;
            }
        }

        // 使用系统默认语言 (Use system default locale)
        Locale defaultLocale = Locale.getDefault();
        if (defaultLocale != null && "zh".equalsIgnoreCase(defaultLocale.getLanguage())) {
            return Locale.SIMPLIFIED_CHINESE;
        }
        return Locale.ENGLISH;
    }

    /**
     * 静态方法：在任何场景下直接调用以获取日志模板
     * (Static method: get log message template in any scenario)
     *
     * @param key  消息键 (Message key)
     * @param args 格式化参数 (Format arguments)
     * @return 国际化消息 (Internationalized message)
     */
    public static String get(String key, Object... args) {
        Locale locale = determineStaticLocale();
        return getMessageWithLocale(key, locale, args);
    }

    /**
     * 根据指定语言获取消息（用于 API 响应）
     * (Get message with specified language for API responses)
     *
     * @param key  消息键 (Message key)
     * @param lang 语言参数 (zh/en/null) (Language parameter)
     * @param args 格式化参数 (Format arguments)
     * @return 国际化消息 (Internationalized message)
     */
    public static String getLang(String key, String lang, Object... args) {
        Locale locale;
        if ("en".equalsIgnoreCase(lang)) {
            locale = Locale.ENGLISH;
        } else if ("zh".equalsIgnoreCase(lang)) {
            locale = Locale.SIMPLIFIED_CHINESE;
        } else {
            // 如果lang为null或其他值，使用默认行为（自动检测）
            // If lang is null or other values, use default behavior (auto-detect)
            locale = determineStaticLocale();
        }
        return getMessageWithLocale(key, locale, args);
    }

    /**
     * 内部方法：根据 Locale 获取消息
     * (Internal method: get message with Locale)
     */
    private static String getMessageWithLocale(String key, Locale locale, Object... args) {
        String pattern = null;

        // 根据语言环境选择消息源 (Select message source based on locale)
        if (Locale.SIMPLIFIED_CHINESE.equals(locale)) {
            pattern = messagesZh.get(key);
        }

        // 如果中文未找到，尝试英文 (Fallback to English if Chinese not found)
        if (pattern == null) {
            pattern = messagesEn.get(key);
        }

        // 如果都未找到，返回 key 本身 (If not found, return key itself)
        if (pattern == null) {
            log.debug("Missing static log key {} in resources", key);
            pattern = "[" + key + "]";
        }

        // 格式化消息 (Format message)
        try {
            return MessageFormat.format(pattern, args == null ? new Object[0] : args);
        } catch (IllegalArgumentException e) {
            log.warn("Failed to format message for key: {} with pattern: {}", key, pattern, e);
            return pattern;
        }
    }
}

