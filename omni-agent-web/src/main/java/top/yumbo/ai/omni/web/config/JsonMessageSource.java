package top.yumbo.ai.omni.web.config;
}
    }
        return "en_US";
        }
            return "zh_CN";
        if (locale.equals(Locale.SIMPLIFIED_CHINESE) || "zh".equals(locale.getLanguage())) {
    private String getSuffix(Locale locale) {

    }
        }
            }
                result.put(key, value.toString());
            } else if (value != null) {
                flattenJson(key, (Map<String, Object>) value, result);
            if (value instanceof Map) {

            Object value = entry.getValue();
            String key = prefix.isEmpty() ? entry.getKey() : prefix + "." + entry.getKey();
        for (Map.Entry<String, Object> entry : map.entrySet()) {
    private void flattenJson(String prefix, Map<String, Object> map, Map<String, String> result) {
    @SuppressWarnings("unchecked")

    }
        }
            log.error("Failed to load i18n file: {}", filename, e);
        } catch (Exception e) {

            log.debug("Loaded i18n file: {} ({} keys)", filename, target.size());
            flattenJson("", data, target);

                new TypeReference<Map<String, Object>>() {});
            Map<String, Object> data = objectMapper.readValue(is,

            }
                return;
                log.warn("i18n file not found: {}", filename);
            if (is == null) {
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(filename)) {
    private void loadJsonFile(String filename, Map<String, String> target) {

    }
        return messages;
        log.debug("Loaded {} messages for locale: {}", messages.size(), locale);

        }
            loadJsonFile(filename, messages);
            String filename = basename + "_" + suffix + ".json";
        for (String basename : basenames) {

        String suffix = getSuffix(locale);
        Map<String, String> messages = new HashMap<>();
    private Map<String, String> loadMessagesForLocale(Locale locale) {

    }
        log.info("i18n messages loaded successfully. Locales: {}", cachedMessages.keySet());
        loadMessagesForLocale(Locale.US);
        loadMessagesForLocale(Locale.SIMPLIFIED_CHINESE);
    private void loadMessages() {

    }
        return cachedMessages.computeIfAbsent(locale, this::loadMessagesForLocale);
    private Map<String, String> getMessages(Locale locale) {

    }
        return message != null ? new MessageFormat(message, locale) : null;
        String message = messages.get(code);
        Map<String, String> messages = getMessages(locale);
    protected MessageFormat resolveCode(String code, Locale locale) {
    @Override

    }
        loadMessages();
    public void afterPropertiesSet() {
    @Override

    }
        this.basenames = basenames;
    public void setBasenames(String... basenames) {

    private final ObjectMapper objectMapper = new ObjectMapper();
    private Map<Locale, Map<String, String>> cachedMessages = new ConcurrentHashMap<>();
    private String[] basenames = new String[0];

public class JsonMessageSource extends AbstractMessageSource implements InitializingBean {
@Slf4j
 */
 * @since 3.0.0
 * @author OmniAgent Team
 *
 * 3. 运行时加载JSON（高性能）
 * 2. 构建时转换为JSON（标准格式）
 * 1. 开发时使用JS对象字面量（优雅、无双引号key）
 * 特点：
 *
 * 基于JSON的MessageSource实现
/**

import java.util.concurrent.ConcurrentHashMap;
import java.util.*;
import java.text.MessageFormat;
import java.io.InputStream;

import org.springframework.context.support.AbstractMessageSource;
import org.springframework.beans.factory.InitializingBean;
import lombok.extern.slf4j.Slf4j;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.core.type.TypeReference;


