package top.yumbo.ai.omni.web.config;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.InitializingBean;
import org.springframework.context.support.AbstractMessageSource;

import java.io.InputStream;
import java.text.MessageFormat;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 基于JSON的MessageSource实现
 *
 * 特点：
 * 1. 开发时使用JS对象字面量（优雅、无双引号key）
 * 2. 构建时转换为JSON（标准格式）
 * 3. 运行时加载JSON（高性能）
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
public class JsonMessageSource extends AbstractMessageSource implements InitializingBean {

    private String[] basenames = new String[0];
    private Map<Locale, Map<String, String>> cachedMessages = new ConcurrentHashMap<>();
    private final ObjectMapper objectMapper = new ObjectMapper();

    public void setBasenames(String... basenames) {
        this.basenames = basenames;
    }

    @Override
    public void afterPropertiesSet() {
        loadMessages();
    }

    @Override
    protected MessageFormat resolveCode(String code, Locale locale) {
        Map<String, String> messages = getMessages(locale);
        String message = messages.get(code);
        return message != null ? new MessageFormat(message, locale) : null;
    }

    private Map<String, String> getMessages(Locale locale) {
        return cachedMessages.computeIfAbsent(locale, this::loadMessagesForLocale);
    }

    private void loadMessages() {
        loadMessagesForLocale(Locale.SIMPLIFIED_CHINESE);
        loadMessagesForLocale(Locale.US);
        log.info("i18n messages loaded successfully. Locales: {}", cachedMessages.keySet());
    }

    private Map<String, String> loadMessagesForLocale(Locale locale) {
        Map<String, String> messages = new HashMap<>();
        String suffix = getSuffix(locale);

        for (String basename : basenames) {
            String filename = basename + "_" + suffix + ".json";
            loadJsonFile(filename, messages);
        }

        log.debug("Loaded {} messages for locale: {}", messages.size(), locale);
        return messages;
    }

    private void loadJsonFile(String filename, Map<String, String> target) {
        try (InputStream is = getClass().getClassLoader().getResourceAsStream(filename)) {
            if (is == null) {
                log.warn("i18n file not found: {}", filename);
                return;
            }

            Map<String, Object> data = objectMapper.readValue(is,
                new TypeReference<>() {});

            flattenJson("", data, target);
            log.debug("Loaded i18n file: {} ({} keys)", filename, target.size());

        } catch (Exception e) {
            log.error("Failed to load i18n file: {}", filename, e);
        }
    }

    @SuppressWarnings("unchecked")
    private void flattenJson(String prefix, Map<String, Object> map, Map<String, String> result) {
        for (Map.Entry<String, Object> entry : map.entrySet()) {
            String key = prefix.isEmpty() ? entry.getKey() : prefix + "." + entry.getKey();
            Object value = entry.getValue();

            if (value instanceof Map) {
                flattenJson(key, (Map<String, Object>) value, result);
            } else if (value != null) {
                result.put(key, value.toString());
            }
        }
    }

    private String getSuffix(Locale locale) {
        if (locale.equals(Locale.SIMPLIFIED_CHINESE) || "zh".equals(locale.getLanguage())) {
            return "zh_CN";
        }
        return "en_US";
    }
}

