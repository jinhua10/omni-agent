package top.yumbo.ai.omni.web.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.context.MessageSource;
import org.springframework.context.i18n.LocaleContextHolder;
import org.springframework.stereotype.Service;

import java.util.Locale;

/**
 * 国际化消息服务
 * <p>
 * 统一的国际化接口，用于API响应和日志
 * 完全基于Spring MessageSource，符合Spring规范
 * <p>
 * 使用示例：
 * <pre>
 * // API响应（自动根据Accept-Language）
 * messageService.get("api.document.upload.success")
 *
 * // 日志（统一使用中文）
 * log.info(messageService.getForLog("log.document.upload.start", filename))
 *
 * // 指定语言
 * messageService.get("api.common.success", "en")
 * </pre>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class MessageService {

    private final MessageSource messageSource;

    public MessageService(MessageSource messageSource) {
        this.messageSource = messageSource;
    }

    /**
     * 获取当前语言的消息
     * 语言从 LocaleContextHolder 自动获取（基于 Accept-Language 请求头）
     * <p>
     * 用于API响应
     *
     * @param key  消息key（如：api.document.upload.success）
     * @param args 格式化参数
     * @return 国际化消息
     */
    public String get(String key, Object... args) {
        return getMessage(key, LocaleContextHolder.getLocale(), args);
    }

    /**
     * 获取指定语言的消息
     * <p>
     * 用于特殊场景（如邮件、推送等）
     *
     * @param key    消息key
     * @param locale 语言
     * @param args   格式化参数
     * @return 国际化消息
     */
    public String get(String key, Locale locale, Object... args) {
        return getMessage(key, locale, args);
    }

    /**
     * 获取指定语言的消息（通过语言代码）
     * <p>
     * 用于前端传递lang参数的场景
     *
     * @param key      消息key
     * @param langCode 语言代码（zh/en/zh-CN/en-US等）
     * @param args     格式化参数
     * @return 国际化消息
     */
    public String get(String key, String langCode, Object... args) {
        Locale locale = parseLocale(langCode);
        return getMessage(key, locale, args);
    }

    /**
     * 获取日志消息（统一使用中文）
     * <p>
     * 用于日志记录，保持日志统一为中文便于运维
     *
     * @param key  消息key（如：log.document.upload.start）
     * @param args 格式化参数
     * @return 国际化消息（中文）
     */
    public String getForLog(String key, Object... args) {
        return getMessage(key, Locale.SIMPLIFIED_CHINESE, args);
    }

    /**
     * 内部方法：获取消息
     */
    private String getMessage(String key, Locale locale, Object... args) {
        try {
            return messageSource.getMessage(key, args, locale);
        } catch (Exception e) {
            log.warn("Failed to get message for key: {} in locale: {}", key, locale);
            return "[" + key + "]";
        }
    }

    /**
     * 解析语言代码为Locale
     */
    private Locale parseLocale(String langCode) {
        if (langCode == null || langCode.isEmpty()) {
            return LocaleContextHolder.getLocale();
        }

        return switch (langCode.toLowerCase()) {
            case "zh", "zh-cn", "zh_cn" -> Locale.SIMPLIFIED_CHINESE;
            case "en", "en-us", "en_us" -> Locale.US;
            default -> Locale.SIMPLIFIED_CHINESE;
        };
    }
}

