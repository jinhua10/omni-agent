package top.yumbo.ai.omni.web.config;

import org.springframework.context.MessageSource;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.LocaleResolver;
import org.springframework.web.servlet.i18n.AcceptHeaderLocaleResolver;

import java.util.List;
import java.util.Locale;

/**
 * 国际化配置
 *
 * 使用JSON格式存储消息（开发时为JS格式，构建时转换）
 * 完美解决以下问题：
 * 1. ✅ 避免properties重复前缀（嵌套结构）
 * 2. ✅ AI友好（JS源文件语法严格，不易出错）
 * 3. ✅ 开发体验优雅（无双引号key，支持注释）
 * 4. ✅ 运行时高性能（加载JSON）
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Configuration
public class I18nConfig {

    @Bean
    public MessageSource messageSource() {
        JsonMessageSource messageSource = new JsonMessageSource();

        // 设置消息文件基础名称（不含语言后缀和扩展名）
        messageSource.setBasenames("i18n/messages");

        return messageSource;
    }

    @Bean
    public LocaleResolver localeResolver() {
        AcceptHeaderLocaleResolver resolver = new AcceptHeaderLocaleResolver();

        // 设置默认语言为中文
        resolver.setDefaultLocale(Locale.SIMPLIFIED_CHINESE);

        // 支持的语言列表
        resolver.setSupportedLocales(List.of(
            Locale.SIMPLIFIED_CHINESE,
            Locale.US
        ));

        return resolver;
    }
}

