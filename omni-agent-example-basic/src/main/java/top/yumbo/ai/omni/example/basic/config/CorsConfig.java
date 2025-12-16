package top.yumbo.ai.omni.example.basic.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.cors.CorsConfiguration;
import org.springframework.web.cors.UrlBasedCorsConfigurationSource;
import org.springframework.web.filter.CorsFilter;

/**
 * 全局CORS跨域配置
 * (Global CORS Configuration)
 *
 * <p>允许所有来源、所有方法、所有头部的跨域请求</p>
 * <p>Allow all origins, methods, and headers for CORS requests</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
public class CorsConfig {

    /**
     * 配置全局CORS过滤器
     * (Configure global CORS filter)
     *
     * @return CORS过滤器 (CORS filter)
     */
    @Bean
    public CorsFilter corsFilter() {
        UrlBasedCorsConfigurationSource source = new UrlBasedCorsConfigurationSource();
        CorsConfiguration config = new CorsConfiguration();

        // 允许所有来源 (Allow all origins)
        config.addAllowedOriginPattern("*");

        // 允许所有请求头 (Allow all headers)
        config.addAllowedHeader("*");

        // 允许所有请求方法 (Allow all methods)
        config.addAllowedMethod("*");

        // 允许发送Cookie (Allow credentials)
        config.setAllowCredentials(true);

        // 预检请求的有效期，单位为秒 (Max age for preflight requests in seconds)
        config.setMaxAge(3600L);

        // 允许浏览器访问的响应头 (Expose response headers to browser)
        config.addExposedHeader("*");

        // 对所有路径应用该配置 (Apply configuration to all paths)
        source.registerCorsConfiguration("/**", config);

        return new CorsFilter(source);
    }
}

