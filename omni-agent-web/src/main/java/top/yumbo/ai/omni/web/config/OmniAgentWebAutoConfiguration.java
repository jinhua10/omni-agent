package top.yumbo.ai.omni.web.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.servlet.config.annotation.CorsRegistry;
import org.springframework.web.servlet.config.annotation.WebMvcConfigurer;

/**
 * OmniAgent Web 自动配置
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Configuration
@ComponentScan("top.yumbo.ai.omni.web")
public class OmniAgentWebAutoConfiguration implements WebMvcConfigurer {

    /**
     * 配置CORS跨域
     */
    @Override
    public void addCorsMappings(CorsRegistry registry) {
        registry.addMapping("/api/**")
                .allowedOrigins("*")
                .allowedMethods("GET", "POST", "PUT", "DELETE", "OPTIONS")
                .allowedHeaders("*")
                .maxAge(3600);
    }
}

