package top.yumbo.ai.omni.web.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.License;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
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

    /**
     * OpenAPI文档配置（可选，需要引入springdoc依赖）
     */
    @Bean
    @ConditionalOnClass(name = "org.springdoc.core.models.GroupedOpenApi")
    public OpenAPI omniAgentOpenAPI() {
        return new OpenAPI()
                .info(new Info()
                        .title("OmniAgent API")
                        .description("OmniAgent RAG优化框架 REST API")
                        .version("3.0.0")
                        .contact(new Contact()
                                .name("OmniAgent Team")
                                .url("https://github.com/omni-agent"))
                        .license(new License()
                                .name("Apache 2.0")
                                .url("https://www.apache.org/licenses/LICENSE-2.0")));
    }
}

