package top.yumbo.ai.omni.web.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.License;
import org.springframework.boot.autoconfigure.condition.ConditionalOnClass;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * OpenAPI/Swagger 文档配置
 *
 * 只有在引入 springdoc-openapi 依赖时才会生效
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Configuration
@ConditionalOnClass(name = "io.swagger.v3.oas.models.OpenAPI")
public class OpenAPIConfiguration {

    /**
     * OpenAPI 配置
     */
    @Bean
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






