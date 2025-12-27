package top.yumbo.ai.omni.example.basic.config;

import org.springframework.context.annotation.ComponentScan;
import org.springframework.context.annotation.Configuration;

/**
 * 工作流配置类
 * (Workflow Configuration)
 *
 * <p>确保工作流引擎相关的 Bean 被正确扫描和注册</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Configuration
@ComponentScan(basePackages = "top.yumbo.ai.omni.workflow")
public class WorkflowConfiguration {
}


