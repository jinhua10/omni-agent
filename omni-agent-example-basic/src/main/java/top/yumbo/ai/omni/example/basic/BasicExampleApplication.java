package top.yumbo.ai.omni.example.basic;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;

/**
 * OmniAgent 基础示例应用
 * 
 * <p>演示如何使用 OmniAgent 四维可插拔架构：</p>
 * <ul>
 *   <li>Persistence: Memory（内存持久化）</li>
 *   <li>Document Storage: File（文件存储）</li>
 *   <li>RAG: File/Lucene（本地检索）</li>
 *   <li>AI: Local Ollama（本地AI）</li>
 * </ul>
 * 
 * <p>切换实现只需要：</p>
 * <ol>
 *   <li>修改 pom.xml 中的 Starter 依赖</li>
 *   <li>修改 application.yml 配置</li>
 * </ol>
 * 
 * @author Jinhua Yu
 * @since 1.0.0
 */
@SpringBootApplication
@ComponentScan(basePackages = {
    "top.yumbo.ai.omni.example.basic",
    "top.yumbo.ai.omni.core"
})
public class BasicExampleApplication {

    public static void main(String[] args) {
        SpringApplication.run(BasicExampleApplication.class, args);
    }
}
