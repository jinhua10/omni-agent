package top.yumbo.ai.omni.example.basic;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.ComponentScan;
import org.springframework.scheduling.annotation.EnableScheduling;

/**
 * OmniAgent 基础示例应用
 * 
 * <p>演示如何使用 OmniAgent 四维可插拔架构：</p>
 * <ul>
 *   <li>Persistence: SQLite + File + H2（异构冗余）</li>
 *   <li>Document Storage: File（多路径同种冗余）</li>
 *   <li>RAG: Lucene + SQLite FTS5（异构冗余）</li>
 *   <li>AI: Online API + Local Ollama（故障转移）</li>
 * </ul>
 *
 * <p>冗余备份策略：</p>
 * <ul>
 *   <li>同种冗余：相同技术的多实例备份（如多路径文件存储）</li>
 *   <li>异构冗余：不同技术栈的备份（如SQLite+File+H2）</li>
 *   <li>自动监控：定时健康检查和数据一致性验证</li>
 *   <li>故障恢复：自动检测和恢复故障备份</li>
 * </ul>
 * 
 * <p>切换实现只需要：</p>
 * <ol>
 *   <li>修改 pom.xml 中的 Starter 依赖</li>
 *   <li>修改 application.yml 配置</li>
 *   <li>使用 application-redundant.yml 启用冗余备份</li>
 * </ol>
 * 
 * @author Jinhua Yu
 * @since 1.0.0
 */
@SpringBootApplication
@EnableScheduling  // 启用定时任务（用于备份监控）
@ComponentScan(basePackages = {
    "top.yumbo.ai.omni.example.basic",
    "top.yumbo.ai.omni.core"
})
public class BasicExampleApplication {

    public static void main(String[] args) {
        SpringApplication.run(BasicExampleApplication.class, args);
    }
}
