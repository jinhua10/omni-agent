package top.yumbo.ai.example.production;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * 生产级应用示例 - 启动类
 * (Production Application - Main Class)
 *
 * <p>
 * 生产级配置示例 (Production Configuration):
 * - Persistence: Elasticsearch（生产级持久化）
 * - Document Storage: AWS S3（公有云对象存储）
 * - RAG: Elasticsearch（向量检索）
 * - AI: Online API（GPT-4/Claude）
 * </p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@SpringBootApplication
public class ProductionApplication {

    public static void main(String[] args) {
        SpringApplication.run(ProductionApplication.class, args);
    }
}

