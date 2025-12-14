package top.yumbo.ai.rag.file;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * File RAG 配置属性
 * (File RAG Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.rag.file")
public class FileRAGProperties {

    /**
     * Lucene 索引路径
     */
    private String indexPath = "./data/lucene-index";

    /**
     * RAM 缓冲区大小（MB）
     */
    private double ramBufferSizeMb = 256.0;

    /**
     * 是否在启动时重建索引
     */
    private boolean rebuildOnStartup = false;

    /**
     * 最大搜索结果数
     */
    private int maxResults = 100;

    /**
     * 默认返回结果数
     */
    private int defaultTopK = 10;

    /**
     * 是否启用高亮
     */
    private boolean highlightEnabled = true;

    /**
     * 高亮前缀
     */
    private String highlightPrefix = "<em>";

    /**
     * 高亮后缀
     */
    private String highlightSuffix = "</em>";

    /**
     * 最小相似度阈值
     */
    private float minScore = 0.0f;
}
