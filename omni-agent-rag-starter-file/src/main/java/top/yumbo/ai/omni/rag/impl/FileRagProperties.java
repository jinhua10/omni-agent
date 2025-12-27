package top.yumbo.ai.omni.rag.impl;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

/**
 * File RAG 配置属性
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Data
@Component
@ConfigurationProperties(prefix = "omni.rag.file")
public class FileRagProperties {

    /**
     * 是否启用
     */
    private boolean enabled = true;

    /**
     * 索引根目录
     */
    private String indexPath = "data/rag/lucene";

    /**
     * 默认域ID
     */
    private String defaultDomainId = "default";
}

