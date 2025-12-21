package top.yumbo.ai.omni.web.model;

import lombok.Data;
import java.util.Map;

/**
 * RAG处理策略模板
 *
 * 用于保存和复用文本提取+分块策略的组合
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
public class RAGStrategyTemplate {

    /**
     * 模板ID
     */
    private String templateId;

    /**
     * 模板名称（用户自定义）
     */
    private String templateName;

    /**
     * 模板描述
     */
    private String description;

    /**
     * 文本提取模型
     */
    private String textExtractionModel;

    /**
     * 分块策略
     */
    private String chunkingStrategy;

    /**
     * 分块参数
     */
    private Map<String, Object> chunkingParams;

    /**
     * 创建时间
     */
    private long createdAt;

    /**
     * 更新时间
     */
    private long updatedAt;

    /**
     * 是否为默认模板
     */
    private boolean isDefault;

    /**
     * 使用次数
     */
    private int useCount;
}

