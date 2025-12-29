package top.yumbo.ai.omni.chunking;

/**
 * 分块策略枚举
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public enum ChunkingStrategy {

    /**
     * PPL 智能分块
     */
    PPL("PPL 智能分块"),

    /**
     * 固定长度分块
     */
    FIXED_LENGTH("固定长度分块"),

    /**
     * 语义分块
     */
    SEMANTIC("语义分块"),

    /**
     * 段落分块
     */
    PARAGRAPH("段落分块"),

    /**
     * 句子分块
     */
    SENTENCE("句子分块"),

    /**
     * Markdown 分块
     */
    MARKDOWN("Markdown 分块");

    private final String description;

    ChunkingStrategy(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }
}

