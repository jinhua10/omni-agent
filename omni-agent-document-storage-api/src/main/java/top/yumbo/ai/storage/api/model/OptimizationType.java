package top.yumbo.ai.storage.api.model;

/**
 * RAG优化算法类型枚举
 * (RAG Optimization Algorithm Type Enum)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public enum OptimizationType {

    /**
     * PPL - Prompt Programming Language
     * 提示词程序化编程，提供结构化的提示词生成
     */
    PPL("ppl", "Prompt Programming Language", "提示词编程"),

    /**
     * HyDE - Hypothetical Document Embeddings
     * 生成假设性文档进行检索
     */
    HYDE("hyde", "Hypothetical Document Embeddings", "假设性文档嵌入"),

    /**
     * Rerank - 语义重排序
     * 使用更强大的模型重新计算相关度
     */
    RERANK("rerank", "Semantic Reranking", "语义重排序"),

    /**
     * Query Expansion - 查询扩展
     * 生成多个查询变体提高召回率
     */
    QUERY_EXPANSION("query_expansion", "Query Expansion", "查询扩展"),

    /**
     * Query Rewrite - 查询改写
     * 优化查询表达提高检索精度
     */
    QUERY_REWRITE("query_rewrite", "Query Rewriting", "查询改写"),

    /**
     * Metadata Filter - 元数据过滤
     * 基于元数据的智能过滤
     */
    METADATA_FILTER("metadata_filter", "Metadata Filtering", "元数据过滤"),

    /**
     * Context Compression - 上下文压缩
     * 压缩长文本提取关键信息
     */
    CONTEXT_COMPRESSION("context_compression", "Context Compression", "上下文压缩"),

    /**
     * Semantic Chunking - 语义分块
     * 基于语义的智能文档分块
     */
    SEMANTIC_CHUNKING("semantic_chunking", "Semantic Chunking", "语义分块"),

    /**
     * Hybrid Search - 混合检索
     * 向量检索 + 关键词检索
     */
    HYBRID_SEARCH("hybrid_search", "Hybrid Search", "混合检索"),

    /**
     * Knowledge Graph - 知识图谱增强
     * 使用知识图谱扩展查询
     */
    KNOWLEDGE_GRAPH("knowledge_graph", "Knowledge Graph Enhancement", "知识图谱增强"),

    /**
     * HOPE Routing - HOPE三层路由
     * 基于HOPE系统的智能知识路由
     */
    HOPE_ROUTING("hope_routing", "HOPE Intelligent Routing", "HOPE智能路由"),

    /**
     * Behavior Analysis - 行为分析增强
     * 基于用户行为优化检索
     */
    BEHAVIOR_ANALYSIS("behavior_analysis", "Behavior Analysis Enhancement", "行为分析增强"),

    /**
     * Multi-Model Voting - 多模型投票
     * 多个模型生成答案并投票
     */
    MULTI_MODEL_VOTING("multi_model_voting", "Multi-Model Voting", "多模型投票"),

    /**
     * Custom - 自定义算法
     * 用户自定义的优化算法
     */
    CUSTOM("custom", "Custom Algorithm", "自定义算法");

    private final String code;
    private final String nameEn;
    private final String nameZh;

    OptimizationType(String code, String nameEn, String nameZh) {
        this.code = code;
        this.nameEn = nameEn;
        this.nameZh = nameZh;
    }

    public String getCode() {
        return code;
    }

    public String getNameEn() {
        return nameEn;
    }

    public String getNameZh() {
        return nameZh;
    }

    /**
     * 根据代码获取枚举
     */
    public static OptimizationType fromCode(String code) {
        for (OptimizationType type : values()) {
            if (type.code.equalsIgnoreCase(code)) {
                return type;
            }
        }
        return CUSTOM;
    }

    /**
     * 判断是否为有效的优化类型
     */
    public static boolean isValid(String code) {
        for (OptimizationType type : values()) {
            if (type.code.equalsIgnoreCase(code)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        return code;
    }
}

