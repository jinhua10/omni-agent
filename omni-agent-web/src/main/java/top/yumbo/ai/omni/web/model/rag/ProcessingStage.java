package top.yumbo.ai.omni.web.model.rag;

/**
 * 文档处理阶段枚举
 * (Document Processing Stage Enum)
 *
 * @author AI Reviewer Team
 * @since 2.0.0 (Phase 4)
 */
public enum ProcessingStage {
    /**
     * 上传阶段 (Upload stage)
     */
    UPLOAD("upload", "文档上传", "Document Upload"),

    /**
     * 文本提取阶段 (Text extraction stage)
     */
    EXTRACT("extract", "文本提取", "Text Extraction"),

    /**
     * 分块阶段 (Chunking stage)
     */
    CHUNK("chunk", "智能分块", "Smart Chunking"),

    /**
     * 向量化阶段 (Vectorization stage)
     */
    VECTORIZE("vectorize", "向量化", "Vectorization"),

    /**
     * 索引阶段 (Indexing stage)
     */
    INDEX("index", "索引存储", "Index Storage"),

    /**
     * 完成阶段 (Completed stage)
     */
    COMPLETED("completed", "处理完成", "Processing Completed");

    // 枚举成员变量
    private final String code;
    private final String nameCn;
    private final String nameEn;

    /**
     * 枚举构造方法
     * @param code 阶段编码
     * @param nameCn 中文名称
     * @param nameEn 英文名称
     */
    ProcessingStage(String code, String nameCn, String nameEn) {
        this.code = code;
        this.nameCn = nameCn;
        this.nameEn = nameEn;
    }

    /**
     * 获取阶段编码
     * @return 编码字符串
     */
    public String getCode() {
        return code;
    }

    /**
     * 获取中文名称
     * @return 中文名称
     */
    public String getNameCn() {
        return nameCn;
    }

    /**
     * 获取英文名称
     * @return 英文名称
     */
    public String getNameEn() {
        return nameEn;
    }

    /**
     * 获取本地化名称
     * (Get localized name)
     * @param lang 语言代码 (Language code: zh/en)
     * @return 本地化名称 (Localized name)
     */
    public String getName(String lang) {
        return "en".equalsIgnoreCase(lang) ? nameEn : nameCn;
    }
}