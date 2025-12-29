package top.yumbo.ai.omni.document.processor;

/**
 * 文档类型枚举
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public enum DocumentType {

    /**
     * PDF 文档
     */
    PDF("PDF", ".pdf"),

    /**
     * Word 文档
     */
    WORD("Word", ".doc", ".docx"),

    /**
     * Excel 表格
     */
    EXCEL("Excel", ".xls", ".xlsx"),

    /**
     * PowerPoint 演示文稿
     */
    PPT("PowerPoint", ".ppt", ".pptx"),

    /**
     * 纯文本
     */
    TEXT("Text", ".txt", ".md", ".log"),

    /**
     * HTML
     */
    HTML("HTML", ".html", ".htm"),

    /**
     * XML
     */
    XML("XML", ".xml"),

    /**
     * JSON
     */
    JSON("JSON", ".json"),

    /**
     * 未知类型
     */
    UNKNOWN("Unknown");

    private final String description;
    private final String[] extensions;

    DocumentType(String description, String... extensions) {
        this.description = description;
        this.extensions = extensions;
    }

    public String getDescription() {
        return description;
    }

    public String[] getExtensions() {
        return extensions;
    }

    /**
     * 根据文件扩展名获取文档类型
     *
     * @param extension 文件扩展名（如 .pdf）
     * @return 文档类型
     */
    public static DocumentType fromExtension(String extension) {
        if (extension == null || extension.isEmpty()) {
            return UNKNOWN;
        }

        String ext = extension.toLowerCase();
        if (!ext.startsWith(".")) {
            ext = "." + ext;
        }

        for (DocumentType type : values()) {
            for (String typeExt : type.extensions) {
                if (typeExt.equalsIgnoreCase(ext)) {
                    return type;
                }
            }
        }

        return UNKNOWN;
    }
}

