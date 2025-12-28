package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hwpf.HWPFDocument;
import org.apache.poi.hwpf.usermodel.Paragraph;
import org.apache.poi.hwpf.usermodel.Picture;
import org.apache.poi.hwpf.usermodel.Range;
import org.apache.poi.xwpf.usermodel.*;
import top.yumbo.ai.omni.document.processor.*;
import top.yumbo.ai.omni.document.processor.starter.config.DocumentProcessorProperties;

import java.io.InputStream;
import java.util.*;

/**
 * Word 文档处理器（增强版）
 *
 * <p>支持 .doc 和 .docx 格式</p>
 * <p>功能：表格转 Markdown、标题识别、结构化处理</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class WordProcessor implements DocumentProcessor {

    private final DocumentProcessorProperties properties;

    public WordProcessor(DocumentProcessorProperties properties) {
        this.properties = properties;
    }

    @Override
    public ProcessedDocument process(String documentId, InputStream input) throws ProcessorException {
        try {
            log.debug("📄 开始处理 Word 文档: {}", documentId);

            String extension = getExtension(documentId);
            String text;
            Map<String, Object> metadata = new HashMap<>();

            if (".docx".equalsIgnoreCase(extension)) {
                // 处理 .docx (Word 2007+)
                text = processDocx(input, metadata);
            } else {
                // 处理 .doc (Word 97-2003)
                text = processDoc(input, metadata);
            }

            log.info("✅ Word 处理完成: {} ({} 字符)", documentId, text.length());

            return ProcessedDocument.builder()
                    .documentId(documentId)
                    .documentType(DocumentType.WORD)
                    .text(text)
                    .characterCount(text.length())
                    .metadata(metadata)
                    .success(true)
                    .build();

        } catch (Exception e) {
            log.error("❌ Word 处理失败: {}", documentId, e);
            throw new ProcessorException("Word 处理失败: " + e.getMessage(), e);
        }
    }

    /**
     * 处理 .docx 文档（Word 2007+）
     */
    private String processDocx(InputStream input, Map<String, Object> metadata) throws Exception {
        try (XWPFDocument document = new XWPFDocument(input)) {
            StringBuilder text = new StringBuilder();
            metadata.put("format", "docx");
            metadata.put("totalParagraphs", document.getParagraphs().size());

            // 遍历文档中的所有内容（段落、表格）
            for (IBodyElement element : document.getBodyElements()) {
                if (element instanceof XWPFParagraph) {
                    XWPFParagraph paragraph = (XWPFParagraph) element;
                    String paragraphText = processParagraph(paragraph);
                    if (!paragraphText.isEmpty()) {
                        text.append(paragraphText).append("\n\n");
                    }

                } else if (element instanceof XWPFTable) {
                    // 提取表格并转换为 Markdown
                    XWPFTable table = (XWPFTable) element;
                    String tableMarkdown = extractTableAsMarkdown(table);
                    if (!tableMarkdown.isEmpty()) {
                        text.append(tableMarkdown).append("\n\n");
                    }
                }
            }

            return text.toString();
        }
    }

    /**
     * 处理段落，识别标题
     */
    private String processParagraph(XWPFParagraph paragraph) {
        String text = paragraph.getText();
        if (text == null || text.trim().isEmpty()) {
            return "";
        }

        // 检查是否是标题
        String style = paragraph.getStyle();
        if (style != null && style.startsWith("Heading")) {
            // 转换为 Markdown 标题
            int level = extractHeadingLevel(style);
            return "#".repeat(level) + " " + text.trim();
        }

        return text.trim();
    }

    /**
     * 提取标题级别
     */
    private int extractHeadingLevel(String style) {
        if (style == null) {
            return 1;
        }

        // 标题样式通常是 "Heading1", "Heading2" 等
        if (style.matches("Heading\\d+")) {
            try {
                return Integer.parseInt(style.substring(7));
            } catch (NumberFormatException e) {
                return 1;
            }
        }

        return 1;
    }

    /**
     * 将表格转换为 Markdown 格式
     */
    private String extractTableAsMarkdown(XWPFTable table) {
        StringBuilder md = new StringBuilder();
        List<XWPFTableRow> rows = table.getRows();

        if (rows.isEmpty()) {
            return "";
        }

        // 表头（第一行）
        XWPFTableRow headerRow = rows.get(0);
        md.append("| ");
        for (XWPFTableCell cell : headerRow.getTableCells()) {
            md.append(escapeMarkdown(cell.getText())).append(" | ");
        }
        md.append("\n");

        // 分隔线
        md.append("|");
        for (int i = 0; i < headerRow.getTableCells().size(); i++) {
            md.append(" --- |");
        }
        md.append("\n");

        // 数据行
        for (int i = 1; i < rows.size(); i++) {
            XWPFTableRow row = rows.get(i);
            md.append("| ");
            for (XWPFTableCell cell : row.getTableCells()) {
                md.append(escapeMarkdown(cell.getText())).append(" | ");
            }
            md.append("\n");
        }

        return md.toString();
    }

    /**
     * 处理 .doc 文档（Word 97-2003）
     */
    private String processDoc(InputStream input, Map<String, Object> metadata) throws Exception {
        try (HWPFDocument document = new HWPFDocument(input)) {
            StringBuilder text = new StringBuilder();
            metadata.put("format", "doc");

            // 提取文本内容
            Range range = document.getRange();
            for (int i = 0; i < range.numParagraphs(); i++) {
                Paragraph paragraph = range.getParagraph(i);
                String paragraphText = paragraph.text();

                if (paragraphText != null && !paragraphText.trim().isEmpty()) {
                    text.append(paragraphText.trim()).append("\n\n");
                }
            }

            return text.toString();
        }
    }

    /**
     * 转义 Markdown 特殊字符
     */
    private String escapeMarkdown(String text) {
        if (text == null || text.isEmpty()) {
            return "";
        }
        return text.replace("|", "\\|")
                   .replace("\n", "<br>")
                   .replace("\r", "")
                   .trim();
    }

    @Override
    public List<DocumentType> getSupportedTypes() {
        return Arrays.asList(DocumentType.WORD);
    }

    @Override
    public boolean supports(DocumentType type) {
        return type == DocumentType.WORD;
    }

    @Override
    public boolean supportsExtension(String extension) {
        return ".doc".equalsIgnoreCase(extension) ||
               ".docx".equalsIgnoreCase(extension);
    }

    private String getExtension(String filename) {
        int lastDot = filename.lastIndexOf('.');
        return lastDot > 0 ? filename.substring(lastDot) : "";
    }

    /**
     * 提取 XWPF 图片（.docx）
     */
    private ExtractedImage extractXWPFPicture(XWPFPicture picture, int imageIndex) {
        try {
            XWPFPictureData pictureData = picture.getPictureData();

            Map<String, Object> metadata = new HashMap<>();
            metadata.put("fileName", pictureData.getFileName());
            metadata.put("imageIndex", imageIndex);
            metadata.put("documentType", "Word");

            return ExtractedImage.builder()
                    .imageId(UUID.randomUUID().toString())
                    .data(pictureData.getData())
                    .format(extractFormat(pictureData.getFileName()))
                    .pageNumber(imageIndex + 1)
                    .metadata(metadata)
                    .createdAt(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.warn("提取 Word 图片失败", e);
            return null;
        }
    }

    /**
     * 提取 HWPF 图片（.doc）
     */
    private ExtractedImage extractHWPFPicture(Picture picture, int imageIndex) {
        try {
            Map<String, Object> metadata = new HashMap<>();
            metadata.put("imageIndex", imageIndex);
            metadata.put("documentType", "Word");
            metadata.put("mimeType", picture.getMimeType());

            String format = "png";
            if (picture.getMimeType() != null) {
                if (picture.getMimeType().contains("jpeg") || picture.getMimeType().contains("jpg")) {
                    format = "jpg";
                } else if (picture.getMimeType().contains("png")) {
                    format = "png";
                } else if (picture.getMimeType().contains("gif")) {
                    format = "gif";
                }
            }

            return ExtractedImage.builder()
                    .imageId(UUID.randomUUID().toString())
                    .data(picture.getContent())
                    .format(format)
                    .pageNumber(imageIndex + 1)
                    .metadata(metadata)
                    .createdAt(System.currentTimeMillis())
                    .build();
        } catch (Exception e) {
            log.warn("提取 Word 图片失败", e);
            return null;
        }
    }

    /**
     * 从文件名提取格式
     */
    private String extractFormat(String fileName) {
        if (fileName == null || !fileName.contains(".")) {
            return "png";
        }
        String ext = fileName.substring(fileName.lastIndexOf(".") + 1).toLowerCase();
        return ext.isEmpty() ? "png" : ext;
    }
}

