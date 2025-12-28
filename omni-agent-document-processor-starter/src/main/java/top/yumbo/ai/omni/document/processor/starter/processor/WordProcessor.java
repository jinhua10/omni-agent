package top.yumbo.ai.omni.document.processor.starter.processor;

import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hwpf.HWPFDocument;
import org.apache.poi.hwpf.model.PicturesTable;
import org.apache.poi.hwpf.usermodel.*;
import org.apache.poi.xwpf.usermodel.*;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.document.processor.AbstractDocumentProcessor;

import java.io.ByteArrayInputStream;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.*;

/**
 * Word 文档处理器
 * (Word Document Processor)
 *
 * <p>处理策略：</p>
 * <ul>
 *   <li>提取文本内容（段落、标题、列表等）</li>
 *   <li>提取内嵌图片的位置信息</li>
 *   <li>使用 Vision LLM 分析图片内容</li>
 *   <li>将图片描述嵌入到文本对应位置</li>
 * </ul>
 *
 * <p>配置说明：</p>
 * <ul>
 *   <li>默认启用（matchIfMissing = true），无需配置</li>
 *   <li>可通过 omni-agent.word.enabled=false 禁用</li>
 *   <li>未来可能添加更多配置项（如 max-images、extract-comments 等）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
@ConditionalOnProperty(
    prefix = "omni-agent.word",
    name = "enabled",
    havingValue = "true",
    matchIfMissing = true  // 默认启用，无需配置
)
public class WordProcessor extends AbstractDocumentProcessor {

    private static final Set<String> SUPPORTED_EXTENSIONS = Set.of("doc", "docx");

    @Override
    public boolean supports(String fileExtension) {
        return SUPPORTED_EXTENSIONS.contains(fileExtension.toLowerCase());
    }

    @Override
    public String getName() {
        return "WordProcessor";
    }

    @Override
    public int getPriority() {
        return 30; // 高优先级（与 ExcelProcessor 相同）
    }

    @Override
    protected ExtractedContent extractContent(ProcessingContext context) throws Exception {
        String ext = context.getFileExtension().toLowerCase();

        if ("docx".equals(ext)) {
            return extractDocxContent(context);
        } else {
            return extractDocContent(context);
        }
    }

    /**
     * 提取 .docx 文档内容（Word 2007+）
     */
    private ExtractedContent extractDocxContent(ProcessingContext context) throws Exception {
        InputStream inputStream;
        if (context.getFileBytes() != null) {
            inputStream = new ByteArrayInputStream(context.getFileBytes());
        } else {
            inputStream = new FileInputStream(context.getFilePath());
        }

        try (XWPFDocument document = new XWPFDocument(inputStream)) {
            ExtractedContent content = new ExtractedContent();
            content.getMetadata().put("format", "docx");
            content.getMetadata().put("totalParagraphs", document.getParagraphs().size());

            int position = 0;
            int imageCounter = 0;

            // 遍历文档中的所有内容（段落、表格、图片）
            for (IBodyElement element : document.getBodyElements()) {
                if (element instanceof XWPFParagraph) {
                    XWPFParagraph paragraph = (XWPFParagraph) element;

                    // 提取段落文本
                    String text = paragraph.getText();
                    if (text != null && !text.trim().isEmpty()) {
                        // 检查是否是标题
                        String style = paragraph.getStyle();
                        if (style != null && style.startsWith("Heading")) {
                            // ��换为 Markdown 标题
                            int level = extractHeadingLevel(style);
                            text = "#".repeat(level) + " " + text;
                        }
                        content.addTextBlock(text + "\n\n", position++);
                    }

                    // 提取段落中的图片
                    List<XWPFRun> runs = paragraph.getRuns();
                    for (XWPFRun run : runs) {
                        List<XWPFPicture> pictures = run.getEmbeddedPictures();
                        for (XWPFPicture picture : pictures) {
                            ExtractedImage image = extractXWPFPicture(picture, imageCounter++);
                            if (image != null) {
                                content.addImageBlock(image, position++);
                            }
                        }
                    }

                } else if (element instanceof XWPFTable) {
                    // 提取表格
                    XWPFTable table = (XWPFTable) element;
                    String tableText = extractTableAsMarkdown(table);
                    content.addTextBlock(tableText + "\n\n", position++);
                }
            }

            content.getMetadata().put("totalImages", imageCounter);
            return content;
        }
    }

    /**
     * 提取 .doc 文档内容（Word 97-2003）
     */
    private ExtractedContent extractDocContent(ProcessingContext context) throws Exception {
        InputStream inputStream;
        if (context.getFileBytes() != null) {
            inputStream = new ByteArrayInputStream(context.getFileBytes());
        } else {
            inputStream = new FileInputStream(context.getFilePath());
        }

        try (HWPFDocument document = new HWPFDocument(inputStream)) {
            ExtractedContent content = new ExtractedContent();
            content.getMetadata().put("format", "doc");

            int position = 0;
            int imageCounter = 0;

            // 提取文本内容
            Range range = document.getRange();
            for (int i = 0; i < range.numParagraphs(); i++) {
                Paragraph paragraph = range.getParagraph(i);
                String text = paragraph.text();

                if (text != null && !text.trim().isEmpty()) {
                    content.addTextBlock(text + "\n\n", position++);
                }
            }

            // 提取图片
            PicturesTable picturesTable = document.getPicturesTable();
            List<Picture> pictures = picturesTable.getAllPictures();

            for (Picture picture : pictures) {
                ExtractedImage image = extractHWPFPicture(picture, imageCounter++);
                if (image != null) {
                    content.addImageBlock(image, position++);
                }
            }

            content.getMetadata().put("totalImages", imageCounter);
            return content;
        }
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
                    .data(pictureData.getData())
                    .format(extractFormat(pictureData.getFileName()))
                    .pageNumber(imageIndex + 1) // Word 没有页码概念，使用图片序号
                    .metadata(metadata)
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

            String format = "png"; // 默认格式
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
                    .data(picture.getContent())
                    .format(format)
                    .pageNumber(imageIndex + 1)
                    .metadata(metadata)
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
    public ValidationResult validate(ProcessingContext context) {
        if (context.getFileSize() > 100 * 1024 * 1024) {
            return ValidationResult.builder()
                    .valid(false)
                    .message("Word 文件过大（超过100MB）")
                    .build();
        }

        return ValidationResult.builder()
                .valid(true)
                .message("验证通过")
                .build();
    }
}


