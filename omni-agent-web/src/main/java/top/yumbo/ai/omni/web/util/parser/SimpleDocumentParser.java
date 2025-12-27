package top.yumbo.ai.omni.web.util.parser;

import lombok.extern.slf4j.Slf4j;
import org.apache.poi.hslf.usermodel.*;
import org.apache.poi.hwpf.HWPFDocument;
import org.apache.poi.hwpf.extractor.WordExtractor;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xslf.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.apache.poi.xwpf.usermodel.*;
import top.yumbo.ai.omni.web.util.DocumentParser;
import top.yumbo.ai.omni.web.util.parser.image.SmartImageExtractor;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.util.*;

/**
 * 简单文档解析器实现
 * (Simple Document Parser Implementation)
 *
 * <p>支持常见文档格式的解析：txt, md, docx, pptx 等</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class SimpleDocumentParser implements DocumentParser {

    // 图片提取器
    private final SmartImageExtractor imageExtractor;
    private final boolean extractImages; // 是否启用图片提取

    // 支持的MIME类型
    private static final Set<String> SUPPORTED_MIME_TYPES = new HashSet<>(Arrays.asList(
            // 文本
            "text/plain",
            "text/markdown",
            "text/html",
            "text/xml",
            "application/json",
            "text/csv",

            // Office 文档
            "application/vnd.openxmlformats-officedocument.wordprocessingml.document", // docx
            "application/vnd.openxmlformats-officedocument.presentationml.presentation" // pptx
    ));

    // 支持的文件扩展名
    private static final Set<String> SUPPORTED_EXTENSIONS = new HashSet<>(Arrays.asList(
            ".txt", ".md", ".markdown", ".html", ".xml", ".json", ".csv", ".log",
            ".docx", ".doc",   // Word: 新版 + 旧版
            ".pptx", ".ppt",   // PowerPoint: 新版 + 旧版
            ".xlsx", ".xls"    // Excel: 新版 + 旧版
    ));

    /**
     * 默认构造函数（不启用图片提取）
     */
    public SimpleDocumentParser() {
        this(false);
    }

    /**
     * 构造函数
     *
     * @param extractImages 是否启用图片提取
     */
    public SimpleDocumentParser(boolean extractImages) {
        this.extractImages = extractImages;
        this.imageExtractor = extractImages ? new SmartImageExtractor() : null;

        if (extractImages) {
            log.info("图片提取功能已启用，使用策略: {}",
                    imageExtractor.getActiveStrategy().getStrategyName());
        }
    }

    /**
     * 构造函数（自定义图片提取器）
     *
     * @param imageExtractor 自定义的图片提取器
     */
    public SimpleDocumentParser(SmartImageExtractor imageExtractor) {
        this.extractImages = imageExtractor != null;
        this.imageExtractor = imageExtractor;

        if (extractImages) {
            log.info("使用自定义图片提取器，策略: {}",
                    imageExtractor.getActiveStrategy().getStrategyName());
        }
    }

    @Override
    public String parse(File file) {
        try {
            String filename = file.getName();
            String extension = getFileExtension(filename);

            log.debug("开始解析文件: {}, 扩展名: {}", filename, extension);

            // Word 文档
            if (extension.equals(".docx")) {
                return parseDocx(file);
            } else if (extension.equals(".doc")) {
                return parseDoc(file);  // 旧版 Word
            }

            // PowerPoint 文档
            else if (extension.equals(".pptx")) {
                return parsePptx(file);
            } else if (extension.equals(".ppt")) {
                return parsePpt(file);  // 旧版 PowerPoint
            }

            // Excel 文档
            else if (extension.equals(".xlsx")) {
                return parseXlsx(file);
            } else if (extension.equals(".xls")) {
                return parseXls(file);  // 旧版 Excel
            }

            // 默认作为文本文件处理
            else {
                return parseTextFile(file);
            }
        } catch (Exception e) {
            log.error("解析文件失败: {}", file.getAbsolutePath(), e);
            return "";
        }
    }

    @Override
    public String parse(byte[] bytes, String mimeType) {
        try {
            log.debug("解析字节数组: mimeType={}, size={} bytes", mimeType, bytes.length);

            if (mimeType == null) {
                // 尝试作为UTF-8文本解析
                return new String(bytes, StandardCharsets.UTF_8);
            }

            if (mimeType.contains("wordprocessingml")) {
                return parseDocxBytes(bytes);
            } else if (mimeType.contains("presentationml")) {
                return parsePptxBytes(bytes);
            } else {
                // 尝试多种编码
                return parseTextBytes(bytes);
            }
        } catch (Exception e) {
            log.error("解析字节数组失败: mimeType={}", mimeType, e);
            return "";
        }
    }

    @Override
    public boolean supports(String mimeType) {
        if (mimeType == null) {
            return false;
        }
        return SUPPORTED_MIME_TYPES.contains(mimeType) ||
               mimeType.startsWith("text/");
    }

    @Override
    public boolean supportsExtension(String extension) {
        if (extension == null) {
            return false;
        }
        String ext = extension.toLowerCase();
        if (!ext.startsWith(".")) {
            ext = "." + ext;
        }
        return SUPPORTED_EXTENSIONS.contains(ext);
    }

    // ========== 私有解析方法 ==========

    /**
     * 解析文本文件
     */
    private String parseTextFile(File file) throws IOException {
        try {
            // 尝试 UTF-8
            String content = new String(Files.readAllBytes(file.toPath()), StandardCharsets.UTF_8);
            log.debug("成功解析文本文件 (UTF-8): {} bytes", content.length());
            return content;
        } catch (Exception e) {
            // 尝试 GBK
            try {
                String content = new String(Files.readAllBytes(file.toPath()), "GBK");
                log.debug("成功解析文本文件 (GBK): {} bytes", content.length());
                return content;
            } catch (Exception ex) {
                log.warn("无法解析文本文件: {}", file.getName());
                return "";
            }
        }
    }

    /**
     * 解析文本字节数组
     */
    private String parseTextBytes(byte[] bytes) {
        try {
            return new String(bytes, StandardCharsets.UTF_8);
        } catch (Exception e) {
            try {
                return new String(bytes, "GBK");
            } catch (Exception ex) {
                log.warn("无法解析文本字节数组");
                return "";
            }
        }
    }

    /**
     * 解析 Word 文档 (.docx)
     */
    private String parseDocx(File file) throws IOException {
        try (FileInputStream fis = new FileInputStream(file);
             XWPFDocument document = new XWPFDocument(fis)) {

            StringBuilder content = new StringBuilder();
            List<XWPFParagraph> paragraphs = document.getParagraphs();

            // 提取文本
            for (XWPFParagraph paragraph : paragraphs) {
                String text = paragraph.getText();
                if (text != null && !text.trim().isEmpty()) {
                    content.append(text).append("\n");
                }
            }

            // 提取图片（如果启用）
            if (extractImages && imageExtractor != null) {
                List<XWPFPictureData> pictures = document.getAllPictures();
                log.debug("Word 文档中包含 {} 张图片", pictures.size());

                for (int i = 0; i < pictures.size(); i++) {
                    try {
                        XWPFPictureData picture = pictures.get(i);
                        byte[] imageBytes = picture.getData();
                        String imageName = String.format("docx_image%d.%s",
                                i + 1, picture.suggestFileExtension());

                        ByteArrayInputStream imageStream = new ByteArrayInputStream(imageBytes);
                        String imageContent = imageExtractor.extractContent(imageStream, imageName);
                        content.append("\n").append(imageContent).append("\n");

                        log.debug("提取了 Word 文档中的图片: {}", imageName);
                    } catch (Exception e) {
                        log.warn("提取 Word 文档中的图片失败", e);
                    }
                }
            }

            String result = content.toString().trim();
            log.debug("成功解析 DOCX 文件: {} bytes", result.length());
            return result;
        } catch (Exception e) {
            log.error("解析 DOCX 文件失败: {}", file.getName(), e);
            throw new IOException("解析 DOCX 文件失败", e);
        }
    }

    /**
     * 解析旧版 Word 文档 (.doc)
     */
    private String parseDoc(File file) throws IOException {
        try (FileInputStream fis = new FileInputStream(file);
             HWPFDocument document = new HWPFDocument(fis)) {

            WordExtractor extractor = new WordExtractor(document);
            StringBuilder content = new StringBuilder(extractor.getText());

            // ⭐ 提取图片（如果启用）
            if (extractImages && imageExtractor != null) {
                try {
                    List<org.apache.poi.hwpf.usermodel.Picture> pictures = document.getPicturesTable().getAllPictures();
                    log.debug("旧版 Word 文档中包含 {} 张图片", pictures.size());

                    for (int i = 0; i < pictures.size(); i++) {
                        try {
                            org.apache.poi.hwpf.usermodel.Picture picture = pictures.get(i);
                            byte[] imageBytes = picture.getContent();

                            // 获取图片扩展名
                            String extension = picture.suggestFileExtension();
                            String imageName = String.format("doc_image%d.%s", i + 1, extension);

                            ByteArrayInputStream imageStream = new ByteArrayInputStream(imageBytes);
                            String imageContent = imageExtractor.extractContent(imageStream, imageName);
                            content.append("\n").append(imageContent).append("\n");

                            log.debug("提取了旧版 Word 文档中的图片: {}", imageName);
                        } catch (Exception e) {
                            log.warn("提取旧版 Word 文档中的第 {} 张图片失败", i + 1, e);
                        }
                    }
                } catch (Exception e) {
                    log.warn("获取旧版 Word 文档图片列表失败: {}", e.getMessage());
                }
            }

            String result = content.toString().trim();
            log.debug("成功解析 DOC 文件: {} bytes", result.length());
            return result;
        } catch (Exception e) {
            log.error("解析 DOC 文件失败: {}", file.getName(), e);
            throw new IOException("解析 DOC 文件失败", e);
        }
    }

    /**
     * 解析 PowerPoint 文档 (.pptx)
     */
    private String parsePptx(File file) throws IOException {
        try (FileInputStream fis = new FileInputStream(file);
             XMLSlideShow ppt = new XMLSlideShow(fis)) {

            StringBuilder content = new StringBuilder();
            List<XSLFSlide> slides = ppt.getSlides();

            for (int i = 0; i < slides.size(); i++) {
                XSLFSlide slide = slides.get(i);
                content.append("=== 幻灯片 ").append(i + 1).append(" ===\n");

                // 提取文本
                slide.getShapes().forEach(shape -> {
                    if (shape instanceof XSLFTextShape) {
                        XSLFTextShape textShape = (XSLFTextShape) shape;
                        String text = textShape.getText();
                        if (text != null && !text.trim().isEmpty()) {
                            content.append(text).append("\n");
                        }
                    }
                });

                // 提取图片（如果启用）
                if (extractImages && imageExtractor != null) {
                    int imageCount = 0;
                    for (XSLFShape shape : slide.getShapes()) {
                        if (shape instanceof XSLFPictureShape) {
                            XSLFPictureShape picture = (XSLFPictureShape) shape;
                            try {
                                XSLFPictureData pictureData = picture.getPictureData();
                                byte[] imageBytes = pictureData.getData();
                                String imageName = String.format("slide%d_image%d.%s",
                                        i + 1, ++imageCount, pictureData.getType().extension);

                                ByteArrayInputStream imageStream = new ByteArrayInputStream(imageBytes);
                                String imageContent = imageExtractor.extractContent(imageStream, imageName);
                                content.append(imageContent).append("\n");

                                log.debug("提取了幻灯片 {} 中的图片: {}", i + 1, imageName);
                            } catch (Exception e) {
                                log.warn("提取幻灯片 {} 中的图片失败", i + 1, e);
                            }
                        }
                    }
                }

                content.append("\n");
            }

            String result = content.toString().trim();
            log.debug("成功解析 PPTX 文件: {} slides, {} bytes", slides.size(), result.length());
            return result;
        } catch (Exception e) {
            log.error("解析 PPTX 文件失败: {}", file.getName(), e);
            throw new IOException("解析 PPTX 文件失败", e);
        }
    }

    /**
     * 解析旧版 PowerPoint 文档 (.ppt)
     */
    private String parsePpt(File file) throws IOException {
        try (FileInputStream fis = new FileInputStream(file);
             HSLFSlideShow ppt = new HSLFSlideShow(fis)) {

            StringBuilder content = new StringBuilder();
            List<HSLFSlide> slides = ppt.getSlides();

            for (int i = 0; i < slides.size(); i++) {
                HSLFSlide slide = slides.get(i);
                content.append("=== 幻灯片 ").append(i + 1).append(" ===\n");

                // 提取文本
                slide.getShapes().forEach(shape -> {
                    if (shape instanceof HSLFTextShape) {
                        HSLFTextShape textShape = (HSLFTextShape) shape;
                        String text = textShape.getText();
                        if (text != null && !text.trim().isEmpty()) {
                            content.append(text).append("\n");
                        }
                    }
                });

                // ⭐ 提取图片（如果启用）
                if (extractImages && imageExtractor != null) {
                    int imageCount = 0;
                    for (HSLFShape shape : slide.getShapes()) {
                        if (shape instanceof org.apache.poi.hslf.usermodel.HSLFPictureShape) {
                            org.apache.poi.hslf.usermodel.HSLFPictureShape picture =
                                (org.apache.poi.hslf.usermodel.HSLFPictureShape) shape;
                            try {
                                org.apache.poi.hslf.usermodel.HSLFPictureData pictureData = picture.getPictureData();
                                byte[] imageBytes = pictureData.getData();

                                // 获取图片格式
                                String extension = "png";
                                if (pictureData.getType() == org.apache.poi.hslf.usermodel.HSLFPictureData.PictureType.JPEG) {
                                    extension = "jpg";
                                } else if (pictureData.getType() == org.apache.poi.hslf.usermodel.HSLFPictureData.PictureType.PNG) {
                                    extension = "png";
                                }

                                String imageName = String.format("slide%d_image%d.%s",
                                        i + 1, ++imageCount, extension);

                                ByteArrayInputStream imageStream = new ByteArrayInputStream(imageBytes);
                                String imageContent = imageExtractor.extractContent(imageStream, imageName);
                                content.append(imageContent).append("\n");

                                log.debug("提取了旧版幻灯片 {} 中的图片: {}", i + 1, imageName);
                            } catch (Exception e) {
                                log.warn("提取旧版幻灯片 {} 中的图片失败", i + 1, e);
                            }
                        }
                    }
                }

                content.append("\n");
            }

            String result = content.toString().trim();
            log.debug("成功解析 PPT 文件: {} slides, {} bytes", slides.size(), result.length());
            return result;
        } catch (Exception e) {
            log.error("解析 PPT 文件失败: {}", file.getName(), e);
            throw new IOException("解析 PPT 文件失败", e);
        }
    }

    /**
     * 解析新版 Excel 文档 (.xlsx)
     */
    private String parseXlsx(File file) throws IOException {
        try (FileInputStream fis = new FileInputStream(file);
             Workbook workbook = new XSSFWorkbook(fis)) {

            return parseExcelWorkbook(workbook, file.getName());
        } catch (Exception e) {
            log.error("解析 XLSX 文件失败: {}", file.getName(), e);
            throw new IOException("解析 XLSX 文件失败", e);
        }
    }

    /**
     * 解析旧版 Excel 文档 (.xls)
     */
    private String parseXls(File file) throws IOException {
        try (FileInputStream fis = new FileInputStream(file);
             Workbook workbook = new HSSFWorkbook(fis)) {

            return parseExcelWorkbook(workbook, file.getName());
        } catch (Exception e) {
            log.error("解析 XLS 文件失败: {}", file.getName(), e);
            throw new IOException("解析 XLS 文件失败", e);
        }
    }

    /**
     * 解析 Excel Workbook（通用方法，支持新旧版本）
     * ⭐ 支持图片提取，将图片描述添加到表格末尾
     */
    private String parseExcelWorkbook(Workbook workbook, String filename) {
        StringBuilder content = new StringBuilder();
        int sheetCount = workbook.getNumberOfSheets();

        for (int i = 0; i < sheetCount; i++) {
            Sheet sheet = workbook.getSheetAt(i);
            content.append("=== 工作表: ").append(sheet.getSheetName()).append(" ===\n");

            // 提取表格数据
            for (Row row : sheet) {
                boolean hasContent = false;
                StringBuilder rowContent = new StringBuilder();

                for (Cell cell : row) {
                    String cellValue = getCellValueAsString(cell);
                    if (cellValue != null && !cellValue.trim().isEmpty()) {
                        if (hasContent) {
                            rowContent.append("\t");
                        }
                        rowContent.append(cellValue);
                        hasContent = true;
                    }
                }

                if (hasContent) {
                    content.append(rowContent).append("\n");
                }
            }

            // ⭐ 提取图片（如果启用）
            if (extractImages && imageExtractor != null) {
                List<String> imageDescriptions = extractExcelImages(workbook, sheet, i);
                if (!imageDescriptions.isEmpty()) {
                    content.append("\n--- 图片内容 ---\n");
                    for (String desc : imageDescriptions) {
                        content.append(desc).append("\n");
                    }
                }
            }

            content.append("\n");
        }

        String result = content.toString().trim();
        log.debug("成功解析 Excel 文件 {}: {} sheets, {} bytes", filename, sheetCount, result.length());
        return result;
    }

    /**
     * 提取 Excel 工作表中的图片
     * ⭐ 支持新旧版本 Excel
     *
     * @param workbook Excel workbook
     * @param sheet 当前工作表
     * @param sheetIndex 工作表索引
     * @return 图片描述列表
     */
    private List<String> extractExcelImages(Workbook workbook, Sheet sheet, int sheetIndex) {
        List<String> imageDescriptions = new ArrayList<>();
        int imageCount = 0;

        try {
            // 新版 Excel (.xlsx)
            if (workbook instanceof XSSFWorkbook) {
                org.apache.poi.xssf.usermodel.XSSFDrawing drawing =
                    ((org.apache.poi.xssf.usermodel.XSSFSheet) sheet).getDrawingPatriarch();
                if (drawing != null) {
                    for (org.apache.poi.xssf.usermodel.XSSFShape shape : drawing.getShapes()) {
                        if (shape instanceof org.apache.poi.xssf.usermodel.XSSFPicture) {
                            org.apache.poi.xssf.usermodel.XSSFPicture picture =
                                (org.apache.poi.xssf.usermodel.XSSFPicture) shape;

                            try {
                                org.apache.poi.xssf.usermodel.XSSFPictureData pictureData = picture.getPictureData();
                                byte[] imageBytes = pictureData.getData();

                                String extension = pictureData.suggestFileExtension();
                                String imageName = String.format("sheet%d_image%d.%s",
                                        sheetIndex + 1, ++imageCount, extension);

                                ByteArrayInputStream imageStream = new ByteArrayInputStream(imageBytes);
                                String imageContent = imageExtractor.extractContent(imageStream, imageName);

                                // 获取图片所在位置（锚点）
                                org.apache.poi.xssf.usermodel.XSSFClientAnchor anchor = picture.getClientAnchor();
                                String location = String.format("位置: 第%d行, 第%d列",
                                        anchor.getRow1() + 1, anchor.getCol1() + 1);

                                imageDescriptions.add(String.format("[图片 %d] %s\n%s",
                                        imageCount, location, imageContent));

                                log.debug("提取了 Excel 工作表 {} 中的图片: {} ({})",
                                        sheet.getSheetName(), imageName, location);
                            } catch (Exception e) {
                                log.warn("提取 Excel 工作表 {} 中的图片 {} 失败",
                                        sheet.getSheetName(), imageCount, e);
                            }
                        }
                    }
                }
            }
            // 旧版 Excel (.xls)
            else if (workbook instanceof HSSFWorkbook) {
                org.apache.poi.hssf.usermodel.HSSFPatriarch patriarch =
                    ((org.apache.poi.hssf.usermodel.HSSFSheet) sheet).getDrawingPatriarch();
                if (patriarch != null) {
                    for (org.apache.poi.hssf.usermodel.HSSFShape shape : patriarch.getChildren()) {
                        if (shape instanceof org.apache.poi.hssf.usermodel.HSSFPicture) {
                            org.apache.poi.hssf.usermodel.HSSFPicture picture =
                                (org.apache.poi.hssf.usermodel.HSSFPicture) shape;

                            try {
                                org.apache.poi.hssf.usermodel.HSSFPictureData pictureData = picture.getPictureData();
                                byte[] imageBytes = pictureData.getData();

                                String extension = pictureData.suggestFileExtension();
                                String imageName = String.format("sheet%d_image%d.%s",
                                        sheetIndex + 1, ++imageCount, extension);

                                ByteArrayInputStream imageStream = new ByteArrayInputStream(imageBytes);
                                String imageContent = imageExtractor.extractContent(imageStream, imageName);

                                // 获取图片所在位置（锚点）
                                org.apache.poi.hssf.usermodel.HSSFClientAnchor anchor = picture.getClientAnchor();
                                String location = String.format("位置: 第%d行, 第%d列",
                                        anchor.getRow1() + 1, anchor.getCol1() + 1);

                                imageDescriptions.add(String.format("[图片 %d] %s\n%s",
                                        imageCount, location, imageContent));

                                log.debug("提取了旧版 Excel 工作表 {} 中的图片: {} ({})",
                                        sheet.getSheetName(), imageName, location);
                            } catch (Exception e) {
                                log.warn("提取旧版 Excel 工作表 {} 中的图片 {} 失败",
                                        sheet.getSheetName(), imageCount, e);
                            }
                        }
                    }
                }
            }

            if (imageCount > 0) {
                log.info("从 Excel 工作表 {} 提取了 {} 张图片", sheet.getSheetName(), imageCount);
            }
        } catch (Exception e) {
            log.error("提取 Excel 工作表 {} 图片失败: {}", sheet.getSheetName(), e.getMessage());
        }

        return imageDescriptions;
    }

    /**
     * 获取单元格值作为字符串
     */
    private String getCellValueAsString(Cell cell) {
        if (cell == null) {
            return "";
        }

        switch (cell.getCellType()) {
            case STRING:
                return cell.getStringCellValue();
            case NUMERIC:
                if (org.apache.poi.ss.usermodel.DateUtil.isCellDateFormatted(cell)) {
                    return cell.getDateCellValue().toString();
                } else {
                    return String.valueOf(cell.getNumericCellValue());
                }
            case BOOLEAN:
                return String.valueOf(cell.getBooleanCellValue());
            case FORMULA:
                return cell.getCellFormula();
            case BLANK:
                return "";
            default:
                return "";
        }
    }

    /**
     * 解析 Word 字节数组
     */
    private String parseDocxBytes(byte[] bytes) throws IOException {
        try (ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
             XWPFDocument document = new XWPFDocument(bis)) {

            StringBuilder content = new StringBuilder();
            List<XWPFParagraph> paragraphs = document.getParagraphs();

            for (XWPFParagraph paragraph : paragraphs) {
                String text = paragraph.getText();
                if (text != null && !text.trim().isEmpty()) {
                    content.append(text).append("\n");
                }
            }

            return content.toString().trim();
        }
    }

    /**
     * 解析 PowerPoint 字节数组
     */
    private String parsePptxBytes(byte[] bytes) throws IOException {
        try (ByteArrayInputStream bis = new ByteArrayInputStream(bytes);
             XMLSlideShow ppt = new XMLSlideShow(bis)) {

            StringBuilder content = new StringBuilder();
            List<XSLFSlide> slides = ppt.getSlides();

            for (int i = 0; i < slides.size(); i++) {
                XSLFSlide slide = slides.get(i);
                content.append("=== 幻灯片 ").append(i + 1).append(" ===\n");

                slide.getShapes().forEach(shape -> {
                    if (shape instanceof XSLFTextShape) {
                        XSLFTextShape textShape = (XSLFTextShape) shape;
                        String text = textShape.getText();
                        if (text != null && !text.trim().isEmpty()) {
                            content.append(text).append("\n");
                        }
                    }
                });

                content.append("\n");
            }

            return content.toString().trim();
        }
    }

    /**
     * 获取文件扩展名
     */
    private String getFileExtension(String filename) {
        int lastDot = filename.lastIndexOf('.');
        if (lastDot > 0 && lastDot < filename.length() - 1) {
            return filename.substring(lastDot).toLowerCase();
        }
        return "";
    }
}






