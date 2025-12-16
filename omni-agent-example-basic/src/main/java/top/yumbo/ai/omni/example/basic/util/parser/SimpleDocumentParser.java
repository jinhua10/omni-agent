package top.yumbo.ai.omni.example.basic.util.parser;

import lombok.extern.slf4j.Slf4j;
import org.apache.poi.xslf.usermodel.*;
import org.apache.poi.xwpf.usermodel.*;
import top.yumbo.ai.omni.example.basic.util.DocumentParser;
import top.yumbo.ai.omni.example.basic.util.parser.image.SmartImageExtractor;

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
            ".docx", ".pptx"
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

            if (extension.equals(".docx")) {
                return parseDocx(file);
            } else if (extension.equals(".pptx")) {
                return parsePptx(file);
            } else {
                // 默认作为文本文件处理
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

