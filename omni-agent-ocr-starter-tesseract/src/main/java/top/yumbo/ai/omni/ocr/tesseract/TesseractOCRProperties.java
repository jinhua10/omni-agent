package top.yumbo.ai.omni.ocr.tesseract;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Tesseract OCR 配置属性
 * (Tesseract OCR Configuration Properties)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni-agent.ocr.tesseract")
public class TesseractOCRProperties {

    /**
     * 是否启用 OCR
     */
    private boolean enabled = false;

    /**
     * 识别语言（默认：简体中文 + 英文）
     * 常用值：
     * - eng: 英文
     * - chi_sim: 简体中文
     * - chi_tra: 繁体中文
     * - chi_sim+eng: 简体中文 + 英文
     */
    private String language = "chi_sim+eng";

    /**
     * Tesseract 数据文件路径
     * 如果不设置，使用系统默认路径
     */
    private String dataPath;

    /**
     * DPI 设置（默认：300）
     * 更高的 DPI 可以提高识别准确度，但会增加处理时间
     */
    private int dpi = 300;

    /**
     * 页面分割模式（默认：3）
     * 0 = Orientation and script detection (OSD) only.
     * 1 = Automatic page segmentation with OSD.
     * 2 = Automatic page segmentation, but no OSD, or OCR.
     * 3 = Fully automatic page segmentation, but no OSD. (Default)
     * 4 = Assume a single column of text of variable sizes.
     * 5 = Assume a single uniform block of vertically aligned text.
     * 6 = Assume a single uniform block of text.
     * 7 = Treat the image as a single text line.
     * 8 = Treat the image as a single word.
     * 9 = Treat the image as a single word in a circle.
     * 10 = Treat the image as a single character.
     */
    private int pageSegmentationMode = 3;

    /**
     * OCR 引擎模式（默认：3）
     * 0 = Legacy engine only.
     * 1 = Neural nets LSTM engine only.
     * 2 = Legacy + LSTM engines.
     * 3 = Default, based on what is available.
     */
    private int ocrEngineMode = 3;

    /**
     * 最小文本置信度（0-100，默认：0）
     * 低于此置信度的文本将被过滤
     */
    private int minConfidence = 0;

    /**
     * 超时时间（秒，默认：30）
     */
    private int timeout = 30;
}

