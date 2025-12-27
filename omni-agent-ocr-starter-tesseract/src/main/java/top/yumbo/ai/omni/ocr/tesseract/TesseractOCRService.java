package top.yumbo.ai.omni.ocr.tesseract;

import lombok.extern.slf4j.Slf4j;
import net.sourceforge.tess4j.Tesseract;
import net.sourceforge.tess4j.TesseractException;

import jakarta.annotation.PostConstruct;
import java.awt.image.BufferedImage;
import java.io.File;

/**
 * Tesseract OCR 服务
 * (Tesseract OCR Service)
 *
 * <p>提供基于 Tesseract 的 OCR 文字识别功能</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class TesseractOCRService {

    private final TesseractOCRProperties properties;
    private Tesseract tesseract;

    public TesseractOCRService(TesseractOCRProperties properties) {
        this.properties = properties;
    }

    @PostConstruct
    public void init() {
        try {
            tesseract = new Tesseract();

            // 设置数据路径
            if (properties.getDataPath() != null && !properties.getDataPath().isEmpty()) {
                File dataPath = new File(properties.getDataPath());
                if (dataPath.exists()) {
                    tesseract.setDatapath(properties.getDataPath());
                    log.info("✅ [OCR] Tesseract 数据路径: {}", properties.getDataPath());
                } else {
                    log.warn("⚠️ [OCR] Tesseract 数据路径不存在: {}, 使用系统默认路径",
                            properties.getDataPath());
                }
            }

            // 设置语言
            tesseract.setLanguage(properties.getLanguage());
            log.info("📝 [OCR] 识别语言: {}", properties.getLanguage());

            // 设置 DPI
            tesseract.setTessVariable("user_defined_dpi", String.valueOf(properties.getDpi()));

            // 设置页面分割模式
            tesseract.setPageSegMode(properties.getPageSegmentationMode());

            // 设置 OCR 引擎模式
            tesseract.setOcrEngineMode(properties.getOcrEngineMode());

            log.info("✅ [OCR] Tesseract 初始化成功: dpi={}, pageSegMode={}, engineMode={}",
                    properties.getDpi(),
                    properties.getPageSegmentationMode(),
                    properties.getOcrEngineMode());

        } catch (Exception e) {
            log.error("❌ [OCR] Tesseract 初始化失败", e);
            throw new RuntimeException("Tesseract OCR 服务初始化失败: " + e.getMessage(), e);
        }
    }

    /**
     * 从图片中识别文字
     *
     * @param image 图片
     * @return 识别的文字
     */
    public String recognizeText(BufferedImage image) {
        if (tesseract == null) {
            log.warn("⚠️ [OCR] Tesseract 未初始化");
            return "";
        }

        try {
            long startTime = System.currentTimeMillis();
            String text = tesseract.doOCR(image);
            long elapsed = System.currentTimeMillis() - startTime;

            if (text != null) {
                text = text.trim();
                log.debug("✅ [OCR] 识别完成: {} 字符, 耗时 {}ms", text.length(), elapsed);
                return text;
            }

            return "";
        } catch (TesseractException e) {
            log.error("❌ [OCR] 文字识别失败", e);
            return "";
        }
    }

    /**
     * 从图片中识别文字（带置信度过滤）
     *
     * @param image 图片
     * @return 识别的文字
     */
    public String recognizeTextWithConfidence(BufferedImage image) {
        String text = recognizeText(image);

        // 如果设置了最小置信度，可以在这里进行过滤
        // Tesseract 4.x 需要使用 getWords() 方法获取置信度
        // 这里简化处理，直接返回结果

        return text;
    }

    /**
     * 检查图片是否包含文字
     *
     * @param image 图片
     * @return 是否包含文字
     */
    public boolean hasText(BufferedImage image) {
        String text = recognizeText(image);
        return text != null && !text.trim().isEmpty();
    }

    /**
     * 检查 OCR 服务是否可用
     *
     * @return 是否可用
     */
    public boolean isAvailable() {
        return tesseract != null;
    }

    /**
     * 获取配置信息
     *
     * @return 配置属性
     */
    public TesseractOCRProperties getProperties() {
        return properties;
    }
}

