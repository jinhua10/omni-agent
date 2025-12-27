package top.yumbo.ai.omni.web.util.parser.image;

import java.io.File;
import java.io.InputStream;

/**
 * 图片内容提取策略接口
 * (Image Content Extractor Strategy Interface)
 *
 * <p>支持多种图片处理方式：占位符、Vision LLM 语义理解</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface ImageContentExtractorStrategy {

    /**
     * 从图片提取文本内容
     *
     * @param imageStream 图片输入流
     * @param imageName   图片名称（用于日志）
     * @return 提取的文本内容
     */
    String extractContent(InputStream imageStream, String imageName);

    /**
     * 从图片文件提取文本内容
     *
     * @param imageFile 图片文件
     * @return 提取的文本内容
     */
    String extractContent(File imageFile);

    /**
     * 获取策略名称
     *
     * @return 策略名称
     */
    String getStrategyName();

    /**
     * 检查策略是否可用
     *
     * @return 是否可用
     */
    boolean isAvailable();
}



