package top.yumbo.ai.omni.web.util.parser.image;

import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.InputStream;

/**
 * 占位符图片策略
 * (Placeholder Image Strategy)
 *
 * <p>默认策略，不进行实际的图片内容提取，仅返回占位符文本</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class PlaceholderImageStrategy implements ImageContentExtractorStrategy {

    @Override
    public String extractContent(InputStream imageStream, String imageName) {
        log.debug("使用占位符策略处理图片: {}", imageName);
        return String.format("[图片: %s]", imageName);
    }

    @Override
    public String extractContent(File imageFile) {
        return extractContent(null, imageFile.getName());
    }

    @Override
    public String getStrategyName() {
        return "Placeholder";
    }

    @Override
    public boolean isAvailable() {
        return true; // 占位符策略总是可用
    }
}



