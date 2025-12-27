package top.yumbo.ai.omni.web.util.parser.image;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

/**
 * 智能图片内容提取器
 * (Smart Image Extractor)
 *
 * <p>支持多种策略，按优先级自动选择可用的策略：</p>
 * <ol>
 *   <li>Vision LLM（语义理解，推荐）- 使用 GPT-4o 等多模态 LLM</li>
 *   <li>Placeholder（占位符，兜底）</li>
 * </ol>
 *
 * <p>使用方法:</p>
 * <pre>
 * // 默认配置（使用占位符）
 * SmartImageExtractor extractor = new SmartImageExtractor();
 *
 * // 启用 Vision LLM（推荐）
 * SmartImageExtractor extractor = SmartImageExtractor.withVisionLLM(apiKey);
 *
 * // 自定义策略
 * SmartImageExtractor extractor = new SmartImageExtractor();
 * extractor.addStrategy(new VisionLLMStrategy(apiKey, model, endpoint));
 * </pre>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class SmartImageExtractor {

    private final List<ImageContentExtractorStrategy> strategies;

    @Getter
    private ImageContentExtractorStrategy activeStrategy;

    /**
     * 默认构造函数（只使用占位符）
     */
    public SmartImageExtractor() {
        this.strategies = new ArrayList<>();
        this.strategies.add(new PlaceholderImageStrategy());
        selectActiveStrategy();
    }

    /**
     * 创建带 Vision LLM 的提取器
     *
     * @param apiKey Vision LLM API 密钥
     * @return SmartImageExtractor 实例
     */
    public static SmartImageExtractor withVisionLLM(String apiKey) {
        SmartImageExtractor extractor = new SmartImageExtractor();
        // TODO: 实现 VisionLLMStrategy
        // extractor.addStrategy(new VisionLLMStrategy(apiKey));
        log.info("Vision LLM 功能开发中，当前使用占位符策略");
        return extractor;
    }

    /**
     * 添加策略
     *
     * @param strategy 图片内容提取策略
     * @return 当前实例（支持链式调用）
     */
    public SmartImageExtractor addStrategy(ImageContentExtractorStrategy strategy) {
        if (strategy != null) {
            strategies.add(0, strategy); // 添加到最前面（高优先级）
            selectActiveStrategy();
        }
        return this;
    }

    /**
     * 选择可用的策略
     */
    private void selectActiveStrategy() {
        for (ImageContentExtractorStrategy strategy : strategies) {
            if (strategy.isAvailable()) {
                activeStrategy = strategy;
                log.info("图片提取策略已选择: {}", strategy.getStrategyName());
                return;
            }
        }

        // 兜底：使用占位符
        activeStrategy = new PlaceholderImageStrategy();
        log.warn("未找到可用的图片提取策略，使用占位符");
    }

    /**
     * 提取图片内容
     *
     * @param imageStream 图片输入流
     * @param imageName   图片名称
     * @return 提取的文本内容
     */
    public String extractContent(InputStream imageStream, String imageName) {
        if (activeStrategy == null) {
            return String.format("[图片: %s]", imageName);
        }

        try {
            return activeStrategy.extractContent(imageStream, imageName);
        } catch (Exception e) {
            log.error("图片内容提取失败: {}", imageName, e);
            return String.format("[图片提取失败: %s]", imageName);
        }
    }

    /**
     * 提取图片内容
     *
     * @param imageFile 图片文件
     * @return 提取的文本内容
     */
    public String extractContent(File imageFile) {
        if (activeStrategy == null) {
            return String.format("[图片: %s]", imageFile.getName());
        }

        try {
            return activeStrategy.extractContent(imageFile);
        } catch (Exception e) {
            log.error("图片内容提取失败: {}", imageFile.getName(), e);
            return String.format("[图片提取失败: %s]", imageFile.getName());
        }
    }

    /**
     * 获取所有已注册的策略
     *
     * @return 策略列表
     */
    public List<ImageContentExtractorStrategy> getStrategies() {
        return new ArrayList<>(strategies);
    }
}



