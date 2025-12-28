package top.yumbo.ai.omni.document.processor.extension;

import top.yumbo.ai.omni.document.processor.DocumentProcessor;

/**
 * 文档处理器扩展接口 - 基础接口
 * (Document Processor Extension Interface - Base Interface)
 *
 * <p>
 * 所有扩展接口的基础，定义了扩展的基本属性。
 * 类似于 Spring 的 Ordered 接口。
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface DocumentProcessorExtension {

    /**
     * 获取扩展名称
     *
     * @return 扩展名称
     */
    String getName();

    /**
     * 获取执行顺序（数字越小优先级越高）
     *
     * @return 执行顺序
     */
    default int getOrder() {
        return 100;
    }

    /**
     * 判断是否支持该处理器
     *
     * @param processorName 处理器名称
     * @return true 如果支持
     */
    default boolean supports(String processorName) {
        return true; // 默认支持所有处理器
    }

    /**
     * 判断是否启用该扩展
     *
     * @return true 如果启用
     */
    default boolean isEnabled() {
        return true;
    }
}

