package top.yumbo.ai.omni.web.util;

import java.io.File;

/**
 * 文档解析器接口 (Document parser interface)
 * 负责将各种格式的文件解析为文本内容 (Responsible for parsing various format files into text content)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface DocumentParser {

    /**
     * 解析文件 (Parse file)
     *
     * @param file 文件对象 (file object)
     * @return 解析后的文本内容 (parsed text content)
     */
    String parse(File file);

    /**
     * 解析字节数组 (Parse byte array)
     *
     * @param bytes 文件字节数组 (file byte array)
     * @param mimeType MIME类型 (MIME type)
     * @return 解析后的文本内容 (parsed text content)
     */
    String parse(byte[] bytes, String mimeType);

    /**
     * 检查是否支持该MIME类型 (Check if supports the MIME type)
     *
     * @param mimeType MIME类型 (MIME type)
     * @return 是否支持 (whether supported)
     */
    boolean supports(String mimeType);

    /**
     * 检查是否支持该文件扩展名 (Check if supports the file extension)
     *
     * @param extension 文件扩展名（如 .pdf） (file extension, e.g. .pdf)
     * @return 是否支持 (whether supported)
     */
    boolean supportsExtension(String extension);
}






