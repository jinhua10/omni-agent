package top.yumbo.ai.omni.web.util;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;
import org.springframework.web.multipart.MultipartFile;
import top.yumbo.ai.omni.web.util.parser.SimpleDocumentParser;

import java.io.File;
import java.io.IOException;

/**
 * 文档解析工具类
 * (Document Parser Utility)
 *
 * <p>提供便捷的文档解析方法，自动使用 Spring 容器中的 DocumentParser Bean</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class DocumentParserUtil implements ApplicationContextAware {

    private static ApplicationContext applicationContext;
    private static DocumentParser cachedParser = null;

    @Override
    public void setApplicationContext(ApplicationContext context) throws BeansException {
        applicationContext = context;
    }

    /**
     * 获取文档解析器（优先使用 Spring Bean）
     */
    private static DocumentParser getParser() {
        if (cachedParser != null) {
            return cachedParser;
        }

        // 尝试从 Spring 容器获取
        if (applicationContext != null) {
            try {
                cachedParser = applicationContext.getBean(DocumentParser.class);
                log.debug("使用 Spring Bean 中的 DocumentParser");
                return cachedParser;
            } catch (Exception e) {
                log.debug("Spring 容器中未找到 DocumentParser Bean，使用默认解析器");
            }
        }

        // 使用默认解析器
        cachedParser = new SimpleDocumentParser(false);
        return cachedParser;
    }

    /**
     * 解析 MultipartFile
     *
     * @param file 上传的文件
     * @return 解析后的文本内容
     * @throws IOException 如果解析失败
     */
    public static String parseDocument(MultipartFile file) throws IOException {
        if (file == null || file.isEmpty()) {
            throw new IOException("文件为空");
        }

        String filename = file.getOriginalFilename();
        if (filename == null) {
            throw new IOException("文件名为空");
        }

        log.debug("开始解析文档: {}, size={} bytes", filename, file.getSize());

        try {
            DocumentParser parser = getParser();

            // 获取文件内容类型
            String contentType = file.getContentType();

            // 使用字节数组和MIME类型解析
            String content = parser.parse(file.getBytes(), contentType);

            if (content == null || content.trim().isEmpty()) {
                log.warn("解析结果为空: {}", filename);
                // 尝试作为纯文本解析
                content = new String(file.getBytes(), "UTF-8");
            }

            log.info("文档解析成功: {}, 内容长度: {} chars", filename, content.length());
            return content;

        } catch (Exception e) {
            log.error("文档解析失败: {}", filename, e);
            throw new IOException("文档解析失败: " + e.getMessage(), e);
        }
    }

    /**
     * 解析 File
     *
     * @param file 文件对象
     * @return 解析后的文本内容
     */
    public static String parseDocument(File file) {
        if (file == null || !file.exists()) {
            log.error("文件不存在");
            return "";
        }

        log.debug("开始解析文件: {}", file.getAbsolutePath());
        DocumentParser parser = getParser();
        return parser.parse(file);
    }

    /**
     * 检查文件类型是否支持
     *
     * @param filename 文件名
     * @return 是否支持
     */
    public static boolean isSupportedFileType(String filename) {
        if (filename == null || filename.isEmpty()) {
            return false;
        }

        String extension = getFileExtension(filename);
        DocumentParser parser = getParser();
        return parser.supportsExtension(extension);
    }

    /**
     * 获取文件扩展名
     */
    private static String getFileExtension(String filename) {
        int lastDot = filename.lastIndexOf('.');
        if (lastDot > 0 && lastDot < filename.length() - 1) {
            return filename.substring(lastDot);
        }
        return "";
    }

    /**
     * 获取默认解析器
     */
    public static DocumentParser getDefaultParser() {
        return getParser();
    }
}






