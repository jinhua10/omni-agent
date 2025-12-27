package top.yumbo.ai.omni.web.util;

/**
 * JSON 工具类
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
public class JsonUtil {

    /**
     * JSON字符串转义（用于SSE事件）
     *
     * @param text 原始文本
     * @return 转义后的文本
     */
    public static String escapeJson(String text) {
        if (text == null) {
            return "";
        }
        return text
                .replace("\\", "\\\\")
                .replace("\"", "\\\"")
                .replace("\n", "\\n")
                .replace("\r", "\\r")
                .replace("\t", "\\t");
    }
}






