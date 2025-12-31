package top.yumbo.ai.omni.common.http;

import java.net.MalformedURLException;
import java.net.URL;

/**
 * URL验证工具类
 * <p>
 * 提供完善的URL格式验证功能
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class UrlValidator {

    /**
     * 验证URL格式（基础验证）
     *
     * @param url 请求URL
     * @throws IllegalArgumentException URL格式错误
     */
    public static void validateBasic(String url) {
        if (url == null || url.trim().isEmpty()) {
            throw new IllegalArgumentException("URL cannot be null or empty");
        }
        if (!url.startsWith("http://") && !url.startsWith("https://")) {
            throw new IllegalArgumentException("Invalid URL protocol, must be http:// or https://");
        }
    }

    /**
     * 验证URL格式（完整验证）
     *
     * @param url 请求URL
     * @throws IllegalArgumentException URL格式错误
     */
    public static void validateFull(String url) {
        // 基础验证
        validateBasic(url);

        // 使用Java URL类进行完整验证
        try {
            URL urlObj = new URL(url);

            // 验证host不为空
            String host = urlObj.getHost();
            if (host == null || host.trim().isEmpty()) {
                throw new IllegalArgumentException("URL host cannot be empty");
            }

            // 验证host格式（不能包含非法字符）
            if (host.contains(" ") || host.contains("\t") || host.contains("\n")) {
                throw new IllegalArgumentException("URL host contains illegal characters");
            }

        } catch (MalformedURLException e) {
            throw new IllegalArgumentException("Malformed URL: " + e.getMessage(), e);
        }
    }

    /**
     * 验证URL格式（严格验证）
     *
     * @param url 请求URL
     * @throws IllegalArgumentException URL格式错误
     */
    public static void validateStrict(String url) {
        // 完整验证
        validateFull(url);

        try {
            URL urlObj = new URL(url);

            // 验证端口范围
            int port = urlObj.getPort();
            if (port < -1 || port > 65535) {
                throw new IllegalArgumentException("Invalid port number: " + port);
            }

            // 验证协议只能是http或https
            String protocol = urlObj.getProtocol();
            if (!"http".equalsIgnoreCase(protocol) && !"https".equalsIgnoreCase(protocol)) {
                throw new IllegalArgumentException("Protocol must be http or https, got: " + protocol);
            }

        } catch (MalformedURLException e) {
            throw new IllegalArgumentException("Malformed URL: " + e.getMessage(), e);
        }
    }

    /**
     * 检查URL是否有效（不抛出异常）
     *
     * @param url 请求URL
     * @return true-有效，false-无效
     */
    public static boolean isValid(String url) {
        try {
            validateBasic(url);
            return true;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    /**
     * 检查URL是否为HTTPS
     *
     * @param url 请求URL
     * @return true-HTTPS，false-HTTP或其他
     */
    public static boolean isHttps(String url) {
        if (url == null) {
            return false;
        }
        return url.trim().toLowerCase().startsWith("https://");
    }

    /**
     * 规范化URL（移除多余空格等）
     *
     * @param url 原始URL
     * @return 规范化后的URL
     */
    public static String normalize(String url) {
        if (url == null) {
            return null;
        }
        return url.trim();
    }
}

