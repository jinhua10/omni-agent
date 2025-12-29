package top.yumbo.ai.omni.ai.starter.impl;

import ai.onnxruntime.OrtEnvironment;
import ai.onnxruntime.OrtException;
import ai.onnxruntime.OrtSession;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 共享 ONNX 模型管理器
 * <p>
 * 用于避免同一个模型被多次加载，节省内存资源
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class SharedOnnxModelManager {

    private final OrtEnvironment environment;

    // 模型缓存：key=模型路径, value=Session实例
    private final ConcurrentHashMap<String, OrtSession> sessionCache = new ConcurrentHashMap<>();

    // 引用计数：key=模型路径, value=引用次数
    private final ConcurrentHashMap<String, Integer> refCount = new ConcurrentHashMap<>();

    /**
     * 模型信息
     */
    @Getter
    public static class ModelInfo {
        private final OrtSession session;
        private final String modelName;
        private final String modelPath;

        public ModelInfo(OrtSession session, String modelName, String modelPath) {
            this.session = session;
            this.modelName = modelName;
            this.modelPath = modelPath;
        }
    }

    public SharedOnnxModelManager() {
        this.environment = OrtEnvironment.getEnvironment();
        log.info("✅ 共享 ONNX 模型管理器已初始化");
    }

    /**
     * 获取或创建 ONNX Session
     *
     * @param modelPath 模型文件路径
     * @return ModelInfo 包含session和模型信息
     * @throws OrtException ONNX Runtime异常
     * @throws IOException IO异常
     */
    public synchronized ModelInfo getOrCreateSession(String modelPath) throws OrtException, IOException {
        String resolvedPath = resolveModelPath(modelPath);

        // 检查缓存
        OrtSession session = sessionCache.get(resolvedPath);
        if (session != null) {
            // 增加引用计数
            refCount.merge(resolvedPath, 1, Integer::sum);
            log.info("♻️ 复用已加载的 ONNX 模型: {} (引用计数: {})",
                    resolvedPath, refCount.get(resolvedPath));

            return new ModelInfo(session, extractModelName(resolvedPath), resolvedPath);
        }

        // 创建新session
        log.info("📦 加载 ONNX 模型: {}", resolvedPath);
        OrtSession.SessionOptions options = new OrtSession.SessionOptions();
        options.setOptimizationLevel(OrtSession.SessionOptions.OptLevel.BASIC_OPT);

        session = environment.createSession(resolvedPath, options);

        // 缓存session
        sessionCache.put(resolvedPath, session);
        refCount.put(resolvedPath, 1);

        log.info("✅ ONNX 模型加载成功: {} (引用计数: 1)", resolvedPath);

        return new ModelInfo(session, extractModelName(resolvedPath), resolvedPath);
    }

    /**
     * 释放模型引用
     *
     * @param modelPath 模型路径
     */
    public synchronized void releaseSession(String modelPath) {
        try {
            String resolvedPath = resolveModelPath(modelPath);
            Integer count = refCount.get(resolvedPath);

            if (count == null) {
                log.warn("⚠️ 尝试释放未加载的模型: {}", resolvedPath);
                return;
            }

            if (count <= 1) {
                // 最后一个引用，关闭session
                OrtSession session = sessionCache.remove(resolvedPath);
                refCount.remove(resolvedPath);

                if (session != null) {
                    session.close();
                    log.info("🔒 ONNX 模型已关闭: {} (引用计数: 0)", resolvedPath);
                }
            } else {
                // 减少引用计数
                refCount.put(resolvedPath, count - 1);
                log.info("📉 减少模型引用: {} (引用计数: {})", resolvedPath, count - 1);
            }
        } catch (Exception e) {
            log.error("释放模型引用失败", e);
        }
    }

    /**
     * 获取 ONNX 环境
     */
    public OrtEnvironment getEnvironment() {
        return environment;
    }

    /**
     * 解析模型路径
     */
    private String resolveModelPath(String modelPath) throws IOException {
        // 尝试作为文件路径
        Path path = Paths.get(modelPath);
        if (Files.exists(path)) {
            return path.toAbsolutePath().toString();
        }

        // 尝试作为 classpath 资源
        try {
            Path resourcePath = Paths.get(
                    getClass().getClassLoader().getResource(modelPath).toURI()
            );
            if (Files.exists(resourcePath)) {
                return resourcePath.toString();
            }
        } catch (Exception e) {
            // 忽略，继续尝试其他方式
        }

        throw new IOException(String.format(
                "模型文件不存在: %s\n\n" +
                        "📖 推荐模型（国产）：\n" +
                        "  中文（推荐）：BAAI/bge-base-zh-v1.5 (768维，~400MB)\n" +
                        "  多语言大模型：BAAI/bge-m3 (1024维，~2GB)\n" +
                        "  中文大模型：BAAI/bge-large-zh (1024维)\n" +
                        "\n" +
                        "📁 模型放置位置：\n" +
                        "  1. 外部目录（推荐）：./models/bge-base-zh/model.onnx\n" +
                        "  2. 开发环境：src/main/resources/models/bge-base-zh/model.onnx",
                modelPath
        ));
    }

    /**
     * 提取模型名称
     */
    private String extractModelName(String modelPath) {
        Path path = Paths.get(modelPath);
        Path parent = path.getParent();
        return parent != null ? parent.getFileName().toString() : "unknown";
    }

    /**
     * 关闭所有会话
     */
    public synchronized void closeAll() {
        log.info("🔒 关闭所有 ONNX 模型会话");

        for (OrtSession session : sessionCache.values()) {
            try {
                session.close();
            } catch (Exception e) {
                log.error("关闭 session 失败", e);
            }
        }

        sessionCache.clear();
        refCount.clear();

        log.info("✅ 所有 ONNX 模型会话已关闭");
    }
}

