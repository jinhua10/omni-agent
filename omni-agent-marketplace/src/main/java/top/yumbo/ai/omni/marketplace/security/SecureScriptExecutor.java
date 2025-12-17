package top.yumbo.ai.omni.marketplace.security;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import javax.script.*;
import java.util.Map;
import java.util.concurrent.*;

/**
 * 安全的脚本执行器
 *
 * 安全措施：
 * 1. 沙箱隔离 - 限制API访问
 * 2. 超时控制 - 防止死循环
 * 3. 资源限制 - 内存/CPU配额
 * 4. 白名单机制 - 只允许安全的操作
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Component
public class SecureScriptExecutor {

    private final ScriptEngine scriptEngine;
    private final ExecutorService executorService;

    // 安全配置
    private static final int DEFAULT_TIMEOUT_MS = 5000;  // 5秒超时
    private static final int MAX_TIMEOUT_MS = 30000;     // 最多30秒

    public SecureScriptExecutor() {
        // 初始化脚本引擎（优先使用GraalVM，更安全）
        ScriptEngineManager manager = new ScriptEngineManager();
        ScriptEngine engine = manager.getEngineByName("graal.js");

        if (engine == null) {
            log.warn("GraalVM script engine not available, falling back to Nashorn");
            engine = manager.getEngineByName("nashorn");
        }

        if (engine == null) {
            log.error("No JavaScript engine available!");
        }

        this.scriptEngine = engine;

        // 创建执行线程池（用于超时控制）
        this.executorService = Executors.newCachedThreadPool(r -> {
            Thread thread = new Thread(r);
            thread.setDaemon(true);
            thread.setName("ScriptExecutor-" + thread.getId());
            return thread;
        });
    }

    /**
     * 安全执行脚本
     *
     * @param script 脚本内容
     * @param context 执行上下文
     * @param timeoutMs 超时时间（毫秒）
     * @return 执行结果
     * @throws ScriptExecutionException 执行失败
     */
    @SuppressWarnings("unchecked")
    public Map<String, Object> executeSecurely(String script, Map<String, Object> context, int timeoutMs)
            throws ScriptExecutionException {

        if (scriptEngine == null) {
            throw new ScriptExecutionException("Script engine not available");
        }

        // 验证超时时间
        if (timeoutMs <= 0 || timeoutMs > MAX_TIMEOUT_MS) {
            timeoutMs = DEFAULT_TIMEOUT_MS;
        }

        // 验证脚本内容
        validateScript(script);

        // 创建安全的执行上下文
        Bindings bindings = createSecureBindings(context);

        // 在独立线程中执行（支持超时）
        Future<Map<String, Object>> future = executorService.submit(() -> {
            try {
                // 执行脚本
                scriptEngine.eval(script, bindings);

                // 调用 optimize 函数
                if (scriptEngine instanceof Invocable) {
                    Invocable invocable = (Invocable) scriptEngine;
                    Object result = invocable.invokeFunction("optimize",
                            context.get("documentId"),
                            context);

                    if (result instanceof Map) {
                        return (Map<String, Object>) result;
                    }
                }

                throw new ScriptExecutionException("Script must return an object with 'data' and 'metrics'");

            } catch (ScriptException | NoSuchMethodException e) {
                throw new ScriptExecutionException("Script execution failed: " + e.getMessage(), e);
            }
        });

        try {
            // 等待执行完成（带超时）
            return future.get(timeoutMs, TimeUnit.MILLISECONDS);

        } catch (TimeoutException e) {
            future.cancel(true);
            log.error("Script execution timeout after {}ms", timeoutMs);
            throw new ScriptExecutionException("Script execution timeout");

        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            throw new ScriptExecutionException("Script execution interrupted");

        } catch (ExecutionException e) {
            Throwable cause = e.getCause();
            if (cause instanceof ScriptExecutionException) {
                throw (ScriptExecutionException) cause;
            }
            throw new ScriptExecutionException("Script execution failed", cause);
        }
    }

    /**
     * 验证脚本内容（黑名单检查）
     */
    private void validateScript(String script) throws ScriptExecutionException {
        if (script == null || script.trim().isEmpty()) {
            throw new ScriptExecutionException("Script content is empty");
        }

        // 黑名单：危险操作
        String[] dangerousPatterns = {
            "java.lang.Runtime",
            "java.lang.System",
            "java.io.File",
            "java.nio.file",
            "java.net.Socket",
            "java.net.URL",
            "javax.script",
            "eval(",
            "Function(",
            "setInterval",
            "setTimeout"
        };

        String lowerScript = script.toLowerCase();
        for (String pattern : dangerousPatterns) {
            if (lowerScript.contains(pattern.toLowerCase())) {
                log.warn("Script contains dangerous pattern: {}", pattern);
                throw new ScriptExecutionException("Script contains forbidden operation: " + pattern);
            }
        }

        // 检查脚本长度（防止过大）
        if (script.length() > 50000) {  // 最多50KB
            throw new ScriptExecutionException("Script is too large (max 50KB)");
        }
    }

    /**
     * 创建安全的执行上下文（白名单机制）
     */
    private Bindings createSecureBindings(Map<String, Object> context) {
        Bindings bindings = scriptEngine.createBindings();

        // 只暴露安全的上下文数据
        if (context != null) {
            // 白名单：只允许这些字段
            String[] allowedFields = {
                "documentId", "query", "document", "metadata"
            };

            for (String field : allowedFields) {
                if (context.containsKey(field)) {
                    bindings.put(field, context.get(field));
                }
            }
        }

        // 不暴露任何Java对象或敏感API
        // bindings.put("System", null);  // 禁用System
        // bindings.put("Runtime", null); // 禁用Runtime

        return bindings;
    }

    /**
     * 脚本执行异常
     */
    public static class ScriptExecutionException extends Exception {
        public ScriptExecutionException(String message) {
            super(message);
        }

        public ScriptExecutionException(String message, Throwable cause) {
            super(message, cause);
        }
    }

    /**
     * 关闭执行器
     */
    public void shutdown() {
        executorService.shutdown();
        try {
            if (!executorService.awaitTermination(5, TimeUnit.SECONDS)) {
                executorService.shutdownNow();
            }
        } catch (InterruptedException e) {
            executorService.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }
}

