package top.yumbo.ai.omni.common.http;

/**
 * 重试策略接口
 * <p>
 * 定义HTTP请求失败时的重试行为
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface RetryPolicy {

    /**
     * 判断是否应该重试
     *
     * @param attempt 当前重试次数（从1开始）
     * @param exception 发生的异常
     * @return true-应该重试，false-不重试
     */
    boolean shouldRetry(int attempt, Exception exception);

    /**
     * 获取重试延迟时间
     *
     * @param attempt 当前重试次数（从1开始）
     * @return 延迟毫秒数
     */
    long getDelayMillis(int attempt);

    /**
     * 获取最大重试次数
     *
     * @return 最大重试次数
     */
    int getMaxRetries();

    /**
     * 判断异常是否可重试
     *
     * @param exception 异常对象
     * @return true-可重试，false-不可重试
     */
    default boolean isRetriable(Exception exception) {
        // 默认只重试网络相关异常
        if (exception instanceof java.net.SocketTimeoutException) {
            return true;
        }
        if (exception instanceof java.net.ConnectException) {
            return true;
        }
        if (exception instanceof java.io.IOException) {
            return true;
        }
        // HTTP异常根据状态码判断
        if (exception instanceof top.yumbo.ai.omni.common.exception.HttpException httpEx) {
            // 5xx服务器错误可重试，4xx客户端错误不重试
            return httpEx.isServerError();
        }
        return false;
    }

    /**
     * 无重试策略
     */
    static RetryPolicy noRetry() {
        return new RetryPolicy() {
            @Override
            public boolean shouldRetry(int attempt, Exception exception) {
                return false;
            }

            @Override
            public long getDelayMillis(int attempt) {
                return 0;
            }

            @Override
            public int getMaxRetries() {
                return 0;
            }
        };
    }

    /**
     * 固定延迟重试策略
     */
    static RetryPolicy fixedDelay(int maxRetries, long delayMillis) {
        return new RetryPolicy() {
            @Override
            public boolean shouldRetry(int attempt, Exception exception) {
                return attempt <= maxRetries && isRetriable(exception);
            }

            @Override
            public long getDelayMillis(int attempt) {
                return delayMillis;
            }

            @Override
            public int getMaxRetries() {
                return maxRetries;
            }
        };
    }

    /**
     * 指数退避重试策略
     */
    static RetryPolicy exponentialBackoff(int maxRetries, long initialDelayMillis) {
        return new RetryPolicy() {
            @Override
            public boolean shouldRetry(int attempt, Exception exception) {
                return attempt <= maxRetries && isRetriable(exception);
            }

            @Override
            public long getDelayMillis(int attempt) {
                // 2^(attempt-1) * initialDelay
                return initialDelayMillis * (1L << (attempt - 1));
            }

            @Override
            public int getMaxRetries() {
                return maxRetries;
            }
        };
    }

    /**
     * 指数退避重试策略（带最大延迟）
     */
    static RetryPolicy exponentialBackoffWithLimit(int maxRetries, long initialDelayMillis, long maxDelayMillis) {
        return new RetryPolicy() {
            @Override
            public boolean shouldRetry(int attempt, Exception exception) {
                return attempt <= maxRetries && isRetriable(exception);
            }

            @Override
            public long getDelayMillis(int attempt) {
                long delay = initialDelayMillis * (1L << (attempt - 1));
                return Math.min(delay, maxDelayMillis);
            }

            @Override
            public int getMaxRetries() {
                return maxRetries;
            }
        };
    }
}

