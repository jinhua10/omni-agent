package top.yumbo.ai.omni.common.http;

import okhttp3.ConnectionPool;
import okhttp3.OkHttpClient;

import java.lang.reflect.Field;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * 连接池监控拦截器
 * <p>
 * 监控OkHttp3连接池的使用情况
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public class ConnectionPoolMonitor {

    private final OkHttpClient client;
    private final AtomicInteger totalRequests = new AtomicInteger(0);
    private final AtomicInteger activeRequests = new AtomicInteger(0);

    public ConnectionPoolMonitor(OkHttpClient client) {
        this.client = client;
    }

    /**
     * 获取连接池中的连接数
     */
    public int getConnectionCount() {
        try {
            ConnectionPool pool = client.connectionPool();
            return pool.connectionCount();
        } catch (Exception e) {
            return -1;
        }
    }

    /**
     * 获取空闲连接数
     */
    public int getIdleConnectionCount() {
        try {
            ConnectionPool pool = client.connectionPool();
            return pool.idleConnectionCount();
        } catch (Exception e) {
            return -1;
        }
    }

    /**
     * 获取总请求数
     */
    public int getTotalRequests() {
        return totalRequests.get();
    }

    /**
     * 获取当前活动请求数
     */
    public int getActiveRequests() {
        return activeRequests.get();
    }

    /**
     * 记录请求开始
     */
    public void onRequestStart() {
        totalRequests.incrementAndGet();
        activeRequests.incrementAndGet();
    }

    /**
     * 记录请求结束
     */
    public void onRequestEnd() {
        activeRequests.decrementAndGet();
    }

    /**
     * 获取监控统计信息
     */
    public PoolStats getStats() {
        return new PoolStats(
            getConnectionCount(),
            getIdleConnectionCount(),
            getTotalRequests(),
            getActiveRequests()
        );
    }

    /**
     * 连接池统计信息
     */
    public static class PoolStats {
        private final int connectionCount;
        private final int idleConnectionCount;
        private final int totalRequests;
        private final int activeRequests;

        public PoolStats(int connectionCount, int idleConnectionCount,
                        int totalRequests, int activeRequests) {
            this.connectionCount = connectionCount;
            this.idleConnectionCount = idleConnectionCount;
            this.totalRequests = totalRequests;
            this.activeRequests = activeRequests;
        }

        public int getConnectionCount() {
            return connectionCount;
        }

        public int getIdleConnectionCount() {
            return idleConnectionCount;
        }

        public int getActiveConnectionCount() {
            return connectionCount - idleConnectionCount;
        }

        public int getTotalRequests() {
            return totalRequests;
        }

        public int getActiveRequests() {
            return activeRequests;
        }

        @Override
        public String toString() {
            return "PoolStats{" +
                    "connections=" + connectionCount +
                    ", idle=" + idleConnectionCount +
                    ", active=" + getActiveConnectionCount() +
                    ", totalRequests=" + totalRequests +
                    ", activeRequests=" + activeRequests +
                    '}';
        }
    }
}

