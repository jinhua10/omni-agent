package top.yumbo.ai.p2p.api;

import java.util.List;
import java.util.Map;
import java.util.function.Function;

/**
 * P2P数据传输桥接服务
 * (P2P Data Transfer Bridge Service)
 *
 * <p>连接源存储和目标存储，实现异构数据传输</p>
 * <p>Example: SQLite → Elasticsearch, File → MongoDB</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface P2PTransferBridge {

    /**
     * 执行数据传输
     * (Execute data transfer)
     *
     * @param sourceService 源数据服务
     * @param targetService 目标数据服务
     * @param query 查询条件
     * @param transformer 数据转换器
     * @param batchSize 批次大小
     * @return 传输结果
     */
    P2PDataTransferService.TransferResult transfer(
            P2PDataTransferService sourceService,
            P2PDataTransferService targetService,
            Map<String, Object> query,
            Function<Map<String, Object>, Map<String, Object>> transformer,
            int batchSize
    );

    /**
     * 双向同步
     * (Bi-directional sync)
     *
     * @param service1 服务1
     * @param service2 服务2
     * @param syncStrategy 同步策略
     * @return 同步结果
     */
    SyncResult bidirectionalSync(
            P2PDataTransferService service1,
            P2PDataTransferService service2,
            SyncStrategy syncStrategy
    );

    /**
     * 同步策略
     */
    enum SyncStrategy {
        /**
         * 源覆盖目标
         */
        SOURCE_WINS,
        
        /**
         * 目标覆盖源
         */
        TARGET_WINS,
        
        /**
         * 最新修改时间优先
         */
        LATEST_WINS,
        
        /**
         * 合并
         */
        MERGE
    }

    /**
     * 同步结果
     */
    class SyncResult {
        private int service1ToService2Count;
        private int service2ToService1Count;
        private long durationMs;
        private List<String> conflicts;

        public SyncResult(int service1ToService2Count, int service2ToService1Count, long durationMs) {
            this.service1ToService2Count = service1ToService2Count;
            this.service2ToService1Count = service2ToService1Count;
            this.durationMs = durationMs;
        }

        // Getters and Setters
        public int getService1ToService2Count() { return service1ToService2Count; }
        public void setService1ToService2Count(int count) { this.service1ToService2Count = count; }
        public int getService2ToService1Count() { return service2ToService1Count; }
        public void setService2ToService1Count(int count) { this.service2ToService1Count = count; }
        public long getDurationMs() { return durationMs; }
        public void setDurationMs(long durationMs) { this.durationMs = durationMs; }
        public List<String> getConflicts() { return conflicts; }
        public void setConflicts(List<String> conflicts) { this.conflicts = conflicts; }
    }
}
