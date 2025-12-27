package top.yumbo.ai.omni.p2p.api;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * P2P数据传输服务接口
 * (P2P Data Transfer Service Interface)
 *
 * <p>支持异构存储之间的数据传输和转换</p>
 * <p>例如: SQLite → Elasticsearch, File → MongoDB等</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface P2PDataTransferService {

    /**
     * 从源存储读取数据
     * (Read data from source storage)
     *
     * @param query 查询条件
     * @return 数据列表
     */
    List<Map<String, Object>> readFromSource(Map<String, Object> query);

    /**
     * 写入数据到目标存储
     * (Write data to target storage)
     *
     * @param data 数据列表
     * @return 成功写入的数量
     */
    int writeToTarget(List<Map<String, Object>> data);

    /**
     * 数据转换
     * (Transform data)
     *
     * @param sourceData 源数据
     * @return 转换后的数据
     */
    Map<String, Object> transformData(Map<String, Object> sourceData);

    /**
     * 批量传输数据
     * (Batch transfer data)
     *
     * @param sourceQuery 源查询条件
     * @param batchSize 批次大小
     * @return 传输结果统计
     */
    TransferResult batchTransfer(Map<String, Object> sourceQuery, int batchSize);

    /**
     * 获取传输统计信息
     * (Get transfer statistics)
     *
     * @return 统计信息
     */
    Map<String, Object> getTransferStatistics();

    /**
     * 传输结果
     */
    @Data
    class TransferResult {
        private int totalRecords;
        private int successCount;
        private int failureCount;
        private long durationMs;
        private List<String> errors;

        public TransferResult(int totalRecords, int successCount, int failureCount, long durationMs) {
            this.totalRecords = totalRecords;
            this.successCount = successCount;
            this.failureCount = failureCount;
            this.durationMs = durationMs;
        }

    }
}
