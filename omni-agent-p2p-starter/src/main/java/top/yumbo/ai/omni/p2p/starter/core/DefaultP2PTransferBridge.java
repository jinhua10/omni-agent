package top.yumbo.ai.omni.p2p.starter.core;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.p2p.api.P2PDataTransferService;
import top.yumbo.ai.omni.p2p.api.P2PTransferBridge;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * P2P传输桥接服务默认实现
 * (Default P2P Transfer Bridge Implementation)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class DefaultP2PTransferBridge implements P2PTransferBridge {

    @Override
    public P2PDataTransferService.TransferResult transfer(
            P2PDataTransferService sourceService,
            P2PDataTransferService targetService,
            Map<String, Object> query,
            Function<Map<String, Object>, Map<String, Object>> transformer,
            int batchSize) {

        long startTime = System.currentTimeMillis();
        
        log.info("Starting P2P transfer from {} to {}", 
            sourceService.getClass().getSimpleName(),
            targetService.getClass().getSimpleName());

        try {
            // 从源读取数据
            List<Map<String, Object>> sourceData = sourceService.readFromSource(query);
            int totalRecords = sourceData.size();
            
            log.info("Read {} records from source", totalRecords);

            if (sourceData.isEmpty()) {
                return new P2PDataTransferService.TransferResult(0, 0, 0, 
                    System.currentTimeMillis() - startTime);
            }

            // 应用转换
            List<Map<String, Object>> transformedData = sourceData.stream()
                .map(data -> transformer != null ? transformer.apply(data) : data)
                .collect(Collectors.toList());

            int successCount = 0;
            int failureCount = 0;

            // 分批写入目标
            for (int i = 0; i < transformedData.size(); i += batchSize) {
                int end = Math.min(i + batchSize, transformedData.size());
                List<Map<String, Object>> batch = transformedData.subList(i, end);
                
                try {
                    int written = targetService.writeToTarget(batch);
                    successCount += written;
                    failureCount += (batch.size() - written);
                    
                    log.info("Batch {}: wrote {}/{} records", 
                        (i / batchSize + 1), written, batch.size());
                        
                } catch (Exception e) {
                    log.error("Failed to write batch", e);
                    failureCount += batch.size();
                }
            }

            long duration = System.currentTimeMillis() - startTime;
            
            log.info("Transfer completed: total={}, success={}, failure={}, duration={}ms",
                totalRecords, successCount, failureCount, duration);

            return new P2PDataTransferService.TransferResult(
                totalRecords, successCount, failureCount, duration);

        } catch (Exception e) {
            log.error("Transfer failed", e);
            long duration = System.currentTimeMillis() - startTime;
            return new P2PDataTransferService.TransferResult(0, 0, 0, duration);
        }
    }

    @Override
    public SyncResult bidirectionalSync(
            P2PDataTransferService service1,
            P2PDataTransferService service2,
            SyncStrategy syncStrategy) {

        long startTime = System.currentTimeMillis();
        
        log.info("Starting bidirectional sync between {} and {} with strategy {}",
            service1.getClass().getSimpleName(),
            service2.getClass().getSimpleName(),
            syncStrategy);

        try {
            // 从两边读取所有数据
            List<Map<String, Object>> data1 = service1.readFromSource(Collections.emptyMap());
            List<Map<String, Object>> data2 = service2.readFromSource(Collections.emptyMap());

            log.info("Service1 has {} records, Service2 has {} records", 
                data1.size(), data2.size());

            // 根据策略同步
            int count1to2 = 0;
            int count2to1 = 0;

            switch (syncStrategy) {
                case SOURCE_WINS:
                    // Service1 → Service2
                    count1to2 = service2.writeToTarget(data1);
                    break;

                case TARGET_WINS:
                    // Service2 → Service1
                    count2to1 = service1.writeToTarget(data2);
                    break;

                case LATEST_WINS:
                case MERGE:
                    // 双向合并（简化实现）
                    Set<String> ids1 = extractIds(data1);
                    Set<String> ids2 = extractIds(data2);

                    // Service1中有但Service2中没有的
                    List<Map<String, Object>> onlyIn1 = data1.stream()
                        .filter(d -> !ids2.contains(String.valueOf(d.get("id"))))
                        .collect(Collectors.toList());
                    count1to2 = service2.writeToTarget(onlyIn1);

                    // Service2中有但Service1中没有的
                    List<Map<String, Object>> onlyIn2 = data2.stream()
                        .filter(d -> !ids1.contains(String.valueOf(d.get("id"))))
                        .collect(Collectors.toList());
                    count2to1 = service1.writeToTarget(onlyIn2);
                    break;
            }

            long duration = System.currentTimeMillis() - startTime;

            log.info("Sync completed: 1→2={}, 2→1={}, duration={}ms",
                count1to2, count2to1, duration);

            return new SyncResult(count1to2, count2to1, duration);

        } catch (Exception e) {
            log.error("Sync failed", e);
            long duration = System.currentTimeMillis() - startTime;
            return new SyncResult(0, 0, duration);
        }
    }

    private Set<String> extractIds(List<Map<String, Object>> data) {
        return data.stream()
            .map(d -> String.valueOf(d.get("id")))
            .filter(Objects::nonNull)
            .collect(Collectors.toSet());
    }
}


