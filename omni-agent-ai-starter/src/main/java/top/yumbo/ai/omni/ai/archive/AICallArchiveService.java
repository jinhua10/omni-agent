package top.yumbo.ai.omni.ai.archive;

import lombok.Data;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import top.yumbo.ai.omni.ai.archive.model.AICallArchive;

import java.util.List;
import java.util.Optional;

/**
 * AI调用归档服务接口
 *
 * <p>负责异步归档AI调用记录，用于知识网络学习</p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
public interface AICallArchiveService {

    /**
     * 异步归档AI调用记录（非流式）
     *
     * @param archive 归档记录
     * @return 归档ID
     */
    Mono<String> archiveAsync(AICallArchive archive);

    /**
     * 异步归档流式AI调用（等待流完成后归档）
     *
     * @param archive      归档记录（响应文本可能为空）
     * @param responseFlux 响应流
     * @return 归档ID（在流完成后发射）
     */
    Mono<String> archiveStreamAsync(AICallArchive archive, Flux<String> responseFlux);

    /**
     * 同步归档（阻塞，仅用于特殊场景）
     *
     * @param archive 归档记录
     * @return 归档ID
     */
    String archive(AICallArchive archive);

    /**
     * 获取归档记录
     *
     * @param archiveId 归档ID
     * @return 归档记录
     */
    Optional<AICallArchive> getArchive(String archiveId);

    /**
     * 查询归档记录（按时间范围）
     *
     * @param startTime 开始时间戳
     * @param endTime   结束时间戳
     * @param limit     限制数量
     * @return 归档记录列表
     */
    List<AICallArchive> queryByTimeRange(Long startTime, Long endTime, Integer limit);

    /**
     * 查询归档记录（按模型）
     *
     * @param model 模型名称
     * @param limit 限制数量
     * @return 归档记录列表
     */
    List<AICallArchive> queryByModel(String model, Integer limit);

    /**
     * 查询归档记录（按文档）
     *
     * @param documentId 文档ID
     * @return 归档记录列表
     */
    List<AICallArchive> queryByDocument(String documentId);

    /**
     * 获取归档统计
     *
     * @return 统计信息
     */
    ArchiveStatistics getStatistics();

    /**
     * 清理旧的归档记录
     *
     * @param olderThan 时间戳，删除早于此时间的记录
     * @return 删除的记录数
     */
    int cleanOldArchives(Long olderThan);

    /**
     * 归档统计信息
     */
    @Data
    class ArchiveStatistics {
        private Long totalCalls;
        private Long successCalls;
        private Long failedCalls;
        private Long streamCalls;
        private Long totalTokens;
        private Double avgDurationMs;

    }
}

