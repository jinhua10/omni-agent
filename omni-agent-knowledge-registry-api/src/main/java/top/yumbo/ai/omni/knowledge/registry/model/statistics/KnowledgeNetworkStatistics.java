package top.yumbo.ai.omni.knowledge.registry.model.statistics;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

/**
 * 知识网络统计信息
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeNetworkStatistics implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 已处理文档数量
     */
    private int processedDocuments;

    /**
     * 待处理任务数量
     */
    private int pendingTasks;

    /**
     * 是否启用
     */
    private boolean enabled;

    /**
     * 总知识数量
     */
    private long totalKnowledge;

    /**
     * 活跃知识域数量
     */
    private int activeDomains;
}

