package top.yumbo.ai.omni.knowledge.registry.network;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;

/**
 * 知识构建结果
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeBuildResult implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 文档ID
     */
    private String documentId;

    /**
     * 目标知识域ID
     */
    private String domainId;

    /**
     * 是否成功
     */
    private boolean success;

    /**
     * 提取的知识数量
     */
    private int knowledgeCount;

    /**
     * 错误消息（如果失败）
     */
    private String errorMessage;

    /**
     * 提取的知识ID列表
     */
    @Builder.Default
    private List<String> knowledgeIds = new ArrayList<>();

    /**
     * 构建开始时间
     */
    private LocalDateTime startTime;

    /**
     * 构建结束时间
     */
    private LocalDateTime endTime;

    /**
     * 耗时（毫秒）
     */
    private long duration;
}

