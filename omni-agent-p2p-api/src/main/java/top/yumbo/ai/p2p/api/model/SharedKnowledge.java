package top.yumbo.ai.p2p.api.model;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * 共享知识模型
 * (Shared Knowledge Model)
 *
 * 表示在P2P网络中共享的知识
 * (Represents knowledge shared in P2P network)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
public class SharedKnowledge {

    /**
     * 知识ID
     * (Knowledge ID)
     */
    private String knowledgeId;

    /**
     * 来源用户ID
     * (Source user ID)
     */
    private String sourceUserId;

    /**
     * 来源用户昵称
     * (Source user nickname)
     */
    private String sourceUserName;

    /**
     * 知识内容（已加密）
     * (Knowledge content - encrypted)
     */
    private String encryptedContent;

    /**
     * 知识类型
     * (Knowledge type)
     */
    private String knowledgeType;

    /**
     * 创建时间
     * (Create time)
     */
    private LocalDateTime createTime;

    /**
     * 质量评分
     * (Quality score)
     */
    private Double qualityScore;

    /**
     * 是否已验证
     * (Is verified)
     */
    private boolean verified;

    /**
     * 标签
     * (Tags)
     */
    private String[] tags;
}

