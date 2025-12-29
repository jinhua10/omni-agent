package top.yumbo.ai.omni.voting.api.model;

import lombok.Builder;
import lombok.Data;

/**
 * 投票结果模型
 * (Voting Result Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
public class VotingResult {

    /**
     * 会话ID
     */
    private String sessionId;

    /**
     * 支持票数
     */
    private int approveCount;

    /**
     * 反对票数
     */
    private int rejectCount;

    /**
     * 弃权票数
     */
    private int abstainCount;

    /**
     * 支持加权分数
     */
    private double approveScore;

    /**
     * 反对加权分数
     */
    private double rejectScore;

    /**
     * 弃权加权分数
     */
    private double abstainScore;

    /**
     * 总参与人数
     */
    private int totalParticipants;

    /**
     * 通过率（%）
     */
    private double approvalRate;

    /**
     * 最终决定
     */
    private Decision finalDecision;

    /**
     * 决定理由
     */
    private String reason;

    /**
     * 决定枚举
     */
    public enum Decision {
        APPROVED,   // 通过
        REJECTED,   // 否决
        PENDING     // 待定
    }
}

