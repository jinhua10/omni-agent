package top.yumbo.ai.omni.voting.api.model;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;

/**
 * 投票模型
 * (Vote Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
public class Vote {

    /**
     * 投票ID
     */
    private String voteId;

    /**
     * 会话ID
     */
    private String sessionId;

    /**
     * 投票者ID
     */
    private String voterId;

    /**
     * 投票者类型
     */
    private VoterType voterType;

    /**
     * 投票选择（支持/反对）
     */
    private VoteChoice choice;

    /**
     * 投票理由
     */
    private String reason;

    /**
     * 投票时间
     */
    private LocalDateTime voteTime;

    /**
     * 投票权重
     */
    private double weight;

    /**
     * 投票选择枚举
     */
    public enum VoteChoice {
        APPROVE,  // 支持
        REJECT,   // 反对
        ABSTAIN   // 弃权
    }
}

