package top.yumbo.ai.voting.api.model;

import lombok.Builder;
import lombok.Data;

import java.time.LocalDateTime;
import java.util.List;

/**
 * 投票会话模型
 * (Voting Session Model)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
public class VotingSession {

    /**
     * 会话ID
     */
    private String sessionId;

    /**
     * 议题
     */
    private String topic;

    /**
     * 议题描述
     */
    private String description;

    /**
     * 发起者ID
     */
    private String initiatorId;

    /**
     * 创建时间
     */
    private LocalDateTime createTime;

    /**
     * 截止时间
     */
    private LocalDateTime deadline;

    /**
     * 状态
     */
    private VotingStatus status;

    /**
     * 最小参与人数
     */
    private int minParticipants;

    /**
     * 通过阈值（百分比）
     */
    private double approvalThreshold;

    /**
     * 投票列表
     */
    private List<Vote> votes;

    /**
     * 投票结果
     */
    private VotingResult result;

    /**
     * 投票状态枚举
     */
    public enum VotingStatus {
        OPEN,       // 进行中
        CLOSED,     // 已关闭
        APPROVED,   // 已通过
        REJECTED,   // 已否决
        EXPIRED     // 已过期
    }
}

