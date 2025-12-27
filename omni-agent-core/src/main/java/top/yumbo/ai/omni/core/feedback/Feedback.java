package top.yumbo.ai.omni.core.feedback;

import lombok.Builder;
import lombok.Data;

import java.util.Date;

/**
 * 反馈模型 (Feedback Model)
 *
 * 表示用户对系统输出的反馈
 * (Represents user feedback on system output)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@Data
@Builder
public class Feedback {

    /**
     * 反馈ID (Feedback ID)
     */
    private String id;

    /**
     * 会话ID (Session ID)
     */
    private String sessionId;

    /**
     * 用户ID (User ID)
     */
    private String userId;

    /**
     * 问题 (Question)
     */
    private String question;

    /**
     * 答案 (Answer)
     */
    private String answer;

    /**
     * 反馈值 (Feedback value)
     * 1: 好/有帮助 (good/helpful)
     * 0: 中立 (neutral)
     * -1: 差/无帮助 (bad/not helpful)
     */
    private double value;

    /**
     * 反馈类型 (Feedback type)
     */
    private FeedbackType type;

    /**
     * 反馈来源 (Feedback source)
     */
    private FeedbackSource source;

    /**
     * 反馈标签 (Feedback tags)
     */
    private String[] tags;

    /**
     * 评论 (Comment)
     */
    private String comment;

    /**
     * 创建时间 (Create time)
     */
    private Date createTime;

    /**
     * 处理状态 (Processing status)
     */
    private ProcessingStatus status;

    /**
     * 反馈类型枚举 (Feedback Type Enum)
     */
    public enum FeedbackType {
        EXPLICIT,  // 显式反馈 (Explicit feedback)
        IMPLICIT   // 隐式反馈 (Implicit feedback)
    }

    /**
     * 反馈来源枚举 (Feedback Source Enum)
     */
    public enum FeedbackSource {
        USER,      // 用户 (User)
        SYSTEM,    // 系统 (System)
        AUTO       // 自动 (Auto)
    }

    /**
     * 处理状态枚举 (Processing Status Enum)
     */
    public enum ProcessingStatus {
        PENDING,   // 待处理 (Pending)
        PROCESSED, // 已处理 (Processed)
        IGNORED    // 已忽略 (Ignored)
    }
}


