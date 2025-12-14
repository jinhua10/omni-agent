package top.yumbo.ai.behavior.api.model;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.List;

/**
 * 态度评分 (Attitude Score)
 *
 * 基于行为信号推断出的用户态度评分
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
public class AttitudeScore {

    /**
     * 用户ID (User ID)
     */
    private String userId;

    /**
     * 答案ID (Answer ID)
     */
    private String answerId;

    /**
     * 原始评分 (Raw Score)
     * 范围：-1.0（非常不满意）到 +1.0（非常满意）
     */
    private double rawScore;

    /**
     * 归一化评分 (Normalized Score)
     * 范围：0.0 到 1.0，用于与显式评分对齐
     */
    private double normalizedScore;

    /**
     * 置信度 (Confidence)
     * 范围：0.0 到 1.0，表示推断的可信程度
     */
    private double confidence;

    /**
     * 态度等级 (Attitude Level)
     */
    private AttitudeLevel level;

    /**
     * 支撑信号列表 (Supporting Signals)
     * 用于推断的行为信号
     */
    private List<BehaviorSignalEvent> supportingSignals = new ArrayList<>();

    /**
     * 推断解释 (Inference Explanation)
     * 说明评分的依据
     */
    private String explanation;

    /**
     * 构造函数 (Constructor)
     *
     * @param userId 用户ID
     * @param answerId 答案ID
     * @param rawScore 原始评分
     * @param confidence 置信度
     */
    public AttitudeScore(String userId, String answerId, double rawScore, double confidence) {
        this.userId = userId;
        this.answerId = answerId;
        this.rawScore = rawScore;
        this.confidence = confidence;
        this.normalizedScore = normalizeScore(rawScore);
        this.level = AttitudeLevel.fromScore(rawScore);
        this.supportingSignals = new ArrayList<>();
    }

    /**
     * 添加支撑信号 (Add Supporting Signal)
     *
     * @param signal 行为信号事件
     */
    public void addSupportingSignal(BehaviorSignalEvent signal) {
        this.supportingSignals.add(signal);
    }

    /**
     * 将原始评分归一化到 0-1 范围 (Normalize Score to 0-1)
     * 公式：(score + 1) / 2
     *
     * @param score 原始评分
     * @return 归一化评分
     */
    private double normalizeScore(double score) {
        return (score + 1.0) / 2.0;
    }

    /**
     * 设置原始评分并自动更新归一化评分和等级 (Set Raw Score and Update)
     *
     * @param rawScore 原始评分
     */
    public void setRawScore(double rawScore) {
        this.rawScore = rawScore;
        this.normalizedScore = normalizeScore(rawScore);
        this.level = AttitudeLevel.fromScore(rawScore);
    }

    /**
     * 设置置信度（限制在0-1范围内）(Set Confidence with Limit)
     *
     * @param confidence 置信度
     */
    public void setConfidence(double confidence) {
        this.confidence = Math.max(0.0, Math.min(1.0, confidence));
    }

    /**
     * 判断是否为正面态度 (Is Positive)
     *
     * @return true if positive
     */
    public boolean isPositive() {
        return level != null && level.isPositive();
    }

    /**
     * 判断是否为负面态度 (Is Negative)
     *
     * @return true if negative
     */
    public boolean isNegative() {
        return level != null && level.isNegative();
    }

    /**
     * 判断是否为中立态度 (Is Neutral)
     *
     * @return true if neutral
     */
    public boolean isNeutral() {
        return level != null && level == AttitudeLevel.NEUTRAL;
    }
}

