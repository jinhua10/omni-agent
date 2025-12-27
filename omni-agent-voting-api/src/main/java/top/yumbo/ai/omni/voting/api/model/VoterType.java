package top.yumbo.ai.omni.voting.api.model;

/**
 * 投票者类型枚举
 * (Voter Type Enum)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public enum VoterType {

    /**
     * 普通用户（权重：1.0）
     */
    USER(1.0),

    /**
     * 领域专家（权重：2.0）
     */
    EXPERT(2.0),

    /**
     * AI助手（权重：0.5）
     */
    AI(0.5),

    /**
     * 管理员（权重：3.0）
     */
    ADMIN(3.0);

    private final double defaultWeight;

    VoterType(double defaultWeight) {
        this.defaultWeight = defaultWeight;
    }

    public double getDefaultWeight() {
        return defaultWeight;
    }
}

