package top.yumbo.ai.behavior.api.model;

/**
 * 态度等级枚举 (Attitude Level Enum)
 *
 * 将连续的态度评分离散化为5个等级
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public enum AttitudeLevel {

    /**
     * 非常满意 (Very Satisfied)
     * 评分范围：0.6 ~ 1.0
     */
    VERY_SATISFIED(0.6, 1.0, "非常满意", "Very Satisfied"),

    /**
     * 满意 (Satisfied)
     * 评分范围：0.2 ~ 0.6
     */
    SATISFIED(0.2, 0.6, "满意", "Satisfied"),

    /**
     * 中立 (Neutral)
     * 评分范围：-0.2 ~ 0.2
     */
    NEUTRAL(-0.2, 0.2, "中立", "Neutral"),

    /**
     * 不满意 (Dissatisfied)
     * 评分范围：-0.6 ~ -0.2
     */
    DISSATISFIED(-0.6, -0.2, "不满意", "Dissatisfied"),

    /**
     * 非常不满意 (Very Dissatisfied)
     * 评分范围：-1.0 ~ -0.6
     */
    VERY_DISSATISFIED(-1.0, -0.6, "非常不满意", "Very Dissatisfied");

    private final double minScore;
    private final double maxScore;
    private final String zhName;
    private final String enName;

    AttitudeLevel(double minScore, double maxScore, String zhName, String enName) {
        this.minScore = minScore;
        this.maxScore = maxScore;
        this.zhName = zhName;
        this.enName = enName;
    }

    public double getMinScore() {
        return minScore;
    }

    public double getMaxScore() {
        return maxScore;
    }

    public String getZhName() {
        return zhName;
    }

    public String getEnName() {
        return enName;
    }

    /**
     * 根据评分获取态度等级 (Get Attitude Level by Score)
     *
     * @param score 评分（范围：-1.0 ~ 1.0）
     * @return 态度等级
     */
    public static AttitudeLevel fromScore(double score) {
        for (AttitudeLevel level : values()) {
            if (score >= level.minScore && score <= level.maxScore) {
                return level;
            }
        }
        return NEUTRAL; // 默认返回中立
    }

    /**
     * 判断是否为正面态度 (Is Positive Attitude)
     *
     * @return true if positive
     */
    public boolean isPositive() {
        return this == VERY_SATISFIED || this == SATISFIED;
    }

    /**
     * 判断是否为负面态度 (Is Negative Attitude)
     *
     * @return true if negative
     */
    public boolean isNegative() {
        return this == VERY_DISSATISFIED || this == DISSATISFIED;
    }

    /**
     * 获取等级的数值表示 (Get Numeric Value)
     *
     * @return 数值（1-5）
     */
    public int getNumericValue() {
        return switch (this) {
            case VERY_SATISFIED -> 5;
            case SATISFIED -> 4;
            case NEUTRAL -> 3;
            case DISSATISFIED -> 2;
            case VERY_DISSATISFIED -> 1;
        };
    }
}

