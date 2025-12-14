package top.yumbo.ai.behavior.api.model;

/**
 * 信号权重配置 (Signal Weight Configuration)
 *
 * 定义不同信号类型对态度推断的权重影响
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public class SignalWeight {

    /**
     * 信号类型 (Signal Type)
     */
    private SignalType signalType;

    /**
     * 基础权重 (Base Weight)
     * 范围：0.0 ~ 1.0
     */
    private double baseWeight;

    /**
     * 方向系数 (Direction Coefficient)
     * 正数表示正面影响，负数表示负面影响
     * 范围：-1.0 ~ 1.0
     */
    private double direction;

    /**
     * 衰减因子 (Decay Factor)
     * 用于时间衰减，数值越大衰减越快
     */
    private double decayFactor;

    /**
     * 是否启用 (Enabled)
     */
    private boolean enabled = true;

    /**
     * 默认构造函数 (Default Constructor)
     */
    public SignalWeight() {
    }

    /**
     * 构造函数 (Constructor)
     *
     * @param signalType 信号类型
     * @param baseWeight 基础权重
     * @param direction 方向系数
     */
    public SignalWeight(SignalType signalType, double baseWeight, double direction) {
        this.signalType = signalType;
        this.baseWeight = baseWeight;
        this.direction = direction;
        this.decayFactor = 0.1; // 默认衰减因子
    }

    /**
     * 完整构造函数 (Full Constructor)
     *
     * @param signalType 信号类型
     * @param baseWeight 基础权重
     * @param direction 方向系数
     * @param decayFactor 衰减因子
     */
    public SignalWeight(SignalType signalType, double baseWeight, double direction, double decayFactor) {
        this.signalType = signalType;
        this.baseWeight = baseWeight;
        this.direction = direction;
        this.decayFactor = decayFactor;
    }

    // ========== Getter/Setter ==========

    public SignalType getSignalType() {
        return signalType;
    }

    public void setSignalType(SignalType signalType) {
        this.signalType = signalType;
    }

    public double getBaseWeight() {
        return baseWeight;
    }

    public void setBaseWeight(double baseWeight) {
        this.baseWeight = Math.max(0.0, Math.min(1.0, baseWeight));
    }

    public double getDirection() {
        return direction;
    }

    public void setDirection(double direction) {
        this.direction = Math.max(-1.0, Math.min(1.0, direction));
    }

    public double getDecayFactor() {
        return decayFactor;
    }

    public void setDecayFactor(double decayFactor) {
        this.decayFactor = decayFactor;
    }

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    // ========== 默认权重配置 (Default Weight Configurations) ==========

    /**
     * 获取默认权重配置 (Get Default Weight Configuration)
     *
     * @param signalType 信号类型
     * @return 权重配置
     */
    public static SignalWeight getDefault(SignalType signalType) {
        return switch (signalType) {
            case VIEW -> new SignalWeight(SignalType.VIEW, 0.1, 0.1, 0.2);
            case DWELL -> new SignalWeight(SignalType.DWELL, 0.3, 0.5, 0.15);
            case COPY -> new SignalWeight(SignalType.COPY, 0.5, 0.7, 0.1);
            case LIKE -> new SignalWeight(SignalType.LIKE, 1.0, 1.0, 0.05);
            case DISLIKE -> new SignalWeight(SignalType.DISLIKE, 1.0, -1.0, 0.05);
            case SHARE -> new SignalWeight(SignalType.SHARE, 0.8, 0.9, 0.08);
            case BOOKMARK -> new SignalWeight(SignalType.BOOKMARK, 0.7, 0.8, 0.1);
            case COMMENT -> new SignalWeight(SignalType.COMMENT, 0.6, 0.6, 0.12);
            case SEARCH -> new SignalWeight(SignalType.SEARCH, 0.2, 0.3, 0.18);
            case CLICK -> new SignalWeight(SignalType.CLICK, 0.4, 0.4, 0.15);
        };
    }

    /**
     * 计算实际权重（考虑启用状态）(Calculate Actual Weight)
     *
     * @return 实际权重
     */
    public double getActualWeight() {
        return enabled ? baseWeight : 0.0;
    }

    /**
     * 计算加权后的影响值 (Calculate Weighted Impact)
     *
     * @return 加权影响值
     */
    public double getWeightedImpact() {
        return enabled ? baseWeight * direction : 0.0;
    }

    @Override
    public String toString() {
        return "SignalWeight{" +
                "signalType=" + signalType +
                ", baseWeight=" + baseWeight +
                ", direction=" + direction +
                ", decayFactor=" + decayFactor +
                ", enabled=" + enabled +
                '}';
    }
}

