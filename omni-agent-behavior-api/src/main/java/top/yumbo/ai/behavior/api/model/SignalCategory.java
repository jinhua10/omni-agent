package top.yumbo.ai.behavior.api.model;

/**
 * 信号类别枚举 (Signal Category Enum)
 *
 * 将不同的信号类型分组到类别中，便于统计和分析
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public enum SignalCategory {

    /**
     * 浏览类 (View Category)
     * 包含：浏览、停留
     */
    VIEW("浏览类", "View"),

    /**
     * 交互类 (Interaction Category)
     * 包含：复制、点击
     */
    INTERACTION("交互类", "Interaction"),

    /**
     * 反馈类 (Feedback Category)
     * 包含：点赞、踩、评论
     */
    FEEDBACK("反馈类", "Feedback"),

    /**
     * 社交类 (Social Category)
     * 包含：分享、收藏
     */
    SOCIAL("社交类", "Social"),

    /**
     * 搜索类 (Search Category)
     * 包含：搜索
     */
    SEARCH("搜索类", "Search");

    private final String zhName;
    private final String enName;

    SignalCategory(String zhName, String enName) {
        this.zhName = zhName;
        this.enName = enName;
    }

    public String getZhName() {
        return zhName;
    }

    public String getEnName() {
        return enName;
    }

    /**
     * 根据信号类型获取类别 (Get Category by Signal Type)
     *
     * @param signalType 信号类型
     * @return 信号类别
     */
    public static SignalCategory fromSignalType(SignalType signalType) {
        return switch (signalType) {
            case VIEW, DWELL -> VIEW;
            case COPY, CLICK -> INTERACTION;
            case LIKE, DISLIKE, COMMENT -> FEEDBACK;
            case SHARE, BOOKMARK -> SOCIAL;
            case SEARCH -> SEARCH;
        };
    }
}

