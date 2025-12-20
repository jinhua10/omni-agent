package top.yumbo.ai.omni.workflow.api;

import lombok.Data;
import lombok.Builder;

/**
 * 评分请求
 */
@Data
@Builder
public class RatingRequest {
    private Integer rating;  // 1-5星
    private String comment;
}

