package top.yumbo.ai.omni.storage.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;

/**
 * 分页请求参数
 * (Pagination Request)
 *
 * @author OmniAgent Team
 * @since 1.0.1
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PageRequest implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 页码（从0开始）
     */
    @Builder.Default
    private int page = 0;

    /**
     * 每页大小
     */
    @Builder.Default
    private int size = 20;

    /**
     * 排序字段
     */
    private String sortBy;

    /**
     * 排序方向（ASC/DESC）
     */
    @Builder.Default
    private String sortDirection = "DESC";

    /**
     * 计算偏移量
     */
    public int getOffset() {
        return page * size;
    }

    /**
     * 获取限制数量
     */
    public int getLimit() {
        return size;
    }

    /**
     * 创建默认分页请求
     */
    public static PageRequest of(int page, int size) {
        return PageRequest.builder()
                .page(page)
                .size(size)
                .build();
    }

    /**
     * 创建第一页
     */
    public static PageRequest firstPage(int size) {
        return of(0, size);
    }
}

