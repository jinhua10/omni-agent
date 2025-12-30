package top.yumbo.ai.omni.storage.api.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.util.Collections;
import java.util.List;

/**
 * 分页响应结果
 * (Paginated Response)
 *
 * @author OmniAgent Team
 * @since 1.0.1
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class PageResult<T> implements Serializable {
    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 当前页数据
     */
    private List<T> content;

    /**
     * 当前页码（从0开始）
     */
    private int page;

    /**
     * 每页大小
     */
    private int size;

    /**
     * 总记录数
     */
    private long totalElements;

    /**
     * 总页数
     */
    private int totalPages;

    /**
     * 是否有下一页
     */
    private boolean hasNext;

    /**
     * 是否有上一页
     */
    private boolean hasPrevious;

    /**
     * 是否是第一页
     */
    private boolean isFirst;

    /**
     * 是否是最后一页
     */
    private boolean isLast;

    /**
     * 创建分页结果
     *
     * @param content 数据列表
     * @param pageRequest 分页请求
     * @param totalElements 总记录数
     * @param <T> 数据类型
     * @return 分页结果
     */
    public static <T> PageResult<T> of(List<T> content, PageRequest pageRequest, long totalElements) {
        int page = pageRequest.getPage();
        int size = pageRequest.getSize();
        int totalPages = (int) Math.ceil((double) totalElements / size);

        return PageResult.<T>builder()
                .content(content != null ? content : Collections.emptyList())
                .page(page)
                .size(size)
                .totalElements(totalElements)
                .totalPages(totalPages)
                .hasNext(page < totalPages - 1)
                .hasPrevious(page > 0)
                .isFirst(page == 0)
                .isLast(page >= totalPages - 1)
                .build();
    }

    /**
     * 创建空的分页结果
     */
    public static <T> PageResult<T> empty(PageRequest pageRequest) {
        return of(Collections.emptyList(), pageRequest, 0);
    }

    /**
     * 获取当前页的数据数量
     */
    public int getNumberOfElements() {
        return content != null ? content.size() : 0;
    }
}

