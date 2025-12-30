package top.yumbo.ai.omni.orchestrator.model;

import lombok.Getter;
import top.yumbo.ai.omni.rag.model.SearchResult;

import java.util.List;

/**
 * 分页结果
 * (Paged Result)
 *
 * <p>
 * 封装分页查询的结果信息
 * (Encapsulates paginated query result information)
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Getter
public class PagedResult {

    /**
     * 搜索结果
     * (Search result)
     */
    private final List<SearchResult> searchResults;

    /**
     * 当前页码
     * (Current page number)
     */
    private final int currentPage;

    /**
     * 每页大小
     * (Page size)
     */
    private final int pageSize;

    /**
     * 总结果数
     * (Total results)
     */
    private final int totalResults;

    /**
     * 总页数
     * (Total pages)
     */
    private final int totalPages;

    /**
     * 是否有下一页
     * (Whether there is a next page)
     */
    private final boolean hasNext;

    /**
     * 是否有上一页
     * (Whether there is a previous page)
     */
    private final boolean hasPrevious;

    /**
     * 构造函数
     * (Constructor)
     *
     * @param results 搜索结果列表 (Search result list)
     * @param currentPage 当前页码 (Current page number)
     * @param pageSize 每页大小 (Page size)
     */
    public PagedResult(List<SearchResult> results, int currentPage, int pageSize) {
        this.searchResults = results;
        this.currentPage = currentPage;
        this.pageSize = pageSize;
        this.totalResults = results.size();
        this.totalPages = (int) Math.ceil((double) totalResults / pageSize);
        this.hasNext = currentPage < totalPages - 1;
        this.hasPrevious = currentPage > 0;
    }

    /**
     * 获取当前页的结果
     * (Get results for current page)
     *
     * @return 当前页结果 (Current page results)
     */
    public List<SearchResult> getPageResults() {
        int fromIndex = currentPage * pageSize;
        int toIndex = Math.min(fromIndex + pageSize, totalResults);
        return searchResults.subList(fromIndex, toIndex);
    }
}


