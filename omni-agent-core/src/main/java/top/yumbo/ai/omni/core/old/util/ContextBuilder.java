package top.yumbo.ai.omni.core.util;

import top.yumbo.ai.omni.rag.model.SearchResult;

import java.util.List;

/**
 * 上下文构建工具类
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
public class ContextBuilder {

    /**
     * 构建RAG上下文
     *
     * @param searchResults 搜索结果列表
     * @return 格式化的上下文字符串
     */
    public static String buildContext(List<SearchResult> searchResults) {
        if (searchResults == null || searchResults.isEmpty()) {
            return "";
        }

        StringBuilder context = new StringBuilder();
        for (int i = 0; i < searchResults.size(); i++) {
            SearchResult sr = searchResults.get(i);
            if (sr.getDocument() != null) {
                context.append(String.format("[参考%d] %s\n", i + 1, sr.getDocument().getContent()));
            }
        }
        return context.toString();
    }

    /**
     * 构建角色上下文
     *
     * @param searchResults 搜索结果列表
     * @return 格式化的角色上下文字符串
     */
    public static String buildRoleContext(List<SearchResult> searchResults) {
        if (searchResults == null || searchResults.isEmpty()) {
            return "";
        }

        StringBuilder context = new StringBuilder();
        for (int i = 0; i < searchResults.size(); i++) {
            SearchResult sr = searchResults.get(i);
            if (sr.getDocument() != null) {
                context.append(String.format("[知识%d] %s\n", i + 1, sr.getDocument().getContent()));
            }
        }
        return context.toString();
    }
}

