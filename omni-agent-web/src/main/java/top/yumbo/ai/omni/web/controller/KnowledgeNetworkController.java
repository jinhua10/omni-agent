package top.yumbo.ai.omni.web.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.knowledge.registry.network.KnowledgeAssociationService;
import top.yumbo.ai.omni.knowledge.registry.service.query.CrossDomainQueryService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 知识网络控制器
 * (Knowledge Network Controller)
 *
 * <p>提供跨域查询和知识关联的 REST API</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/knowledge-network")
@CrossOrigin(origins = "*")
@RequiredArgsConstructor
@Tag(name = "Knowledge Network", description = "知识网络API - 跨域查询和知识关联")
public class KnowledgeNetworkController {

    private final CrossDomainQueryService crossDomainQueryService;
    private final KnowledgeAssociationService associationService;

    /**
     * 跨域查询
     *
     * @param request 查询请求
     * @return 查询结果
     */
    @PostMapping("/cross-domain-search")
    @Operation(summary = "跨域查询", description = "在多个知识域中联合搜索")
    public Map<String, Object> crossDomainSearch(@RequestBody CrossDomainSearchRequest request) {
        log.info("跨域查询: query={}", request.getQuery());

        int maxResults = request.getMaxResults() != null ? request.getMaxResults() : 10;

        var result = crossDomainQueryService.crossDomainSearch(request.getQuery(), maxResults);

        Map<String, Object> response = new HashMap<>();
        response.put("query", result.getQuery());
        response.put("totalDomains", result.getTotalDomains());
        response.put("queriedDomains", result.getQueriedDomains());
        response.put("results", result.getResults());
        response.put("resultCount", result.getResults().size());
        response.put("queryTime", result.getQueryTime());
        response.put("routeConfidence", result.getRouteConfidence());
        response.put("isCrossDomain", result.isCrossDomain());

        return response;
    }

    /**
     * 查找相关域
     *
     * @param domainId 域ID
     * @param topK 返回Top K个相关域
     * @return 相关域列表
     */
    @GetMapping("/domains/{domainId}/related")
    @Operation(summary = "查找相关域", description = "发现与指定域相关的其他知识域")
    public Map<String, Object> findRelatedDomains(
            @PathVariable String domainId,
            @RequestParam(defaultValue = "5") int topK) {

        log.info("查找相关域: domainId={}, topK={}", domainId, topK);

        List<KnowledgeAssociationService.DomainAssociation> associations =
                associationService.findRelatedDomains(domainId, topK);

        Map<String, Object> response = new HashMap<>();
        response.put("sourceDomainId", domainId);
        response.put("relatedDomains", associations);
        response.put("count", associations.size());

        return response;
    }

    /**
     * 推荐知识域
     *
     * @param query 查询文本
     * @param topK 返回Top K个推荐
     * @return 推荐域列表
     */
    @GetMapping("/recommendations")
    @Operation(summary = "推荐知识域", description = "基于查询推荐相关的知识域")
    public Map<String, Object> recommendDomains(
            @RequestParam String query,
            @RequestParam(defaultValue = "5") int topK) {

        log.info("推荐知识域: query={}, topK={}", query, topK);

        List<KnowledgeAssociationService.DomainRecommendation> recommendations =
                associationService.recommendDomains(query, topK);

        Map<String, Object> response = new HashMap<>();
        response.put("query", query);
        response.put("recommendations", recommendations);
        response.put("count", recommendations.size());

        return response;
    }

    /**
     * 跨域搜索请求
     */
    @lombok.Data
    public static class CrossDomainSearchRequest {
        /** 查询文本 */
        private String query;

        /** 每个域的最大结果数 */
        private Integer maxResults;

        /** 指定要搜索的域（可选，不指定则自动路由） */
        private List<String> domainIds;
    }
}

