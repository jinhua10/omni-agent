package top.yumbo.ai.omni.web.controller.router;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.knowledge.registry.router.DomainRouter;
import top.yumbo.ai.omni.knowledge.registry.router.QueryRouteResult;

/**
 * 领域路由器控制器
 *
 * <p>提供查询路由 API</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/router")
@RequiredArgsConstructor
public class DomainRouterController {

    private final DomainRouter domainRouter;

    /**
     * 路由查询到合适的知识域
     *
     * @param request 路由请求
     * @return 路由结果
     */
    @PostMapping("/route")
    public ResponseEntity<QueryRouteResult> route(@RequestBody RouteRequest request) {
        try {
            QueryRouteResult result = domainRouter.route(request.query());
            return ResponseEntity.ok(result);
        } catch (Exception e) {
            log.error("路由查询失败", e);
            throw e;
        }
    }

    /**
     * 路由请求
     */
    public record RouteRequest(String query) {
    }
}






