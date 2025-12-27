package top.yumbo.ai.omni.web.controller.domain;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.core.dto.domain.CreateDomainRequest;
import top.yumbo.ai.omni.core.dto.domain.UpdateDomainRequest;
import top.yumbo.ai.omni.core.service.domain.KnowledgeDomainService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 知识域管理控制器
 *
 * <p>提供知识域的 CRUD 操作 API</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RestController
@RequestMapping("/api/knowledge-domains")
@RequiredArgsConstructor
public class KnowledgeDomainController {

    private final KnowledgeDomainService domainService;

    /**
     * 创建知识域
     *
     * @param request 创建请求
     * @return 创建的知识域
     */
    @PostMapping
    public ResponseEntity<KnowledgeDomain> createDomain(@RequestBody CreateDomainRequest request) {
        try {
            KnowledgeDomain domain = domainService.createDomain(request);
            return ResponseEntity.ok(domain);
        } catch (Exception e) {
            log.error("创建知识域失败", e);
            throw e;
        }
    }

    /**
     * 获取知识域详情
     *
     * @param domainId 域ID
     * @return 知识域详情
     */
    @GetMapping("/{domainId}")
    public ResponseEntity<KnowledgeDomain> getDomain(@PathVariable String domainId) {
        try {
            KnowledgeDomain domain = domainService.getDomain(domainId);
            return ResponseEntity.ok(domain);
        } catch (Exception e) {
            log.error("获取知识域失败: {}", domainId, e);
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * 列出所有知识域
     *
     * @param type 域类型（可选）
     * @param status 域状态（可选）
     * @return 知识域列表
     */
    @GetMapping
    public ResponseEntity<List<KnowledgeDomain>> listDomains(
            @RequestParam(required = false) DomainType type,
            @RequestParam(required = false) DomainStatus status) {
        try {
            List<KnowledgeDomain> domains;

            if (type != null) {
                domains = domainService.listDomainsByType(type);
            } else if (status != null) {
                domains = domainService.listDomainsByStatus(status);
            } else {
                domains = domainService.listAllDomains();
            }

            return ResponseEntity.ok(domains);
        } catch (Exception e) {
            log.error("列出知识域失败", e);
            throw e;
        }
    }

    /**
     * 更新知识域
     *
     * @param domainId 域ID
     * @param request 更新请求
     * @return 更新后的知识域
     */
    @PutMapping("/{domainId}")
    public ResponseEntity<KnowledgeDomain> updateDomain(
            @PathVariable String domainId,
            @RequestBody UpdateDomainRequest request) {
        try {
            KnowledgeDomain domain = domainService.updateDomain(domainId, request);
            return ResponseEntity.ok(domain);
        } catch (Exception e) {
            log.error("更新知识域失败: {}", domainId, e);
            return ResponseEntity.notFound().build();
        }
    }

    /**
     * 删除知识域
     *
     * @param domainId 域ID
     * @return 删除结果
     */
    @DeleteMapping("/{domainId}")
    public ResponseEntity<Map<String, Object>> deleteDomain(@PathVariable String domainId) {
        try {
            domainService.deleteDomain(domainId);

            Map<String, Object> response = new HashMap<>();
            response.put("success", true);
            response.put("message", "Domain deleted successfully");
            response.put("domainId", domainId);

            return ResponseEntity.ok(response);
        } catch (Exception e) {
            log.error("删除知识域失败: {}", domainId, e);

            Map<String, Object> response = new HashMap<>();
            response.put("success", false);
            response.put("message", e.getMessage());

            return ResponseEntity.badRequest().body(response);
        }
    }

    /**
     * 获取统计信息
     *
     * @return 统计信息
     */
    @GetMapping("/statistics")
    public ResponseEntity<Map<String, Object>> getStatistics() {
        try {
            Map<String, Object> stats = new HashMap<>();
            stats.put("totalDomains", domainService.countDomains());
            stats.put("documentDomains", domainService.countDomainsByType(DomainType.DOCUMENT));
            stats.put("sourceCodeDomains", domainService.countDomainsByType(DomainType.SOURCE_CODE));
            stats.put("roleKnowledgeDomains", domainService.countDomainsByType(DomainType.ROLE_KNOWLEDGE));

            return ResponseEntity.ok(stats);
        } catch (Exception e) {
            log.error("获取统计信息失败", e);
            throw e;
        }
    }
}



