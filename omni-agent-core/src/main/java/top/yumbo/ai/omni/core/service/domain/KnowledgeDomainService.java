package top.yumbo.ai.omni.core.service.domain;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.core.dto.domain.CreateDomainRequest;
import top.yumbo.ai.omni.core.dto.domain.UpdateDomainRequest;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

/**
 * 知识域管理服务
 *
 * <p>负责知识域的生命周期管理，包括创建、查询、更新、删除等操作</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class KnowledgeDomainService {

    @Autowired(required = false)
    private KnowledgeRegistry knowledgeRegistry;

    /**
     * 初始化后检查依赖
     */
    @jakarta.annotation.PostConstruct
    public void init() {
        if (knowledgeRegistry == null) {
            log.warn("⚠️ KnowledgeRegistry not available - KnowledgeDomainService will use fallback mode");
        } else {
            log.info("✅ KnowledgeDomainService initialized with KnowledgeRegistry");
        }
    }

    /**
     * 创建知识域
     *
     * @param request 创建请求
     * @return 创建的知识域
     */
    public KnowledgeDomain createDomain(CreateDomainRequest request) {
        log.info("创建知识域: {}", request.getDomainName());

        // 生成域ID
        String domainId = UUID.randomUUID().toString();

        // 构建存储路径
        String basePath = "data/knowledge-network/domains/" + domainId;
        String storagePath = basePath + "/storage";
        String ragIndexPath = basePath + "/rag-index";

        // 创建目录结构
        try {
            Files.createDirectories(Paths.get(storagePath + "/documents"));
            Files.createDirectories(Paths.get(storagePath + "/chunks"));
            Files.createDirectories(Paths.get(storagePath + "/extracted"));
            Files.createDirectories(Paths.get(ragIndexPath));
            log.info("✅ 创建域目录结构: {}", basePath);
        } catch (Exception e) {
            log.error("创建域目录失败", e);
            throw new RuntimeException("Failed to create domain directories", e);
        }

        // 创建域实体
        KnowledgeDomain domain = KnowledgeDomain.builder()
                .domainId(domainId)
                .domainName(request.getDomainName())
                .domainType(request.getDomainType())
                .description(request.getDescription())
                .storagePath(storagePath)
                .ragIndexPath(ragIndexPath)
                .config(request.getConfig())
                .status(DomainStatus.ACTIVE)
                .linkedEntityId(request.getLinkedEntityId())
                .build();

        // 保存到注册表
        knowledgeRegistry.saveDomain(domain);

        log.info("✅ 知识域创建成功: {} ({})", domain.getDomainName(), domainId);
        return domain;
    }

    /**
     * 获取知识域
     *
     * @param domainId 域ID
     * @return 知识域
     */
    public KnowledgeDomain getDomain(String domainId) {
        return knowledgeRegistry.findDomainById(domainId)
                .orElseThrow(() -> new RuntimeException("Domain not found: " + domainId));
    }

    /**
     * 列出所有知识域
     *
     * @return 知识域列表
     */
    public List<KnowledgeDomain> listAllDomains() {
        return knowledgeRegistry.findAllDomains();
    }

    /**
     * 根据类型列出知识域
     *
     * @param type 域类型
     * @return 知识域列表
     */
    public List<KnowledgeDomain> listDomainsByType(DomainType type) {
        return knowledgeRegistry.findDomainsByType(type);
    }

    /**
     * 根据状态列出知识域
     *
     * @param status 域状态
     * @return 知识域列表
     */
    public List<KnowledgeDomain> listDomainsByStatus(DomainStatus status) {
        return knowledgeRegistry.findDomainsByStatus(status);
    }

    /**
     * 更新知识域
     *
     * @param domainId 域ID
     * @param request 更新请求
     * @return 更新后的知识域
     */
    public KnowledgeDomain updateDomain(String domainId, UpdateDomainRequest request) {
        log.info("更新知识域: {}", domainId);

        KnowledgeDomain domain = getDomain(domainId);

        // 更新字段
        if (request.getDomainName() != null) {
            domain.setDomainName(request.getDomainName());
        }
        if (request.getDescription() != null) {
            domain.setDescription(request.getDescription());
        }
        if (request.getStatus() != null) {
            domain.setStatus(request.getStatus());
        }

        // 保存更新
        knowledgeRegistry.updateDomain(domain);

        log.info("✅ 知识域更新成功: {}", domainId);
        return domain;
    }

    /**
     * 删除知识域
     *
     * @param domainId 域ID
     */
    public void deleteDomain(String domainId) {
        log.info("删除知识域: {}", domainId);

        // 检查域是否存在
        if (!knowledgeRegistry.domainExists(domainId)) {
            throw new RuntimeException("Domain not found: " + domainId);
        }

        // 删除域
        knowledgeRegistry.deleteDomain(domainId);

        // TODO: 可选，删除文件系统中的数据

        log.info("✅ 知识域删除成功: {}", domainId);
    }

    /**
     * 统计知识域数量
     *
     * @return 总数量
     */
    public long countDomains() {
        return knowledgeRegistry.countDomains();
    }

    /**
     * 按类型统计知识域数量
     *
     * @param type 域类型
     * @return 数量
     */
    public long countDomainsByType(DomainType type) {
        return knowledgeRegistry.countDomainsByType(type);
    }
}


