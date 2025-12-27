package top.yumbo.ai.omni.knowledge.registry.memory;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;

import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 基于内存的知识注册表实现
 *
 * <p>使用 ConcurrentHashMap 存储知识域信息</p>
 * <p>适用于开发和测试环境，数据不持久化</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class MemoryKnowledgeRegistry implements KnowledgeRegistry {

    private final Map<String, KnowledgeDomain> domainStore = new ConcurrentHashMap<>();

    @Override
    public String saveDomain(KnowledgeDomain domain) {
        domain.prePersist();
        domainStore.put(domain.getDomainId(), domain);
        log.info("✅ 保存知识域到内存: {} ({})", domain.getDomainName(), domain.getDomainId());
        return domain.getDomainId();
    }

    @Override
    public Optional<KnowledgeDomain> findDomainById(String domainId) {
        return Optional.ofNullable(domainStore.get(domainId));
    }

    @Override
    public List<KnowledgeDomain> findAllDomains() {
        return List.copyOf(domainStore.values());
    }

    @Override
    public List<KnowledgeDomain> findDomainsByType(DomainType type) {
        return domainStore.values().stream()
                .filter(d -> d.getDomainType() == type)
                .collect(Collectors.toList());
    }

    @Override
    public List<KnowledgeDomain> findDomainsByStatus(DomainStatus status) {
        return domainStore.values().stream()
                .filter(d -> d.getStatus() == status)
                .collect(Collectors.toList());
    }

    @Override
    public List<KnowledgeDomain> findDomainsByLinkedEntity(String linkedEntityId) {
        return domainStore.values().stream()
                .filter(d -> linkedEntityId.equals(d.getLinkedEntityId()))
                .collect(Collectors.toList());
    }

    @Override
    public boolean updateDomain(KnowledgeDomain domain) {
        if (!domainStore.containsKey(domain.getDomainId())) {
            log.warn("域不存在，无法更新: {}", domain.getDomainId());
            return false;
        }

        domain.preUpdate();
        domainStore.put(domain.getDomainId(), domain);
        log.info("✅ 更新内存中的知识域: {} ({})", domain.getDomainName(), domain.getDomainId());
        return true;
    }

    @Override
    public boolean deleteDomain(String domainId) {
        KnowledgeDomain removed = domainStore.remove(domainId);
        if (removed != null) {
            log.info("✅ 从内存删除知识域: {}", domainId);
            return true;
        }
        return false;
    }

    @Override
    public boolean domainExists(String domainId) {
        return domainStore.containsKey(domainId);
    }

    @Override
    public long countDomains() {
        return domainStore.size();
    }

    @Override
    public long countDomainsByType(DomainType type) {
        return domainStore.values().stream()
                .filter(d -> d.getDomainType() == type)
                .count();
    }

    /**
     * 清空所有域（仅用于测试）
     */
    public void clear() {
        domainStore.clear();
        log.info("✅ 清空内存中的所有知识域");
    }
}

