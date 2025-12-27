package top.yumbo.ai.omni.knowledge.registry.elasticsearch;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.SearchHit;
import org.springframework.data.elasticsearch.core.query.Criteria;
import org.springframework.data.elasticsearch.core.query.CriteriaQuery;
import org.springframework.data.elasticsearch.core.query.Query;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.exception.KnowledgeRegistryException;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;

import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * 基于 Elasticsearch 的知识注册表实现
 *
 * <p>使用 Elasticsearch 存储知识域信息，支持全文搜索</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@RequiredArgsConstructor
public class ElasticsearchKnowledgeRegistry implements KnowledgeRegistry {

    private final ElasticsearchOperations elasticsearchOperations;
    private final String indexName;

    @Override
    public String saveDomain(KnowledgeDomain domain) {
        try {
            domain.prePersist();
            elasticsearchOperations.save(domain);
            log.info("✅ 保存知识域到 Elasticsearch: {} ({})", domain.getDomainName(), domain.getDomainId());
            return domain.getDomainId();
        } catch (Exception e) {
            log.error("保存知识域到 Elasticsearch 失败: {}", domain.getDomainId(), e);
            throw new KnowledgeRegistryException("Failed to save domain to Elasticsearch", e);
        }
    }

    @Override
    public Optional<KnowledgeDomain> findDomainById(String domainId) {
        try {
            KnowledgeDomain domain = elasticsearchOperations.get(domainId, KnowledgeDomain.class);
            return Optional.ofNullable(domain);
        } catch (Exception e) {
            log.error("从 Elasticsearch 查询知识域失败: {}", domainId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeDomain> findAllDomains() {
        try {
            Query query = Query.findAll();
            return elasticsearchOperations.search(query, KnowledgeDomain.class)
                    .stream()
                    .map(SearchHit::getContent)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("从 Elasticsearch 查询所有知识域失败", e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByType(DomainType type) {
        try {
            Criteria criteria = new Criteria("domainType").is(type.name());
            Query query = new CriteriaQuery(criteria);
            return elasticsearchOperations.search(query, KnowledgeDomain.class)
                    .stream()
                    .map(SearchHit::getContent)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("从 Elasticsearch 按类型查询知识域失败: {}", type, e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByStatus(DomainStatus status) {
        try {
            Criteria criteria = new Criteria("status").is(status.name());
            Query query = new CriteriaQuery(criteria);
            return elasticsearchOperations.search(query, KnowledgeDomain.class)
                    .stream()
                    .map(SearchHit::getContent)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("从 Elasticsearch 按状态查询知识域失败: {}", status, e);
            return List.of();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByLinkedEntity(String linkedEntityId) {
        try {
            Criteria criteria = new Criteria("linkedEntityId").is(linkedEntityId);
            Query query = new CriteriaQuery(criteria);
            return elasticsearchOperations.search(query, KnowledgeDomain.class)
                    .stream()
                    .map(SearchHit::getContent)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("从 Elasticsearch 按关联实体查询知识域失败: {}", linkedEntityId, e);
            return List.of();
        }
    }

    @Override
    public boolean updateDomain(KnowledgeDomain domain) {
        try {
            domain.preUpdate();
            elasticsearchOperations.save(domain);
            log.info("✅ 更新 Elasticsearch 中的知识域: {} ({})", domain.getDomainName(), domain.getDomainId());
            return true;
        } catch (Exception e) {
            log.error("更新 Elasticsearch 中的知识域失败: {}", domain.getDomainId(), e);
            return false;
        }
    }

    @Override
    public boolean deleteDomain(String domainId) {
        try {
            elasticsearchOperations.delete(domainId, KnowledgeDomain.class);
            log.info("✅ 从 Elasticsearch 删除知识域: {}", domainId);
            return true;
        } catch (Exception e) {
            log.error("从 Elasticsearch 删除知识域失败: {}", domainId, e);
            return false;
        }
    }

    @Override
    public boolean domainExists(String domainId) {
        try {
            return elasticsearchOperations.exists(domainId, KnowledgeDomain.class);
        } catch (Exception e) {
            log.error("检查 Elasticsearch 中知识域是否存在失败: {}", domainId, e);
            return false;
        }
    }

    @Override
    public long countDomains() {
        try {
            Query query = Query.findAll();
            return elasticsearchOperations.count(query, KnowledgeDomain.class);
        } catch (Exception e) {
            log.error("统计 Elasticsearch 中知识域数量失败", e);
            return 0;
        }
    }

    @Override
    public long countDomainsByType(DomainType type) {
        try {
            Criteria criteria = new Criteria("domainType").is(type.name());
            Query query = new CriteriaQuery(criteria);
            return elasticsearchOperations.count(query, KnowledgeDomain.class);
        } catch (Exception e) {
            log.error("统计 Elasticsearch 中指定类型知识域数量失败: {}", type, e);
            return 0;
        }
    }
}

