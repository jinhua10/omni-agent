package top.yumbo.ai.omni.core.service.rag;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.rag.RagService;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * RAG 服务工厂
 * (RAG Service Factory)
 *
 * <p>管理多个知识域的 RAG 服务实例</p>
 * <p>支持域隔离和按需创建</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class RAGServiceFactory {

    @Autowired(required = false)
    private RagService defaultRagService;

    @Autowired
    private ApplicationContext applicationContext;

    /**
     * 域RAG服务缓存
     * Key: 域ID (domainId)
     * Value: RAG服务实例
     */
    private final Map<String, RagService> domainRAGServices = new ConcurrentHashMap<>();

    /**
     * 获取或创建域的 RAG 服务
     *
     * @param domainId 域ID
     * @return RAG服务实例
     */
    public RagService getOrCreateRAGService(String domainId) {
        if (domainId == null || domainId.isEmpty()) {
            log.warn("域ID为空，使用默认RAG服务");
            return getDefaultRAGService();
        }

        return domainRAGServices.computeIfAbsent(domainId, id -> {
            log.info("为域 {} 创建新的RAG服务实例", id);
            // 目前共享同一个RAG服务实例
            // TODO: 未来可以根据域配置创建独立的RAG实例
            return getDefaultRAGService();
        });
    }

    /**
     * 获取默认 RAG 服务
     *
     * @return 默认RAG服务
     */
    public RagService getDefaultRAGService() {
        if (defaultRagService == null) {
            throw new IllegalStateException(
                    "未配置RAG服务！请在application.yml中启用RAG实现（file/h2/redis/mongodb/elasticsearch）");
        }
        return defaultRagService;
    }

    /**
     * 检查域是否已有 RAG 服务
     *
     * @param domainId 域ID
     * @return 是否存在
     */
    public boolean hasDomainRAGService(String domainId) {
        return domainRAGServices.containsKey(domainId);
    }

    /**
     * 移除域的 RAG 服务
     *
     * @param domainId 域ID
     */
    public void removeDomainRAGService(String domainId) {
        RagService removed = domainRAGServices.remove(domainId);
        if (removed != null) {
            log.info("已移除域 {} 的RAG服务", domainId);
        }
    }

    /**
     * 清空所有域的 RAG 服务缓存
     */
    public void clearAll() {
        log.info("清空所有域的RAG服务缓存，共 {} 个域", domainRAGServices.size());
        domainRAGServices.clear();
    }

    /**
     * 获取当前管理的域数量
     *
     * @return 域数量
     */
    public int getDomainCount() {
        return domainRAGServices.size();
    }

    /**
     * 检查 RAG 服务是否可用
     *
     * @return 是否可用
     */
    public boolean isRAGServiceAvailable() {
        return defaultRagService != null;
    }
}


