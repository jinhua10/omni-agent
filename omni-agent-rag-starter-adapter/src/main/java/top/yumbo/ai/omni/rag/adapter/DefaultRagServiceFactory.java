package top.yumbo.ai.omni.rag.adapter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.RagServiceFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * RAG 服务工厂实现
 *
 * <p>管理不同知识域的 RAG 服务实例</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class DefaultRagServiceFactory implements RagServiceFactory {

    /**
     * 域 RAG 服务缓存
     */
    private final Map<String, RagService> ragServiceCache = new ConcurrentHashMap<>();

    /**
     * RAG 配置
     */
    private final RagAdapterProperties properties;

    public DefaultRagServiceFactory(RagAdapterProperties properties) {
        this.properties = properties;
    }

    @Override
    public RagService getOrCreateRagService(String domainId) {
        return ragServiceCache.computeIfAbsent(domainId, this::createRagService);
    }

    @Override
    public boolean hasRagService(String domainId) {
        return ragServiceCache.containsKey(domainId);
    }

    @Override
    public void removeRagService(String domainId) {
        RagService removed = ragServiceCache.remove(domainId);
        if (removed != null) {
            log.info("移除域 {} 的 RAG 服务", domainId);
        }
    }

    /**
     * 创建 RAG 服务实例
     */
    private RagService createRagService(String domainId) {
        log.info("为域 {} 创建 RAG 服务", domainId);

        // 根据配置选择不同的 RAG 实现
        String ragType = properties.getType();

        return switch (ragType) {
            case "file" -> new FileBasedRagService(domainId, properties);
            case "elasticsearch" -> new ElasticsearchRagService(domainId, properties);
            case "mock" -> new MockRagService(domainId);
            default -> throw new IllegalArgumentException("不支持的 RAG 类型: " + ragType);
        };
    }
}

