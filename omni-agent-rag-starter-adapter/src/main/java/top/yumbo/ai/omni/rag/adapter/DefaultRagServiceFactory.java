package top.yumbo.ai.omni.rag.adapter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.RagServiceFactory;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * RAG 服务工厂实现
 *
 * <p>管理不同知识域的 RAG 服务实例</p>
 * <p>通过 Spring 依赖注入获取实际的 RAG 服务实现，避免硬编码</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class DefaultRagServiceFactory implements RagServiceFactory {

    /**
     * 域 RAG 服务缓存
     * key: domainId, value: RagService
     */
    private final Map<String, RagService> ragServiceCache = new ConcurrentHashMap<>();

    /**
     * RAG 配置
     */
    private final RagAdapterProperties properties;

    /**
     * RAG 服务提供者（从 Spring 容器中获取）
     */
    private final ObjectProvider<RagService> ragServiceProvider;

    public DefaultRagServiceFactory(
            RagAdapterProperties properties,
            ObjectProvider<RagService> ragServiceProvider) {
        this.properties = properties;
        this.ragServiceProvider = ragServiceProvider;

        log.info("✅ RAG 服务工厂初始化完成");
        log.info("  - 配置类型: {}", properties.getType());
        log.info("  - 可用实现: {}", ragServiceProvider.stream().count());
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
            log.info("✅ 移除域 {} 的 RAG 服务", domainId);
        }
    }

    /**
     * 创建 RAG 服务实例
     *
     * <p>策略：</p>
     * <ul>
     *   <li>优先使用 Spring 容器中已注册的 RagService Bean</li>
     *   <li>如果容器中有多个实现，根据配置的 type 选择</li>
     *   <li>如果没有可用实现，返回 Mock 实现（开发阶段）</li>
     * </ul>
     */
    private RagService createRagService(String domainId) {
        log.info("📋 为域 {} 创建 RAG 服务", domainId);

        // 从 Spring 容器获取 RagService
        RagService ragService = ragServiceProvider.getIfAvailable();

        if (ragService == null) {
            log.warn("⚠️ 未找到 RagService 实现，请确保引入了对应的 starter 模块");
            log.warn("   可用模块：omni-agent-rag-starter-file, omni-agent-rag-starter-mongodb 等");

            // 返回一个简单的 Mock 实现
            return createMockRagService(domainId);
        }

        log.info("✅ 使用 RAG 服务: {} (域: {})",
                ragService.getClass().getSimpleName(), domainId);

        return ragService;
    }

    /**
     * 创建 Mock RAG 服务（用于开发和测试）
     */
    private RagService createMockRagService(String domainId) {
        log.info("🔧 创建 Mock RAG 服务 (域: {})", domainId);
        return new MockRagService(domainId);
    }
}

