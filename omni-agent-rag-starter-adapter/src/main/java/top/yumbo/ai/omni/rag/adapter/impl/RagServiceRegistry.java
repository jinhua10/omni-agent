package top.yumbo.ai.omni.rag.adapter.impl;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.rag.RagService;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * RAG 服务注册表
 *
 * <p>管理和访问多个 RAG 服务实例</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class RagServiceRegistry {

    private final Map<String, RagService> services;

    public RagServiceRegistry(Map<String, RagService> services) {
        this.services = services;
        log.info("✅ RAG 服务注册表初始化完成，共 {} 个实例", services.size());
    }

    /**
     * 获取指定 ID 的 RAG 服务
     *
     * @param instanceId 实例 ID
     * @return RAG 服务
     */
    public Optional<RagService> getService(String instanceId) {
        return Optional.ofNullable(services.get(instanceId));
    }

    /**
     * 获取指定 ID 的 RAG 服务（如果不存在则抛出异常）
     *
     * @param instanceId 实例 ID
     * @return RAG 服务
     * @throws IllegalArgumentException 如果实例不存在
     */
    public RagService getServiceOrThrow(String instanceId) {
        RagService service = services.get(instanceId);
        if (service == null) {
            throw new IllegalArgumentException("RAG 实例不存在: " + instanceId);
        }
        return service;
    }

    /**
     * 获取所有 RAG 服务实例 ID
     *
     * @return 实例 ID 集合
     */
    public Set<String> getInstanceIds() {
        return services.keySet();
    }

    /**
     * 获取所有 RAG 服务
     *
     * @return RAG 服务 Map
     */
    public Map<String, RagService> getAllServices() {
        return Map.copyOf(services);
    }

    /**
     * 检查实例是否存在
     *
     * @param instanceId 实例 ID
     * @return 是否存在
     */
    public boolean hasInstance(String instanceId) {
        return services.containsKey(instanceId);
    }

    /**
     * 获取实例数量
     *
     * @return 实例数量
     */
    public int size() {
        return services.size();
    }
}

