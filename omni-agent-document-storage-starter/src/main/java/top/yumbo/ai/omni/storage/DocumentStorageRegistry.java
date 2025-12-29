package top.yumbo.ai.omni.storage;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;

import java.util.Map;
import java.util.Optional;
import java.util.Set;

/**
 * 文档存储服务注册表
 *
 * <p>管理和访问多个文档存储服务实例</p>
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class DocumentStorageRegistry {

    private final Map<String, DocumentStorageService> services;

    public DocumentStorageRegistry(Map<String, DocumentStorageService> services) {
        this.services = services;
        log.info("✅ 文档存储注册表初始化完成，共 {} 个实例", services.size());
    }

    /**
     * 获取指定 ID 的存储服务
     *
     * @param instanceId 实例 ID
     * @return 存储服务
     */
    public Optional<DocumentStorageService> getService(String instanceId) {
        return Optional.ofNullable(services.get(instanceId));
    }

    /**
     * 获取指定 ID 的存储服务（如果不存在则抛出异常）
     *
     * @param instanceId 实例 ID
     * @return 存储服务
     * @throws IllegalArgumentException 如果实例不存在
     */
    public DocumentStorageService getServiceOrThrow(String instanceId) {
        DocumentStorageService service = services.get(instanceId);
        if (service == null) {
            throw new IllegalArgumentException("文档存储实例不存在: " + instanceId);
        }
        return service;
    }

    /**
     * 获取所有实例 ID
     *
     * @return 实例 ID 集合
     */
    public Set<String> getInstanceIds() {
        return services.keySet();
    }

    /**
     * 获取所有存储服务
     *
     * @return 存储服务 Map
     */
    public Map<String, DocumentStorageService> getAllServices() {
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


