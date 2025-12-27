package top.yumbo.ai.omni.knowledge.registry;

import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;

import java.util.List;
import java.util.Optional;

/**
 * 知识注册表接口
 *
 * <p>用于存储和管理知识网络中的元数据，包括：</p>
 * <ul>
 *     <li>知识域 (knowledge-domain)</li>
 *     <li>知识角色 (knowledge-role) - 未来扩展</li>
 *     <li>源码项目 (source-project) - 未来扩展</li>
 * </ul>
 *
 * <p>实现方式：</p>
 * <ul>
 *     <li>FileKnowledgeRegistry - 基于JSON文件（默认）</li>
 *     <li>MongoKnowledgeRegistry - 基于MongoDB（可选）</li>
 *     <li>RedisKnowledgeRegistry - 基于Redis（可选）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface KnowledgeRegistry {

    // ========== 知识域管理 (Knowledge Domain Management) ==========

    /**
     * 保存知识域
     *
     * @param domain 知识域对象
     * @return 域ID
     */
    String saveDomain(KnowledgeDomain domain);

    /**
     * 查找知识域
     *
     * @param domainId 域ID
     * @return 知识域对象（Optional）
     */
    Optional<KnowledgeDomain> findDomainById(String domainId);

    /**
     * 列出所有知识域
     *
     * @return 知识域列表
     */
    List<KnowledgeDomain> findAllDomains();

    /**
     * 根据类型查找域
     *
     * @param type 域类型
     * @return 知识域列表
     */
    List<KnowledgeDomain> findDomainsByType(DomainType type);

    /**
     * 根据状态查找域
     *
     * @param status 域状态
     * @return 知识域列表
     */
    List<KnowledgeDomain> findDomainsByStatus(DomainStatus status);

    /**
     * 根据关联实体ID查找域
     *
     * @param linkedEntityId 关联实体ID
     * @return 知识域列表
     */
    List<KnowledgeDomain> findDomainsByLinkedEntity(String linkedEntityId);

    /**
     * 更新知识域
     *
     * @param domain 知识域对象
     * @return 是否成功
     */
    boolean updateDomain(KnowledgeDomain domain);

    /**
     * 删除知识域
     *
     * @param domainId 域ID
     * @return 是否成功
     */
    boolean deleteDomain(String domainId);

    // ========== 通用方法 (Common Methods) ==========

    /**
     * 检查域是否存在
     *
     * @param domainId 域ID
     * @return 是否存在
     */
    boolean domainExists(String domainId);

    /**
     * 统计域数量
     *
     * @return 总数量
     */
    long countDomains();

    /**
     * 统计指定类型的域数量
     *
     * @param type 域类型
     * @return 数量
     */
    long countDomainsByType(DomainType type);
}

