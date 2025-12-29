package top.yumbo.ai.omni.knowledge.registry.network;

import top.yumbo.ai.omni.knowledge.registry.model.RefinedKnowledge;

import java.util.List;

/**
 * 知识存储服务接口
 *
 * <p>负责将提炼的知识存储到对应的知识域</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface KnowledgeStorageService {

    /**
     * 存储单个知识到指定域
     *
     * @param knowledge 知识对象
     * @param domainId 目标域ID
     * @return 是否成功
     */
    boolean storeKnowledge(RefinedKnowledge knowledge, String domainId);

    /**
     * 批量存储知识
     *
     * @param knowledgeList 知识列表
     * @param domainId 目标域ID
     * @return 成功存储的数量
     */
    int batchStoreKnowledge(List<RefinedKnowledge> knowledgeList, String domainId);

    /**
     * 更新知识
     *
     * @param knowledge 知识对象
     * @param domainId 域ID
     * @return 是否成功
     */
    boolean updateKnowledge(RefinedKnowledge knowledge, String domainId);

    /**
     * 删除知识
     *
     * @param knowledgeId 知识ID
     * @param domainId 域ID
     * @return 是否成功
     */
    boolean deleteKnowledge(String knowledgeId, String domainId);

    /**
     * 查询知识
     *
     * @param knowledgeId 知识ID
     * @param domainId 域ID
     * @return 知识对象
     */
    RefinedKnowledge getKnowledge(String knowledgeId, String domainId);

    /**
     * 搜索知识
     *
     * @param query 查询字符串
     * @param domainId 域ID
     * @param maxResults 最大结果数
     * @return 知识列表
     */
    List<RefinedKnowledge> searchKnowledge(String query, String domainId, int maxResults);
}

