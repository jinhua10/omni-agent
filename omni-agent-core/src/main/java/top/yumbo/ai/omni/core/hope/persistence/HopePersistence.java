package top.yumbo.ai.omni.core.hope.persistence;

import top.yumbo.ai.omni.core.hope.model.QuestionTypeConfig;

import java.util.List;
import java.util.Optional;

/**
 * HOPE 系统持久化接口
 * 用于问题分类器的配置持久化
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface HopePersistence {

    // ========== 问题类型管理 ==========

    /**
     * 保存问题类型
     */
    boolean saveQuestionType(QuestionTypeConfig config);

    /**
     * 批量保存问题类型
     */
    int saveQuestionTypes(List<QuestionTypeConfig> configs);

    /**
     * 获取问题类型
     */
    Optional<QuestionTypeConfig> getQuestionType(String typeId);

    /**
     * 获取所有问题类型
     */
    List<QuestionTypeConfig> getAllQuestionTypes();

    /**
     * 更新问题类型
     */
    boolean updateQuestionType(QuestionTypeConfig config);

    /**
     * 删除问题类型
     */
    boolean deleteQuestionType(String typeId);

    // ========== 关键词管理 ==========

    /**
     * 保存类型关键词
     */
    boolean saveKeywords(String typeId, List<String> keywords);

    /**
     * 添加关键词
     */
    boolean addKeywords(String typeId, List<String> keywords);

    /**
     * 获取类型关键词
     */
    List<String> getKeywords(String typeId);

    /**
     * 删除关键词
     */
    boolean removeKeywords(String typeId, List<String> keywords);

    // ========== 模式管理 ==========

    /**
     * 保存类型模式
     */
    boolean savePatterns(String typeId, List<String> patterns);

    /**
     * 添加模式
     */
    boolean addPatterns(String typeId, List<String> patterns);

    /**
     * 获取类型模式
     */
    List<String> getPatterns(String typeId);

    /**
     * 删除模式
     */
    boolean removePatterns(String typeId, List<String> patterns);
}

