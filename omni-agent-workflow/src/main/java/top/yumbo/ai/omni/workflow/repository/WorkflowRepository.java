package top.yumbo.ai.omni.workflow.repository;

import top.yumbo.ai.omni.workflow.market.MarketWorkflow;
import top.yumbo.ai.omni.workflow.market.WorkflowInstallation;
import top.yumbo.ai.omni.workflow.market.WorkflowRating;

import java.util.List;
import java.util.Optional;

/**
 * 工作流存储接口（可插拔）
 * (Workflow Repository Interface)
 *
 * <p>支持多种存储后端：File, SQLite, MongoDB, Elasticsearch</p>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
public interface WorkflowRepository {

    // ========== 基础 CRUD ==========

    /**
     * 保存工作流
     */
    String save(MarketWorkflow workflow);

    /**
     * 更新工作流
     */
    boolean update(MarketWorkflow workflow);

    /**
     * 删除工作流
     */
    boolean delete(String workflowId);

    /**
     * 根据ID查询
     */
    Optional<MarketWorkflow> findById(String workflowId);

    /**
     * 根据名称和版本查询
     */
    Optional<MarketWorkflow> findByNameAndVersion(String name, String version);

    /**
     * 查询所有版本
     */
    List<MarketWorkflow> findAllVersions(String name);

    /**
     * 查询最新版本
     */
    Optional<MarketWorkflow> findLatestVersion(String name);

    // ========== 市场查询 ==========

    /**
     * 查询所有公开工作流
     */
    List<MarketWorkflow> findPublic(int page, int size);

    /**
     * 按分类查询
     */
    List<MarketWorkflow> findByCategory(String category, int page, int size);

    /**
     * 按标签查询
     */
    List<MarketWorkflow> findByTag(String tag, int page, int size);

    /**
     * 按作者查询
     */
    List<MarketWorkflow> findByAuthor(String authorId, int page, int size);

    /**
     * 搜索（名称、描述、标签）
     */
    List<MarketWorkflow> search(String keyword, int page, int size);

    /**
     * 热门工作流（按下载量排序）
     */
    List<MarketWorkflow> findPopular(int limit);

    /**
     * 最新工作流
     */
    List<MarketWorkflow> findRecent(int limit);

    /**
     * 高评分工作流
     */
    List<MarketWorkflow> findTopRated(int limit);

    // ========== 统计更新 ==========

    /**
     * 增加下载次数
     */
    void incrementDownloadCount(String workflowId);

    /**
     * 增加安装次数
     */
    void incrementInstallCount(String workflowId);

    /**
     * 增加收藏次数
     */
    void incrementFavoriteCount(String workflowId);

    /**
     * 更新评分
     */
    void updateRating(String workflowId, double rating, long ratingCount);

    // ========== 评分和评论 ==========

    /**
     * 保存评分
     */
    String saveRating(WorkflowRating rating);

    /**
     * 查询工作流的评分
     */
    List<WorkflowRating> findRatings(String workflowId, int page, int size);

    /**
     * 查询用户的评分
     */
    Optional<WorkflowRating> findUserRating(String workflowId, String userId);

    // ========== 安装记录 ==========

    /**
     * 保存安装记录
     */
    String saveInstallation(WorkflowInstallation installation);

    /**
     * 查询用户已安装的工作流
     */
    List<WorkflowInstallation> findUserInstallations(String userId);

    /**
     * 检查是否已安装
     */
    boolean isInstalled(String workflowId, String userId);
}

