package top.yumbo.ai.omni.workflow.market;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.workflow.Workflow;
import top.yumbo.ai.omni.workflow.WorkflowRegistry;
import top.yumbo.ai.omni.workflow.repository.WorkflowRepository;

import java.util.List;
import java.util.Optional;
import java.util.UUID;

/**
 * 工作流市场服务
 * (Workflow Market Service)
 *
 * <p>提供工作流市场的核心功能：发布、搜索、下载、安装、评分等</p>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@Service
public class WorkflowMarketService {

    @Autowired(required = false)
    private WorkflowRepository workflowRepository;

    @Autowired
    private WorkflowRegistry workflowRegistry;

    /**
     * 发布工作流到市场
     *
     * @param workflow 工作流定义
     * @param authorId 作者ID
     * @param authorName 作者名称
     * @return 市场工作流ID
     */
    public String publishWorkflow(Workflow workflow, String authorId, String authorName) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        // 创建市场工作流
        MarketWorkflow marketWorkflow = MarketWorkflow.builder()
                .id(UUID.randomUUID().toString())
                .name(workflow.getName())
                .version(workflow.getVersion())
                .description(workflow.getDescription())
                .category(workflow.getTags() != null && !workflow.getTags().isEmpty()
                        ? workflow.getTags().get(0) : "general")
                .tags(workflow.getTags())
                .authorId(authorId)
                .authorName(authorName)
                .workflowDefinition(workflow)
                .status("published")
                .isPublic(true)
                .license("MIT")
                .createdAt(System.currentTimeMillis())
                .updatedAt(System.currentTimeMillis())
                .publishedAt(System.currentTimeMillis())
                .build();

        String id = workflowRepository.save(marketWorkflow);
        log.info("✅ 工作流已发布到市场: name={}, id={}, author={}",
                 workflow.getName(), id, authorName);

        return id;
    }

    /**
     * 从市场下载工作流
     *
     * @param workflowId 工作流ID
     * @param userId 用户ID
     * @return 工作流定义
     */
    public Workflow downloadWorkflow(String workflowId, String userId) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        Optional<MarketWorkflow> opt = workflowRepository.findById(workflowId);
        if (opt.isEmpty()) {
            throw new RuntimeException("工作流不存在: " + workflowId);
        }

        MarketWorkflow marketWorkflow = opt.get();

        // 增加下载次数
        workflowRepository.incrementDownloadCount(workflowId);

        log.info("⬇️ 用户下载工作流: user={}, workflow={}",
                 userId, marketWorkflow.getName());

        return marketWorkflow.getWorkflowDefinition();
    }

    /**
     * 安装工作流到本地
     *
     * @param workflowId 工作流ID
     * @param userId 用户ID
     * @return 是否成功
     */
    public boolean installWorkflow(String workflowId, String userId) {
        // 下载工作流
        Workflow workflow = downloadWorkflow(workflowId, userId);

        // 注册到本地
        workflowRegistry.register(workflow);

        // 记录安装
        WorkflowInstallation installation = WorkflowInstallation.builder()
                .id(UUID.randomUUID().toString())
                .workflowId(workflowId)
                .workflowVersion(workflow.getVersion())
                .userId(userId)
                .installedAt(System.currentTimeMillis())
                .enabled(true)
                .build();

        workflowRepository.saveInstallation(installation);
        workflowRepository.incrementInstallCount(workflowId);

        log.info("✅ 工作流已安装: user={}, workflow={}", userId, workflow.getName());
        return true;
    }

    /**
     * 搜索工作流
     *
     * @param keyword 关键词
     * @param page 页码
     * @param size 每页大小
     * @return 工作流列表
     */
    public List<MarketWorkflow> searchWorkflows(String keyword, int page, int size) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.search(keyword, page, size);
    }

    /**
     * 获取热门工作流
     *
     * @param limit 数量限制
     * @return 工作流列表
     */
    public List<MarketWorkflow> getPopularWorkflows(int limit) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.findPopular(limit);
    }

    /**
     * 获取最新工作流
     *
     * @param limit 数量限制
     * @return 工作流列表
     */
    public List<MarketWorkflow> getRecentWorkflows(int limit) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.findRecent(limit);
    }

    /**
     * 获取高评分工作流
     *
     * @param limit 数量限制
     * @return 工作流列表
     */
    public List<MarketWorkflow> getTopRatedWorkflows(int limit) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.findTopRated(limit);
    }

    /**
     * 评分工作流
     *
     * @param workflowId 工作流ID
     * @param userId 用户ID
     * @param userName 用户名称
     * @param rating 评分（1-5）
     * @param comment 评论
     * @return 是否成功
     */
    public boolean rateWorkflow(String workflowId, String userId, String userName,
                                int rating, String comment) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        if (rating < 1 || rating > 5) {
            throw new IllegalArgumentException("评分必须在 1-5 之间");
        }

        // 保存评分
        WorkflowRating workflowRating = WorkflowRating.builder()
                .id(UUID.randomUUID().toString())
                .workflowId(workflowId)
                .userId(userId)
                .userName(userName)
                .rating(rating)
                .comment(comment)
                .createdAt(System.currentTimeMillis())
                .build();

        workflowRepository.saveRating(workflowRating);

        // 重新计算平均分
        List<WorkflowRating> allRatings = workflowRepository.findRatings(
                workflowId, 0, Integer.MAX_VALUE);

        double avgRating = allRatings.stream()
                .mapToInt(WorkflowRating::getRating)
                .average()
                .orElse(0.0);

        workflowRepository.updateRating(workflowId, avgRating, allRatings.size());

        log.info("⭐ 用户评分: user={}, workflow={}, rating={}/5",
                 userId, workflowId, rating);

        return true;
    }

    /**
     * 获取工作流的评分列表
     *
     * @param workflowId 工作流ID
     * @param page 页码
     * @param size 每页大小
     * @return 评分列表
     */
    public List<WorkflowRating> getWorkflowRatings(String workflowId, int page, int size) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.findRatings(workflowId, page, size);
    }

    /**
     * 获取用户的工作流
     *
     * @param authorId 作者ID
     * @param page 页码
     * @param size 每页大小
     * @return 工作流列表
     */
    public List<MarketWorkflow> getUserWorkflows(String authorId, int page, int size) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.findByAuthor(authorId, page, size);
    }

    /**
     * 获取用户已安装的工作流
     *
     * @param userId 用户ID
     * @return 安装记录列表
     */
    public List<WorkflowInstallation> getUserInstallations(String userId) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.findUserInstallations(userId);
    }

    /**
     * 按分类获取工作流
     *
     * @param category 分类
     * @param page 页码
     * @param size 每页大小
     * @return 工作流列表
     */
    public List<MarketWorkflow> getWorkflowsByCategory(String category, int page, int size) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.findByCategory(category, page, size);
    }

    /**
     * 按标签获取工作流
     *
     * @param tag 标签
     * @param page 页码
     * @param size 每页大小
     * @return 工作流列表
     */
    public List<MarketWorkflow> getWorkflowsByTag(String tag, int page, int size) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.findByTag(tag, page, size);
    }

    /**
     * 获取工作流详情
     *
     * @param workflowId 工作流ID
     * @return 市场工作流
     */
    public Optional<MarketWorkflow> getWorkflowDetail(String workflowId) {
        if (workflowRepository == null) {
            throw new IllegalStateException("工作流市场未启用");
        }

        return workflowRepository.findById(workflowId);
    }
}

