package top.yumbo.ai.omni.knowledge.registry.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 知识角色实体
 *
 * <p>知识角色是具有特定职责的智能助手，拥有专属的知识库</p>
 * <p>角色可以从多个知识域学习知识，并提供专业化的服务</p>
 *
 * <p>例如：</p>
 * <ul>
 *     <li>安全分析师 - 分析代码安全漏洞</li>
 *     <li>架构师 - 评估系统架构设计</li>
 *     <li>代码审查员 - 审查代码质量</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeRole implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 角色ID（主键）
     */
    private String roleId;

    /**
     * 角色名称
     */
    private String roleName;

    /**
     * 角色描述
     */
    private String description;

    /**
     * 角色职责（用于知识筛选和学习）
     */
    private String responsibilities;

    /**
     * 关联的知识域ID（角色专属的知识库）
     */
    private String knowledgeDomainId;

    /**
     * 学习源域ID列表（从哪些域学习知识）
     */
    @Builder.Default
    private List<String> sourceDomainIds = new ArrayList<>();

    /**
     * 角色状态
     */
    @Builder.Default
    private RoleStatus status = RoleStatus.ACTIVE;

    /**
     * 学习进度（0-100）
     */
    @Builder.Default
    private Integer learningProgress = 0;

    /**
     * 配置信息（AI模型、提示词等）
     */
    @Builder.Default
    private Map<String, Object> config = new HashMap<>();

    /**
     * 创建时间
     */
    private LocalDateTime createdAt;

    /**
     * 更新时间
     */
    private LocalDateTime updatedAt;

    /**
     * 最后学习时间
     */
    private LocalDateTime lastLearnedAt;

    /**
     * 创建前设置默认值
     */
    public void prePersist() {
        if (createdAt == null) {
            createdAt = LocalDateTime.now();
        }
        if (updatedAt == null) {
            updatedAt = LocalDateTime.now();
        }
        if (status == null) {
            status = RoleStatus.ACTIVE;
        }
        if (learningProgress == null) {
            learningProgress = 0;
        }
        if (sourceDomainIds == null) {
            sourceDomainIds = new ArrayList<>();
        }
        if (config == null) {
            config = new HashMap<>();
        }
    }

    /**
     * 更新前设置更新时间
     */
    public void preUpdate() {
        updatedAt = LocalDateTime.now();
    }
}

