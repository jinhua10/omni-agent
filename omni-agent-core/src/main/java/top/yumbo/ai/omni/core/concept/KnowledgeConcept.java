package top.yumbo.ai.omni.core.concept;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;
import java.util.Map;

/**
 * 知识概念 - 知识的最小单元
 * (Knowledge Concept - Minimal Unit of Knowledge)
 *
 * <p>
 * 概念是知识的最小表示单元，具有以下特征：
 * - 原子性：不可再分的知识单元
 * - 可组合：可以与其他概念组合形成更复杂的知识
 * - 有关系：与其他概念之间存在多种关系
 * </p>
 *
 * <p>
 * 示例概念：
 * - "Spring Boot" - 技术概念
 * - "依赖注入" - 设计模式概念
 * - "配置" - 操作概念
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeConcept {

    /**
     * 概念唯一标识
     */
    private String conceptId;

    /**
     * 概念名称
     */
    private String name;

    /**
     * 概念类型
     */
    private ConceptType type;

    /**
     * 概念描述
     */
    private String description;

    /**
     * 概念的关键词/别名
     */
    private List<String> keywords;

    /**
     * 概念的重要性权重 (0.0 - 1.0)
     */
    private double importance;

    /**
     * 概念出现的文档ID列表
     */
    private List<String> documentIds;

    /**
     * 概念的属性 (key-value pairs)
     */
    private Map<String, Object> properties;

    /**
     * 与其他概念的关系
     */
    private List<ConceptRelation> relations;

    /**
     * 概念的嵌入向量（用于语义相似度计算）
     */
    private float[] embedding;

    /**
     * 创建时间
     */
    private long createdAt;

    /**
     * 最后更新时间
     */
    private long updatedAt;

    /**
     * 概念类型枚举
     */
    public enum ConceptType {
        /** 技术概念（如：Spring Boot, React） */
        TECHNOLOGY,

        /** 设计模式概念（如：单例模式，MVC） */
        PATTERN,

        /** 操作概念（如：配置，部署） */
        OPERATION,

        /** 领域概念（如：用户管理，订单处理） */
        DOMAIN,

        /** 问题概念（如：性能优化，错误处理） */
        PROBLEM,

        /** 解决方案概念（如：缓存策略，重试机制） */
        SOLUTION,

        /** 其他概念 */
        OTHER
    }

    /**
     * 概念关系
     */
    @Data
    @Builder
    @NoArgsConstructor
    @AllArgsConstructor
    public static class ConceptRelation {
        /** 关系类型 */
        private RelationType type;

        /** 目标概念ID */
        private String targetConceptId;

        /** 关系强度 (0.0 - 1.0) */
        private double strength;

        /** 关系描述 */
        private String description;
    }

    /**
     * 关系类型枚举
     */
    public enum RelationType {
        /** 是...的一部分（如：Spring MVC 是 Spring Framework 的一部分） */
        PART_OF,

        /** 是...的子类/子集（如：ArrayList 是 List 的子类） */
        IS_A,

        /** 依赖于（如：Spring Boot 依赖于 Spring Framework） */
        DEPENDS_ON,

        /** 用于/应用于（如：缓存 用于 性能优化） */
        USED_FOR,

        /** 相关/关联（如：微服务 相关于 分布式系统） */
        RELATED_TO,

        /** 对比/替代（如：MySQL 对比 PostgreSQL） */
        ALTERNATIVE_TO,

        /** 前置条件（如：部署 需要 配置） */
        PREREQUISITE_OF,

        /** 导致/引起（如：内存泄漏 导致 性能问题） */
        CAUSES,

        /** 解决（如：连接池 解决 连接管理问题） */
        SOLVES
    }
}


