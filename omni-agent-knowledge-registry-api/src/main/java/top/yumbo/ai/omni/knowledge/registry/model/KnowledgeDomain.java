package top.yumbo.ai.omni.knowledge.registry.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serial;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

/**
 * 知识域实体
 *
 * <p>知识域是知识网络中的基本单元，每个域拥有独立的：</p>
 * <ul>
 *     <li>向量空间（独立的RAG索引）</li>
 *     <li>存储空间（独立的文档存储）</li>
 *     <li>配置策略（独立的处理配置）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeDomain implements Serializable {

    @Serial
    private static final long serialVersionUID = 1L;

    /**
     * 域ID（主键）
     */
    private String domainId;

    /**
     * 域名称
     */
    private String domainName;

    /**
     * 域类型
     */
    private DomainType domainType;

    /**
     * 描述
     */
    private String description;

    /**
     * 存储路径（文件系统路径或逻辑路径）
     */
    private String storagePath;

    /**
     * RAG索引路径
     */
    private String ragIndexPath;

    /**
     * 配置信息（灵活的键值对）
     */
    @Builder.Default
    private Map<String, Object> config = new HashMap<>();

    /**
     * 状态
     */
    @Builder.Default
    private DomainStatus status = DomainStatus.ACTIVE;

    /**
     * 关联的实体ID（角色ID或项目ID）
     */
    private String linkedEntityId;

    /**
     * 创建时间
     */
    private LocalDateTime createdAt;

    /**
     * 更新时间
     */
    private LocalDateTime updatedAt;

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
            status = DomainStatus.ACTIVE;
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

