package top.yumbo.ai.omni.core.evolution;

import lombok.Builder;
import lombok.Data;

import java.util.Date;

/**
 * 概念版本模型 (Concept Version Model)
 * <p>
 * 记录概念的演化版本
 * (Records the evolution version of a concept)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@Data
@Builder
public class ConceptVersion {

    /**
     * 版本ID (Version ID)
     */
    private String versionId;

    /**
     * 概念ID (Concept ID)
     */
    private String conceptId;

    /**
     * 版本号 (Version number)
     */
    private int version;

    /**
     * 概念内容 (Concept content)
     */
    private String content;

    /**
     * 变更描述 (Change description)
     */
    private String changeDescription;

    /**
     * 变更类型 (Change type)
     */
    private ChangeType changeType;

    /**
     * 创建时间 (Create time)
     */
    private Date createTime;

    /**
     * 创建者 (Creator)
     */
    private String creator;

    /**
     * 父版本ID (Parent version ID)
     */
    private String parentVersionId;

    /**
     * 是否为当前版本 (Is current version)
     */
    private boolean current;

    /**
     * 变更类型枚举 (Change Type Enum)
     */
    public enum ChangeType {
        CREATE,    // 创建 (Create)
        UPDATE,    // 更新 (Update)
        MERGE,     // 合并 (Merge)
        SPLIT,     // 拆分 (Split)
        DELETE     // 删除 (Delete)
    }
}


