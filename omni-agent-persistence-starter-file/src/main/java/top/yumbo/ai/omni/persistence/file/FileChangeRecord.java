package top.yumbo.ai.omni.persistence.file;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence;

import java.util.Map;

/**
 * 文件持久化变更记录
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class FileChangeRecord implements QuestionClassifierPersistence.ChangeRecord {
    
    private String id;
    private String typeId;
    private String action;
    private String operator;
    private long timestamp;
    private Map<String, Object> details;
    
    @Override
    public String getId() {
        return id;
    }
    
    @Override
    public String getTypeId() {
        return typeId;
    }
    
    @Override
    public String getAction() {
        return action;
    }
    
    @Override
    public String getOperator() {
        return operator;
    }
    
    @Override
    public long getTimestamp() {
        return timestamp;
    }
    
    @Override
    public Map<String, Object> getDetails() {
        return details;
    }
}
