package top.yumbo.ai.persistence.file;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * File 持久化配置属性
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@ConfigurationProperties(prefix = "omni.persistence.file")
public class FilePersistenceProperties {
    
    /**
     * 数据存储目录
     */
    private String dataDir = "./data/persistence";
    
    /**
     * 备份存储目录
     */
    private String backupDir = "./data/persistence/backups";
    
    /**
     * 是否自动保存
     */
    private boolean autoSave = true;
    
    /**
     * 最大变更历史记录数
     */
    private int maxHistorySize = 1000;
}
