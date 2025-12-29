package top.yumbo.ai.omni.web.config;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 文件监听配置
 *
 * 持久化存储在 data/config/file-watcher-config.json
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class FileWatcherConfig {

    /**
     * 是否启用文件监听
     */
    @JsonProperty("enabled")
    @Builder.Default
    private Boolean enabled = true;

    /**
     * 是否自动索引（文件变化时自动重新索引）
     */
    @JsonProperty("auto_index")
    @Builder.Default
    private Boolean autoIndex = false;

    /**
     * 监听的目录路径
     */
    @JsonProperty("watch_directory")
    @Builder.Default
    private String watchDirectory = "./data/documents";

    /**
     * 配置最后更新时间
     */
    @JsonProperty("last_updated")
    private Long lastUpdated;

    /**
     * 配置版本
     */
    @JsonProperty("version")
    @Builder.Default
    private String version = "1.0";
}






