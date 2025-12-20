package top.yumbo.ai.omni.web.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.Getter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.web.config.FileWatcherConfig;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/**
 * 配置持久化服务
 *
 * 负责读写 data/config/ 目录下的配置文件
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class ConfigPersistenceService {

    private static final String CONFIG_DIR = "./data/config";
    private static final String FILE_WATCHER_CONFIG_FILE = "file-watcher-config.json";

    private final ObjectMapper objectMapper = new ObjectMapper();

    // ⭐ 从 application.yml 读取配置
    @Value("${omni-agent.file-watcher.enabled:true}")
    private boolean fileWatcherEnabled;

    @Value("${omni-agent.file-watcher.auto-index:true}")
    private boolean fileWatcherAutoIndex;

    @Value("${omni-agent.file-watcher.watch-directory:./data/documents}")
    private String fileWatcherDirectory;
    /**
     * -- GETTER --
     *  获取配置目录路径
     */
    @Getter
    private final Path configDir;
    private final Path fileWatcherConfigPath;

    public ConfigPersistenceService() {
        this.configDir = Paths.get(CONFIG_DIR);
        this.fileWatcherConfigPath = configDir.resolve(FILE_WATCHER_CONFIG_FILE);
        initConfigDir();
    }

    /**
     * 初始化配置目录
     */
    private void initConfigDir() {
        try {
            if (!Files.exists(configDir)) {
                Files.createDirectories(configDir);
                log.info("✅ 创建配置目录: {}", configDir.toAbsolutePath());
            }
        } catch (IOException e) {
            log.error("❌ 创建配置目录失败", e);
        }
    }

    /**
     * 加载文件监听配置
     */
    public FileWatcherConfig loadFileWatcherConfig() {
        try {
            if (Files.exists(fileWatcherConfigPath)) {
                String json = Files.readString(fileWatcherConfigPath);
                FileWatcherConfig config = objectMapper.readValue(json, FileWatcherConfig.class);
                log.info("✅ 加载文件监听配置: autoIndex={}, enabled={}",
                        config.getAutoIndex(), config.getEnabled());
                return config;
            } else {
                log.info("ℹ️ 配置文件不存在，使用默认配置");
                return createDefaultConfig();
            }
        } catch (IOException e) {
            log.error("❌ 加载配置失败，使用默认配置", e);
            return createDefaultConfig();
        }
    }

    /**
     * 保存文件监听配置
     */
    public boolean saveFileWatcherConfig(FileWatcherConfig config) {
        try {
            config.setLastUpdated(System.currentTimeMillis());
            String json = objectMapper.writerWithDefaultPrettyPrinter()
                    .writeValueAsString(config);
            Files.writeString(fileWatcherConfigPath, json);
            log.info("✅ 保存文件监听配置成功: {}", fileWatcherConfigPath.toAbsolutePath());
            return true;
        } catch (IOException e) {
            log.error("❌ 保存配置失败", e);
            return false;
        }
    }

    /**
     * 创建默认配置（从 application.yml 读取）⭐
     */
    private FileWatcherConfig createDefaultConfig() {
        FileWatcherConfig config = FileWatcherConfig.builder()
                .enabled(fileWatcherEnabled)        // ⭐ 从 application.yml 读取
                .autoIndex(fileWatcherAutoIndex)    // ⭐ 从 application.yml 读取
                .watchDirectory(fileWatcherDirectory)  // ⭐ 从 application.yml 读取
                .lastUpdated(System.currentTimeMillis())
                .version("1.0")
                .build();

        log.info("🔧 创建默认配置: enabled={}, autoIndex={}, watchDirectory={}",
                config.getEnabled(), config.getAutoIndex(), config.getWatchDirectory());

        // 保存默认配置
        saveFileWatcherConfig(config);

        return config;
    }

}

