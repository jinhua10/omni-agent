package top.yumbo.ai.omni.core.config;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Configuration;

import java.util.List;

/**
 * 媒体文件处理配置类
 * (Media Processing Configuration)
 *
 * <p>
 * Phase 3 预留配置，用于未来支持媒体文件处理
 * (Phase 3 reserved configuration for future media file processing support)
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.1.0 (Phase 3)
 */
@Data
@Configuration
@ConfigurationProperties(prefix = "omni-agent.media")
public class MediaProcessingConfig {

    /**
     * 是否启用媒体文件处理
     * (Whether media file processing is enabled)
     */
    private boolean enabled = false;

    /**
     * 视频处理配置
     * (Video processing configuration)
     */
    private VideoConfig video = new VideoConfig();

    /**
     * 音频处理配置
     * (Audio processing configuration)
     */
    private AudioConfig audio = new AudioConfig();

    /**
     * 多模态LLM配置
     * (Multi-modal LLM configuration)
     */
    private MultiModalConfig multiModal = new MultiModalConfig();

    /**
     * 存储配置
     * (Storage configuration)
     */
    private StorageConfig storage = new StorageConfig();

    /**
     * 性能配置
     * (Performance configuration)
     */
    private PerformanceConfig performance = new PerformanceConfig();

    /**
     * 视频配置
     */
    @Data
    public static class VideoConfig {
        /**
         * 最大文件大小（MB）
         */
        private int maxSize = 100;

        /**
         * 支持的视频格式
         */
        private List<String> supportedFormats = List.of("mp4", "avi", "mov", "mkv", "flv", "wmv");

        /**
         * 关键帧提取配置
         */
        private FrameExtractionConfig frameExtraction = new FrameExtractionConfig();

        /**
         * 字幕提取配置
         */
        private SubtitleExtractionConfig subtitleExtraction = new SubtitleExtractionConfig();
    }

    /**
     * 关键帧提取配置
     */
    @Data
    public static class FrameExtractionConfig {
        private boolean enabled = true;
        private int maxFrames = 10;
        private int frameInterval = 5; // seconds
    }

    /**
     * 字幕提取配置
     */
    @Data
    public static class SubtitleExtractionConfig {
        private boolean enabled = true;
        private List<String> supportedFormats = List.of("srt", "ass", "vtt");
    }

    /**
     * 音频配置
     */
    @Data
    public static class AudioConfig {
        /**
         * 最大文件大小（MB）
         */
        private int maxSize = 50;

        /**
         * 支持的音频格式
         */
        private List<String> supportedFormats = List.of("mp3", "wav", "m4a", "flac", "ogg", "aac");

        /**
         * 语音识别配置
         */
        private TranscriptionConfig transcription = new TranscriptionConfig();
    }

    /**
     * 语音识别配置
     */
    @Data
    public static class TranscriptionConfig {
        private boolean enabled = true;
        private String language = "zh";
        private String model = "whisper-large-v3";
        private boolean speakerDiarization = false;
        private boolean timestamp = false;
    }

    /**
     * 多模态配置
     */
    @Data
    public static class MultiModalConfig {
        private boolean enabled = false;
        private String videoModel = "gpt-4-vision";
        private String audioModel = "whisper-1";
        private int timeout = 60; // seconds
        private int maxRetries = 3;
    }

    /**
     * 存储配置
     */
    @Data
    public static class StorageConfig {
        private String tempDir = "./data/temp/media";
        private String processedDir = "./data/storage/media";
        private boolean keepOriginal = true;
        private int tempRetentionHours = 24;
    }

    /**
     * 性能配置
     */
    @Data
    public static class PerformanceConfig {
        private int parallelTasks = 2;
        private int taskTimeoutMinutes = 30;
        private int memoryLimit = 2048; // MB
    }
}

