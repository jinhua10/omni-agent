package top.yumbo.ai.omni.core.document.processor;

import top.yumbo.ai.omni.core.document.DocumentProcessor;
import java.util.List;

/**
 * 媒体文件处理器接口
 * (Media Document Processor Interface)
 *
 * <p>
 * 预留接口，用于未来支持视频、音频等媒体文件的处理
 * (Reserved interface for future support of video, audio and other media files)
 * </p>
 *
 * <p>
 * 设计原则：
 * 1. 当前国内LLM对媒体支持尚不完善，此接口为预留设计
 * 2. 当LLM能力成熟时，可快速实现而无需重构核心架构
 * 3. 支持视频、音频、字幕提取等多种媒体处理能力
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.1.0 (Phase 3)
 */
public interface MediaDocumentProcessor extends DocumentProcessor {

    /**
     * 处理视频文件
     * (Process video file)
     *
     * <p>
     * 支持的处理能力：
     * - 视频帧提取
     * - 关键帧识别
     * - 视频内容理解（通过Vision LLM）
     * - 字幕提取与文本化
     * </p>
     *
     * @param context 视频处理上下文 (Video processing context)
     * @return 处理结果 (Processing result)
     */
    DocumentProcessor.ProcessingResult processVideo(VideoContext context);

    /**
     * 处理音频文件
     * (Process audio file)
     *
     * <p>
     * 支持的处理能力：
     * - 语音识别（ASR）
     * - 说话人分离
     * - 音频内容理解
     * - 音频转文本
     * </p>
     *
     * @param context 音频处理上下文 (Audio processing context)
     * @return 处理结果 (Processing result)
     */
    DocumentProcessor.ProcessingResult processAudio(AudioContext context);

    /**
     * 提取媒体文件的字幕
     * (Extract subtitles from media file)
     *
     * <p>
     * 支持：
     * - 内嵌字幕提取（SRT, ASS, VTT等）
     * - 外挂字幕解析
     * - 语音转字幕（ASR）
     * </p>
     *
     * @param file 媒体文件 (Media file)
     * @return 字幕文本 (Subtitle text)
     */
    String extractSubtitles(MediaFile file);

    /**
     * 提取视频关键帧
     * (Extract key frames from video)
     *
     * <p>
     * 使用场景帧识别算法提取视频的关键帧
     * (Extract key frames using scene detection algorithms)
     * </p>
     *
     * @param videoPath 视频文件路径 (Video file path)
     * @param maxFrames 最大帧数 (Maximum number of frames)
     * @return 关键帧列表 (List of key frames)
     */
    List<VideoFrame> extractKeyFrames(String videoPath, int maxFrames);

    /**
     * 生成视频摘要
     * (Generate video summary)
     *
     * <p>
     * 基于关键帧和字幕生成视频内容摘要
     * (Generate summary based on key frames and subtitles)
     * </p>
     *
     * @param context 视频处理上下文 (Video processing context)
     * @return 视频摘要 (Video summary)
     */
    String generateVideoSummary(VideoContext context);

    /**
     * 检查是否支持该媒体格式
     * (Check if the media format is supported)
     *
     * @param fileExtension 文件扩展名 (File extension)
     * @return 是否支持 (Whether supported)
     */
    boolean supportsMediaFormat(String fileExtension);

    /**
     * 视频处理上下文
     * (Video Processing Context)
     */
    class VideoContext {
        private String filePath;
        private byte[] videoData;
        private String format;
        private VideoProcessingOptions options;

        public String getFilePath() {
            return filePath;
        }

        public void setFilePath(String filePath) {
            this.filePath = filePath;
        }

        public byte[] getVideoData() {
            return videoData;
        }

        public void setVideoData(byte[] videoData) {
            this.videoData = videoData;
        }

        public String getFormat() {
            return format;
        }

        public void setFormat(String format) {
            this.format = format;
        }

        public VideoProcessingOptions getOptions() {
            return options;
        }

        public void setOptions(VideoProcessingOptions options) {
            this.options = options;
        }
    }

    /**
     * 音频处理上下文
     * (Audio Processing Context)
     */
    class AudioContext {
        private String filePath;
        private byte[] audioData;
        private String format;
        private AudioProcessingOptions options;

        public String getFilePath() {
            return filePath;
        }

        public void setFilePath(String filePath) {
            this.filePath = filePath;
        }

        public byte[] getAudioData() {
            return audioData;
        }

        public void setAudioData(byte[] audioData) {
            this.audioData = audioData;
        }

        public String getFormat() {
            return format;
        }

        public void setFormat(String format) {
            this.format = format;
        }

        public AudioProcessingOptions getOptions() {
            return options;
        }

        public void setOptions(AudioProcessingOptions options) {
            this.options = options;
        }
    }

    /**
     * 媒体文件
     * (Media File)
     */
    class MediaFile {
        private String filePath;
        private String mimeType;
        private long fileSize;

        public String getFilePath() {
            return filePath;
        }

        public void setFilePath(String filePath) {
            this.filePath = filePath;
        }

        public String getMimeType() {
            return mimeType;
        }

        public void setMimeType(String mimeType) {
            this.mimeType = mimeType;
        }

        public long getFileSize() {
            return fileSize;
        }

        public void setFileSize(long fileSize) {
            this.fileSize = fileSize;
        }
    }

    /**
     * 视频帧
     * (Video Frame)
     */
    class VideoFrame {
        private int frameNumber;
        private long timestampMs;
        private byte[] imageData;
        private String description;

        public int getFrameNumber() {
            return frameNumber;
        }

        public void setFrameNumber(int frameNumber) {
            this.frameNumber = frameNumber;
        }

        public long getTimestampMs() {
            return timestampMs;
        }

        public void setTimestampMs(long timestampMs) {
            this.timestampMs = timestampMs;
        }

        public byte[] getImageData() {
            return imageData;
        }

        public void setImageData(byte[] imageData) {
            this.imageData = imageData;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }
    }

    /**
     * 视频处理选项
     * (Video Processing Options)
     */
    class VideoProcessingOptions {
        private boolean extractFrames = true;
        private boolean extractSubtitles = true;
        private boolean generateSummary = true;
        private int maxFrames = 10;
        private int frameInterval = 5;

        public boolean isExtractFrames() {
            return extractFrames;
        }

        public void setExtractFrames(boolean extractFrames) {
            this.extractFrames = extractFrames;
        }

        public boolean isExtractSubtitles() {
            return extractSubtitles;
        }

        public void setExtractSubtitles(boolean extractSubtitles) {
            this.extractSubtitles = extractSubtitles;
        }

        public boolean isGenerateSummary() {
            return generateSummary;
        }

        public void setGenerateSummary(boolean generateSummary) {
            this.generateSummary = generateSummary;
        }

        public int getMaxFrames() {
            return maxFrames;
        }

        public void setMaxFrames(int maxFrames) {
            this.maxFrames = maxFrames;
        }

        public int getFrameInterval() {
            return frameInterval;
        }

        public void setFrameInterval(int frameInterval) {
            this.frameInterval = frameInterval;
        }
    }

    /**
     * 音频处理选项
     * (Audio Processing Options)
     */
    class AudioProcessingOptions {
        private boolean transcribe = true;
        private boolean speakerDiarization = false;
        private String language = "zh";
        private String model = "whisper-large-v3";

        public boolean isTranscribe() {
            return transcribe;
        }

        public void setTranscribe(boolean transcribe) {
            this.transcribe = transcribe;
        }

        public boolean isSpeakerDiarization() {
            return speakerDiarization;
        }

        public void setSpeakerDiarization(boolean speakerDiarization) {
            this.speakerDiarization = speakerDiarization;
        }

        public String getLanguage() {
            return language;
        }

        public void setLanguage(String language) {
            this.language = language;
        }

        public String getModel() {
            return model;
        }

        public void setModel(String model) {
            this.model = model;
        }
    }
}


