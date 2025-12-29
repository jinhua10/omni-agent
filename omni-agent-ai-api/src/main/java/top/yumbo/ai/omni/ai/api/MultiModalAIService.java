package top.yumbo.ai.omni.ai.api;

import java.util.List;
import java.util.Map;

/**
 * 多模态AI服务接口
 * (Multi-Modal AI Service Interface)
 *
 * <p>
 * 预留接口，用于未来支持多模态LLM能力（视频、音频、图像联合理解）
 * (Reserved interface for future multi-modal LLM capabilities)
 * </p>
 *
 * <p>
 * 设计原则：
 * 1. 扩展现有AIService接口，增加多模态能力
 * 2. 当LLM提供商支持成熟时，可快速实现
 * 3. 支持视频理解、音频转文本、多模态联合理解等
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.1.0 (Phase 3)
 */
public interface MultiModalAIService extends AIService {

    /**
     * 分析视频内容
     * (Analyze video content)
     *
     * <p>
     * 使用场景：
     * - 理解视频内容和上下文
     * - 生成视频摘要
     * - 回答关于视频的问题
     * - 识别视频中的对象、场景、动作
     * </p>
     *
     * @param videoData 视频数据 (Video data)
     * @param prompt 提示词 (Prompt)
     * @return LLM的分析结果 (Analysis result from LLM)
     */
    String analyzeVideo(byte[] videoData, String prompt);

    /**
     * 分析视频帧序列
     * (Analyze video frame sequence)
     *
     * <p>
     * 分析多个视频帧，理解时序关系和动作
     * (Analyze multiple frames to understand temporal relationships and actions)
     * </p>
     *
     * @param frames 视频帧列表 (List of video frames)
     * @param prompt 提示词 (Prompt)
     * @return LLM的分析结果 (Analysis result from LLM)
     */
    String analyzeVideoFrames(List<VideoFrame> frames, String prompt);

    /**
     * 音频转文本（语音识别）
     * (Transcribe audio to text - ASR)
     *
     * <p>
     * 使用场景：
     * - 会议录音转文字
     * - 视频字幕生成
     * - 语音笔记转文本
     * </p>
     *
     * @param audioData 音频数据 (Audio data)
     * @return 转录的文本 (Transcribed text)
     */
    String transcribeAudio(byte[] audioData);

    /**
     * 音频转文本（带配置）
     * (Transcribe audio with options)
     *
     * @param audioData 音频数据 (Audio data)
     * @param options 转录选项 (Transcription options)
     * @return 转录结果 (Transcription result)
     */
    TranscriptionResult transcribeAudioWithOptions(byte[] audioData, TranscriptionOptions options);

    /**
     * 分析音频内容
     * (Analyze audio content)
     *
     * <p>
     * 不仅转录文本，还理解音频中的情感、语气、意图等
     * (Not only transcribe but also understand emotions, tones, intents)
     * </p>
     *
     * @param audioData 音频数据 (Audio data)
     * @param prompt 提示词 (Prompt)
     * @return LLM的分析结果 (Analysis result from LLM)
     */
    String analyzeAudio(byte[] audioData, String prompt);

    /**
     * 多模态联合理解
     * (Multi-modal joint understanding)
     *
     * <p>
     * 同时处理图像、音频、文本等多种模态的输入
     * (Process multiple modalities: image, audio, text simultaneously)
     * </p>
     *
     * <p>
     * 使用场景：
     * - 视频内容理解（视觉+听觉）
     * - 图文配合分析
     * - 复杂场景理解
     * </p>
     *
     * @param input 多模态输入 (Multi-modal input)
     * @return LLM的理解结果 (Understanding result from LLM)
     */
    String analyzeMultiModal(MultiModalInput input);

    /**
     * 生成图像描述
     * (Generate image description)
     *
     * <p>
     * 已在 AIService 中定义，此处作为多模态能力的一部分重申
     * (Already defined in AIService, reiterated here as part of multi-modal capabilities)
     * </p>
     *
     * @param imageData 图像数据 (Image data)
     * @param prompt 提示词 (Prompt)
     * @return 图像描述 (Image description)
     */
    String analyzeImage(byte[] imageData, String prompt);

    /**
     * 视频帧
     * (Video Frame)
     */
    class VideoFrame {
        private byte[] imageData;
        private long timestampMs;
        private int frameNumber;

        public VideoFrame(byte[] imageData, long timestampMs, int frameNumber) {
            this.imageData = imageData;
            this.timestampMs = timestampMs;
            this.frameNumber = frameNumber;
        }

        // Getters
        public byte[] getImageData() { return imageData; }
        public long getTimestampMs() { return timestampMs; }
        public int getFrameNumber() { return frameNumber; }
    }

    /**
     * 转录选项
     * (Transcription Options)
     */
    class TranscriptionOptions {
        private String language = "zh"; // 语言代码
        private boolean timestampEnabled = false; // 是否包含时间戳
        private boolean speakerDiarization = false; // 是否区分说话人
        private String model = "whisper-1"; // 使用的模型

        // Getters and Setters
        public String getLanguage() { return language; }
        public void setLanguage(String language) { this.language = language; }

        public boolean isTimestampEnabled() { return timestampEnabled; }
        public void setTimestampEnabled(boolean timestampEnabled) { this.timestampEnabled = timestampEnabled; }

        public boolean isSpeakerDiarization() { return speakerDiarization; }
        public void setSpeakerDiarization(boolean speakerDiarization) { this.speakerDiarization = speakerDiarization; }

        public String getModel() { return model; }
        public void setModel(String model) { this.model = model; }
    }

    /**
     * 转录结果
     * (Transcription Result)
     */
    class TranscriptionResult {
        private String text; // 转录文本
        private List<TranscriptionSegment> segments; // 分段（带时间戳）
        private Map<String, Object> metadata; // 元数据

        // Getters and Setters
        public String getText() { return text; }
        public void setText(String text) { this.text = text; }

        public List<TranscriptionSegment> getSegments() { return segments; }
        public void setSegments(List<TranscriptionSegment> segments) { this.segments = segments; }

        public Map<String, Object> getMetadata() { return metadata; }
        public void setMetadata(Map<String, Object> metadata) { this.metadata = metadata; }
    }

    /**
     * 转录片段
     * (Transcription Segment)
     */
    class TranscriptionSegment {
        private String text;
        private long startMs;
        private long endMs;
        private String speaker; // 说话人ID（如果启用说话人分离）

        // Getters and Setters
        public String getText() { return text; }
        public void setText(String text) { this.text = text; }

        public long getStartMs() { return startMs; }
        public void setStartMs(long startMs) { this.startMs = startMs; }

        public long getEndMs() { return endMs; }
        public void setEndMs(long endMs) { this.endMs = endMs; }

        public String getSpeaker() { return speaker; }
        public void setSpeaker(String speaker) { this.speaker = speaker; }
    }

    /**
     * 多模态输入
     * (Multi-Modal Input)
     */
    class MultiModalInput {
        private String text; // 文本输入（可选）
        private byte[] imageData; // 图像数据（可选）
        private byte[] audioData; // 音频数据（可选）
        private byte[] videoData; // 视频数据（可选）
        private String prompt; // 提示词

        // Getters and Setters
        public String getText() { return text; }
        public void setText(String text) { this.text = text; }

        public byte[] getImageData() { return imageData; }
        public void setImageData(byte[] imageData) { this.imageData = imageData; }

        public byte[] getAudioData() { return audioData; }
        public void setAudioData(byte[] audioData) { this.audioData = audioData; }

        public byte[] getVideoData() { return videoData; }
        public void setVideoData(byte[] videoData) { this.videoData = videoData; }

        public String getPrompt() { return prompt; }
        public void setPrompt(String prompt) { this.prompt = prompt; }
    }
}

