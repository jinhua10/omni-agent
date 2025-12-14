package top.yumbo.ai.behavior.api.model;

import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

/**
 * 行为信号事件 (Behavior Signal Event)
 *
 * 记录用户产生的单个行为信号
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Data
@NoArgsConstructor
public class BehaviorSignalEvent {

    /**
     * 事件ID (Event ID)
     */
    private String eventId;

    /**
     * 用户ID (User ID)
     */
    private String userId;

    /**
     * 会话ID (Session ID)
     */
    private String sessionId;

    /**
     * 问答ID (QA ID)
     */
    private String qaId;

    /**
     * 答案ID (Answer ID)
     */
    private String answerId;

    /**
     * 信号类型 (Signal Type)
     */
    private SignalType signalType;

    /**
     * 信号强度 (Signal Strength)
     * 范围：0.0 ~ 1.0，表示信号的确定性
     */
    private double strength = 1.0;

    /**
     * 时间戳 (Timestamp)
     */
    private LocalDateTime timestamp = LocalDateTime.now();

    /**
     * 上下文信息 (Context Information)
     * 存储额外的上下文数据，如阅读时长、点击位置等
     */
    private Map<String, Object> context = new HashMap<>();

    /**
     * 构造函数 (Constructor)
     *
     * @param userId 用户ID
     * @param qaId 问答ID
     * @param answerId 答案ID
     * @param signalType 信号类型
     */
    public BehaviorSignalEvent(String userId, String qaId, String answerId, SignalType signalType) {
        this.userId = userId;
        this.qaId = qaId;
        this.answerId = answerId;
        this.signalType = signalType;
        this.timestamp = LocalDateTime.now();
        this.context = new HashMap<>();
        this.strength = 1.0;
    }

    /**
     * 添加上下文信息 (Add Context Information)
     *
     * @param key 键
     * @param value 值
     */
    public void addContext(String key, Object value) {
        this.context.put(key, value);
    }

    /**
     * 获取上下文信息 (Get Context Information)
     *
     * @param key 键
     * @return 值
     */
    public Object getContext(String key) {
        return this.context.get(key);
    }
}

