package top.yumbo.ai.omni.workflow;

import lombok.Getter;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * 工作流上下文
 * (Workflow Context)
 *
 * <p>在工作流执行过程中保存所有步骤的输入输出和共享数据</p>
 *
 * @author OmniAgent Team
 * @since 4.0.0
 */
@Slf4j
@Getter
public class WorkflowContext {

    /**
     * 工作流初始输入
     */
    private final Object initialInput;

    /**
     * 每个步骤的输出结果（step_id -> result）
     */
    private final Map<String, Object> stepResults = new ConcurrentHashMap<>();

    /**
     * 共享数据（可以在步骤间传递任意数据）
     */
    private final Map<String, Object> sharedData = new ConcurrentHashMap<>();

    /**
     * 工作流元数据
     */
    private final Map<String, Object> metadata = new HashMap<>();

    /**
     * 工作流开始时间
     */
    private final long startTime;

    /**
     * 工作流ID
     */
    private final String workflowId;

    public WorkflowContext(Object initialInput) {
        this.initialInput = initialInput;
        this.startTime = System.currentTimeMillis();
        this.workflowId = java.util.UUID.randomUUID().toString();

        log.debug("创建工作流上下文: workflowId={}", workflowId);
    }

    /**
     * 设置步骤结果
     *
     * @param stepId 步骤ID
     * @param result 结果
     */
    public void setStepResult(String stepId, Object result) {
        stepResults.put(stepId, result);
        log.debug("保存步骤结果: stepId={}, resultType={}",
                 stepId, result != null ? result.getClass().getSimpleName() : "null");
    }

    /**
     * 获取步骤结果
     *
     * @param stepId 步骤ID
     * @return 结果
     */
    public Object getStepResult(String stepId) {
        return stepResults.get(stepId);
    }

    /**
     * 获取步骤结果（指定类型）
     *
     * @param stepId 步骤ID
     * @param type 结果类型
     * @return 结果
     */
    @SuppressWarnings("unchecked")
    public <T> T getStepResult(String stepId, Class<T> type) {
        Object result = stepResults.get(stepId);
        if (result == null) {
            return null;
        }
        if (!type.isInstance(result)) {
            throw new ClassCastException(String.format(
                    "步骤 %s 的结果类型不匹配: 期望 %s, 实际 %s",
                    stepId, type.getName(), result.getClass().getName()));
        }
        return (T) result;
    }

    /**
     * 设置共享数据
     *
     * @param key 键
     * @param value 值
     */
    public void setSharedData(String key, Object value) {
        sharedData.put(key, value);
        log.debug("保存共享数据: key={}, valueType={}",
                 key, value != null ? value.getClass().getSimpleName() : "null");
    }

    /**
     * 获取共享数据
     *
     * @param key 键
     * @return 值
     */
    public Object getSharedData(String key) {
        return sharedData.get(key);
    }

    /**
     * 获取共享数据（指定类型）
     *
     * @param key 键
     * @param type 值类型
     * @return 值
     */
    @SuppressWarnings("unchecked")
    public <T> T getSharedData(String key, Class<T> type) {
        Object value = sharedData.get(key);
        if (value == null) {
            return null;
        }
        if (!type.isInstance(value)) {
            throw new ClassCastException(String.format(
                    "共享数据 %s 的类型不匹配: 期望 %s, 实际 %s",
                    key, type.getName(), value.getClass().getName()));
        }
        return (T) value;
    }

    /**
     * 设置元数据
     *
     * @param key 键
     * @param value 值
     */
    public void setMetadata(String key, Object value) {
        metadata.put(key, value);
    }

    /**
     * 获取元数据
     *
     * @param key 键
     * @return 值
     */
    public Object getMetadata(String key) {
        return metadata.get(key);
    }

    /**
     * 获取工作流执行时长（毫秒）
     *
     * @return 执行时长
     */
    public long getElapsedTime() {
        return System.currentTimeMillis() - startTime;
    }

    /**
     * 获取所有步骤结果
     *
     * @return 所有步骤结果的副本
     */
    public Map<String, Object> getAllStepResults() {
        return new HashMap<>(stepResults);
    }

    /**
     * 清空所有数据（用于重置上下文）
     */
    public void clear() {
        stepResults.clear();
        sharedData.clear();
        metadata.clear();
        log.debug("清空工作流上下文: workflowId={}", workflowId);
    }
}

