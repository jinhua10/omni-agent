package top.yumbo.ai.behavior.api;

import top.yumbo.ai.behavior.api.model.AttitudeScore;
import top.yumbo.ai.behavior.api.model.BehaviorSignalEvent;

import java.util.List;
import java.util.Map;

/**
 * 行为分析服务接口 (Behavior Analysis Service Interface)
 *
 * 提供用户行为信号收集、聚合、推断等核心功能
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
public interface BehaviorAnalysisService {

    /**
     * 收集行为信号 (Collect Behavior Signal)
     *
     * @param signal 行为信号事件
     */
    void collectSignal(BehaviorSignalEvent signal);

    /**
     * 批量收集行为信号 (Batch Collect Behavior Signals)
     *
     * @param signals 行为信号事件列表
     */
    void collectSignals(List<BehaviorSignalEvent> signals);

    /**
     * 推断用户对答案的态度 (Infer User Attitude towards Answer)
     *
     * @param userId 用户ID
     * @param answerId 答案ID
     * @return 态度评分
     */
    AttitudeScore inferAttitude(String userId, String answerId);

    /**
     * 批量推断用户态度 (Batch Infer User Attitudes)
     *
     * @param userId 用户ID
     * @param answerIds 答案ID列表
     * @return 答案ID到态度评分的映射
     */
    Map<String, AttitudeScore> inferAttitudes(String userId, List<String> answerIds);

    /**
     * 获取用户的所有行为信号 (Get User's All Behavior Signals)
     *
     * @param userId 用户ID
     * @return 行为信号列表
     */
    List<BehaviorSignalEvent> getUserSignals(String userId);

    /**
     * 获取特定答案的所有行为信号 (Get All Behavior Signals for Specific Answer)
     *
     * @param answerId 答案ID
     * @return 行为信号列表
     */
    List<BehaviorSignalEvent> getAnswerSignals(String answerId);

    /**
     * 获取用户对特定答案的行为信号 (Get User's Behavior Signals for Specific Answer)
     *
     * @param userId 用户ID
     * @param answerId 答案ID
     * @return 行为信号列表
     */
    List<BehaviorSignalEvent> getUserAnswerSignals(String userId, String answerId);

    /**
     * 计算答案的热度分数 (Calculate Answer Hotness Score)
     *
     * 基于多维度行为信号计算答案的受欢迎程度
     *
     * @param answerId 答案ID
     * @return 热度分数 (0.0 - 1.0)
     */
    double calculateHotness(String answerId);

    /**
     * 获取热门答案列表 (Get Hot Answers List)
     *
     * @param topN 返回前N个热门答案
     * @return 答案ID列表，按热度降序排列
     */
    List<String> getHotAnswers(int topN);

    /**
     * 清除用户的行为信号 (Clear User's Behavior Signals)
     *
     * @param userId 用户ID
     */
    void clearUserSignals(String userId);

    /**
     * 清除特定答案的行为信号 (Clear Behavior Signals for Specific Answer)
     *
     * @param answerId 答案ID
     */
    void clearAnswerSignals(String answerId);
}

