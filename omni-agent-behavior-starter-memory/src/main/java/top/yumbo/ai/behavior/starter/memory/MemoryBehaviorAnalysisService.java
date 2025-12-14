package top.yumbo.ai.behavior.starter.memory;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.behavior.api.BehaviorAnalysisService;
import top.yumbo.ai.behavior.api.model.AttitudeLevel;
import top.yumbo.ai.behavior.api.model.AttitudeScore;
import top.yumbo.ai.behavior.api.model.BehaviorSignalEvent;
import top.yumbo.ai.behavior.api.model.SignalWeight;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 基于内存的行为分析服务实现 (Memory-based Behavior Analysis Service)
 *
 * 使用ConcurrentHashMap存储行为信号数据
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class MemoryBehaviorAnalysisService implements BehaviorAnalysisService {

    /**
     * 用户-答案维度的信号存储 (User-Answer Signal Storage)
     * Key: userId + ":" + answerId
     */
    private final Map<String, List<BehaviorSignalEvent>> userAnswerSignals = new ConcurrentHashMap<>();

    /**
     * 用户维度的信号存储 (User Signal Storage)
     * Key: userId
     */
    private final Map<String, List<BehaviorSignalEvent>> userSignals = new ConcurrentHashMap<>();

    /**
     * 答案维度的信号存储 (Answer Signal Storage)
     * Key: answerId
     */
    private final Map<String, List<BehaviorSignalEvent>> answerSignals = new ConcurrentHashMap<>();

    /**
     * 态度评分缓存 (Attitude Score Cache)
     * Key: userId + ":" + answerId
     */
    private final Map<String, AttitudeScore> attitudeCache = new ConcurrentHashMap<>();

    /**
     * 信号权重配置 (Signal Weight Configuration)
     */
    private final Map<top.yumbo.ai.behavior.api.model.SignalType, SignalWeight> weightConfig = new ConcurrentHashMap<>();

    /**
     * 构造函数 - 初始化默认权重配置 (Constructor - Initialize Default Weights)
     */
    public MemoryBehaviorAnalysisService() {
        initializeDefaultWeights();
        log.info("✅ MemoryBehaviorAnalysisService initialized");
    }

    /**
     * 初始化默认权重配置 (Initialize Default Weight Configuration)
     */
    private void initializeDefaultWeights() {
        for (top.yumbo.ai.behavior.api.model.SignalType type : top.yumbo.ai.behavior.api.model.SignalType.values()) {
            weightConfig.put(type, SignalWeight.getDefault(type));
        }
    }

    @Override
    public void collectSignal(BehaviorSignalEvent signal) {
        if (signal == null || signal.getUserId() == null || signal.getAnswerId() == null) {
            log.warn("⚠️ Invalid signal: {}", signal);
            return;
        }

        // 生成事件ID
        if (signal.getEventId() == null) {
            signal.setEventId(UUID.randomUUID().toString());
        }

        // 存储到三个维度
        String userAnswerKey = getUserAnswerKey(signal.getUserId(), signal.getAnswerId());
        userAnswerSignals.computeIfAbsent(userAnswerKey, k -> new ArrayList<>()).add(signal);
        userSignals.computeIfAbsent(signal.getUserId(), k -> new ArrayList<>()).add(signal);
        answerSignals.computeIfAbsent(signal.getAnswerId(), k -> new ArrayList<>()).add(signal);

        // 清除缓存
        attitudeCache.remove(userAnswerKey);

        log.debug("📡 Signal collected: user={}, answer={}, type={}",
                signal.getUserId(), signal.getAnswerId(), signal.getSignalType());
    }

    @Override
    public void collectSignals(List<BehaviorSignalEvent> signals) {
        if (signals == null || signals.isEmpty()) {
            return;
        }
        signals.forEach(this::collectSignal);
        log.info("📡 Batch collected {} signals", signals.size());
    }

    @Override
    public AttitudeScore inferAttitude(String userId, String answerId) {
        if (userId == null || answerId == null) {
            log.warn("⚠️ Invalid parameters: userId={}, answerId={}", userId, answerId);
            return new AttitudeScore(userId, answerId, 0.0, 0.0);
        }

        String key = getUserAnswerKey(userId, answerId);

        // 检查缓存
        AttitudeScore cached = attitudeCache.get(key);
        if (cached != null) {
            log.debug("💾 Attitude score from cache: user={}, answer={}, score={}",
                    userId, answerId, cached.getRawScore());
            return cached;
        }

        // 获取信号列表
        List<BehaviorSignalEvent> signals = userAnswerSignals.getOrDefault(key, Collections.emptyList());

        if (signals.isEmpty()) {
            log.debug("ℹ️ No signals found for user={}, answer={}", userId, answerId);
            return new AttitudeScore(userId, answerId, 0.0, 0.0);
        }

        // 计算态度评分
        AttitudeScore score = calculateAttitudeScore(userId, answerId, signals);

        // 缓存结果
        attitudeCache.put(key, score);

        log.debug("🎯 Attitude inferred: user={}, answer={}, score={}, level={}, confidence={}",
                userId, answerId, score.getRawScore(), score.getLevel(), score.getConfidence());

        return score;
    }

    @Override
    public Map<String, AttitudeScore> inferAttitudes(String userId, List<String> answerIds) {
        if (userId == null || answerIds == null || answerIds.isEmpty()) {
            return Collections.emptyMap();
        }

        Map<String, AttitudeScore> result = new HashMap<>();
        for (String answerId : answerIds) {
            result.put(answerId, inferAttitude(userId, answerId));
        }

        log.info("🎯 Batch inferred {} attitudes for user={}", result.size(), userId);
        return result;
    }

    @Override
    public List<BehaviorSignalEvent> getUserSignals(String userId) {
        if (userId == null) {
            return Collections.emptyList();
        }
        return new ArrayList<>(userSignals.getOrDefault(userId, Collections.emptyList()));
    }

    @Override
    public List<BehaviorSignalEvent> getAnswerSignals(String answerId) {
        if (answerId == null) {
            return Collections.emptyList();
        }
        return new ArrayList<>(answerSignals.getOrDefault(answerId, Collections.emptyList()));
    }

    @Override
    public List<BehaviorSignalEvent> getUserAnswerSignals(String userId, String answerId) {
        if (userId == null || answerId == null) {
            return Collections.emptyList();
        }
        String key = getUserAnswerKey(userId, answerId);
        return new ArrayList<>(userAnswerSignals.getOrDefault(key, Collections.emptyList()));
    }

    @Override
    public double calculateHotness(String answerId) {
        if (answerId == null) {
            return 0.0;
        }

        List<BehaviorSignalEvent> signals = answerSignals.getOrDefault(answerId, Collections.emptyList());
        if (signals.isEmpty()) {
            return 0.0;
        }

        // 统计各类信号的数量
        Map<top.yumbo.ai.behavior.api.model.SignalType, Long> signalCounts = signals.stream()
                .collect(Collectors.groupingBy(BehaviorSignalEvent::getSignalType, Collectors.counting()));

        // 计算加权热度
        double totalHotness = 0.0;
        double totalWeight = 0.0;

        for (Map.Entry<top.yumbo.ai.behavior.api.model.SignalType, Long> entry : signalCounts.entrySet()) {
            SignalWeight weight = weightConfig.get(entry.getKey());
            if (weight != null && weight.isEnabled()) {
                double contribution = entry.getValue() * weight.getBaseWeight();
                totalHotness += contribution;
                totalWeight += weight.getBaseWeight();
            }
        }

        // 归一化到0-1范围
        double hotness = totalWeight > 0 ? Math.min(totalHotness / (totalWeight * 10), 1.0) : 0.0;

        log.debug("🔥 Hotness calculated: answer={}, hotness={}, signalCount={}",
                answerId, hotness, signals.size());

        return hotness;
    }

    @Override
    public List<String> getHotAnswers(int topN) {
        if (topN <= 0) {
            return Collections.emptyList();
        }

        // 计算所有答案的热度
        Map<String, Double> hotnessMap = new HashMap<>();
        for (String answerId : answerSignals.keySet()) {
            hotnessMap.put(answerId, calculateHotness(answerId));
        }

        // 按热度排序并返回前N个
        List<String> hotAnswers = hotnessMap.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .limit(topN)
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());

        log.info("🔥 Top {} hot answers retrieved", hotAnswers.size());
        return hotAnswers;
    }

    @Override
    public void clearUserSignals(String userId) {
        if (userId == null) {
            return;
        }

        // 清除用户相关的所有数据
        userSignals.remove(userId);

        // 清除用户-答案维度的数据
        userAnswerSignals.keySet().removeIf(key -> key.startsWith(userId + ":"));

        // 清除缓存
        attitudeCache.keySet().removeIf(key -> key.startsWith(userId + ":"));

        log.info("🗑️ Cleared signals for user={}", userId);
    }

    @Override
    public void clearAnswerSignals(String answerId) {
        if (answerId == null) {
            return;
        }

        // 清除答案相关的所有数据
        answerSignals.remove(answerId);

        // 清除用户-答案维度的数据
        userAnswerSignals.keySet().removeIf(key -> key.endsWith(":" + answerId));

        // 清除缓存
        attitudeCache.keySet().removeIf(key -> key.endsWith(":" + answerId));

        log.info("🗑️ Cleared signals for answer={}", answerId);
    }

    // ========== 私有辅助方法 (Private Helper Methods) ==========

    /**
     * 生成用户-答案组合键 (Generate User-Answer Key)
     */
    private String getUserAnswerKey(String userId, String answerId) {
        return userId + ":" + answerId;
    }

    /**
     * 计算态度评分 (Calculate Attitude Score)
     */
    private AttitudeScore calculateAttitudeScore(String userId, String answerId,
                                                  List<BehaviorSignalEvent> signals) {
        double totalScore = 0.0;
        double totalWeight = 0.0;
        int signalCount = 0;

        LocalDateTime now = LocalDateTime.now();

        for (BehaviorSignalEvent signal : signals) {
            SignalWeight weight = weightConfig.get(signal.getSignalType());
            if (weight == null || !weight.isEnabled()) {
                continue;
            }

            // 计算时间衰减
            Duration duration = Duration.between(signal.getTimestamp(), now);
            double hours = duration.toHours();
            double timeDecay = Math.exp(-weight.getDecayFactor() * hours / 24.0);

            // 计算加权贡献
            double contribution = weight.getWeightedImpact() * signal.getStrength() * timeDecay;
            totalScore += contribution;
            totalWeight += weight.getBaseWeight() * timeDecay;
            signalCount++;
        }

        // 计算原始评分和置信度
        double rawScore = totalWeight > 0 ? Math.max(-1.0, Math.min(1.0, totalScore / totalWeight)) : 0.0;
        double confidence = calculateConfidence(signalCount, totalWeight);

        // 创建态度评分对象
        AttitudeScore score = new AttitudeScore(userId, answerId, rawScore, confidence);
        score.getSupportingSignals().addAll(signals);
        score.setExplanation(generateExplanation(signals, rawScore, confidence));

        return score;
    }

    /**
     * 计算置信度 (Calculate Confidence)
     */
    private double calculateConfidence(int signalCount, double totalWeight) {
        // 基于信号数量和总权重计算置信度
        double countFactor = Math.min(signalCount / 10.0, 1.0); // 10个信号达到最大置信度
        double weightFactor = Math.min(totalWeight / 5.0, 1.0);  // 总权重5达到最大置信度
        return (countFactor + weightFactor) / 2.0;
    }

    /**
     * 生成推断解释 (Generate Inference Explanation)
     */
    private String generateExplanation(List<BehaviorSignalEvent> signals,
                                        double rawScore, double confidence) {
        // 统计各类信号
        Map<top.yumbo.ai.behavior.api.model.SignalType, Long> signalCounts = signals.stream()
                .collect(Collectors.groupingBy(BehaviorSignalEvent::getSignalType, Collectors.counting()));

        StringBuilder explanation = new StringBuilder();
        explanation.append(String.format("基于%d个行为信号推断: ", signals.size()));

        // 列出主要信号
        signalCounts.entrySet().stream()
                .sorted(Map.Entry.<top.yumbo.ai.behavior.api.model.SignalType, Long>comparingByValue().reversed())
                .limit(3)
                .forEach(entry -> explanation.append(String.format("%s(%d次), ",
                        entry.getKey().name(), entry.getValue())));

        // 添加评分和置信度信息
        AttitudeLevel level = AttitudeLevel.fromScore(rawScore);
        explanation.append(String.format("态度等级: %s, 置信度: %.2f",
                level != null ? level.getZhName() : "未知", confidence));

        return explanation.toString();
    }

    /**
     * 获取统计信息 (Get Statistics)
     */
    public Map<String, Object> getStatistics() {
        Map<String, Object> stats = new HashMap<>();
        stats.put("totalUsers", userSignals.size());
        stats.put("totalAnswers", answerSignals.size());
        stats.put("totalSignals", userAnswerSignals.values().stream()
                .mapToInt(List::size).sum());
        stats.put("cachedScores", attitudeCache.size());
        return stats;
    }
}

