package top.yumbo.ai.behavior.starter.redis;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.stereotype.Service;
import top.yumbo.ai.behavior.api.BehaviorAnalysisService;
import top.yumbo.ai.behavior.api.model.AttitudeLevel;
import top.yumbo.ai.behavior.api.model.AttitudeScore;
import top.yumbo.ai.behavior.api.model.BehaviorSignalEvent;
import top.yumbo.ai.behavior.api.model.SignalWeight;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

/**
 * 基于 Redis 的行为分析服务实现 (Redis-based Behavior Analysis Service)
 *
 * 支持分布式部署场景，使用 Redis 作为共享缓存
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class RedisBehaviorAnalysisService implements BehaviorAnalysisService {

    private final RedisTemplate<String, Object> redisTemplate;
    private final ObjectMapper objectMapper;

    /**
     * 信号过期时间（7天）
     */
    private static final long SIGNAL_EXPIRE_DAYS = 7;

    /**
     * 态度评分缓存过期时间（1小时）
     */
    private static final long SCORE_CACHE_HOURS = 1;

    /**
     * Redis Key 前缀
     */
    private static final String KEY_USER_ANSWER_SIGNALS = "behavior:signals:user_answer:";
    private static final String KEY_USER_SIGNALS = "behavior:signals:user:";
    private static final String KEY_ANSWER_SIGNALS = "behavior:signals:answer:";
    private static final String KEY_ATTITUDE_CACHE = "behavior:attitude:";
    private static final String KEY_WEIGHT_CONFIG = "behavior:weight:";

    public RedisBehaviorAnalysisService(RedisTemplate<String, Object> redisTemplate) {
        this.redisTemplate = redisTemplate;
        this.objectMapper = new ObjectMapper();
        initializeDefaultWeights();
        log.info("✅ RedisBehaviorAnalysisService initialized (Distributed Mode)");
    }

    /**
     * 初始化默认权重配置
     */
    private void initializeDefaultWeights() {
        for (top.yumbo.ai.behavior.api.model.SignalType type : top.yumbo.ai.behavior.api.model.SignalType.values()) {
            String key = KEY_WEIGHT_CONFIG + type.name();
            if (Boolean.FALSE.equals(redisTemplate.hasKey(key))) {
                redisTemplate.opsForValue().set(key, SignalWeight.getDefault(type));
            }
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

        try {
            String signalJson = objectMapper.writeValueAsString(signal);

            // 存储到三个维度
            String userAnswerKey = KEY_USER_ANSWER_SIGNALS + getUserAnswerKey(signal.getUserId(), signal.getAnswerId());
            String userKey = KEY_USER_SIGNALS + signal.getUserId();
            String answerKey = KEY_ANSWER_SIGNALS + signal.getAnswerId();

            redisTemplate.opsForList().rightPush(userAnswerKey, signalJson);
            redisTemplate.opsForList().rightPush(userKey, signalJson);
            redisTemplate.opsForList().rightPush(answerKey, signalJson);

            // 设置过期时间
            redisTemplate.expire(userAnswerKey, SIGNAL_EXPIRE_DAYS, TimeUnit.DAYS);
            redisTemplate.expire(userKey, SIGNAL_EXPIRE_DAYS, TimeUnit.DAYS);
            redisTemplate.expire(answerKey, SIGNAL_EXPIRE_DAYS, TimeUnit.DAYS);

            // 清除缓存
            String cacheKey = KEY_ATTITUDE_CACHE + getUserAnswerKey(signal.getUserId(), signal.getAnswerId());
            redisTemplate.delete(cacheKey);

            log.debug("📡 Signal collected to Redis: user={}, answer={}, type={}",
                    signal.getUserId(), signal.getAnswerId(), signal.getSignalType());
        } catch (JsonProcessingException e) {
            log.error("❌ Failed to serialize signal: {}", signal, e);
        }
    }

    @Override
    public void collectSignals(List<BehaviorSignalEvent> signals) {
        if (signals == null || signals.isEmpty()) {
            return;
        }
        signals.forEach(this::collectSignal);
        log.info("📡 Batch collected {} signals to Redis", signals.size());
    }

    @Override
    public AttitudeScore inferAttitude(String userId, String answerId) {
        if (userId == null || answerId == null) {
            log.warn("⚠️ Invalid parameters: userId={}, answerId={}", userId, answerId);
            return new AttitudeScore(userId, answerId, 0.0, 0.0);
        }

        String cacheKey = KEY_ATTITUDE_CACHE + getUserAnswerKey(userId, answerId);

        // 检查缓存
        try {
            String cachedJson = (String) redisTemplate.opsForValue().get(cacheKey);
            if (cachedJson != null) {
                AttitudeScore cached = objectMapper.readValue(cachedJson, AttitudeScore.class);
                log.debug("💾 Attitude score from Redis cache: user={}, answer={}, score={}",
                        userId, answerId, cached.getRawScore());
                return cached;
            }
        } catch (JsonProcessingException e) {
            log.warn("⚠️ Failed to deserialize cached score", e);
        }

        // 获取信号列表
        List<BehaviorSignalEvent> signals = getUserAnswerSignals(userId, answerId);

        if (signals.isEmpty()) {
            log.debug("ℹ️ No signals found in Redis for user={}, answer={}", userId, answerId);
            return new AttitudeScore(userId, answerId, 0.0, 0.0);
        }

        // 计算态度评分
        AttitudeScore score = calculateAttitudeScore(userId, answerId, signals);

        // 缓存结果
        try {
            String scoreJson = objectMapper.writeValueAsString(score);
            redisTemplate.opsForValue().set(cacheKey, scoreJson, SCORE_CACHE_HOURS, TimeUnit.HOURS);
        } catch (JsonProcessingException e) {
            log.error("❌ Failed to cache attitude score", e);
        }

        log.debug("🎯 Attitude inferred from Redis: user={}, answer={}, score={}, level={}, confidence={}",
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

        log.info("🎯 Batch inferred {} attitudes for user={} from Redis", result.size(), userId);
        return result;
    }

    @Override
    public List<BehaviorSignalEvent> getUserSignals(String userId) {
        if (userId == null) {
            return Collections.emptyList();
        }
        return getSignalsFromRedis(KEY_USER_SIGNALS + userId);
    }

    @Override
    public List<BehaviorSignalEvent> getAnswerSignals(String answerId) {
        if (answerId == null) {
            return Collections.emptyList();
        }
        return getSignalsFromRedis(KEY_ANSWER_SIGNALS + answerId);
    }

    @Override
    public List<BehaviorSignalEvent> getUserAnswerSignals(String userId, String answerId) {
        if (userId == null || answerId == null) {
            return Collections.emptyList();
        }
        String key = KEY_USER_ANSWER_SIGNALS + getUserAnswerKey(userId, answerId);
        return getSignalsFromRedis(key);
    }

    @Override
    public double calculateHotness(String answerId) {
        if (answerId == null) {
            return 0.0;
        }

        List<BehaviorSignalEvent> signals = getAnswerSignals(answerId);
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
            SignalWeight weight = getWeightConfig(entry.getKey());
            if (weight != null && weight.isEnabled()) {
                double contribution = entry.getValue() * weight.getBaseWeight();
                totalHotness += contribution;
                totalWeight += weight.getBaseWeight();
            }
        }

        // 归一化到0-1范围
        double hotness = totalWeight > 0 ? Math.min(totalHotness / (totalWeight * 10), 1.0) : 0.0;

        log.debug("🔥 Hotness calculated from Redis: answer={}, hotness={}, signalCount={}",
                answerId, hotness, signals.size());

        return hotness;
    }

    @Override
    public List<String> getHotAnswers(int topN) {
        if (topN <= 0) {
            return Collections.emptyList();
        }

        // 获取所有答案ID
        Set<String> answerKeys = redisTemplate.keys(KEY_ANSWER_SIGNALS + "*");
        if (answerKeys == null || answerKeys.isEmpty()) {
            return Collections.emptyList();
        }

        // 计算所有答案的热度
        Map<String, Double> hotnessMap = new HashMap<>();
        for (String key : answerKeys) {
            String answerId = key.substring(KEY_ANSWER_SIGNALS.length());
            hotnessMap.put(answerId, calculateHotness(answerId));
        }

        // 按热度排序并返回前N个
        List<String> hotAnswers = hotnessMap.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .limit(topN)
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());

        log.info("🔥 Top {} hot answers retrieved from Redis", hotAnswers.size());
        return hotAnswers;
    }

    @Override
    public void clearUserSignals(String userId) {
        if (userId == null) {
            return;
        }

        // 清除用户相关的所有数据
        redisTemplate.delete(KEY_USER_SIGNALS + userId);

        // 清除用户-答案维度的数据
        Set<String> userAnswerKeys = redisTemplate.keys(KEY_USER_ANSWER_SIGNALS + userId + ":*");
        if (userAnswerKeys != null && !userAnswerKeys.isEmpty()) {
            redisTemplate.delete(userAnswerKeys);
        }

        // 清除缓存
        Set<String> cacheKeys = redisTemplate.keys(KEY_ATTITUDE_CACHE + userId + ":*");
        if (cacheKeys != null && !cacheKeys.isEmpty()) {
            redisTemplate.delete(cacheKeys);
        }

        log.info("🗑️ Cleared signals from Redis for user={}", userId);
    }

    @Override
    public void clearAnswerSignals(String answerId) {
        if (answerId == null) {
            return;
        }

        // 清除答案相关的所有数据
        redisTemplate.delete(KEY_ANSWER_SIGNALS + answerId);

        // 清除用户-答案维度的数据
        Set<String> userAnswerKeys = redisTemplate.keys(KEY_USER_ANSWER_SIGNALS + "*:" + answerId);
        if (userAnswerKeys != null && !userAnswerKeys.isEmpty()) {
            redisTemplate.delete(userAnswerKeys);
        }

        // 清除缓存
        Set<String> cacheKeys = redisTemplate.keys(KEY_ATTITUDE_CACHE + "*:" + answerId);
        if (cacheKeys != null && !cacheKeys.isEmpty()) {
            redisTemplate.delete(cacheKeys);
        }

        log.info("🗑️ Cleared signals from Redis for answer={}", answerId);
    }

    // ========== 私有辅助方法 ==========

    private String getUserAnswerKey(String userId, String answerId) {
        return userId + ":" + answerId;
    }

    private List<BehaviorSignalEvent> getSignalsFromRedis(String key) {
        List<Object> jsonList = redisTemplate.opsForList().range(key, 0, -1);
        if (jsonList == null || jsonList.isEmpty()) {
            return Collections.emptyList();
        }

        List<BehaviorSignalEvent> signals = new ArrayList<>();
        for (Object obj : jsonList) {
            try {
                BehaviorSignalEvent signal = objectMapper.readValue((String) obj, BehaviorSignalEvent.class);
                signals.add(signal);
            } catch (JsonProcessingException e) {
                log.warn("⚠️ Failed to deserialize signal from Redis", e);
            }
        }
        return signals;
    }

    private SignalWeight getWeightConfig(top.yumbo.ai.behavior.api.model.SignalType type) {
        String key = KEY_WEIGHT_CONFIG + type.name();
        return (SignalWeight) redisTemplate.opsForValue().get(key);
    }

    private AttitudeScore calculateAttitudeScore(String userId, String answerId,
                                                  List<BehaviorSignalEvent> signals) {
        double totalScore = 0.0;
        double totalWeight = 0.0;
        int signalCount = 0;

        LocalDateTime now = LocalDateTime.now();

        for (BehaviorSignalEvent signal : signals) {
            SignalWeight weight = getWeightConfig(signal.getSignalType());
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

    private double calculateConfidence(int signalCount, double totalWeight) {
        double countFactor = Math.min(signalCount / 10.0, 1.0);
        double weightFactor = Math.min(totalWeight / 5.0, 1.0);
        return (countFactor + weightFactor) / 2.0;
    }

    private String generateExplanation(List<BehaviorSignalEvent> signals,
                                        double rawScore, double confidence) {
        Map<top.yumbo.ai.behavior.api.model.SignalType, Long> signalCounts = signals.stream()
                .collect(Collectors.groupingBy(BehaviorSignalEvent::getSignalType, Collectors.counting()));

        StringBuilder explanation = new StringBuilder();
        explanation.append(String.format("基于%d个行为信号推断(Redis): ", signals.size()));

        signalCounts.entrySet().stream()
                .sorted(Map.Entry.<top.yumbo.ai.behavior.api.model.SignalType, Long>comparingByValue().reversed())
                .limit(3)
                .forEach(entry -> explanation.append(String.format("%s(%d次), ",
                        entry.getKey().name(), entry.getValue())));

        AttitudeLevel level = AttitudeLevel.fromScore(rawScore);
        explanation.append(String.format("态度等级: %s, 置信度: %.2f",
                level != null ? level.getZhName() : "未知", confidence));

        return explanation.toString();
    }

    /**
     * 获取统计信息
     */
    public Map<String, Object> getStatistics() {
        Map<String, Object> stats = new HashMap<>();
        
        Set<String> userKeys = redisTemplate.keys(KEY_USER_SIGNALS + "*");
        Set<String> answerKeys = redisTemplate.keys(KEY_ANSWER_SIGNALS + "*");
        Set<String> cacheKeys = redisTemplate.keys(KEY_ATTITUDE_CACHE + "*");
        
        stats.put("totalUsers", userKeys != null ? userKeys.size() : 0);
        stats.put("totalAnswers", answerKeys != null ? answerKeys.size() : 0);
        stats.put("cachedScores", cacheKeys != null ? cacheKeys.size() : 0);
        stats.put("storage", "Redis (Distributed)");
        
        return stats;
    }
}
