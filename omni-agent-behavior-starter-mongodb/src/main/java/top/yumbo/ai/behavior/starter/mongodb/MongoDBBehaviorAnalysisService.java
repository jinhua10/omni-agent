package top.yumbo.ai.behavior.starter.mongodb;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.index.Index;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
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
 * 基于 MongoDB 的行为分析服务实现 (MongoDB-based Behavior Analysis Service)
 *
 * 支持用户行为画像和历史数据分析
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class MongoDBBehaviorAnalysisService implements BehaviorAnalysisService {

    private final MongoTemplate mongoTemplate;

    /**
     * Collection 名称
     */
    private static final String COLLECTION_SIGNALS = "behavior_signals";
    private static final String COLLECTION_SCORES = "behavior_scores";

    /**
     * 内存缓存（用于加速查询）
     */
    private final Map<String, AttitudeScore> attitudeCache = new ConcurrentHashMap<>();

    /**
     * 信号权重配置
     */
    private final Map<top.yumbo.ai.behavior.api.model.SignalType, SignalWeight> weightConfig = new ConcurrentHashMap<>();

    public MongoDBBehaviorAnalysisService(MongoTemplate mongoTemplate) {
        this.mongoTemplate = mongoTemplate;
        initializeDefaultWeights();
        ensureIndexes();
        log.info("✅ MongoDBBehaviorAnalysisService initialized (Historical Analysis Mode)");
    }

    /**
     * 初始化默认权重配置
     */
    private void initializeDefaultWeights() {
        for (top.yumbo.ai.behavior.api.model.SignalType type : top.yumbo.ai.behavior.api.model.SignalType.values()) {
            weightConfig.put(type, SignalWeight.getDefault(type));
        }
    }

    /**
     * 确保 MongoDB 索引
     */
    private void ensureIndexes() {
        try {
            mongoTemplate.indexOps(COLLECTION_SIGNALS).ensureIndex(
                    new Index()
                            .on("userId", Sort.Direction.ASC)
                            .on("answerId", Sort.Direction.ASC)
                            .on("timestamp", Sort.Direction.DESC)
            );
            mongoTemplate.indexOps(COLLECTION_SIGNALS).ensureIndex(
                    new Index().on("userId", Sort.Direction.ASC)
            );
            mongoTemplate.indexOps(COLLECTION_SIGNALS).ensureIndex(
                    new Index().on("answerId", Sort.Direction.ASC)
            );
            log.info("✅ MongoDB indexes created for behavior signals");
        } catch (Exception e) {
            log.warn("⚠️ Failed to create indexes: {}", e.getMessage());
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

        // 设置时间戳
        if (signal.getTimestamp() == null) {
            signal.setTimestamp(LocalDateTime.now());
        }

        // 存储到 MongoDB
        mongoTemplate.save(signal, COLLECTION_SIGNALS);

        // 清除缓存
        String cacheKey = getUserAnswerKey(signal.getUserId(), signal.getAnswerId());
        attitudeCache.remove(cacheKey);

        log.debug("📡 Signal collected to MongoDB: user={}, answer={}, type={}",
                signal.getUserId(), signal.getAnswerId(), signal.getSignalType());
    }

    @Override
    public void collectSignals(List<BehaviorSignalEvent> signals) {
        if (signals == null || signals.isEmpty()) {
            return;
        }

        // 批量插入
        signals.forEach(signal -> {
            if (signal.getEventId() == null) {
                signal.setEventId(UUID.randomUUID().toString());
            }
            if (signal.getTimestamp() == null) {
                signal.setTimestamp(LocalDateTime.now());
            }
        });

        mongoTemplate.insertAll(signals);
        
        // 清除相关缓存
        signals.forEach(signal -> {
            String cacheKey = getUserAnswerKey(signal.getUserId(), signal.getAnswerId());
            attitudeCache.remove(cacheKey);
        });

        log.info("📡 Batch collected {} signals to MongoDB", signals.size());
    }

    @Override
    public AttitudeScore inferAttitude(String userId, String answerId) {
        if (userId == null || answerId == null) {
            log.warn("⚠️ Invalid parameters: userId={}, answerId={}", userId, answerId);
            return new AttitudeScore(userId, answerId, 0.0, 0.0);
        }

        String cacheKey = getUserAnswerKey(userId, answerId);

        // 检查缓存
        AttitudeScore cached = attitudeCache.get(cacheKey);
        if (cached != null) {
            log.debug("💾 Attitude score from cache: user={}, answer={}, score={}",
                    userId, answerId, cached.getRawScore());
            return cached;
        }

        // 从 MongoDB 查询信号
        List<BehaviorSignalEvent> signals = getUserAnswerSignals(userId, answerId);

        if (signals.isEmpty()) {
            log.debug("ℹ️ No signals found in MongoDB for user={}, answer={}", userId, answerId);
            return new AttitudeScore(userId, answerId, 0.0, 0.0);
        }

        // 计算态度评分
        AttitudeScore score = calculateAttitudeScore(userId, answerId, signals);

        // 缓存结果
        attitudeCache.put(cacheKey, score);

        // 保存到 MongoDB
        mongoTemplate.save(score, COLLECTION_SCORES);

        log.debug("🎯 Attitude inferred from MongoDB: user={}, answer={}, score={}, level={}, confidence={}",
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

        log.info("🎯 Batch inferred {} attitudes for user={} from MongoDB", result.size(), userId);
        return result;
    }

    @Override
    public List<BehaviorSignalEvent> getUserSignals(String userId) {
        if (userId == null) {
            return Collections.emptyList();
        }

        Query query = new Query(Criteria.where("userId").is(userId));
        query.with(Sort.by(Sort.Direction.DESC, "timestamp"));
        
        return mongoTemplate.find(query, BehaviorSignalEvent.class, COLLECTION_SIGNALS);
    }

    @Override
    public List<BehaviorSignalEvent> getAnswerSignals(String answerId) {
        if (answerId == null) {
            return Collections.emptyList();
        }

        Query query = new Query(Criteria.where("answerId").is(answerId));
        query.with(Sort.by(Sort.Direction.DESC, "timestamp"));
        
        return mongoTemplate.find(query, BehaviorSignalEvent.class, COLLECTION_SIGNALS);
    }

    @Override
    public List<BehaviorSignalEvent> getUserAnswerSignals(String userId, String answerId) {
        if (userId == null || answerId == null) {
            return Collections.emptyList();
        }

        Query query = new Query(Criteria.where("userId").is(userId).and("answerId").is(answerId));
        query.with(Sort.by(Sort.Direction.DESC, "timestamp"));
        
        return mongoTemplate.find(query, BehaviorSignalEvent.class, COLLECTION_SIGNALS);
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
            SignalWeight weight = weightConfig.get(entry.getKey());
            if (weight != null && weight.isEnabled()) {
                double contribution = entry.getValue() * weight.getBaseWeight();
                totalHotness += contribution;
                totalWeight += weight.getBaseWeight();
            }
        }

        // 归一化到0-1范围
        double hotness = totalWeight > 0 ? Math.min(totalHotness / (totalWeight * 10), 1.0) : 0.0;

        log.debug("🔥 Hotness calculated from MongoDB: answer={}, hotness={}, signalCount={}",
                answerId, hotness, signals.size());

        return hotness;
    }

    @Override
    public List<String> getHotAnswers(int topN) {
        if (topN <= 0) {
            return Collections.emptyList();
        }

        // 获取所有答案ID（使用聚合查询）
        Query query = new Query();
        List<BehaviorSignalEvent> allSignals = mongoTemplate.find(query, BehaviorSignalEvent.class, COLLECTION_SIGNALS);
        
        Set<String> answerIds = allSignals.stream()
                .map(BehaviorSignalEvent::getAnswerId)
                .collect(Collectors.toSet());

        // 计算所有答案的热度
        Map<String, Double> hotnessMap = new HashMap<>();
        for (String answerId : answerIds) {
            hotnessMap.put(answerId, calculateHotness(answerId));
        }

        // 按热度排序并返回前N个
        List<String> hotAnswers = hotnessMap.entrySet().stream()
                .sorted(Map.Entry.<String, Double>comparingByValue().reversed())
                .limit(topN)
                .map(Map.Entry::getKey)
                .collect(Collectors.toList());

        log.info("🔥 Top {} hot answers retrieved from MongoDB", hotAnswers.size());
        return hotAnswers;
    }

    @Override
    public void clearUserSignals(String userId) {
        if (userId == null) {
            return;
        }

        // 删除用户相关的所有信号
        Query query = new Query(Criteria.where("userId").is(userId));
        mongoTemplate.remove(query, BehaviorSignalEvent.class, COLLECTION_SIGNALS);

        // 删除用户相关的所有评分
        mongoTemplate.remove(query, AttitudeScore.class, COLLECTION_SCORES);

        // 清除缓存
        attitudeCache.keySet().removeIf(key -> key.startsWith(userId + ":"));

        log.info("🗑️ Cleared signals from MongoDB for user={}", userId);
    }

    @Override
    public void clearAnswerSignals(String answerId) {
        if (answerId == null) {
            return;
        }

        // 删除答案相关的所有信号
        Query query = new Query(Criteria.where("answerId").is(answerId));
        mongoTemplate.remove(query, BehaviorSignalEvent.class, COLLECTION_SIGNALS);

        // 删除答案相关的所有评分
        mongoTemplate.remove(query, AttitudeScore.class, COLLECTION_SCORES);

        // 清除缓存
        attitudeCache.keySet().removeIf(key -> key.endsWith(":" + answerId));

        log.info("🗑️ Cleared signals from MongoDB for answer={}", answerId);
    }

    // ========== 私有辅助方法 ==========

    private String getUserAnswerKey(String userId, String answerId) {
        return userId + ":" + answerId;
    }

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
        explanation.append(String.format("基于%d个行为信号推断(MongoDB): ", signals.size()));

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
        
        long totalSignals = mongoTemplate.count(new Query(), COLLECTION_SIGNALS);
        long totalScores = mongoTemplate.count(new Query(), COLLECTION_SCORES);
        
        // 统计用户数
        Query userQuery = new Query();
        List<BehaviorSignalEvent> allSignals = mongoTemplate.find(userQuery, BehaviorSignalEvent.class, COLLECTION_SIGNALS);
        long totalUsers = allSignals.stream()
                .map(BehaviorSignalEvent::getUserId)
                .distinct()
                .count();
        
        // 统计答案数
        long totalAnswers = allSignals.stream()
                .map(BehaviorSignalEvent::getAnswerId)
                .distinct()
                .count();
        
        stats.put("totalUsers", totalUsers);
        stats.put("totalAnswers", totalAnswers);
        stats.put("totalSignals", totalSignals);
        stats.put("cachedScores", attitudeCache.size());
        stats.put("persistedScores", totalScores);
        stats.put("storage", "MongoDB (Historical Analysis)");
        
        return stats;
    }

    /**
     * 获取用户行为画像（历史分析功能）
     */
    public Map<String, Object> getUserProfile(String userId, int recentDays) {
        if (userId == null) {
            return Collections.emptyMap();
        }

        LocalDateTime since = LocalDateTime.now().minusDays(recentDays);
        
        Query query = new Query(Criteria.where("userId").is(userId)
                .and("timestamp").gte(since));
        List<BehaviorSignalEvent> signals = mongoTemplate.find(query, BehaviorSignalEvent.class, COLLECTION_SIGNALS);

        Map<String, Object> profile = new HashMap<>();
        profile.put("userId", userId);
        profile.put("period", recentDays + " days");
        profile.put("totalSignals", signals.size());
        
        // 信号类型分布
        Map<top.yumbo.ai.behavior.api.model.SignalType, Long> signalDistribution = signals.stream()
                .collect(Collectors.groupingBy(BehaviorSignalEvent::getSignalType, Collectors.counting()));
        profile.put("signalDistribution", signalDistribution);
        
        // 活跃度分析
        Map<String, Long> dailyActivity = signals.stream()
                .collect(Collectors.groupingBy(
                        s -> s.getTimestamp().toLocalDate().toString(),
                        Collectors.counting()
                ));
        profile.put("dailyActivity", dailyActivity);
        
        log.info("👤 User profile generated for user={}: {} signals in {} days", 
                userId, signals.size(), recentDays);
        
        return profile;
    }
}
