# OmniAgent Behavior Analysis Module 🧠

**行为分析与态度推断模块** - 基于用户隐式行为信号的智能态度推断系统

## 📋 目录

- [模块概述](#模块概述)
- [核心功能](#核心功能)
- [快速开始](#快速开始)
- [API接口](#api接口)
- [使用示例](#使用示例)
- [配置说明](#配置说明)
- [扩展开发](#扩展开发)

---

## 模块概述

行为分析模块通过收集和分析用户的隐式行为信号（浏览、停留、复制、点赞等），推断用户对答案的真实态度，比显式评分更准确地反映用户满意度。

### 架构设计

```
┌─────────────────────────────────────────────────┐
│            Behavior Analysis API                │
│  ┌──────────────────────────────────────────┐  │
│  │   BehaviorAnalysisService 接口           │  │
│  └──────────────────────────────────────────┘  │
└─────────────────────────────────────────────────┘
                      ↓
┌─────────────────────────────────────────────────┐
│              Behavior Starters                  │
│  ┌──────────────────────────────────────────┐  │
│  │   Memory Starter (当前已实现)            │  │
│  │   Redis Starter (计划中)                │  │
│  │   MongoDB Starter (计划中)              │  │
│  │   Elasticsearch Starter (计划中)        │  │
│  └──────────────────────────────────────────┘  │
└─────────────────────────────────────────────────┘
```

---

## 核心功能

### 1. 行为信号收集 📡

支持10种行为信号类型：

| 信号类型 | 说明 | 权重方向 | 基础权重 |
|---------|------|---------|---------|
| `VIEW` | 浏览 | 正面 (+0.1) | 0.1 |
| `DWELL` | 停留 | 正面 (+0.5) | 0.3 |
| `COPY` | 复制 | 正面 (+0.7) | 0.5 |
| `LIKE` | 点赞 | 正面 (+1.0) | 1.0 |
| `DISLIKE` | 踩 | 负面 (-1.0) | 1.0 |
| `SHARE` | 分享 | 正面 (+0.9) | 0.8 |
| `BOOKMARK` | 收藏 | 正面 (+0.8) | 0.7 |
| `COMMENT` | 评论 | 正面 (+0.6) | 0.6 |
| `SEARCH` | 搜索 | 正面 (+0.3) | 0.2 |
| `CLICK` | 点击 | 正面 (+0.4) | 0.4 |

### 2. 态度推断 🎯

将多维度行为信号聚合为态度评分：

- **原始评分**：-1.0（非常不满意）到 +1.0（非常满意）
- **归一化评分**：0.0 到 1.0，便于与显式评分对齐
- **置信度**：0.0 到 1.0，表示推断的可信程度
- **态度等级**：5个等级（非常满意、满意、中立、不满意、非常不满意）

### 3. 热度计算 🔥

基于多维度行为信号计算答案的受欢迎程度，用于：
- 热门答案排序
- 推荐系统
- 内容质量评估

### 4. 时间衰减 ⏰

行为信号随时间衰减，近期信号权重更高：
- 点赞/踩：衰减因子 0.05（衰减最慢）
- 复制：衰减因子 0.1
- 浏览：衰减因子 0.2（衰减最快）

---

## 快速开始

### 1. 添加依赖

```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-behavior-starter-memory</artifactId>
    <version>1.0.0</version>
</dependency>
```

### 2. 自动配置

Spring Boot 会自动配置 `BehaviorAnalysisService`，无需额外配置。

### 3. 使用服务

```java
@Service
public class MyService {
    
    @Autowired
    private BehaviorAnalysisService behaviorService;
    
    public void trackUserBehavior(String userId, String answerId) {
        // 收集行为信号
        BehaviorSignalEvent signal = new BehaviorSignalEvent(
            userId, 
            "qa-123", 
            answerId, 
            SignalType.LIKE
        );
        behaviorService.collectSignal(signal);
        
        // 推断用户态度
        AttitudeScore attitude = behaviorService.inferAttitude(userId, answerId);
        System.out.println("态度评分: " + attitude.getRawScore());
        System.out.println("态度等级: " + attitude.getLevel().getZhName());
        System.out.println("置信度: " + attitude.getConfidence());
    }
}
```

---

## API接口

### BehaviorAnalysisService

#### 信号收集

```java
// 收集单个行为信号
void collectSignal(BehaviorSignalEvent signal);

// 批量收集行为信号
void collectSignals(List<BehaviorSignalEvent> signals);
```

#### 态度推断

```java
// 推断用户对答案的态度
AttitudeScore inferAttitude(String userId, String answerId);

// 批量推断态度
Map<String, AttitudeScore> inferAttitudes(String userId, List<String> answerIds);
```

#### 查询信号

```java
// 获取用户的所有行为信号
List<BehaviorSignalEvent> getUserSignals(String userId);

// 获取特定答案的所有行为信号
List<BehaviorSignalEvent> getAnswerSignals(String answerId);

// 获取用户对特定答案的行为信号
List<BehaviorSignalEvent> getUserAnswerSignals(String userId, String answerId);
```

#### 热度分析

```java
// 计算答案的热度分数
double calculateHotness(String answerId);

// 获取热门答案列表
List<String> getHotAnswers(int topN);
```

#### 数据清理

```java
// 清除用户的行为信号
void clearUserSignals(String userId);

// 清除特定答案的行为信号
void clearAnswerSignals(String answerId);
```

---

## 使用示例

### 示例1：收集用户行为

```java
@RestController
@RequestMapping("/api/answers")
public class AnswerController {
    
    @Autowired
    private BehaviorAnalysisService behaviorService;
    
    // 用户浏览答案
    @PostMapping("/{answerId}/view")
    public void trackView(@PathVariable String answerId, @RequestParam String userId) {
        BehaviorSignalEvent signal = new BehaviorSignalEvent(
            userId, "qa-" + answerId, answerId, SignalType.VIEW
        );
        signal.addContext("viewDuration", System.currentTimeMillis());
        behaviorService.collectSignal(signal);
    }
    
    // 用户点赞答案
    @PostMapping("/{answerId}/like")
    public void trackLike(@PathVariable String answerId, @RequestParam String userId) {
        BehaviorSignalEvent signal = new BehaviorSignalEvent(
            userId, "qa-" + answerId, answerId, SignalType.LIKE
        );
        behaviorService.collectSignal(signal);
    }
    
    // 用户停留
    @PostMapping("/{answerId}/dwell")
    public void trackDwell(@PathVariable String answerId, @RequestParam String userId, 
                           @RequestParam long duration) {
        BehaviorSignalEvent signal = new BehaviorSignalEvent(
            userId, "qa-" + answerId, answerId, SignalType.DWELL
        );
        signal.addContext("duration", duration); // 毫秒
        signal.setStrength(Math.min(duration / 10000.0, 1.0)); // 10秒为满分
        behaviorService.collectSignal(signal);
    }
}
```

### 示例2：推断用户态度

```java
@Service
public class RecommendationService {
    
    @Autowired
    private BehaviorAnalysisService behaviorService;
    
    // 获取用户对多个答案的态度
    public Map<String, AttitudeScore> getUserAttitudes(String userId, List<String> answerIds) {
        return behaviorService.inferAttitudes(userId, answerIds);
    }
    
    // 推荐用户可能喜欢的答案
    public List<String> recommendAnswers(String userId, List<String> candidateAnswers) {
        Map<String, AttitudeScore> attitudes = getUserAttitudes(userId, candidateAnswers);
        
        return attitudes.entrySet().stream()
            .filter(e -> e.getValue().isPositive()) // 只推荐正面态度的答案
            .filter(e -> e.getValue().getConfidence() > 0.5) // 置信度>50%
            .sorted((e1, e2) -> Double.compare(
                e2.getValue().getRawScore(), 
                e1.getValue().getRawScore()
            ))
            .map(Map.Entry::getKey)
            .limit(10)
            .collect(Collectors.toList());
    }
}
```

### 示例3：热门答案排行

```java
@Service
public class HotAnswerService {
    
    @Autowired
    private BehaviorAnalysisService behaviorService;
    
    // 获取热门答案
    public List<String> getHotAnswers(int topN) {
        return behaviorService.getHotAnswers(topN);
    }
    
    // 获取答案的热度详情
    public Map<String, Object> getHotnessDetails(String answerId) {
        double hotness = behaviorService.calculateHotness(answerId);
        List<BehaviorSignalEvent> signals = behaviorService.getAnswerSignals(answerId);
        
        Map<SignalType, Long> signalCounts = signals.stream()
            .collect(Collectors.groupingBy(
                BehaviorSignalEvent::getSignalType, 
                Collectors.counting()
            ));
        
        Map<String, Object> details = new HashMap<>();
        details.put("hotness", hotness);
        details.put("totalSignals", signals.size());
        details.put("signalBreakdown", signalCounts);
        return details;
    }
}
```

### 示例4：用户满意度分析

```java
@Service
public class SatisfactionAnalysisService {
    
    @Autowired
    private BehaviorAnalysisService behaviorService;
    
    // 分析用户满意度
    public Map<String, Object> analyzeSatisfaction(String userId, String answerId) {
        AttitudeScore attitude = behaviorService.inferAttitude(userId, answerId);
        List<BehaviorSignalEvent> signals = behaviorService.getUserAnswerSignals(userId, answerId);
        
        Map<String, Object> analysis = new HashMap<>();
        analysis.put("score", attitude.getRawScore());
        analysis.put("normalizedScore", attitude.getNormalizedScore());
        analysis.put("level", attitude.getLevel().getZhName());
        analysis.put("confidence", attitude.getConfidence());
        analysis.put("isPositive", attitude.isPositive());
        analysis.put("isNegative", attitude.isNegative());
        analysis.put("signalCount", signals.size());
        analysis.put("explanation", attitude.getExplanation());
        
        return analysis;
    }
    
    // 生成满意度报告
    public String generateReport(String userId, List<String> answerIds) {
        Map<String, AttitudeScore> attitudes = behaviorService.inferAttitudes(userId, answerIds);
        
        long positive = attitudes.values().stream().filter(AttitudeScore::isPositive).count();
        long negative = attitudes.values().stream().filter(AttitudeScore::isNegative).count();
        long neutral = attitudes.values().stream().filter(AttitudeScore::isNeutral).count();
        
        double avgScore = attitudes.values().stream()
            .mapToDouble(AttitudeScore::getRawScore)
            .average()
            .orElse(0.0);
        
        return String.format(
            "用户满意度报告:\n" +
            "- 总答案数: %d\n" +
            "- 正面态度: %d (%.1f%%)\n" +
            "- 负面态度: %d (%.1f%%)\n" +
            "- 中立态度: %d (%.1f%%)\n" +
            "- 平均评分: %.2f",
            answerIds.size(),
            positive, positive * 100.0 / answerIds.size(),
            negative, negative * 100.0 / answerIds.size(),
            neutral, neutral * 100.0 / answerIds.size(),
            avgScore
        );
    }
}
```

---

## 配置说明

### 信号权重自定义

可以自定义信号类型的权重配置：

```java
@Configuration
public class BehaviorConfig {
    
    @Bean
    public BehaviorAnalysisService customBehaviorService() {
        MemoryBehaviorAnalysisService service = new MemoryBehaviorAnalysisService();
        
        // 自定义点赞权重
        SignalWeight likeWeight = new SignalWeight(SignalType.LIKE, 1.0, 1.0, 0.03);
        // 自定义复制权重
        SignalWeight copyWeight = new SignalWeight(SignalType.COPY, 0.7, 0.8, 0.08);
        
        // 注意：当前版本的MemoryBehaviorAnalysisService使用内部默认权重
        // 如需自定义，可扩展实现
        
        return service;
    }
}
```

---

## 扩展开发

### 实现自定义 Starter

参考 `omni-agent-behavior-starter-memory` 实现：

1. **创建实现类**：实现 `BehaviorAnalysisService` 接口
2. **创建自动配置类**：使用 `@AutoConfiguration` 注解
3. **注册自动配置**：在 `META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports` 中注册

### Redis Starter 实现示例

```java
@Service
public class RedisBehaviorAnalysisService implements BehaviorAnalysisService {
    
    @Autowired
    private RedisTemplate<String, Object> redisTemplate;
    
    @Override
    public void collectSignal(BehaviorSignalEvent signal) {
        String key = "behavior:signal:" + signal.getUserId() + ":" + signal.getAnswerId();
        redisTemplate.opsForList().rightPush(key, signal);
    }
    
    @Override
    public AttitudeScore inferAttitude(String userId, String answerId) {
        String key = "behavior:signal:" + userId + ":" + answerId;
        List<Object> signals = redisTemplate.opsForList().range(key, 0, -1);
        // 态度推断逻辑...
        return new AttitudeScore(userId, answerId, 0.0, 0.0);
    }
    
    // 实现其他方法...
}
```

---

## 性能优化建议

### 1. 缓存策略
- ✅ 已实现：态度评分缓存
- 建议：使用 Redis 缓存热门答案的态度评分

### 2. 批量处理
- ✅ 已实现：批量收集信号、批量推断态度
- 建议：异步批量处理用户行为日志

### 3. 数据清理
- 建议：定期清理过期信号（如超过30天的信号）
- 建议：实现信号归档功能

### 4. 热点数据
- 建议：对热门答案的信号进行预计算
- 建议：使用布隆过滤器优化查询

---

## 最佳实践

### 1. 信号采集
- ✅ 在用户交互时立即采集信号
- ✅ 为停留信号设置合理的强度值
- ✅ 记录上下文信息（如停留时长、点击位置）

### 2. 态度推断
- ✅ 结合多种信号类型，避免单一信号偏差
- ✅ 关注置信度，低置信度时谨慎使用推断结果
- ✅ 定期更新态度评分（清除缓存）

### 3. 数据分析
- ✅ 使用热度计算识别优质内容
- ✅ 分析用户满意度趋势
- ✅ 结合显式反馈与隐式行为

---

## 技术规格

- **Java版本**：21
- **Spring Boot版本**：3.2.11
- **并发安全**：使用 ConcurrentHashMap
- **时间复杂度**：
  - 信号收集：O(1)
  - 态度推断：O(n)，n为信号数量
  - 热度计算：O(n)

---

## 路线图

### 当前版本 (1.0.0)
- ✅ Memory Starter实现
- ✅ 10种行为信号类型
- ✅ 态度推断算法
- ✅ 热度计算
- ✅ 时间衰减

### 计划中
- 🔲 Redis Starter
- 🔲 MongoDB Starter
- 🔲 Elasticsearch Starter
- 🔲 可视化仪表板
- 🔲 A/B测试支持
- 🔲 机器学习增强

---

## 常见问题

**Q: 为什么使用隐式行为而不是显式评分？**  
A: 隐式行为更真实，用户在自然交互中产生的信号比主动评分更能反映真实满意度。

**Q: 态度推断的准确率如何？**  
A: 依赖信号数量和类型，通常10+个信号可达到80%+置信度。

**Q: 如何处理用户恶意行为？**  
A: 建议结合用户信誉系统、异常检测算法过滤异常信号。

**Q: 是否支持实时推断？**  
A: 是的，内存实现支持实时推断（毫秒级），缓存命中更快。

---

## 贡献指南

欢迎贡献代码、报告问题、提出建议！

1. Fork 项目
2. 创建特性分支
3. 提交更改
4. 推送到分支
5. 开启 Pull Request

---

## 许可证

Apache License 2.0

---

**维护者**: OmniAgent Team  
**最后更新**: 2025-12-15

