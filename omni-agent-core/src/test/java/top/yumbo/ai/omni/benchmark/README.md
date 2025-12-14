# OmniAgent 性能基准测试

**版本**: 1.0.1  
**日期**: 2025-12-15  
**P1-3任务**: 性能基准测试

---

## 📋 概述

使用JMH (Java Microbenchmark Harness) 对OmniAgent核心组件进行性能基准测试。

### 测试范围

1. **KnowledgeLoader** - 知识加载器性能
2. **FeedbackService** - 反馈服务性能
3. **RoleService** - 角色服务性能
4. **EvolutionService** - 演化服务性能

---

## 🚀 运行基准测试

### 方法1: 使用BenchmarkRunner

```bash
# 运行所有基准测试
cd omni-agent-core/src/test/java
javac -cp [classpath] top/yumbo/ai/omni/benchmark/BenchmarkRunner.java
java -cp [classpath] top.yumbo.ai.omni.benchmark.BenchmarkRunner
```

### 方法2: 直接运行单个基准测试

```bash
# KnowledgeLoader基准测试
java -cp [classpath] top.yumbo.ai.omni.benchmark.KnowledgeLoaderBenchmark

# CoreServices基准测试
java -cp [classpath] top.yumbo.ai.omni.benchmark.CoreServicesBenchmark
```

### 方法3: 使用IDE

直接运行各基准测试类的`main()`方法：
- `KnowledgeLoaderBenchmark.main()`
- `CoreServicesBenchmark.main()`
- `BenchmarkRunner.main()`

---

## 📊 基准测试列表

### KnowledgeLoaderBenchmark (9个测试)

| # | 测试名称 | 描述 | 关键指标 |
|---|---------|------|----------|
| 1 | testCacheHit | 缓存命中性能 | 耗时(μs) |
| 2 | testCacheMiss | 缓存未命中性能 | 耗时(μs) |
| 3 | testBatchLoad | 批量加载性能 (10条) | 耗时(μs) |
| 4 | testLRUEviction | LRU淘汰性能 | 耗时(μs) |
| 5 | testStatisticsAccess | 统计信息访问 | 耗时(μs) |
| 6 | testCacheRefresh | 缓存刷新性能 | 耗时(μs) |
| 7 | testSingleThreadRead | 单线程读取 | 耗时(μs) |
| 8 | testMultiThreadRead | 4线程并发读取 | 耗时(μs) |
| 9 | testMixedWorkload | 混合负载(70%读30%写) | 耗时(μs) |

### CoreServicesBenchmark (15个测试)

#### FeedbackService (4个测试)
| # | 测试名称 | 描述 |
|---|---------|------|
| 1 | testCollectExplicitFeedback | 收集显式反馈 |
| 2 | testCollectImplicitFeedback | 收集隐式反馈 |
| 3 | testGetSessionFeedback | 获取会话反馈 |
| 4 | testGetFeedbackStatistics | 获取反馈统计 |

#### RoleService (6个测试)
| # | 测试名称 | 描述 |
|---|---------|------|
| 5 | testRegisterRole | 注册角色 |
| 6 | testGetRole | 获取角色 |
| 7 | testGetEnabledRoles | 获取所有启用角色 |
| 8 | testMatchRolesByKeywords | 关键词匹配角色 |
| 9 | testRecordRoleUsage | 记录角色使用 |
| 10 | testGetRoleUsageStats | 获取使用统计 |

#### EvolutionService (4个测试)
| # | 测试名称 | 描述 |
|---|---------|------|
| 11 | testCreateVersion | 创建概念版本 |
| 12 | testGetCurrentVersion | 获取当前版本 |
| 13 | testGetVersionHistory | 获取版本历史 |
| 14 | testGetVersionStatistics | 获取版本统计 |

#### 综合测试 (1个测试)
| # | 测试名称 | 描述 |
|---|---------|------|
| 15 | testIntegratedWorkflow | 综合工作流测试 |

**总计**: 24个基准测试

---

## 🔧 配置说明

### JMH配置参数

```java
@BenchmarkMode(Mode.AverageTime)        // 测量平均时间
@OutputTimeUnit(TimeUnit.MICROSECONDS)  // 输出单位：微秒
@State(Scope.Thread)                    // 每个线程独立状态
@Fork(value = 1, warmups = 1)           // 1个JVM进程，1次预热fork
@Warmup(iterations = 3, time = 1)       // 3次预热，每次1秒
@Measurement(iterations = 5, time = 1)  // 5次测量，每次1秒
```

### 修改配置

可以在基准测试类中修改注解参数：
- **增加精确度**: 增加`iterations`
- **加快测试**: 减少`iterations`
- **多线程测试**: 使用`@Threads(n)`
- **更改模式**: `Mode.Throughput` / `Mode.SampleTime` / `Mode.All`

---

## 📈 预期性能指标

### KnowledgeLoader

| 操作 | 预期耗时 | 吞吐量 |
|------|---------|--------|
| 缓存命中 | < 1 μs | > 1M ops/s |
| 缓存未命中 | < 10 μs | > 100K ops/s |
| LRU淘汰 | < 2 μs | > 500K ops/s |
| 批量加载(10) | < 50 μs | > 20K ops/s |

### FeedbackService

| 操作 | 预期耗时 |
|------|---------|
| 收集反馈 | < 50 μs |
| 获取反馈 | < 10 μs |
| 统计信息 | < 100 μs |

### RoleService

| 操作 | 预期耗时 |
|------|---------|
| 注册角色 | < 20 μs |
| 获取角色 | < 5 μs |
| 关键词匹配 | < 50 μs |

### EvolutionService

| 操作 | 预期耗时 |
|------|---------|
| 创建版本 | < 100 μs |
| 获取版本 | < 10 μs |
| 版本历史 | < 50 μs |

---

## 📊 结果分析

### 输出格式

```
Benchmark                                          Mode  Cnt   Score   Error  Units
KnowledgeLoaderBenchmark.testCacheHit              avgt    5   0.523 ± 0.012  us/op
KnowledgeLoaderBenchmark.testCacheMiss             avgt    5   8.245 ± 0.345  us/op
```

### 关键指标

- **Score**: 平均耗时（越低越好）
- **Error**: 误差范围
- **Units**: 单位（us/op = 微秒/操作）

### 性能判断标准

| 级别 | 耗时范围 | 评价 |
|------|---------|------|
| 优秀 | < 1 μs | ⭐⭐⭐⭐⭐ |
| 良好 | 1-10 μs | ⭐⭐⭐⭐ |
| 一般 | 10-100 μs | ⭐⭐⭐ |
| 较慢 | 100-1000 μs | ⭐⭐ |
| 慢 | > 1000 μs | ⭐ |

---

## 🔍 故障排查

### 问题1: 编译错误

```bash
# 确保JMH依赖已添加到pom.xml
mvn clean compile
```

### 问题2: 运行超时

```bash
# 减少迭代次数
@Warmup(iterations = 1)
@Measurement(iterations = 2)
```

### 问题3: 内存不足

```bash
# 增加JVM堆内存
java -Xmx4g -jar benchmark.jar
```

---

## 📝 最佳实践

1. **预热充分**: 至少3次预热迭代
2. **多次测量**: 至少5次测量迭代
3. **隔离环境**: 关闭其他程序
4. **稳定系统**: 避免CPU频率调整
5. **重复验证**: 多次运行确认结果

---

## 🚀 后续优化方向

### 短期
1. 优化缓存命中率
2. 减少对象创建
3. 使用对象池

### 中期
1. 异步处理
2. 批量操作优化
3. 数据结构优化

### 长期
1. 分布式缓存
2. 持久化层优化
3. 硬件加速

---

## 📚 参考资料

- [JMH官方文档](https://github.com/openjdk/jmh)
- [JMH样例](https://github.com/openjdk/jmh/tree/master/jmh-samples/src/main/java/org/openjdk/jmh/samples)
- [Java性能优化指南](https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/doc-files/performanceguide.html)

---

**维护者**: OmniAgent Team  
**最后更新**: 2025-12-15
