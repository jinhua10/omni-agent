# OmniAgent RAG 文档索引

## 📚 核心概念文档

### 1. EmbeddingService、RAGService 和 PPL 的关系
**文件**: [BATCH_MERGE_FIX.md](./BATCH_MERGE_FIX.md)

**内容摘要**:
- EmbeddingService（向量生成服务）的作用
- RAGService（检索增强生成服务）的功能
- PPL 困惑度模型与向量模型的区别
- 重构前的 ONNX Runtime 实现详解
- Maven 依赖配置和核心代码示例

**适合人群**: 
- 想了解系统架构的开发者
- 需要理解各个服务关系的技术人员
- 对 ONNX Runtime 实现感兴趣的工程师

---

### 2. 不使用向量模型实现 RAG
**文件**: [RAG_WITHOUT_EMBEDDING.md](./RAG_WITHOUT_EMBEDDING.md) ⭐ **推荐**

**内容摘要**:
- ✅ **完全可以不用向量模型！**
- 三种非向量化检索方案详解
- Lucene、H2、Elasticsearch 等实现对比
- 快速开始指南和配置示例
- 实际应用场景和最佳实践
- 性能测试数据（实测）

**适合人群**:
- 想快速实现 RAG 的开发者
- 资源受限的项目团队
- 需要高性能检索的场景
- 对向量模型不熟悉的初学者

**快速跳转**:
- [快速开始 - Lucene 方案](./RAG_WITHOUT_EMBEDDING.md#方案-1使用-lucene推荐)
- [性能对比数据](./RAG_WITHOUT_EMBEDDING.md#📈-性能对比实测数据)
- [使用场景建议](./RAG_WITHOUT_EMBEDDING.md#🤔-如何选择)

---

### 3. 向量检索 vs 全文检索对比指南
**文件**: [RAG_COMPARISON_GUIDE.md](./RAG_COMPARISON_GUIDE.md) ⭐⭐ **强烈推荐**

**内容摘要**:
- 📊 全维度对比表格（18个维度）
- 🎯 使用场景推荐（何时用哪种方案）
- 💰 成本对比（10万条文档实测）
- 📈 性能测试数据（索引、查询、准确率）
- 🚀 不同规模项目的实施建议
- 💡 **核心结论：80% 的场景，全文检索就够了！**

**适合人群**:
- 架构师和技术决策者
- 需要选型的项目负责人
- 对性能和成本敏感的团队
- 想了解全局对比的开发者

**快速跳转**:
- [快速对比表](./RAG_COMPARISON_GUIDE.md#📊-快速对比表)
- [使用场景推荐](./RAG_COMPARISON_GUIDE.md#🎯-使用场景推荐)
- [成本对比](./RAG_COMPARISON_GUIDE.md#💰-成本对比10万条文档768维向量)
- [性能测试数据](./RAG_COMPARISON_GUIDE.md#📈-性能测试数据实测)

---

## 🔧 配置和示例

### 4. 纯文本检索配置示例
**文件**: [application-text-only-rag.yml](./application-text-only-rag.yml)

**内容摘要**:
- 6 种后端的完整配置示例
  - Lucene (推荐)
  - H2 (嵌入式)
  - SQLite (轻量级)
  - Elasticsearch (企业级)
  - MongoDB (NoSQL)
  - Redis (高速缓存)
- 性能调优建议
- 生产环境配置

**使用方式**:
```bash
# 复制配置文件
cp docs/application-text-only-rag.yml src/main/resources/application.yml

# 选择配置方案（默认 Lucene）
spring:
  profiles:
    active: default  # 或 h2, sqlite, elasticsearch, mongodb, redis
```

---

### 5. 代码示例
**文件**: [omni-agent-example-basic/src/main/java/top/yumbo/ai/omni/example/TextOnlyRAGExample.java](../omni-agent-example-basic/src/main/java/top/yumbo/ai/omni/example/TextOnlyRAGExample.java)

**内容摘要**:
- ✅ 8 个完整的使用示例
  1. 索引文档（不使用向量）
  2. 纯文本检索
  3. 多字段检索
  4. 带过滤条件的检索
  5. 专业术语精确匹配
  6. 代码搜索
  7. 性能对比测试
  8. 批量索引测试

**运行方式**:
```java
@Autowired
private TextOnlyRAGExample example;

// 运行所有示例
example.runAllExamples();
```

---

## 🎯 快速导航

### 我想...

#### 快速开始，不想配置向量模型
👉 [RAG_WITHOUT_EMBEDDING.md - 快速开始](./RAG_WITHOUT_EMBEDDING.md#🚀-快速开始不使用向量的-rag)

#### 了解向量检索和全文检索的区别
👉 [RAG_COMPARISON_GUIDE.md - 快速对比表](./RAG_COMPARISON_GUIDE.md#📊-快速对比表)

#### 选择合适的技术方案
👉 [RAG_COMPARISON_GUIDE.md - 使用场景推荐](./RAG_COMPARISON_GUIDE.md#🎯-使用场景推荐)

#### 了解性能和成本
👉 [RAG_COMPARISON_GUIDE.md - 成本对比](./RAG_COMPARISON_GUIDE.md#💰-成本对比10万条文档768维向量)

#### 看实际代码示例
👉 [TextOnlyRAGExample.java](../omni-agent-example-basic/src/main/java/top/yumbo/ai/omni/example/TextOnlyRAGExample.java)

#### 配置生产环境
👉 [application-text-only-rag.yml](./application-text-only-rag.yml)

#### 了解重构前的实现
👉 [BATCH_MERGE_FIX.md - 重构前实现](./BATCH_MERGE_FIX.md#🔧-重构前的实现old-目录)

---

## 📖 学习路径建议

### 初学者路径

1. **第一步：理解基本概念**
   - 阅读 [BATCH_MERGE_FIX.md](./BATCH_MERGE_FIX.md)
   - 了解 EmbeddingService 和 RAGService 的区别

2. **第二步：快速上手**
   - 阅读 [RAG_WITHOUT_EMBEDDING.md](./RAG_WITHOUT_EMBEDDING.md)
   - 按照快速开始指南配置 Lucene
   - 运行 [TextOnlyRAGExample.java](../omni-agent-example-basic/src/main/java/top/yumbo/ai/omni/example/TextOnlyRAGExample.java)

3. **第三步：优化和调优**
   - 参考 [application-text-only-rag.yml](./application-text-only-rag.yml)
   - 根据实际需求调整配置

4. **第四步：评估是否需要向量检索**
   - 阅读 [RAG_COMPARISON_GUIDE.md](./RAG_COMPARISON_GUIDE.md)
   - 对比性能和成本
   - 做出技术选型决策

---

### 架构师路径

1. **第一步：全局对比**
   - 重点阅读 [RAG_COMPARISON_GUIDE.md](./RAG_COMPARISON_GUIDE.md)
   - 理解各方案的优劣势

2. **第二步：评估需求**
   - 确定文档数量、查询类型、性能要求
   - 参考"使用场景推荐"做选型

3. **第三步：成本估算**
   - 参考"成本对比"章节
   - 评估服务器、存储、带宽成本

4. **第四步：技术验证**
   - 小规模 POC 测试
   - 运行性能测试
   - 评估实际效果

---

## ❓ 常见问题

### Q1: 不用向量模型，检索效果会差吗？

**A**: 不一定！对于关键词明确的场景（如专业术语、代码搜索），全文检索效果**更好**。

参考: [RAG_COMPARISON_GUIDE.md - 准确率对比](./RAG_COMPARISON_GUIDE.md#准确率对比)

---

### Q2: 什么情况下必须用向量检索？

**A**: 只有在需要**语义理解**时才必须用向量检索，如：
- 智能客服（用户问法多样）
- 内容推荐（相似度匹配）
- 跨语言搜索

参考: [RAG_COMPARISON_GUIDE.md - 使用场景](./RAG_COMPARISON_GUIDE.md#✅-推荐使用向量检索的场景)

---

### Q3: 如何在两种方案之间切换？

**A**: 系统已经支持灵活切换：

```java
// 文本检索（不用向量）
Query textQuery = Query.builder()
    .text("ONNX Runtime")
    .mode(SearchMode.TEXT)  // 指定 TEXT 模式
    .build();

// 向量检索（需要配置 EmbeddingService）
Query vectorQuery = Query.builder()
    .text("ONNX Runtime")
    .mode(SearchMode.SEMANTIC)  // 语义搜索
    .build();
```

参考: [TextOnlyRAGExample.java](../omni-agent-example-basic/src/main/java/top/yumbo/ai/omni/example/TextOnlyRAGExample.java)

---

### Q4: 性能差异有多大？

**A**: 实测数据（10,000 条文档）：

| 指标 | 全文检索 | 向量检索 | 差异 |
|------|---------|---------|------|
| 查询延迟 | 3ms | 45ms | **15倍** |
| QPS | 300 | 22 | **14倍** |
| 内存占用 | 100MB | 1.8GB | **18倍** |

参考: [RAG_COMPARISON_GUIDE.md - 性能测试](./RAG_COMPARISON_GUIDE.md#查询性能)

---

### Q5: 我的项目该选哪个方案？

**A**: 快速判断：

```
文档量 < 10,000 条？ → 全文检索 (Lucene)
关键词明确？ → 全文检索
资源受限？ → 全文检索
需要语义理解？ → 向量检索
文档量 > 100,000 条？ → 向量检索或混合
```

参考: [RAG_COMPARISON_GUIDE.md - 实施建议](./RAG_COMPARISON_GUIDE.md#🚀-实施建议)

---

## 🎓 进阶阅读

### 技术原理

- [Apache Lucene 官方文档](https://lucene.apache.org/)
- [BM25 算法详解](https://en.wikipedia.org/wiki/Okapi_BM25)
- [Elasticsearch 全文搜索](https://www.elastic.co/guide/en/elasticsearch/reference/current/full-text-queries.html)

### 向量检索（高级）

- ONNX Runtime 实现: [BATCH_MERGE_FIX.md](./BATCH_MERGE_FIX.md#🔧-重构前的实现old-目录)
- 混合检索策略: [RAG_COMPARISON_GUIDE.md](./RAG_COMPARISON_GUIDE.md#🎨-混合方案推荐)

---

## 📝 文档贡献

如有问题或建议，欢迎提交 Issue 或 Pull Request！

---

## 🏷️ 标签导航

- #RAG #检索增强生成
- #全文检索 #Lucene #BM25
- #向量检索 #Embedding #ONNX
- #性能对比 #成本分析
- #快速开始 #配置示例
- #最佳实践 #架构设计

---

**最后更新**: 2025-12-25
**维护者**: OmniAgent Team

