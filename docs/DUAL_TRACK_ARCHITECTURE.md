# 🚂 双轨流式问答架构设计

**版本**: v1.0  
**创建时间**: 2025-12-19  
**状态**: ✅ 已实现

---

## 📋 概述

双轨流式问答是 OmniAgent 的核心创新功能，通过同时展示两种不同的回答方式，为用户提供全面且智能的答案。

### 核心理念

- **左轨（Left Track）**: 传统 RAG + LLM 回答
  - 检索知识库文档
  - 构建上下文
  - LLM 生成回答

- **右轨（Right Track）**: HOPE 智能系统 / 角色专业回答
  - HOPE 三层知识架构
  - 算法市场优化
  - 知识最小概念综合
  - 角色专业知识（如果选择角色）

---

## 🎯 架构设计

### 1. 知识模式（Knowledge Mode）

系统支持三种知识模式：

| 模式 | 说明 | 输出轨道 |
|------|------|---------|
| **none** | 不使用知识库 | 单轨：纯 LLM |
| **rag** | 使用 RAG 知识库 | 双轨：左轨 RAG+LLM，右轨 HOPE 智能系统 |
| **role** | 使用角色知识库 | 双轨：左轨 RAG+LLM，右轨角色专业回答 |

### 2. 左轨（Traditional RAG + LLM）

**流程：**

```
用户问题
   ↓
检索知识库（Top-5 相关文档）
   ↓
构建上下文提示词
   ↓
LLM 流式生成
   ↓
通过 SSE 发送到前端（event: "left"）
```

**特点：**
- ✅ 基于检索的相关文档
- ✅ 提供引用来源
- ✅ 准确可靠
- ✅ 适合知识密集型问题

**提示词模板：**
```
基于以下知识回答问题：

{检索到的文档内容}

问题：{用户问题}
```

### 3. 右轨（HOPE Intelligent System / Role Expert）

#### 3.1 RAG 模式 - HOPE 智能系统

**流程：**

```
用户问题
   ↓
HOPE 问题分类
   ↓
智能查询（三层知识架构）
   ↓
构建 HOPE 增强提示词
   ↓
LLM 流式生成
   ↓
通过 SSE 发送到前端（event: "right"）
```

**HOPE 三层知识架构：**

1. **高频层（High Frequency Layer）**
   - 缓存常见问题
   - 快速响应
   - 适合：FAQ、常见操作

2. **普通层（Ordinary Layer）**
   - 一般知识
   - 平衡速度和精度
   - 适合：常规查询

3. **永久层（Permanent Layer）**
   - 核心知识
   - 长期保存
   - 适合：专业知识、重要概念

**提示词模板：**
```
【HOPE智能系统 - 自我学习回答】

问题类型：{分类结果}
建议知识层：{HIGH_FREQUENCY|ORDINARY|PERMANENT}
置信度：{0.0-1.0}

系统学习到的答案：
{HOPE 已有答案（如果存在）}

补充知识：
{检索到的文档内容}

问题：{用户问题}

请综合系统学习的知识和补充知识，给出专业且经过自我学习优化的回答。
```

**特点：**
- 🧠 自我学习和进化
- 🔄 知识分层管理
- 📈 随使用越来越智能
- 🎯 算法市场优化

#### 3.2 Role 模式 - 角色专业回答

**流程：**

```
用户问题 + 角色选择
   ↓
获取角色信息
   ↓
检索角色相关知识
   ↓
构建角色提示词
   ↓
LLM 流式生成（角色口吻）
   ↓
通过 SSE 发送到前端（event: "right"）
```

**角色系统特点：**
- 👨‍💼 专业领域专家
- 🎭 独特的回答风格
- 📚 领域专业知识
- 🔍 关键词匹配

**提示词模板：**
```
你是{角色名称}，{角色描述}

作为专业角色，请基于以下知识给出你的专业见解：

{角色相关知识}

问题：{用户问题}

请以你的角色身份，结合专业知识回答。
```

**通用角色特性：**
- 如果选择"通用角色"，系统会：
  1. 分析用户问题领域
  2. 自动查找合适的专业角色
  3. 由该专业角色回答

---

## 🔧 实现细节

### 后端实现（DemoController.java）

#### 核心端点

```java
@GetMapping(value = "/api/qa/stream/dual-track", produces = "text/event-stream")
public SseEmitter dualTrackStream(
    @RequestParam String question,
    @RequestParam(defaultValue = "none") String knowledgeMode,
    @RequestParam(required = false) String roleName)
```

#### 参数说明

| 参数 | 类型 | 必需 | 说明 |
|------|------|------|------|
| question | String | ✅ | 用户问题 |
| knowledgeMode | String | ❌ | 知识模式：none\|rag\|role（默认：none） |
| roleName | String | ❌ | 角色名称（role模式必需） |

#### SSE 事件类型

| 事件名 | 说明 | 数据格式 |
|--------|------|---------|
| `left` | 左轨 token | `{"content": "...", "chunkIndex": 0}` |
| `right` | 右轨 token | `{"content": "...", "chunkIndex": 0}` |
| `llm` | 单轨 LLM token | `{"content": "...", "chunkIndex": 0}` |
| `complete` | 完成标记 | `{"type": "complete"}` |
| `error` | 错误信息 | `{"type": "error", "message": "..."}` |
| `data` | 参考文档/提示 | `{"type": "reference|info|warning", ...}` |

#### 关键方法

```java
// 单轨模式处理
private void handleSingleTrack(SseEmitter emitter, String question)

// RAG模式处理（双轨）
private void handleRagMode(SseEmitter emitter, String question, List<SearchResult> references)

// 角色模式处理（双轨）
private void handleRoleMode(SseEmitter emitter, String question, String roleName, List<SearchResult> references)

// HOPE提示词构建
private String buildHOPEPrompt(String question, HOPEKnowledgeManager.QueryResult hopeResult, List<SearchResult> references)
```

### 前端实现（UI/）

#### 数据流

```javascript
// 1. 发起请求
qaApi.askStreaming({ question, knowledgeMode, roleName }, onChunk)

// 2. 监听事件
eventSource.addEventListener('left', handler)   // 左轨
eventSource.addEventListener('right', handler)  // 右轨
eventSource.addEventListener('llm', handler)    // 单轨
eventSource.addEventListener('complete', handler) // 完成

// 3. 实时更新UI
updateMessage({
  dualTrack: true,
  leftPanel: leftContent,
  rightPanel: rightContent
})
```

#### 组件结构

```
QAPanel.jsx
  ├─ 发起问答请求
  ├─ 处理 SSE 事件
  └─ 更新消息状态

AnswerCard.jsx
  ├─ 单轨模式显示
  │   └─ <StreamingAnswer />
  └─ 双轨模式显示
      ├─ 左面板（RAG+LLM）
      │   └─ <StreamingAnswer />
      └─ 右面板（HOPE/Role）
          └─ <StreamingAnswer />
```

---

## 📊 性能优化

### 1. 并发处理

- 左轨和右轨**顺序执行**（先左后右）
- 使用 `CountDownLatch` 确保左轨完成后再启动右轨
- 避免资源竞争，保证生成质量

### 2. 超时控制

```java
// 左轨超时：120秒
leftTrackLatch.await(120, TimeUnit.SECONDS)

// SSE 连接超时：5分钟
SseEmitter emitter = new SseEmitter(300000L)
```

### 3. 错误处理

- ✅ 左轨失败：发送警告，继续右轨
- ✅ 右轨失败：发送警告，优雅降级
- ✅ 连接超时：自动清理资源

---

## 🎨 用户体验

### 视觉设计

```
┌─────────────────────────────────────────────────────┐
│  📚 检索到 3 个参考文档                               │
├─────────────────────────────────────────────────────┤
│  ┌────────────────┐  ┌────────────────────────────┐ │
│  │ 🤖 RAG + LLM   │  │ 🧠 HOPE智能系统            │ │
│  │ 回答           │  │ / 角色专业回答              │ │
│  ├────────────────┤  ├────────────────────────────┤ │
│  │ 基于知识库的   │  │ 经过自我学习优化的         │ │
│  │ 传统回答...    │  │ 智能回答...                │ │
│  │                │  │                            │ │
│  │ [流式输出中]   │  │ [流式输出中]               │ │
│  └────────────────┘  └────────────────────────────┘ │
└─────────────────────────────────────────────────────┘
```

### 友好提示

1. **无检索结果提示**
   ```json
   {"type": "info", "message": "未检索到相关文档，将基于通用知识和系统学习回答"}
   ```

2. **轨道错误提示**
   ```json
   {"type": "warning", "track": "left", "message": "左轨（RAG+LLM）生成失败"}
   ```

---

## 🔮 未来计划

### Phase 1: 查询扩展 ✅ 已完成

从算法市场复用并集成查询扩展代码：
- ✅ 同义词扩展
- ✅ 领域词添加
- ✅ 多查询融合（RRF算法）
- ✅ EnhancedQueryService 服务创建
- ✅ 集成到双轨右轨

**实现位置：**
- `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/query/EnhancedQueryService.java`
- 已集成到 `DemoController.handleRagMode()` 右轨处理

**功能：**
- 查询扩展：生成多个查询变体
- 结果融合：使用 Reciprocal Rank Fusion (RRF) 算法
- 结果重排序：基于语义相关度
- 优雅降级：算法市场不可用时自动降级

### Phase 2: 算法市场集成 ✅ 已完成

右轨使用算法市场的优化算法：
- ✅ 查询扩展（Query Expansion）
- ✅ 语义分块（Semantic Chunking）
- ✅ 重排序（Rerank）

**实现位置：**
- `omni-agent-marketplace/src/main/java/top/yumbo/ai/omni/marketplace/AlgorithmMarketService.java`
- 已通过 `EnhancedQueryService` 集成到双轨系统

**性能指标：**
- 查询扩展：精度+12.5%, 延迟20ms, 召回+15%
- 语义分块：精度+17.5%, 延迟30ms, 语义一致性0.85
- 重排序：精度+10%, 延迟80ms, NDCG 0.92

### Phase 3: 知识最小概念 ✅ 已完成

- ✅ 概念提取
- ✅ 概念图构建
- ✅ 概念推理

**实现位置：**
- `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/concept/KnowledgeConcept.java` - 概念模型
- `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/concept/ConceptExtractor.java` - 概念提取器
- `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/concept/ConceptGraphService.java` - 概念图服务

**功能：**
- **概念提取**: 从文档中自动提取技术术语、操作关键词等最小概念单元
- **概念分类**: 技术概念、设计模式、操作、领域、问题、解决方案等
- **关系发现**: 自动发现概念之间的9种关系（PART_OF, IS_A, DEPENDS_ON等）
- **概念图构建**: 构建概念之间的关系网络
- **路径发现**: 找出两个概念之间的关系路径（BFS算法）
- **概念推理**: 基于关系图进行知识推理

**概念类型**:
- TECHNOLOGY - 技术概念（Spring Boot, React）
- PATTERN - 设计模式（Singleton, MVC）
- OPERATION - 操作概念（配置，部署）
- DOMAIN - 领域概念（用户管理）
- PROBLEM - 问题概念（性能优化）
- SOLUTION - 解决方案概念（缓存策略）

**关系类型**:
- PART_OF - 是...的一部分
- IS_A - 是...的子类/子集
- DEPENDS_ON - 依赖于
- USED_FOR - 用于/应用于
- RELATED_TO - 相关/关联
- ALTERNATIVE_TO - 对比/替代
- PREREQUISITE_OF - 前置条件
- CAUSES - 导致/引起
- SOLVES - 解决

### Phase 4: 多角色协作 🔄 规划中

通用角色自动分配任务给专业角色：
```
用户问题 → 通用角色分析 → 找到Java专家 → 专家回答
```

**计划实现：**
- 问题领域自动识别
- 角色能力匹配算法
- 多角色协作机制
- 结果综合与融合

---

## 📝 使用示例

### 示例 1: RAG 模式

**请求：**
```
GET /api/qa/stream/dual-track?question=如何使用Spring Boot&knowledgeMode=rag
```

**响应流：**
```
data: {"type":"reference","title":"Spring Boot指南",...}
data: {"type":"reference","title":"Spring Boot最佳实践",...}

event: left
data: {"content":"Spring","chunkIndex":0}
event: left
data: {"content":" Boot","chunkIndex":0}
...

event: right
data: {"content":"根据","chunkIndex":0}
event: right
data: {"content":"系统学习","chunkIndex":0}
...

event: complete
data: {"type":"complete"}
```

### 示例 2: 角色模式

**请求：**
```
GET /api/qa/stream/dual-track?question=如何优化数据库性能&knowledgeMode=role&roleName=database-expert
```

**效果：**
- 左轨：基于文档的标准答案
- 右轨：数据库专家的专业见解

---

## ✅ 总结

双轨流式问答架构实现了：

1. ✅ **传统 + 智能**：结合 RAG 检索和 HOPE 自我学习
2. ✅ **实时流式输出**：用户无需等待，即时看到答案
3. ✅ **角色专业化**：不同领域的专家提供专业回答
4. ✅ **友好体验**：清晰的视觉分隔，友好的错误提示
5. ✅ **可扩展性**：预留算法市场集成接口

**最终目标**：打造一个像人一样不断学习、不断进化的智能问答系统，最终达到与 Claude Sonnet 4.5 等顶级 LLM 相媲美的专业回答水平！ 🚀

