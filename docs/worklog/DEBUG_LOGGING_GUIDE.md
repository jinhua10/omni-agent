# 🔍 Debug 模式详细日志配置指南

## 📋 概述

现在系统已经在关键组件中添加了详细的 debug 日志，可以帮助你查看：
- 📤 发送给 LLM 的完整请求内容（系统提示、用户消息）
- 📥 LLM 的完整响应内容
- 🔍 RAG 检索的详细过程（候选文档、相似度计算）
- 📄 文档分块的策略选择和结果
- 🎨 Vision LLM 的提示词构建过程
- 🔎 查询服务的搜索详情

## 🎯 启用 Debug 日志

### 方法 1: 修改 application.yml

在 `omni-agent-example-basic/src/main/resources/application.yml` 中添加：

```yaml
logging:
  level:
    root: INFO
    # LLM 相关日志
    top.yumbo.ai.ai.ollama: DEBUG
    top.yumbo.ai.ai.online: DEBUG
    # RAG 相关日志
    top.yumbo.ai.rag: DEBUG
    # 核心功能日志
    top.yumbo.ai.omni.core: DEBUG
    # 特定组件日志（更细粒度控制）
    top.yumbo.ai.omni.core.document.processor.VisionLLMDocumentProcessor: DEBUG
    top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager: DEBUG
    top.yumbo.ai.omni.core.query.QueryService: DEBUG
```

### 方法 2: 使用环境变量

```bash
# Windows PowerShell
$env:LOGGING_LEVEL_TOP_YUMBO_AI_AI_OLLAMA="DEBUG"
$env:LOGGING_LEVEL_TOP_YUMBO_AI_RAG="DEBUG"
$env:LOGGING_LEVEL_TOP_YUMBO_AI_OMNI_CORE="DEBUG"

# 启动应用
cd omni-agent-example-basic
mvn spring-boot:run
```

### 方法 3: 命令行参数

```bash
cd omni-agent-example-basic
mvn spring-boot:run -Dspring-boot.run.arguments="--logging.level.top.yumbo.ai.ai.ollama=DEBUG --logging.level.top.yumbo.ai.rag=DEBUG --logging.level.top.yumbo.ai.omni.core=DEBUG"
```

## 📊 日志输出示例

### 1. LLM 请求日志

```
DEBUG [OllamaAIService] 📤 [LLM Request] System Prompt:
请分析这张图片并提取其中的关键信息。

DEBUG [OllamaAIService] 📤 [LLM Request] Message [user]:
# 任务说明
请将这张 PPT 幻灯片的内容转换为文字描述。

## 文档信息
- 文件名：节约用水.pptx
- 总幻灯片数：10
- 当前页码：第 1 页

## 幻灯片中的文字内容
```
节约用水
从我做起
```

DEBUG [OllamaAIService] 📤 [LLM Request] URL: http://localhost:11434/api/chat, Model: qwen2.5:14b, Messages Count: 2

DEBUG [OllamaAIService] 📥 [LLM Response] Duration: 2345ms, Content Length: 256 chars
DEBUG [OllamaAIService] 📥 [LLM Response] Content:
这是一张关于节约用水主题的幻灯片。标题为"节约用水"，副标题是"从我做起"...
```

### 2. RAG 向量搜索日志

```
DEBUG [SQLiteRAGService] 🔍 [RAG Vector Search] Starting search - Embedding dim: 768, topK: 5
DEBUG [SQLiteRAGService] 🔍 [RAG Vector Search] Found 150 candidate documents with embeddings
DEBUG [SQLiteRAGService] 🔍 [RAG Vector Search] Doc [chunk_001]: similarity=0.8934, title=节约用水的重要性
DEBUG [SQLiteRAGService] 🔍 [RAG Vector Search] Doc [chunk_045]: similarity=0.8523, title=水资源现状分析
DEBUG [SQLiteRAGService] 🔍 [RAG Vector Search] Doc [chunk_089]: similarity=0.8201, title=节水小技巧
...
DEBUG [SQLiteRAGService] 🔍 [RAG Vector Search] Completed in 45ms - Returned 5 results
DEBUG [SQLiteRAGService] 🔍 [RAG Vector Search] Result #1: score=0.8934, docId=chunk_001, content preview: 节约用水是每个人的责任。水资源是有限的...
DEBUG [SQLiteRAGService] 🔍 [RAG Vector Search] Result #2: score=0.8523, docId=chunk_045, content preview: 全球水资源分布不均，许多地区面临严重的水资源短缺...
```

### 3. 文档分块日志

```
DEBUG [ChunkingStrategyManager] 📄 [Chunking] Starting auto chunking - docId: doc_123, fileName: 技术文档.md, content length: 15234
DEBUG [ChunkingStrategyManager] 📄 [Chunking] Inferred document type: TECHNICAL
DEBUG [ChunkingStrategyManager] 📄 [Chunking] Selected strategy: semantic_chunking
DEBUG [ChunkingStrategyManager] 📄 [Chunking] Strategy params: {chunkSize=512, overlap=50}
INFO  [ChunkingStrategyManager] Auto-selected chunking strategy: semantic_chunking for document type: TECHNICAL
DEBUG [ChunkingStrategyManager] 📄 [Chunking] Completed in 234ms - Generated 23 chunks
DEBUG [ChunkingStrategyManager] 📄 [Chunking] Chunk #1: id=chunk_001, content length=498, preview: # 技术架构概述\n\n本文档描述了系统的整体技术架构...
DEBUG [ChunkingStrategyManager] 📄 [Chunking] Chunk #2: id=chunk_002, content length=512, preview: ## 核心组件\n\n系统由以下核心组件构成：\n1. API 网关...
DEBUG [ChunkingStrategyManager] 📄 [Chunking] Chunk #3: id=chunk_003, content length=487, preview: ## 数据流程\n\n数据在系统中的流转过程如下：\n- 接收请求...
DEBUG [ChunkingStrategyManager] 📄 [Chunking] ... and 20 more chunks
```

### 4. Vision LLM 提示词构建日志

```
DEBUG [VisionLLMDocumentProcessor] 🎨 [Vision Prompt] Building prompt for page 1
DEBUG [VisionLLMDocumentProcessor] 🎨 [Vision Prompt] Metadata - fileName: 节约用水.pptx, slideText length: 45, hasContext: true
DEBUG [VisionLLMDocumentProcessor] 🎨 [Vision Prompt] Final prompt (856 chars):
# 任务说明
请将这张 PPT 幻灯片的内容转换为文字描述。

## 文档信息
- 文件名：节约用水.pptx
- 总幻灯片数：10
- 当前页码：第 1 页

## 幻灯片中的文字内容
```
节约用水
从我做起
```

## 文档主题参考
前几页的内容：节约用水 从我做起 | 水资源现状 | 节水方法

## 输出要求
请根据上述文字内容和图片中的可视化元素，输出：
...
```

### 5. 查询服务日志

```
DEBUG [QueryService] 🔎 [Query] Text search - query: '如何节约用水', limit: 10
INFO  [QueryService] Search completed for query '如何节约用水': 10 results in 67ms
DEBUG [QueryService] 🔎 [Query] Text search results (10 found):
DEBUG [QueryService] 🔎 [Query] Result #1: score=0.9234, docId=chunk_012, content: 节约用水的方法有很多，首先是日常生活中要注意关紧水龙头...
DEBUG [QueryService] 🔎 [Query] Result #2: score=0.8876, docId=chunk_034, content: 在家庭中节约用水可以从以下几个方面入手：洗菜用盆接水...
...
```

## 🎯 按组件启用日志

### 只看 LLM 交互

```yaml
logging:
  level:
    top.yumbo.ai.ai.ollama.OllamaAIService: DEBUG
```

### 只看 RAG 检索过程

```yaml
logging:
  level:
    top.yumbo.ai.omni.rag.sqlite.SQLiteRAGService: DEBUG
    top.yumbo.ai.omni.core.query.QueryService: DEBUG
```

### 只看文档处理

```yaml
logging:
  level:
    top.yumbo.ai.omni.core.document.processor.VisionLLMDocumentProcessor: DEBUG
    top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager: DEBUG
```

## 📝 日志图标说明

- 📤 **发送请求** - 向外部服务（LLM、API）发送的请求
- 📥 **接收响应** - 从外部服务接收的响应
- 🔍 **RAG 检索** - RAG 向量/文本搜索过程
- 📄 **文档分块** - 文档内容分块处理
- 🎨 **Vision 提示** - Vision LLM 的提示词构建
- 🔎 **查询服务** - 用户查询处理

## 🔧 日志格式自定义

在 `application.yml` 中可以自定义日志格式：

```yaml
logging:
  pattern:
    console: "%d{yyyy-MM-dd HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n"
    file: "%d{yyyy-MM-dd HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n"
  file:
    name: logs/omni-agent.log
    max-size: 100MB
    max-history: 30
```

## 📊 实时查看日志

### Windows PowerShell

```powershell
# 启动应用并实时查看日志
cd omni-agent-example-basic
mvn spring-boot:run | Tee-Object -FilePath logs.txt

# 在另一个终端监控日志
Get-Content logs.txt -Wait -Tail 50
```

### 使用 IDE

在 IntelliJ IDEA 中：
1. 打开 Run Configuration
2. 添加 VM options: `-Dlogging.level.top.yumbo.ai=DEBUG`
3. 在 Console 面板查看彩色日志

## 🎯 调试特定场景

### 场景 1: 调试 Vision LLM 分析不准确

启用 Vision LLM 和 AI 服务的 debug 日志：

```yaml
logging:
  level:
    top.yumbo.ai.omni.core.document.processor.VisionLLMDocumentProcessor: DEBUG
    top.yumbo.ai.ai.ollama.OllamaAIService: DEBUG
```

查看：
- 🎨 构建的提示词是否包含了正确的上下文
- 📤 发送给 LLM 的完整提示
- 📥 LLM 返回的完整内容

### 场景 2: 调试 RAG 检索不到相关文档

启用 RAG 和查询服务的 debug 日志：

```yaml
logging:
  level:
    top.yumbo.ai.omni.rag.sqlite.SQLiteRAGService: DEBUG
    top.yumbo.ai.omni.core.query.QueryService: DEBUG
```

查看：
- 🔍 候选文档数量
- 🔍 每个文档的相似度分数
- 🔍 返回的 top-K 结果

### 场景 3: 调试文档分块不合理

启用分块管理器的 debug 日志：

```yaml
logging:
  level:
    top.yumbo.ai.omni.core.chunking.ChunkingStrategyManager: DEBUG
```

查看：
- 📄 推断的文档类型
- 📄 选择的分块策略
- 📄 策略参数
- 📄 生成的分块数量和内容预览

## ✅ 完整示例配置

创建 `application-debug.yml`：

```yaml
# Debug 模式配置
spring:
  profiles: debug

logging:
  level:
    root: INFO
    # 所有 omni-agent 相关日志设为 DEBUG
    top.yumbo.ai: DEBUG
    # SQL 日志（如果需要）
    org.springframework.jdbc.core: DEBUG
    # HTTP 请求日志（如果需要）
    org.springframework.web.client.RestTemplate: DEBUG
  
  pattern:
    console: "%clr(%d{yyyy-MM-dd HH:mm:ss.SSS}){faint} %clr(${LOG_LEVEL_PATTERN:-%5p}) %clr([%15.15t]){faint} %clr(%-40.40logger{39}){cyan} %clr(:){faint} %m%n${LOG_EXCEPTION_CONVERSION_WORD:%wEx}"
  
  file:
    name: logs/omni-agent-debug.log
    max-size: 100MB
    max-history: 7
```

使用：

```bash
cd omni-agent-example-basic
mvn spring-boot:run -Dspring-boot.run.profiles=debug
```

## 🎉 总结

现在你可以在 debug 模式下看到：

| 组件 | 日志内容 | 图标 |
|------|---------|-----|
| **OllamaAIService** | 完整的 LLM 请求和响应 | 📤📥 |
| **SQLiteRAGService** | RAG 检索的详细过程 | 🔍 |
| **VisionLLMDocumentProcessor** | Vision 提示词构建 | 🎨 |
| **ChunkingStrategyManager** | 文档分块策略和结果 | 📄 |
| **QueryService** | 查询处理和结果 | 🔎 |

所有内容都**不会被截断**，你可以看到每一个细节！🚀

