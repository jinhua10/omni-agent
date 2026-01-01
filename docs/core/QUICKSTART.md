# OmniAgent 快速开始指南

> **版本：** 1.0.0  
> **更新时间：** 2026-01-01  
> **状态：** ✅ 生产就绪

---

## 📋 目录

1. [环境要求](#环境要求)
2. [快速安装](#快速安装)
3. [基础配置](#基础配置)
4. [第一个示例](#第一个示例)
5. [常见问题](#常见问题)

---

## 🔧 环境要求

### 必需环境

| 组件 | 版本要求 | 说明 |
|------|---------|------|
| **Java** | 21+ | OpenJDK 或 Oracle JDK |
| **Maven** | 3.6+ | 构建工具 |
| **内存** | 4GB+ | 推荐 8GB 以上 |

### 可选环境

| 组件 | 版本 | 用途 |
|------|------|------|
| **Ollama** | 最新版 | 本地 LLM 推理 |
| **MongoDB** | 4.4+ | 文档存储（可选） |
| **Redis** | 6.0+ | 缓存（可选） |
| **Elasticsearch** | 7.x+ | 搜索引擎（可选） |

---

## 🚀 快速安装

### 方式1: 使用示例项目（推荐）

```bash
# 1. 克隆项目
git clone https://github.com/jinhua10/omni-agent.git
cd omni-agent

# 2. 构建项目
mvn clean package -DskipTests

# 3. 进入示例项目
cd omni-agent-example-basic

# 4. 启动应用
java -jar target/omni-agent-example-basic-1.0.0.jar
```

### 方式2: Maven 依赖（自定义项目）

```xml
<dependencies>
    <!-- 核心依赖 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-core</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- Web 模块（如需 Web 服务） -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-web</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- HOPE 自学习系统 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-hope-starter</artifactId>
        <version>1.0.0</version>
    </dependency>
</dependencies>
```

---

## ⚙️ 基础配置

### application.yml 配置示例

```yaml
# ============================================
# OmniAgent 基础配置
# ============================================

# AI 服务配置
omni-agent:
  ai:
    provider: deepseek           # AI 提供商: ollama/deepseek/openai/qwen
    model: deepseek-chat         # 模型名称
    api-key: ${DEEPSEEK_API_KEY} # API Key（从环境变量读取）
    base-url: https://api.deepseek.com/v1
    
  # 文档存储配置
  storage:
    instances:
      - id: dev-storage
        type: file               # 存储类型: file/mongodb/elasticsearch
        base-path: ./data/storage
        
  # RAG 配置
  rag:
    instances:
      - id: default
        type: file               # RAG 类型: file/elasticsearch
        index-path: ./data/rag-index
        
  # 工作流配置
  workflow:
    storage-type: sqlite         # 工作流存储: sqlite/mongodb/elasticsearch
    sqlite-db-path: ./data/workflows/workflows.db
    
  # P2P 配置
  p2p:
    enabled: true
    storage-type: memory         # P2P 存储: memory/sqlite/redis

# Spring Boot 配置
spring:
  application:
    name: omni-agent-example
    
server:
  port: 8080
```

### 环境变量配置

```bash
# API Keys
export DEEPSEEK_API_KEY="your-api-key-here"
export DASHSCOPE_API_KEY="your-qwen-api-key"  # 如使用通义千问

# Ollama 配置（如使用本地 Ollama）
export OLLAMA_BASE_URL="http://localhost:11434"
```

---

## 💡 第一个示例

### 示例1: 简单问答

创建 `QuickStartExample.java`:

```java
package top.yumbo.ai.omni.example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.hope.starter.impl.HOPEKnowledgeManager;

@SpringBootApplication
@RestController
@RequestMapping("/api")
public class QuickStartExample {

    private final AIService aiService;
    private final HOPEKnowledgeManager hopeManager;

    public QuickStartExample(AIService aiService, 
                            HOPEKnowledgeManager hopeManager) {
        this.aiService = aiService;
        this.hopeManager = hopeManager;
    }

    public static void main(String[] args) {
        SpringApplication.run(QuickStartExample.class, args);
    }

    /**
     * 简单问答接口
     */
    @PostMapping("/ask")
    public String ask(@RequestParam String question) {
        // 使用 HOPE 系统智能检索和回答
        return hopeManager.query(question).getAnswer();
    }

    /**
     * AI 对话接口
     */
    @PostMapping("/chat")
    public String chat(@RequestParam String message) {
        // 直接调用 AI 服务
        return aiService.chat(message);
    }
}
```

### 示例2: 文档上传和智能问答

```java
@RestController
@RequestMapping("/api/documents")
public class DocumentController {

    private final DocumentProcessingService documentService;
    private final HOPEKnowledgeManager hopeManager;

    /**
     * 上传文档
     */
    @PostMapping("/upload")
    public String uploadDocument(@RequestParam("file") MultipartFile file) {
        // 处理文档
        String docId = documentService.processDocument(file);
        return "文档上传成功，ID: " + docId;
    }

    /**
     * 基于文档的智能问答
     */
    @PostMapping("/query")
    public QueryResult queryDocument(@RequestParam String question) {
        // HOPE 系统会自动从上传的文档中检索相关内容
        return hopeManager.smartQuery(question, null);
    }
}
```

### 测试接口

```bash
# 1. 启动应用
java -jar target/omni-agent-example-basic-1.0.0.jar

# 2. 测试简单问答
curl -X POST "http://localhost:8080/api/ask?question=什么是OmniAgent?"

# 3. 测试 AI 对话
curl -X POST "http://localhost:8080/api/chat?message=你好"

# 4. 上传文档
curl -X POST -F "file=@document.pdf" "http://localhost:8080/api/documents/upload"

# 5. 文档问答
curl -X POST "http://localhost:8080/api/documents/query?question=文档中提到了什么？"
```

---

## 🎯 核心功能使用

### 1. HOPE 自学习系统

```java
@Service
public class IntelligentQAService {
    
    private final HOPEKnowledgeManager hopeManager;
    
    /**
     * 智能问答（带上下文）
     */
    public QueryResult intelligentQuery(String question, String context) {
        // HOPE 会自动：
        // 1. 分类问题类型
        // 2. 选择合适的知识层级
        // 3. 智能检索相关知识
        // 4. 学习和优化
        return hopeManager.smartQuery(question, context);
    }
    
    /**
     * 添加知识到特定层级
     */
    public void addKnowledge(String content, String layer) {
        // layer: permanent/ordinary/high-frequency
        hopeManager.addToLayer(content, layer);
    }
}
```

### 2. 知识网络系统

```java
@Service
public class KnowledgeNetworkService {
    
    private final KnowledgeRegistry knowledgeRegistry;
    
    /**
     * 创建知识域
     */
    public void createDomain(String domainId, String name) {
        KnowledgeDomain domain = KnowledgeDomain.builder()
            .domainId(domainId)
            .name(name)
            .description("领域描述")
            .build();
        knowledgeRegistry.registerDomain(domain);
    }
    
    /**
     * 添加知识到域
     */
    public void addKnowledgeToDomain(String domainId, String content) {
        Knowledge knowledge = Knowledge.builder()
            .domainId(domainId)
            .content(content)
            .build();
        knowledgeRegistry.addKnowledge(knowledge);
    }
}
```

### 3. 文档处理

```java
@Service
public class DocumentService {
    
    private final DocumentProcessingService processingService;
    private final ChunkingService chunkingService;
    
    /**
     * 智能分块处理
     */
    public void processWithSmartChunking(File document) {
        // 使用 PPL（困惑度）分块策略
        ChunkingConfig config = ChunkingConfig.builder()
            .strategy("PPL")           // 推荐：基于AI的语义边界
            .maxChunkSize(1000)
            .overlapSize(100)
            .build();
            
        List<Chunk> chunks = chunkingService.chunk(
            document, 
            config
        );
        
        // 存储分块结果
        processingService.saveChunks(chunks);
    }
}
```

---

## ❓ 常见问题

### Q1: 如何选择 AI 提供商？

**回答：**
- **Ollama**（推荐新手）: 本地运行，免费，隐私好
- **DeepSeek**: 性价比高，API 便宜
- **OpenAI**: 效果好，但成本高
- **通义千问**: 国内访问快

### Q2: 启动时报错 "No qualifying bean"？

**回答：**
检查配置文件中是否正确配置了必需的服务：

```yaml
omni-agent:
  ai:
    provider: deepseek  # 必须配置
    api-key: xxx        # 必须配置
```

### Q3: 如何自定义分块策略？

**回答：**
实现 `ChunkingStrategy` 接口：

```java
@Component
public class MyChunkingStrategy implements ChunkingStrategy {
    
    @Override
    public List<Chunk> chunk(String content, ChunkingConfig config) {
        // 自定义分块逻辑
        return customChunks;
    }
    
    @Override
    public String getStrategyName() {
        return "MY_CUSTOM";
    }
}
```

### Q4: 如何启用 P2P 知识共享？

**回答：**
在配置文件中启用 P2P：

```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: sqlite
    connection-code: "your-unique-code"
```

### Q5: 内存占用太高怎么办？

**回答：**
调整 JVM 参数：

```bash
java -Xmx2g -Xms512m -jar omni-agent-example-basic-1.0.0.jar
```

或使用更轻量的配置：

```yaml
omni-agent:
  rag:
    cache-size: 100      # 减小缓存
  chunking:
    max-chunk-size: 500  # 减小分块大小
```

---

## 📚 下一步

- 📖 [完整架构文档](ARCHITECTURE.md)
- 🧠 [HOPE 系统详解](HOPE_SYSTEM.md)
- 🕸️ [知识网络架构](KNOWLEDGE_NETWORK.md)
- 🔧 [高级配置指南](ADVANCED_CONFIG.md)
- 🚀 [部署指南](DEPLOYMENT.md)

---

**快速开始遇到问题？**

- 📧 [提交 Issue](https://github.com/jinhua10/omni-agent/issues)
- 💬 [加入讨论](https://github.com/jinhua10/omni-agent/discussions)
- 📖 [查看完整文档](../README.md)

---

**文档维护者：** OmniAgent Team  
**最后更新：** 2026-01-01

