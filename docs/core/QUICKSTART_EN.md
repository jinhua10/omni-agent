# OmniAgent Quick Start Guide

> **Version:** 1.0.0  
> **Updated:** 2026-01-01  
> **Status:** ✅ Production Ready

---

## 📋 Table of Contents

1. [Environment Requirements](#environment-requirements)
2. [Quick Installation](#quick-installation)
3. [Basic Configuration](#basic-configuration)
4. [First Example](#first-example)
5. [Frequently Asked Questions](#frequently-asked-questions)

---

## 🔧 Environment Requirements

### Required Environment

| Component | Version | Description |
|-----------|---------|-------------|
| **Java** | 21+ | OpenJDK or Oracle JDK |
| **Maven** | 3.6+ | Build tool |
| **Memory** | 4GB+ | 8GB+ recommended |

### Optional Environment

| Component | Version | Purpose |
|-----------|---------|---------|
| **Ollama** | Latest | Local LLM inference |
| **MongoDB** | 4.4+ | Document storage (optional) |
| **Redis** | 6.0+ | Cache (optional) |
| **Elasticsearch** | 7.x+ | Search engine (optional) |

---

## 🚀 Quick Installation

### Method 1: Using Example Project (Recommended)

```bash
# 1. Clone project
git clone https://github.com/jinhua10/omni-agent.git
cd omni-agent

# 2. Build project
mvn clean package -DskipTests

# 3. Enter example project
cd omni-agent-example-basic

# 4. Start application
java -jar target/omni-agent-example-basic-1.0.0.jar
```

### Method 2: Maven Dependencies (Custom Project)

Due to the large number of system modules, we are still polishing the details. The following dependencies have not yet been published to the Maven Central Repository. After stabilization, version 1.0.0 will be released. Please clone the source code and install it to your local repository first:

```bash
# Clone and install to local Maven repository
git clone https://github.com/jinhua10/omni-agent.git
cd omni-agent
mvn clean install -DskipTests
```

Then add dependencies to your project:

```xml
<dependencies>
    <!-- Core dependency -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-core</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- Web module (if web service needed) -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-web</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- HOPE Self-Learning System -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-hope-starter</artifactId>
        <version>1.0.0</version>
    </dependency>
</dependencies>
```

---

## ⚙️ Basic Configuration

### application.yml Configuration Example

```yaml
# ============================================
# OmniAgent Basic Configuration
# ============================================

# AI Service Configuration
omni-agent:
  ai:
    provider: deepseek           # AI Provider: ollama/deepseek/openai/qwen
    model: deepseek-chat         # Model name
    api-key: ${DEEPSEEK_API_KEY} # API Key (read from environment variable)
    base-url: https://api.deepseek.com/v1
    
  # Document Storage Configuration
  storage:
    instances:
      - id: dev-storage
        type: file               # Storage type: file/mongodb/elasticsearch
        base-path: ./data/storage
        
  # RAG Configuration
  rag:
    instances:
      - id: default
        type: file               # RAG type: file/elasticsearch
        index-path: ./data/rag-index
        
  # Workflow Configuration
  workflow:
    storage-type: sqlite         # Workflow storage: sqlite/mongodb/elasticsearch
    sqlite-db-path: ./data/workflows/workflows.db
    
  # P2P Configuration
  p2p:
    enabled: true
    storage-type: memory         # P2P storage: memory/sqlite/redis

# Spring Boot Configuration
spring:
  application:
    name: omni-agent-example
    
server:
  port: 8080
```

### Environment Variables Configuration

```bash
# API Keys
export DEEPSEEK_API_KEY="your-api-key-here"
export DASHSCOPE_API_KEY="your-qwen-api-key"  # If using Qwen

# Ollama Configuration (if using local Ollama)
export OLLAMA_BASE_URL="http://localhost:11434"
```

---

## 💡 First Example

### Example 1: Simple Q&A

Create `QuickStartExample.java`:

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
     * Simple Q&A endpoint
     */
    @PostMapping("/ask")
    public String ask(@RequestParam String question) {
        // Use HOPE system for intelligent retrieval and answering
        return hopeManager.query(question).getAnswer();
    }

    /**
     * AI conversation endpoint
     */
    @PostMapping("/chat")
    public String chat(@RequestParam String message) {
        // Direct AI service call
        return aiService.chat(message);
    }
}
```

### Example 2: Document Upload and Intelligent Q&A

```java
@RestController
@RequestMapping("/api/documents")
public class DocumentController {

    private final DocumentProcessingService documentService;
    private final HOPEKnowledgeManager hopeManager;

    /**
     * Upload document
     */
    @PostMapping("/upload")
    public String uploadDocument(@RequestParam("file") MultipartFile file) {
        // Process document
        String docId = documentService.processDocument(file);
        return "Document uploaded successfully, ID: " + docId;
    }

    /**
     * Document-based intelligent Q&A
     */
    @PostMapping("/query")
    public QueryResult queryDocument(@RequestParam String question) {
        // HOPE system automatically retrieves relevant content from uploaded documents
        return hopeManager.smartQuery(question, null);
    }
}
```

### Test Endpoints

```bash
# 1. Start application
java -jar target/omni-agent-example-basic-1.0.0.jar

# 2. Test simple Q&A
curl -X POST "http://localhost:8080/api/ask?question=What is OmniAgent?"

# 3. Test AI conversation
curl -X POST "http://localhost:8080/api/chat?message=Hello"

# 4. Upload document
curl -X POST -F "file=@document.pdf" "http://localhost:8080/api/documents/upload"

# 5. Document Q&A
curl -X POST "http://localhost:8080/api/documents/query?question=What does the document mention?"
```

---

## 🎯 Core Features Usage

### 1. HOPE Self-Learning System

```java
@Service
public class IntelligentQAService {
    
    private final HOPEKnowledgeManager hopeManager;
    
    /**
     * Intelligent Q&A (with context)
     */
    public QueryResult intelligentQuery(String question, String context) {
        // HOPE automatically:
        // 1. Classifies question type
        // 2. Selects appropriate knowledge layer
        // 3. Intelligently retrieves relevant knowledge
        // 4. Learns and optimizes
        return hopeManager.smartQuery(question, context);
    }
    
    /**
     * Add knowledge to specific layer
     */
    public void addKnowledge(String content, String layer) {
        // layer: permanent/ordinary/high-frequency
        hopeManager.addToLayer(content, layer);
    }
}
```

### 2. Knowledge Network System

```java
@Service
public class KnowledgeNetworkService {
    
    private final KnowledgeRegistry knowledgeRegistry;
    
    /**
     * Create knowledge domain
     */
    public void createDomain(String domainId, String name) {
        KnowledgeDomain domain = KnowledgeDomain.builder()
            .domainId(domainId)
            .name(name)
            .description("Domain description")
            .build();
        knowledgeRegistry.registerDomain(domain);
    }
    
    /**
     * Add knowledge to domain
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

### 3. Document Processing

```java
@Service
public class DocumentService {
    
    private final DocumentProcessingService processingService;
    private final ChunkingService chunkingService;
    
    /**
     * Smart chunking processing
     */
    public void processWithSmartChunking(File document) {
        // Use PPL (Perplexity) chunking strategy
        ChunkingConfig config = ChunkingConfig.builder()
            .strategy("PPL")           // Recommended: AI-based semantic boundaries
            .maxChunkSize(1000)
            .overlapSize(100)
            .build();
            
        List<Chunk> chunks = chunkingService.chunk(
            document, 
            config
        );
        
        // Store chunking results
        processingService.saveChunks(chunks);
    }
}
```

---

## ❓ Frequently Asked Questions

### Q1: How to choose an AI provider?

**Answer:**
- **Ollama** (recommended for beginners): Local, free, privacy-friendly
- **DeepSeek**: Cost-effective, affordable API
- **OpenAI**: Best quality, but higher cost
- **Qwen (通义千问)**: Fast access in China

### Q2: Getting "No qualifying bean" error on startup?

**Answer:**
Check if required services are properly configured in the configuration file:

```yaml
omni-agent:
  ai:
    provider: deepseek  # Must configure
    api-key: xxx        # Must configure
```

### Q3: How to customize chunking strategy?

**Answer:**
Implement the `ChunkingStrategy` interface:

```java
@Component
public class MyChunkingStrategy implements ChunkingStrategy {
    
    @Override
    public List<Chunk> chunk(String content, ChunkingConfig config) {
        // Custom chunking logic
        return customChunks;
    }
    
    @Override
    public String getStrategyName() {
        return "MY_CUSTOM";
    }
}
```

### Q4: How to enable P2P knowledge sharing?

**Answer:**
Enable P2P in configuration file:

```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: sqlite
    connection-code: "your-unique-code"
```

### Q5: What to do if memory usage is too high?

**Answer:**
Adjust JVM parameters:

```bash
java -Xmx2g -Xms512m -jar omni-agent-example-basic-1.0.0.jar
```

Or use lighter configuration:

```yaml
omni-agent:
  rag:
    cache-size: 100      # Reduce cache
  chunking:
    max-chunk-size: 500  # Reduce chunk size
```

---

## 📚 Next Steps

- 📖 [Complete Architecture Documentation](ARCHITECTURE_EN.md)
- 🧠 [HOPE System Details](HOPE_SYSTEM_EN.md)
- 🕸️ [Knowledge Network Architecture](KNOWLEDGE_NETWORK_EN.md)
- 🔧 [Advanced Configuration Guide](ADVANCED_CONFIG.md)
- 🚀 [Deployment Guide](DEPLOYMENT.md)

---

**Having issues with Quick Start?**

- 📧 [Submit Issue](https://github.com/jinhua10/omni-agent/issues)
- 💬 [Join Discussion](https://github.com/jinhua10/omni-agent/discussions)
- 📖 [View Complete Documentation](../README.md)

---

**Maintained by:** OmniAgent Team  
**Last Updated:** 2026-01-01

