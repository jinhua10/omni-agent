# OmniAgent Module Architecture

> **Version:** 1.0.0  
> **Updated:** 2026-01-01  
> **Status:** ✅ Production Ready

---

## 📋 Table of Contents

1. [Module Overview](#module-overview)
2. [Architecture Layers](#architecture-layers)
3. [Core Modules Details](#core-modules-details)
4. [Module Dependencies](#module-dependencies)
5. [Module Selection Guide](#module-selection-guide)

---

## 🎯 Module Overview

OmniAgent adopts a **fully pluggable architecture** based on the **Spring Boot Starter pattern**, with **27 functional modules** divided into **7 major layers**.

### Module Statistics

| Category | Count | Description |
|----------|-------|-------------|
| **API Interface Layer** | 9 | Define core interfaces and models |
| **Starter Implementation Layer** | 10 | Specific functionality implementations |
| **Core Foundation Layer** | 2 | core + common |
| **Orchestration Layer** | 1 | orchestrator |
| **Web Service Layer** | 2 | web + workflow |
| **Algorithm Marketplace** | 1 | marketplace |
| **Example Applications** | 2 | basic + production |
| **Total** | **27** | All modules |

---

## 🏗️ Architecture Layers

### Complete Architecture Diagram

```
┌─────────────────────────────────────────────────────────────────────┐
│                        Application Layer                             │
├─────────────────────────────────────────────────────────────────────┤
│ • omni-agent-example-basic          Basic example application        │
│ • omni-agent-example-production     Production environment example   │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                        Web Service Layer                             │
├─────────────────────────────────────────────────────────────────────┤
│ • omni-agent-web                    RESTful API service              │
│ • omni-agent-workflow               Workflow engine                  │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                     Orchestration Layer                              │
├─────────────────────────────────────────────────────────────────────┤
│ • omni-agent-orchestrator           Service orchestration coordinator│
│   - Query service orchestration                                      │
│   - Context management                                               │
│   - Depends only on API interfaces                                   │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                    Intelligence Layer                                │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌────────────────────────────────────────────────────────────┐   │
│  │ HOPE Self-Learning System ⭐                               │   │
│  ├────────────────────────────────────────────────────────────┤   │
│  │ • omni-agent-hope-api          HOPE interface definitions   │   │
│  │ • omni-agent-hope-starter      HOPE implementation          │   │
│  │                                (classification, learning)   │   │
│  └────────────────────────────────────────────────────────────┘   │
│                                                                      │
│  ┌────────────────────────────────────────────────────────────┐   │
│  │ Knowledge Network System ⭐                                │   │
│  ├────────────────────────────────────────────────────────────┤   │
│  │ • omni-agent-knowledge-registry-api    Registry interfaces  │   │
│  │ • omni-agent-knowledge-registry-starter Network impl        │   │
│  └────────────────────────────────────────────────────────────┘   │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                    Service Layer                                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                      │
│  ┌─────────────────────────┐  ┌──────────────────────────────┐    │
│  │ AI Service              │  │ Document Processing           │    │
│  ├─────────────────────────┤  ├──────────────────────────────┤    │
│  │ • omni-agent-ai-api     │  │ • omni-agent-document-       │    │
│  │ • omni-agent-ai-starter │  │   processor-api              │    │
│  │   - Ollama              │  │ • omni-agent-document-       │    │
│  │   - Online API          │  │   processor-starter          │    │
│  │   - ONNX                │  │   - Word/Excel/PPT/PDF       │    │
│  └─────────────────────────┘  │   - Vision LLM               │    │
│                                └──────────────────────────────┘    │
│                                                                      │
│  ┌─────────────────────────┐  ┌──────────────────────────────┐    │
│  │ RAG Retrieval           │  │ Smart Chunking               │    │
│  ├─────────────────────────┤  ├──────────────────────────────┤    │
│  │ • omni-agent-rag-api    │  │ • omni-agent-chunking-api    │    │
│  │ • omni-agent-rag-       │  │ • omni-agent-chunking-       │    │
│  │   starter-adapter       │  │   starter                    │    │
│  │   - File/Lucene         │  │   - PPL (Perplexity) ⭐      │    │
│  │   - Elasticsearch       │  │   - Semantic                 │    │
│  │   - MongoDB             │  │   - Paragraph                │    │
│  └─────────────────────────┘  └──────────────────────────────┘    │
│                                                                      │
│  ┌─────────────────────────┐  ┌──────────────────────────────┐    │
│  │ P2P Collaboration       │  │ OCR Recognition              │    │
│  ├─────────────────────────┤  ├──────────────────────────────┤    │
│  │ • omni-agent-p2p-api    │  │ • omni-agent-ocr-starter-    │    │
│  │ • omni-agent-p2p-       │  │   tesseract                  │    │
│  │   starter               │  │   - Tesseract OCR            │    │
│  └─────────────────────────┘  └──────────────────────────────┘    │
│                                                                      │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                      Storage Layer                                   │
├─────────────────────────────────────────────────────────────────────┤
│ • omni-agent-document-storage-api        Storage interfaces          │
│ • omni-agent-document-storage-starter    Storage implementations    │
│                                                                      │
│   Supported storage engines:                                         │
│   ✅ File (File system)                                              │
│   ✅ SQLite (Embedded database)                                      │
│   ✅ H2 (In-memory database)                                         │
│   ✅ MongoDB (Document database)                                     │
│   ✅ Redis (Cache)                                                   │
│   ✅ Elasticsearch (Search engine)                                   │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                      Core Foundation Layer                           │
├─────────────────────────────────────────────────────────────────────┤
│ • omni-agent-core              Core framework and infrastructure     │
│ • omni-agent-common            Common utilities and constants        │
└─────────────────────────────────────────────────────────────────────┘
                                  ↓
┌─────────────────────────────────────────────────────────────────────┐
│                    Algorithm Marketplace                             │
├─────────────────────────────────────────────────────────────────────┤
│ • omni-agent-marketplace       Algorithm component registration      │
│   - Query expansion algorithms                                       │
│   - Re-ranking algorithms                                            │
│   - Custom algorithm plugins                                         │
└─────────────────────────────────────────────────────────────────────┘
```

---

## 📦 Core Modules Details

### 1. Core Foundation Layer

#### omni-agent-common

**Responsibility:** Common utilities and constant definitions

**Core Content:**
- ✅ Common utility classes
- ✅ Constant definitions
- ✅ Exception class definitions
- ✅ Basic models

**Package Path:** `top.yumbo.ai.omni.common`

#### omni-agent-core

**Responsibility:** Core framework and infrastructure

**Core Content:**
- ✅ Core infrastructure
- ✅ Configuration management
- ✅ Event system
- ✅ No dependency on specific implementation libraries

**Package Path:** `top.yumbo.ai.omni.core`

**Dependencies:**
```
omni-agent-core
  └─→ omni-agent-common
  └─→ Spring Boot 3.4.1
```

---

### 2. Intelligence Layer

#### HOPE Self-Learning System

**Module Composition:**
- **omni-agent-hope-api** - Interface definitions
- **omni-agent-hope-starter** - Concrete implementation

**Core Functions:**
```
┌─────────────────────────────────────────────┐
│ HOPE = Hierarchical Omni-Agent             │
│        Persistent Engine                   │
├─────────────────────────────────────────────┤
│                                            │
│ 1. Question Classifier                     │
│    - Classify based on keywords & patterns │
│    - Suggest knowledge layer to use        │
│                                            │
│ 2. Three-Layer Knowledge Management        │
│    ├─ Permanent Layer                     │
│    ├─ Ordinary Layer                      │
│    └─ High Frequency Layer                │
│                                            │
│ 3. Auto-Learning Optimization              │
│    - Access frequency statistics           │
│    - Dynamic layer adjustment              │
│    - Knowledge auto-promotion/demotion     │
│                                            │
│ 4. Persistence Mechanism                   │
│    - InMemory implementation (default)     │
│    - KnowledgeRegistry implementation      │
│                                            │
└─────────────────────────────────────────────┘
```

**Key Classes:**
```java
// API Layer
top.yumbo.ai.omni.hope.api.QuestionClassifier
top.yumbo.ai.omni.hope.api.HopePersistence
top.yumbo.ai.omni.hope.api.QuestionTypeConfig

// Implementation Layer
top.yumbo.ai.omni.hope.starter.impl.HOPEKnowledgeManager
top.yumbo.ai.omni.hope.starter.impl.QuestionClassifier
top.yumbo.ai.omni.hope.starter.impl.InMemoryHopePersistence
```

**Configuration Example:**
```yaml
omni-agent:
  hope:
    enabled: true
    persistence: knowledge-registry  # or in-memory
    default-layer: ordinary
```

**Detailed Documentation:** [HOPE System Design](HOPE_SYSTEM_EN.md)

---

#### Knowledge Network System

**Module Composition:**
- **omni-agent-knowledge-registry-api** - Interface definitions
- **omni-agent-knowledge-registry-starter** - Concrete implementation

**Core Functions:**
```
┌─────────────────────────────────────────────┐
│ Knowledge Network                          │
├─────────────────────────────────────────────┤
│                                            │
│ 1. Domain Management                       │
│    - Create and manage knowledge domains   │
│    - Domain types: Technical/Business/General│
│    - Independent vector spaces             │
│                                            │
│ 2. Intelligent Retrieval                   │
│    ├─ Domain Router                       │
│    ├─ Cross-Domain Query                  │
│    ├─ Quality Scorer                      │
│    └─ Query Cache                         │
│                                            │
│ 3. Knowledge Association                   │
│    - Auto association discovery            │
│    - Cross-domain associations             │
│    - Reference relationship tracking       │
│                                            │
│ 4. User Preference Learning                │
│    - Record user behavior                  │
│    - Learn domain preferences              │
│    - Personalized retrieval optimization   │
│                                            │
│ 5. Knowledge Refinement                    │
│    - AI extract core knowledge             │
│    - Generate summaries and keywords       │
│    - Knowledge quality assessment          │
│                                            │
└─────────────────────────────────────────────┘
```

**Key Classes:**
```java
// Domain Management
top.yumbo.ai.omni.knowledge.registry.network.impl.KnowledgeDomainService
top.yumbo.ai.omni.knowledge.registry.router.DomainRouter

// Knowledge Services
top.yumbo.ai.omni.knowledge.registry.network.KnowledgeStorageService
top.yumbo.ai.omni.knowledge.registry.network.KnowledgeExtractionService
top.yumbo.ai.omni.knowledge.registry.network.KnowledgeRefinementService
top.yumbo.ai.omni.knowledge.registry.network.KnowledgeAssociationService

// Intelligent Retrieval
top.yumbo.ai.omni.knowledge.registry.service.query.CrossDomainQueryService
top.yumbo.ai.omni.knowledge.registry.service.quality.DomainQualityScorer
top.yumbo.ai.omni.knowledge.registry.service.cache.QueryResultCache

// User Preference
top.yumbo.ai.omni.knowledge.registry.service.preference.UserPreferenceLearner
```

**Configuration Example:**
```yaml
omni-agent:
  knowledge-registry:
    enabled: true
    cache-size: 1000
    cross-domain-query:
      enabled: true
      thread-pool-size: 10
      timeout: 30000
    quality-scorer:
      enabled: true
      persistence: true
    user-preference:
      enabled: true
      persistence: true
      min-queries: 5
```

**Detailed Documentation:** [Knowledge Network Architecture](KNOWLEDGE_NETWORK_EN.md)

---

### 3. Orchestration Layer

#### omni-agent-orchestrator

**Responsibility:** Service orchestration and coordination

**Core Functions:**
- ✅ Query service orchestration
- ✅ Context management
- ✅ Workflow coordination
- ✅ Depends only on API interfaces, not concrete implementations

**Key Classes:**
```java
top.yumbo.ai.omni.orchestrator.QueryOrchestrator
top.yumbo.ai.omni.orchestrator.ContextManager
```

**Design Principles:**
- Depends only on API interfaces from each module
- Contains no concrete implementation code
- Responsible for inter-service coordination and orchestration

---

### 4. Service Layer

#### AI Service

**Module Composition:**
- **omni-agent-ai-api** - AI service interfaces
- **omni-agent-ai-starter** - AI service implementations

**Supported AI Providers:**

| Provider | Type | Features | Implementation Class |
|----------|------|----------|---------------------|
| **Ollama** | Local | Free, privacy, offline | `OllamaAIService` |
| **DeepSeek** | Online API | Cost-effective | `OnlineAPIAIService` |
| **OpenAI** | Online API | Best quality | `OnlineAPIAIService` |
| **Qwen** | Online API | Fast in China | `OnlineAPIAIService` |
| **ONNX** | Local | Edge devices | `OnnxAIService` |

**Configuration Example:**
```yaml
omni-agent:
  ai:
    provider: ollama              # ollama/deepseek/openai/qwen
    model: qwen2.5:7b
    base-url: http://localhost:11434
    # Or online API
    # api-key: ${AI_API_KEY}
```

---

#### Document Processing

**Module Composition:**
- **omni-agent-document-processor-api** - Document processing interfaces
- **omni-agent-document-processor-starter** - Document processing implementations

**Supported Document Formats:**

| Format | Processor | Features |
|--------|-----------|----------|
| **Word** | `WordDocumentProcessor` | .doc/.docx, table to Markdown |
| **Excel** | `ExcelDocumentProcessor` | .xls/.xlsx, formula calculation |
| **PPT** | `PPTDocumentProcessor` | .ppt/.pptx, slide extraction |
| **PDF** | `PDFDocumentProcessor` | Page-by-page, metadata |
| **Plain Text** | `PlainTextDocumentProcessor` | .txt/.md/.log, etc. |
| **Images** | `VisionLLMDocumentProcessor` | AI image text extraction |

**Configuration Example:**
```yaml
omni-agent:
  document-processor:
    vision-llm:
      enabled: true
      model: qwen-vl-plus
      api-key: ${DASHSCOPE_API_KEY}
```

---

#### Smart Chunking

**Module Composition:**
- **omni-agent-chunking-api** - Chunking interfaces
- **omni-agent-chunking-starter** - Chunking implementations

**Chunking Strategies:**

| Strategy | Implementation Class | Description | Recommended Scenario |
|----------|---------------------|-------------|---------------------|
| **PPL** ⭐ | `PPLChunkingStrategy` | Perplexity-based semantic boundaries | General use, most intelligent |
| **SEMANTIC** | `SemanticStrategy` | Vector similarity-based | Long documents, semantic aggregation |
| **PARAGRAPH** | `ParagraphStrategy` | Natural paragraph-based | Formatted documents |
| **SENTENCE** | `SentenceStrategy` | Sentence boundary-based | Short texts, conversations |
| **FIXED_LENGTH** | `FixedLengthStrategy` | Fixed-length splitting | Simple scenarios |
| **RECURSIVE** | `RecursiveStrategy` | Recursive chunking | Large documents |

**Usage Example:**
```java
@Service
public class DocumentService {
    
    @Autowired
    private ChunkingService chunkingService;
    
    public void process(String content) {
        ChunkingConfig config = ChunkingConfig.builder()
            .strategy("PPL")          // Recommended: perplexity chunking
            .maxChunkSize(1000)
            .overlapSize(100)
            .build();
            
        List<Chunk> chunks = chunkingService.chunk(content, config);
    }
}
```

---

#### RAG Retrieval

**Module Composition:**
- **omni-agent-rag-api** - RAG interfaces
- **omni-agent-rag-starter-adapter** - RAG adapters

**Supported RAG Implementations:**

| Type | Implementation Class | Features |
|------|---------------------|----------|
| **File/Lucene** | `LuceneRAGService` | Default, no dependencies |
| **Elasticsearch** | `ElasticsearchRAGService` | Distributed search |
| **MongoDB** | `MongoRAGService` | Document database |
| **Redis** | `RedisRAGService` | In-memory cache |

**Configuration Example:**
```yaml
omni-agent:
  rag:
    instances:
      - id: default
        type: file
        index-path: ./data/rag-index
      - id: es-rag
        type: elasticsearch
        hosts: localhost:9200
```

---

#### P2P Collaboration

**Module Composition:**
- **omni-agent-p2p-api** - P2P interfaces
- **omni-agent-p2p-starter** - P2P implementations

**Core Functions:**
- ✅ Peer-to-peer connections
- ✅ Knowledge transfer
- ✅ Collaborative learning
- ✅ Connection code mechanism

**Key Classes:**
```java
top.yumbo.ai.omni.p2p.api.P2PConnectionManager
top.yumbo.ai.omni.p2p.api.P2PDataTransferService
top.yumbo.ai.omni.p2p.api.P2PCollaborationService
```

**Configuration Example:**
```yaml
omni-agent:
  p2p:
    enabled: true
    storage-type: memory  # memory/sqlite/h2/redis/mongodb
    connection-code: "unique-code-123"
```

---

#### OCR Recognition

**Module:** `omni-agent-ocr-starter-tesseract`

**Core Functions:**
- ✅ Tesseract OCR integration
- ✅ Image text recognition
- ✅ Multi-language support

**Configuration Example:**
```yaml
omni-agent:
  ocr:
    tesseract:
      enabled: true
      data-path: ./tessdata
      language: chi_sim  # Simplified Chinese
```

---

### 5. Storage Layer

#### Document Storage

**Module Composition:**
- **omni-agent-document-storage-api** - Storage interfaces
- **omni-agent-document-storage-starter** - Storage implementations

**Supported Storage Engines:**

| Engine | Implementation Class | Use Case | Features |
|--------|---------------------|----------|----------|
| **File** | `FileDocumentStorage` | Development/Testing | Simple, no dependencies |
| **SQLite** | `SQLiteDocumentStorage` | Standalone deployment | Embedded database |
| **H2** | `H2DocumentStorage` | In-memory cache | Fast, temporary |
| **MongoDB** | `MongoDocumentStorage` | Production | Distributed, high-performance |
| **Redis** | `RedisDocumentStorage` | Cache layer | Ultra-fast |
| **Elasticsearch** | `ESDocumentStorage` | Large-scale search | Full-text search |

**Multi-Instance Configuration:**
```yaml
omni-agent:
  storage:
    instances:
      - id: primary
        type: file
        base-path: ./data/storage
      - id: backup
        type: mongodb
        database: omni-agent
        collection: documents
```

---

### 6. Web Service Layer

#### omni-agent-web

**Responsibility:** RESTful API service

**Core Functions:**
- ✅ HTTP API interfaces
- ✅ Document upload and management
- ✅ Query and retrieval
- ✅ User management
- ✅ Configuration management

**Main Controllers:**
```java
top.yumbo.ai.omni.web.controller.DocumentController
top.yumbo.ai.omni.web.controller.QueryController
top.yumbo.ai.omni.web.controller.ConfigController
top.yumbo.ai.omni.web.controller.AdvancedQAController
```

---

#### omni-agent-workflow

**Responsibility:** Workflow engine

**Core Functions:**
- ✅ Workflow definition and execution
- ✅ File monitoring and auto-indexing
- ✅ Document processing workflows
- ✅ Workflow marketplace

**Key Classes:**
```java
top.yumbo.ai.omni.workflow.WorkflowRegistry
top.yumbo.ai.omni.workflow.service.FileWatcherService
top.yumbo.ai.omni.workflow.service.DocumentProcessingService
```

**Configuration Example:**
```yaml
omni-agent:
  workflow:
    storage-type: sqlite
    sqlite-db-path: ./data/workflows/workflows.db
    file-watcher:
      enabled: true
      auto-index: true
      watch-path: ./data/documents
```

---

### 7. Algorithm Marketplace

#### omni-agent-marketplace

**Responsibility:** Algorithm component registration and management

**Core Functions:**
- ✅ Algorithm component registration
- ✅ Query expansion algorithms
- ✅ Re-ranking algorithms
- ✅ Custom algorithm plugins

**Built-in Algorithms:**
- `query_expansion` - Query expansion
- `semantic_chunking` - Semantic chunking
- `rerank` - Result re-ranking

**Key Classes:**
```java
top.yumbo.ai.omni.marketplace.AlgorithmMarketService
top.yumbo.ai.omni.marketplace.EnhancedQueryService
```

---

### 8. Example Applications

#### omni-agent-example-basic

**Responsibility:** Basic example application

**Included Features:**
- ✅ Basic configuration examples
- ✅ Local file storage
- ✅ Ollama AI service
- ✅ HOPE system demo

**Startup:**
```bash
java -jar omni-agent-example-basic-1.0.0.jar
```

---

#### omni-agent-example-production

**Responsibility:** Production environment example

**Included Features:**
- ✅ Production-grade configuration
- ✅ MongoDB storage
- ✅ Elasticsearch RAG
- ✅ Distributed deployment configuration

---

## 🔗 Module Dependencies

### Complete Dependency Graph

```
omni-agent-example-basic (Example Application)
  │
  ├─→ omni-agent-web (Web Service)
  │    ├─→ omni-agent-workflow
  │    ├─→ omni-agent-orchestrator
  │    ├─→ omni-agent-hope-starter
  │    └─→ omni-agent-knowledge-registry-starter
  │
  ├─→ omni-agent-orchestrator (Service Orchestration)
  │    ├─→ omni-agent-hope-api
  │    ├─→ omni-agent-knowledge-registry-api
  │    ├─→ omni-agent-rag-api
  │    └─→ omni-agent-ai-api
  │
  ├─→ omni-agent-hope-starter (HOPE Implementation)
  │    ├─→ omni-agent-hope-api
  │    ├─→ omni-agent-knowledge-registry-api
  │    └─→ omni-agent-core
  │
  ├─→ omni-agent-knowledge-registry-starter (Knowledge Network)
  │    ├─→ omni-agent-knowledge-registry-api
  │    ├─→ omni-agent-rag-api
  │    ├─→ omni-agent-ai-api
  │    ├─→ omni-agent-document-storage-api
  │    └─→ omni-agent-core
  │
  ├─→ omni-agent-rag-starter-adapter (RAG Adapter)
  │    ├─→ omni-agent-rag-api
  │    ├─→ omni-agent-ai-api
  │    └─→ omni-agent-core
  │
  ├─→ omni-agent-document-processor-starter (Document Processing)
  │    ├─→ omni-agent-document-processor-api
  │    ├─→ omni-agent-chunking-api
  │    ├─→ omni-agent-ai-api
  │    └─→ omni-agent-core
  │
  ├─→ omni-agent-chunking-starter (Smart Chunking)
  │    ├─→ omni-agent-chunking-api
  │    ├─→ omni-agent-ai-api
  │    └─→ omni-agent-core
  │
  ├─→ omni-agent-ai-starter (AI Service)
  │    ├─→ omni-agent-ai-api
  │    └─→ omni-agent-core
  │
  ├─→ omni-agent-document-storage-starter (Document Storage)
  │    ├─→ omni-agent-document-storage-api
  │    └─→ omni-agent-core
  │
  ├─→ omni-agent-p2p-starter (P2P Collaboration)
  │    ├─→ omni-agent-p2p-api
  │    └─→ omni-agent-core
  │
  └─→ omni-agent-core (Core Framework)
       ├─→ omni-agent-common
       └─→ Spring Boot 3.4.1
```

### API Layer Independence

All API modules are independent and don't depend on each other:

```
omni-agent-hope-api              (Independent)
omni-agent-knowledge-registry-api (Independent)
omni-agent-rag-api               (Independent)
omni-agent-ai-api                (Independent)
omni-agent-chunking-api          (Independent)
omni-agent-document-processor-api (Independent)
omni-agent-document-storage-api   (Independent)
omni-agent-p2p-api               (Independent)
```

---

## 🎯 Module Selection Guide

### Minimal Configuration (Development/Testing)

**Use Case:** Local development, feature testing

```xml
<dependencies>
    <!-- Core -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-core</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- AI Service -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-ai-starter</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- HOPE System -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-hope-starter</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- Document Storage (File) -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-document-storage-starter</artifactId>
        <version>1.0.0</version>
    </dependency>
</dependencies>
```

**Configuration:**
```yaml
omni-agent:
  ai:
    provider: ollama
    model: qwen2.5:7b
  storage:
    instances:
      - id: default
        type: file
        base-path: ./data/storage
  hope:
    enabled: true
    persistence: in-memory
```

---

### Recommended Configuration (Production)

**Use Case:** Production deployment, complete features

```xml
<dependencies>
    <!-- Web Service -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-web</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- HOPE + Knowledge Network -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-hope-starter</artifactId>
        <version>1.0.0</version>
    </dependency>
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-knowledge-registry-starter</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- Document Processing -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-document-processor-starter</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- RAG -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-rag-starter-adapter</artifactId>
        <version>1.0.0</version>
    </dependency>
    
    <!-- Workflow -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-workflow</artifactId>
        <version>1.0.0</version>
    </dependency>
</dependencies>
```

**Configuration:**
```yaml
omni-agent:
  ai:
    provider: deepseek
    api-key: ${DEEPSEEK_API_KEY}
  storage:
    instances:
      - id: primary
        type: mongodb
        database: omni-agent
  rag:
    instances:
      - id: default
        type: elasticsearch
        hosts: localhost:9200
  knowledge-registry:
    enabled: true
    cross-domain-query:
      enabled: true
  hope:
    enabled: true
    persistence: knowledge-registry
```

---

### Advanced Configuration (Enterprise)

**Use Case:** Large-scale deployment, high availability

**Additional Modules:**
```xml
<!-- P2P Collaboration -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-p2p-starter</artifactId>
    <version>1.0.0</version>
</dependency>

<!-- Algorithm Marketplace -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-marketplace</artifactId>
    <version>1.0.0</version>
</dependency>
```

**Configuration Features:**
- ✅ Multi-storage engine redundancy
- ✅ Distributed RAG
- ✅ P2P knowledge sharing
- ✅ Custom algorithm plugins
- ✅ Complete monitoring and logging

---

## 📊 Module Comparison

### API vs Starter

| Feature | API Module | Starter Module |
|---------|-----------|----------------|
| **Responsibility** | Define interfaces and models | Provide concrete implementations |
| **Dependencies** | Only depends on common | Depends on corresponding API + core |
| **Independence** | Fully independent | Depends on API |
| **Extensibility** | Define extension points | Pluggable implementations |
| **Example** | `omni-agent-ai-api` | `omni-agent-ai-starter` |

### Storage Engine Comparison

| Engine | Performance | Reliability | Scalability | Use Case |
|--------|------------|-------------|-------------|----------|
| **File** | ⭐⭐ | ⭐⭐ | ⭐ | Development/Testing |
| **SQLite** | ⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐ | Standalone deployment |
| **H2** | ⭐⭐⭐⭐ | ⭐⭐ | ⭐ | Temporary cache |
| **MongoDB** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | Production |
| **Redis** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐⭐⭐ | Cache layer |
| **Elasticsearch** | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | Large-scale search |

---

## 📚 Related Documentation

- 🏗️ [Complete System Architecture](ARCHITECTURE_EN.md) - OmniAgent overall architecture
- 🧠 [HOPE Self-Learning System](HOPE_SYSTEM_EN.md) - HOPE detailed design
- 🕸️ [Knowledge Network Architecture](KNOWLEDGE_NETWORK_EN.md) - Knowledge network details
- 🚀 [Quick Start Guide](QUICKSTART_EN.md) - How to use each module

---

**Maintained by:** OmniAgent Team  
**Last Updated:** 2026-01-01  
**Version:** 1.0.0

