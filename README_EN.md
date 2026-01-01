# OmniAgent - Full-Scenario Intelligent Agent Framework 🚀

<div align="center">

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE.txt)
[![Java](https://img.shields.io/badge/Java-21-orange.svg)](https://openjdk.org/)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-3.4.1-brightgreen.svg)](https://spring.io/projects/spring-boot)
[![Build](https://img.shields.io/badge/build-passing-brightgreen.svg)]()

**Making AI More Intelligent, Controllable, and Practical**

### 🌐 [**Live Demo →**](https://yumbo.top/) | 📖 [**Quick Start**](#-quick-start) | 🎯 [**Key Features**](#-key-features)

**[中文](README.md) | English**

**Empowering Agents Everywhere - The Agent Era Begins!**

</div>

---

## 🎯 Why OmniAgent?

OmniAgent is a **full-scenario Agent development framework**, designed to solve fundamental defects of traditional RAG systems:

- 🏗️ **Build Enterprise-Grade Distributed Agent Platforms** - Multi-instance, multi-strategy, disaster recovery
- 🧠 **Create Smarter Agent Applications** - HOPE Self-Learning Architecture + Knowledge Network System  
- 🚀 **Rapid AI Application Development** - Complete context management + ready-to-use components
- 📊 **Professional Knowledge Services** - Copilot/Cursor-like intelligent assistants
- 🔧 **Build Context-Aware AI Applications** - Project analysis, automated testing, code generation, etc.

### 📈 Project Statistics

| Metric | Value |
|--------|-------|
| **Code Lines** | 85,144 lines of Java code |
| **Backend Modules** | 25 functional modules |
| **Storage Engines** | 6 types (File/SQLite/H2/MongoDB/Redis/Elasticsearch) |
| **RAG Strategies** | 6+ intelligent chunking strategies |
| **Supported Models** | 3 types (Ollama/Online API/ONNX) |
| **Document Formats** | Word/Excel/PPT/PDF + All text formats |
| **Deployment** | Local/Docker/K8s/Cloud Server |

---

## 🏗️ Complete System Architecture

**Comprehensive Documentation**: [Complete Architecture with HOPE System](docs/core/ARCHITECTURE.md)

### HOPE Self-Learning System ⭐

**HOPE = Hierarchical Omni-Agent Persistent Engine**

The core intelligence management brain of OmniAgent, featuring:

```
┌─────────────────────────────────────────┐
│  Permanent Layer                         │
│  - Core knowledge, system docs           │
│  - Long-term stable, manually managed    │
├─────────────────────────────────────────┤
│  Ordinary Layer                          │
│  - General knowledge, business docs      │
│  - Dynamically updated, regular retrieval│
├─────────────────────────────────────────┤
│  High Frequency Layer                    │
│  - Hot topics, recent Q&A                │
│  - Auto-adjusted, priority retrieval     │
└─────────────────────────────────────────┘
```

**Self-Learning Capabilities**:
- 🎓 **Question Classification Learning** - Auto-classify based on keywords and patterns
- 🔍 **Knowledge Gap Detection** - Discover knowledge base blind spots
- 📈 **Strategy Auto-Optimization** - Dynamic layer adjustment based on access frequency
- 🔄 **Continuous Improvement** - Learn and optimize from every interaction

**Detailed Documentation**: [HOPE System Design](docs/core/HOPE_SYSTEM_EN.md)

---

## ✅ Architectural Innovations

### 🎯 Core Advantages Comparison

| Feature | Traditional RAG | OmniAgent |
|---------|----------------|-----------|
| **Chunking Strategy** | Fixed-size splitting | 6 intelligent strategies (PPL/Semantic/Paragraph, etc.) ⭐ |
| **Storage** | Single vector DB | 6 storage engines with redundancy + disaster recovery |
| **RAG System** | Single instance | Multi-instance with different vector dimensions in parallel |
| **Knowledge Organization** | Flat retrieval | Domain indexing + Knowledge network + HOPE self-learning |
| **Context Management** | None | Complete conversation history + Intent analysis + Gap detection |
| **Distributed** | Not supported | P2P knowledge sharing + Connection code mechanism |
| **Model Support** | Single | ONNX local/Ollama/Online API - 3 types |

### 🚀 Intelligent Chunking Strategies

```
Traditional Approach:
[Fixed 500 chars] [Fixed 500 chars] [Fixed 500 chars]... ❌ Semantic fragmentation
         
OmniAgent Smart Chunking:
├─ PPL Chunking (AI-driven semantic boundary) ⭐ Recommended
│  └─ Auto-identify natural semantic boundaries, preserve complete context
├─ Semantic Chunking (based on vector similarity)
│  └─ Aggregate semantically similar content
├─ Paragraph Chunking (based on natural paragraphs)
│  └─ Maintain original document structure
├─ Sliding Window (overlapping windows)
│  └─ Preserve context continuity
├─ Recursive Chunking (hierarchical)
│  └─ Hierarchical processing for large documents
└─ Fixed Size (compatibility mode)
   └─ Compatible with traditional RAG needs
```

---

## 🎯 Key Features

### 1. 🤖 Intelligent Agent Construction
- ✅ Complete conversation history management
- ✅ Intent analysis and understanding
- ✅ Multi-turn conversation support
- ✅ Automatic context retention
- ✅ Role system (multi-role collaboration)

### 2. 📚 Comprehensive Document Processing

**Office Suite**:
- ✅ **Word** (.doc/.docx) - Table to Markdown, style preservation
- ✅ **Excel** (.xls/.xlsx) - Formula calculation, intelligent data segmentation
- ✅ **PowerPoint** (.ppt/.pptx) - Slide content extraction
- ✅ **PDF** - Page-by-page extraction, page numbering, metadata

**All Text Formats**:
- ✅ Basic text: .txt, .md, .log, .csv
- ✅ Config files: .yml, .json, .xml, .ini, .properties
- ✅ Programming languages: .java, .py, .js, .cpp, .go, .ts, .kt, .swift, etc.
- ✅ **Build complete knowledge base for entire code projects**

**Advanced Features**:
- ✅ **Vision LLM Image Extraction** - AI-powered image content understanding (Qwen3-VL, etc.)
- ✅ **OCR Text Recognition** - Tesseract optical recognition
- ✅ **Local Model/Ollama/Online API** - Flexible extraction methods

### 3. 🧠 Advanced RAG Technology

**6 Intelligent Chunking Strategies**:
- ✅ **PPL Smart Chunking** ⭐ Recommended - AI-driven semantic boundary identification
- ✅ **Semantic Chunking** - Vector similarity-based aggregation
- ✅ **Paragraph Chunking** - Maintain natural paragraph structure
- ✅ **Sliding Window** - Overlapping to preserve context
- ✅ **Recursive Chunking** - Hierarchical processing for large documents
- ✅ **Fixed Size** - Compatibility mode

**Multi-Dimensional Vectorization**:
- ✅ **ONNX Local Models** - bge-base-zh, bge-m3, etc.
- ✅ **Ollama Service** - Local deployment, data security
- ✅ **Online Vector API** - Qwen, DeepSeek, etc.
- ✅ **Multi-RAG Parallel** - Different dimensions working simultaneously

### 4. 💾 Heterogeneous Redundant Storage

**6 Storage Engines**:
- ✅ **File** - File system, zero dependencies
- ✅ **SQLite** - Embedded database, single file
- ✅ **H2** - In-memory database, high performance
- ✅ **MongoDB** - Document database, flexible schema
- ✅ **Redis** - Cache acceleration, millisecond response
- ✅ **Elasticsearch** - Enterprise search, billion-scale data

**Disaster Recovery**:
- ✅ Same data backed up to multiple storages
- ✅ Automatic failover
- ✅ Data consistency guarantee

### 5. 🕸️ Knowledge Network System

**Domain Indexing**:
- ✅ Knowledge organization by domain
- ✅ Independent vector spaces
- ✅ Specialized retrieval strategies
- ✅ Intelligent routing distribution

**Knowledge Graph**:
- ✅ Automatic document association discovery
- ✅ Reference relationship tracking
- ✅ Semantic similarity calculation
- ✅ Intelligent related content recommendation

**P2P Knowledge Sharing**:
- ✅ Connection code mechanism
- ✅ Cross-node knowledge transfer
- ✅ Distributed standalone availability
- ✅ Enterprise internal knowledge network

### 6. 🎓 HOPE Self-Learning

- ✅ **Question Classification** - Auto-identify question types
- ✅ **Knowledge Gap Detection** - Discover knowledge blind spots
- ✅ **Strategy Auto-Optimization** - Improve based on feedback
- ✅ **Continuous Learning** - Evolve from interactions
- ✅ **Pattern Recognition** - Discover common question patterns

---

## 🚀 Quick Start

### 3-Step Launch

#### 1️⃣ Clone Project

```bash
# GitHub
git clone https://github.com/jinhua10/omni-agent.git

# Or Gitee (Recommended for China, faster)
git clone https://gitee.com/gnnu/omni-agent.git

cd omni-agent
```

#### 2️⃣ Build and Start Backend

```bash
# Clean and compile project
mvn clean package \
    -pl omni-agent-example-basic \
    -am \
    -DskipTests

# Start backend service
java -Dfile.encoding=UTF-8 \
     -Dsun.jnu.encoding=UTF-8 \
     -jar omni-agent-example-basic/target/omni-agent-example-basic-1.0.0.jar
```

**Or use startup scripts**:
```bash
# Windows
.\scripts\start.ps1

# Linux/Mac
chmod +x scripts/start.sh
./scripts/start.sh
```

#### 3️⃣ Start Frontend

```bash
cd UI

# Install dependencies
npm install

# Start dev server
npm run dev
```

Visit **http://localhost:3000** to get started!

### 📚 Complete Documentation

- 📖 [Quick Start Guide](docs/core/QUICKSTART.md) - Detailed tutorial
- 🏗️ [Complete System Architecture](docs/core/ARCHITECTURE.md) - Full architecture including HOPE
- 🧠 [HOPE Self-Learning System](docs/core/HOPE_SYSTEM_EN.md) - Hierarchical intelligent engine explained
- 🕸️ [Knowledge Network Architecture](docs/core/KNOWLEDGE_NETWORK.md) - Knowledge graph and domain management
- 📦 [Module Architecture](docs/core/MODULES.md) - 25 functional modules explained
- 📑 [Documentation Index](docs/core/README.md) - Navigation for all docs

### 🌐 Live Demo

**Official Website**: [https://yumbo.top](https://yumbo.top)

Try the online demo now!

---

## 📦 System Architecture

### Backend Modules (25+)

```
omni-agent/
├─ omni-agent-core               # Core infrastructure
├─ omni-agent-common             # Common utilities
├─ omni-agent-hope-api           # HOPE API definitions
├─ omni-agent-hope-starter       # HOPE implementation
├─ omni-agent-orchestrator       # Service orchestration
├─ omni-agent-ai-api             # AI service abstraction
├─ omni-agent-ai-starter         # AI service implementation
├─ omni-agent-rag-api            # RAG abstraction
├─ omni-agent-rag-starter-adapter # RAG adapter
├─ omni-agent-chunking-api       # Chunking strategy API
├─ omni-agent-chunking-starter   # Chunking implementation
├─ omni-agent-document-processor-api    # Document processor API
├─ omni-agent-document-processor-starter # Document processor impl
├─ omni-agent-document-storage-api      # Document storage API
├─ omni-agent-document-storage-starter  # Storage implementation
├─ omni-agent-knowledge-registry-api    # Knowledge registry API
├─ omni-agent-knowledge-registry-starter # Knowledge network impl
├─ omni-agent-ocr-starter-tesseract    # OCR recognition
├─ omni-agent-p2p-api            # P2P API
├─ omni-agent-p2p-starter        # P2P implementation
├─ omni-agent-workflow           # Workflow engine
├─ omni-agent-marketplace        # Workflow marketplace
├─ omni-agent-web                # Web interface
├─ omni-agent-example-basic      # Basic example
└─ omni-agent-example-production # Production example
```

### Frontend Tech Stack

- ⚛️ **React 18** - Modern UI framework
- 🎨 **Ant Design 5** - Enterprise component library
- 🎭 **Framer Motion** - Smooth animations
- 📊 **ECharts** - Data visualization
- 🔄 **React Router** - Route management
- 🎨 **Custom Theme Engine** - Multi-theme switching

---

## 💡 Use Cases

### 1. 🏢 Enterprise Knowledge Management
- Internal document intelligent retrieval
- Technical documentation auto-Q&A
- Project knowledge accumulation
- New employee training assistant

### 2. 💻 Development Tools
- Codebase intelligent analysis
- API documentation auto-generation
- Code review assistant
- Project architecture analysis

### 3. 🎓 Education & Training
- Course material Q&A
- Learning progress tracking
- Knowledge graph construction
- Personalized learning paths

### 4. 🔬 Research Assistant
- Paper intelligent retrieval
- Literature association analysis
- Research result management
- Knowledge discovery

### 5. 🤖 Intelligent Customer Service
- Product documentation Q&A
- FAQ automation
- Multi-turn conversation support
- Knowledge base management

### 6. 📊 Data Analysis
- Report auto-generation
- Data insight discovery
- Trend analysis and prediction
- Anomaly detection

---

## 🗺️ Roadmap

### ✅ Completed (v1.0.0)

- ✅ Core architecture design
- ✅ 6 intelligent chunking strategies
- ✅ 6 storage engine support
- ✅ Multi-dimensional RAG system
- ✅ Knowledge network foundation
- ✅ HOPE self-learning framework
- ✅ Workflow engine
- ✅ P2P knowledge sharing
- ✅ Web management interface
- ✅ Full Office document support
- ✅ Vision LLM integration

### 🚧 In Progress (v1.1.0)

- 🔄 Knowledge graph visualization
- 🔄 Advanced analytics dashboard
- 🔄 More RAG strategies
- 🔄 Performance optimization
- 🔄 Docker deployment solution

### 📅 Planned (v2.0.0)

- 📋 Multi-language support (Python SDK, Node.js SDK)
- 📋 Cloud-native deployment (K8s Operator)
- 📋 Vector database optimization
- 📋 More AI model integrations
- 📋 Enterprise-grade permission system
- 📋 Audit logging system
- 📋 SaaS cloud service version

---

## 👥 Contributing

We welcome all forms of contributions!

### How to Contribute

1. 🐛 **Submit Bugs** - [Issue Tracker](https://github.com/jinhua10/omni-agent/issues)
2. 💡 **Feature Requests** - Submit feature suggestions
3. 📝 **Improve Documentation** - Documentation is never complete enough
4. 🔧 **Submit Code** - Pull Requests are welcome

### Development Workflow

```bash
# 1. Fork the project
# 2. Create feature branch
git checkout -b feature/AmazingFeature

# 3. Commit changes
git commit -m 'Add some AmazingFeature'

# 4. Push to branch
git push origin feature/AmazingFeature

# 5. Submit Pull Request
```

---

## 🌟 Support the Project

### ⭐ Give Us a Star

**If you recognize the vision and direction of this project, please give OmniAgent a Star!**

Your every Star:
- 💪 Motivates the team to continue development
- 📢 Helps more people discover this project
- 🚀 Drives the development of enterprise AI service platforms
- 🌱 Promotes open source community ecosystem

[![GitHub stars](https://img.shields.io/github/stars/jinhua10/omni-agent?style=social)](https://github.com/jinhua10/omni-agent/stargazers)

---

## 📄 License

This project is licensed under **Apache License 2.0**.

See [LICENSE.txt](LICENSE.txt) for details.

---

## 🙏 Acknowledgments

Thanks to the following open source projects:

- [Spring Boot](https://spring.io/projects/spring-boot) - Application framework
- [Apache Lucene](https://lucene.apache.org/) - Full-text search
- [ONNX Runtime](https://onnxruntime.ai/) - Model inference
- [React](https://react.dev/) - Frontend framework
- [Ant Design](https://ant.design/) - UI component library
- [Ollama](https://ollama.ai/) - Local LLM service

---

## 📞 Contact

- 📧 **Email**: 1015770492@qq.com
- 💬 **CSDN Blog**: [https://yumbo.blog.csdn.net/](https://yumbo.blog.csdn.net/)
- 🐙 **GitHub**: [https://github.com/jinhua10](https://github.com/jinhua10)
- 🦊 **Gitee**: [https://gitee.com/gnnu](https://gitee.com/gnnu)
- 🌐 **Website**: [https://yumbo.top](https://yumbo.top)

---

## ⭐ Star History

If this project helps you, please give us a Star! ⭐

[![Star History Chart](https://api.star-history.com/svg?repos=jinhua10/omni-agent&type=Date)](https://star-history.com/#jinhua10/omni-agent&Date)

---

## 💝 Contact & Sponsor

<div align="center">

<table>
<tr>
<td align="center">
  <h3>📱 Contact</h3>
  <img src="UI/src/assets/images/Connect Me.png" alt="Contact QR Code" width="200"/>
  <p><b>Scan to add WeChat<br/>Join tech community</b></p>
</td>
<td align="center">
  <h3>☕ Sponsor</h3>
  <img src="UI/src/assets/images/Payment QR Code.png" alt="Sponsor QR Code" width="200"/>
  <p><b>Buy me a coffee ☕<br/>Your support drives our development! 💪</b></p>
</td>
</tr>
</table>

</div>

---

<div align="center">

**Empowering Agents Everywhere - The Agent Era Begins!**

**OmniAgent - Building Next-Generation Intelligent Agent Applications**

[🌐 Website](https://yumbo.top) • [📖 Core Docs](docs/core/) • [🐛 Issues](https://github.com/jinhua10/omni-agent/issues) • [💬 Discussions](https://github.com/jinhua10/omni-agent/discussions)

Made with ❤️ by OmniAgent Team

</div>

