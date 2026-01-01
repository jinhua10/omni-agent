# OmniAgent - Full-Scenario Intelligent Agent Framework 🚀

<div align="center">

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE.txt)
[![Java](https://img.shields.io/badge/Java-21-orange.svg)](https://openjdk.org/)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-3.4.1-brightgreen.svg)](https://spring.io/projects/spring-boot)
[![Build](https://img.shields.io/badge/build-passing-brightgreen.svg)]()

**Full-scenario Agent development framework for smarter, more controllable, and practical AI**

### 🌐 [**Live Demo →**](https://yumbo.top/) | 📖 [**Quick Start**](#-quick-start) | 🎯 [**Core Features**](#-core-features)

**[中文](README.md) | English**

**Let Agents Bloom Everywhere - The Year of Agent Officially Begins!**

</div>

---

## 🎯 Why Choose OmniAgent?

OmniAgent is a **full-scenario Agent development framework** designed to solve fundamental flaws in traditional RAG systems, enabling you to:

- 🏗️ **Build Distributed Enterprise-level Agent Platform** - Multi-instance, multi-strategy, disaster recovery redundancy
- 🧠 **Create Smarter Agent Applications** - HOPE self-learning architecture + knowledge network system
- 🚀 **Rapid AI Application Development** - Complete context management + out-of-the-box components
- 📊 **Build Professional Knowledge Services** - Intelligent assistants like Copilot/Cursor
- 🔧 **Build Context-Aware AI Applications** - Project analysis, automated testing, code generation, etc.

### 📈 Project Metrics

| Metric | Value |
|--------|-------|
| **Code Lines** | 85,144 lines of Java code |
| **Backend Modules** | 25 functional modules |
| **Storage Engines** | 6 types (File/SQLite/H2/MongoDB/Redis/Elasticsearch) |
| **RAG Strategies** | 6+ intelligent chunking strategies |
| **Model Support** | 3 types: Ollama/Online API/ONNX local models |
| **Document Formats** | Word/Excel/PPT/PDF + all text formats |
| **Deployment** | Local/Docker/K8s/Cloud servers |

---

## ⚠️ Fundamental Flaws of Traditional RAG

### 1. Chunking Causes Semantic Fragmentation
Traditional RAG forcibly splits documents into fixed-size chunks, causing:
- 📄 **Context Breakage** - Important information cut off, complete semantics lost
- 🔍 **Inaccurate Retrieval** - Related content scattered across different chunks
- 💔 **Semantic Integrity Loss** - Cannot understand complete context and logic

### 2. Single Index Chaos
All documents mixed in one vector space:
- 🌀 **Vector Space Pollution** - Different domain documents interfere, reducing precision
- 🎯 **No Targeted Optimization** - Technical and business docs need different strategies
- 📊 **Poor Scalability** - Performance drops sharply as data grows

### 3. Lack of Context Memory
Traditional RAG cannot understand conversation history:
- 💬 **Multi-turn Dialogue Fails** - Cannot understand pronouns like "it", "this"
- 🔄 **Repeated Questions** - Need complete question every time
- 🧠 **No Learning Ability** - Cannot improve from interactions

### 4. No Knowledge Association
Documents lack semantic connections:
- 🔗 **Isolated Knowledge** - Cannot discover related content and references
- 🕸️ **No Knowledge Graph** - Lack structured knowledge organization
- 📚 **Single Dimension Retrieval** - Only simple keyword matching

---

## ✅ OmniAgent's Architectural Innovation

### 🎯 Core Advantages Comparison

| Feature | Traditional RAG | OmniAgent |
|---------|----------------|-----------|
| **Chunking Strategy** | Fixed-size splitting | 6 intelligent strategies (perplexity/semantic/paragraph) ⭐ |
| **Storage Method** | Single vector DB | 6 heterogeneous storage engines + disaster recovery |
| **RAG System** | Single instance | Multiple parallel systems with different vector dimensions |
| **Knowledge Organization** | Flat retrieval | Domain index + knowledge network + HOPE self-learning |
| **Context Management** | None | Complete dialogue history + intent analysis + gap detection |
| **Distributed** | Not supported | P2P knowledge sharing + connection code mechanism |
| **Model Support** | Single | ONNX local/Ollama/Online API - 3 types |

### 🚀 Innovation Architecture Details

#### 1. Multi-Strategy Intelligent Chunking

```
Traditional Method:
[Fixed 500 chars] [Fixed 500 chars] [Fixed 500 chars]... ❌ Semantic fragmentation
         
OmniAgent Intelligent Chunking:
├─ Perplexity-based Chunking (AI-driven semantic boundary detection) ⭐ Recommended
│  └─ Auto-identify natural semantic boundaries, preserve complete context
├─ Semantic Chunking (vector similarity-based)
│  └─ Aggregate semantically similar content
├─ Paragraph Chunking (natural paragraphs)
│  └─ Maintain original document structure
├─ Sliding Window (overlapping windows)
│  └─ Preserve context continuity
├─ Recursive Chunking (hierarchical)
│  └─ Hierarchical processing for large documents
└─ Fixed-size (compatibility mode)
   └─ Compatible with traditional RAG needs
```

#### 2. Heterogeneous Redundant Storage Architecture

```
Same data, 6 storage methods, disaster recovery:
┌─────────────────────────────────────────┐
│  File ←→ SQLite ←→ MongoDB              │
│    ↕️       ↕️        ↕️                    │
│  Redis ←→ H2 ←→ Elasticsearch           │
└─────────────────────────────────────────┘
✅ Disaster Recovery  ✅ Performance  ✅ On-demand  ✅ Read-Write Separation
```

**Advantages**:
- 📦 **File** - Simple and fast, no additional services
- 💾 **SQLite/H2** - Embedded database, single file deployment
- 📊 **MongoDB** - Document database, complex queries
- ⚡ **Redis** - High-speed cache, millisecond response
- 🔍 **Elasticsearch** - Enterprise search, massive data

#### 3. Multi-dimensional RAG Parallel

```
Multiple RAG systems with different dimensions running simultaneously:
┌──────────────────────────────────────────┐
│ RAG-768dim  → General semantic understanding (fast retrieval)  │
│ RAG-1024dim → Domain-specific precision matching (high accuracy) │  
│ RAG-512dim  → Lightweight retrieval (low resource consumption)  │
└──────────────────────────────────────────┘
Smart Routing: Auto-select optimal RAG system based on question type
```

#### 4. Knowledge Network System

```
Traditional RAG:
Doc1  Doc2  Doc3  Doc4  Doc5 (completely isolated) ❌
            
OmniAgent Knowledge Network Architecture:
          [Core Architecture Doc]
         /      |      \
    [API Doc] [Design] [Codebase]
      /  \      |      /  \
[Interface][Tests][Unit Tests][Integration Tests]
      \    |    |    |    /
       [Auto Knowledge Graph Association]
```

**Domain Index Organization**:
```
Project Knowledge Base
├─ Technical Domain
│  ├─ Architecture docs
│  ├─ API docs
│  └─ Code comments
├─ Business Domain
│  ├─ Requirements docs
│  ├─ Flowcharts
│  └─ User manuals
└─ Testing Domain
   ├─ Test cases
   ├─ Test reports
   └─ Bug tracking
```

#### 5. HOPE Self-Learning Architecture ⭐

**HOPE = Hierarchical Omni-Agent Persistent Engine**

```
Traditional RAG: Question → Retrieval → Return Result (fixed process) ❌

OmniAgent HOPE Architecture:
User Question → Classification → Layer Selection → Smart Retrieval
    ↓                                              ↓
Feedback Learning ← Evaluation ← Generation ← Gap Detection
    ↓                                              ↓
Strategy Optimization ← Pattern Recognition ← Knowledge Supplement ← Auto-improvement
```

**Three-layer Knowledge Structure**:

```
┌─────────────────────────────────────────┐
│  Persistent Layer                       │
│  - Core knowledge, system docs, authoritative refs │
│  - Long-term stable, manual management  │
├─────────────────────────────────────────┤
│  Ordinary Layer                         │
│  - General knowledge, business docs, feature descriptions │
│  - Dynamic updates, regular retrieval   │
├─────────────────────────────────────────┤
│  High Frequency Layer                   │
│  - Hot topics, recent Q&A, frequently accessed knowledge │
│  - Auto-adjustment, priority retrieval  │
└─────────────────────────────────────────┘
```

**Self-learning Capabilities**:
- 🎓 **Question Classification Learning** - Auto-classify based on keywords and patterns
- 🔍 **Knowledge Gap Detection** - Discover knowledge base blind spots
- 📈 **Auto Strategy Optimization** - Dynamically adjust layers based on access frequency
- 🔄 **Continuous Improvement** - Learn and optimize from each interaction

**Detailed Documentation**: [HOPE System Design](docs/refactor_01/core/HOPE_SYSTEM_DESIGN.md)

---

## 🎯 Core Features

### 1. 🤖 Intelligent Agent Building
- ✅ Complete conversation history management
- ✅ Intent analysis and understanding
- ✅ Multi-turn dialogue support
- ✅ Automatic context preservation
- ✅ Role system (multi-role collaboration)

### 2. 📚 Comprehensive Document Processing

**Office Suite**:
- ✅ **Word** (.doc/.docx) - Table to Markdown, style preservation
- ✅ **Excel** (.xls/.xlsx) - Formula calculation, intelligent data segmentation
- ✅ **PowerPoint** (.ppt/.pptx) - Slide content extraction
- ✅ **PDF** - Page-by-page extraction, page number marking, metadata

**All Text Formats**:
- ✅ Basic text: .txt, .md, .log, .csv
- ✅ Config files: .yml, .json, .xml, .ini, .properties
- ✅ Programming languages: .java, .py, .js, .cpp, .go, .ts, .kt, .swift, etc.
- ✅ **Build complete knowledge base for entire code projects**

**Advanced Features**:
- ✅ **Vision LLM Image Extraction** - AI understanding of image content (Qwen3-VL, etc.)
- ✅ **OCR Text Recognition** - Tesseract optical recognition
- ✅ **Local Model/Ollama/Online API** - Flexible extraction methods

### 3. 🧠 Advanced RAG Technology

**6 Intelligent Chunking Strategies**:
- ✅ **Perplexity-based Chunking** ⭐ Recommended - AI-driven semantic boundary detection
- ✅ **Semantic Chunking** - Vector similarity-based aggregation
- ✅ **Paragraph Chunking** - Preserve natural paragraph structure
- ✅ **Sliding Window** - Overlap to preserve context
- ✅ **Recursive Chunking** - Hierarchical processing for large docs
- ✅ **Fixed-size** - Compatibility mode

**Multi-dimensional Vectorization**:
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
- ✅ **Elasticsearch** - Enterprise search, billion-level data

**Disaster Recovery**:
- ✅ Multi-storage backup of same data
- ✅ Automatic failover
- ✅ Data consistency guarantee

### 5. 🕸️ Knowledge Network System

**Domain Index**:
- ✅ Organize knowledge by domain
- ✅ Independent vector spaces
- ✅ Specialized retrieval strategies
- ✅ Smart routing distribution

**Knowledge Graph**:
- ✅ Auto-discover document associations
- ✅ Reference relationship tracking
- ✅ Semantic similarity calculation
- ✅ Smart recommendation of related content

**P2P Knowledge Sharing**:
- ✅ Connection code mechanism
- ✅ Cross-node knowledge transfer
- ✅ Distributed monolith availability
- ✅ Enterprise internal knowledge network

### 6. 🎓 HOPE Self-Learning

- ✅ **Question Classification** - Auto-identify question types
- ✅ **Knowledge Gap Detection** - Discover knowledge blind spots
- ✅ **Auto Strategy Optimization** - Improve based on feedback
- ✅ **Continuous Learning** - Evolve from interactions
- ✅ **Pattern Recognition** - Discover common question patterns

### 7. 🔄 Workflow Engine

- ✅ Visual process design
- ✅ Node drag-and-drop orchestration
- ✅ Conditional branch control
- ✅ Loop iteration support
- ✅ Workflow marketplace (share/import)

### 8. 🌐 Distributed Architecture

- ✅ P2P node connection
- ✅ Decentralized design
- ✅ Cross-node knowledge sharing
- ✅ Monolith availability guarantee
- ✅ Enterprise deployment support

---

## 🏗️ Intelligent Full Pipeline

### Complete Document Processing Pipeline

```
📄 Document Upload
    ↓
📑 Intelligent Text Extraction
    ├─ Local model extraction
    ├─ Ollama service extraction  
    └─ Online API extraction (Qwen3-VL, etc.)
    ↓
    Supported Formats:
    • Office: Word/Excel/PPT (.doc/.docx/.xls/.xlsx/.ppt/.pptx)
    • Documents: PDF
    • Text: All text formats (.txt/.md/.json/.xml/.log/.csv, etc.)
    • Code: All programming language files
    ↓
✂️ Intelligent Chunking
    ├─ Perplexity Chunking (AI-driven) ⭐ Recommended
    ├─ Semantic Chunking (vector similarity)
    ├─ Paragraph Chunking (natural paragraphs)
    ├─ Sliding Window (overlap preservation)
    ├─ Recursive Chunking (hierarchical)
    └─ Fixed-size (compatibility mode)
    ↓
🔢 Vectorization
    ├─ ONNX local models (bge-base-zh/bge-m3, etc.)
    ├─ Ollama local service
    ├─ Online vector API
    └─ Multi-RAG parallel support
    ↓
💾 Multi-element Heterogeneous Storage
    ├─ File (simple and fast)
    ├─ SQLite (embedded)
    ├─ H2 (in-memory database)
    ├─ MongoDB (document database)
    ├─ Redis (high-speed cache)
    └─ Elasticsearch (enterprise search)
    ↓
🕸️ Knowledge Network Construction
    ├─ Domain index organization
    ├─ Knowledge graph auto-construction
    ├─ Semantic association analysis
    └─ P2P knowledge sharing
    ↓
🤖 HOPE Self-Learning
    ├─ Question classification learning
    ├─ Knowledge gap detection
    ├─ Auto strategy optimization
    └─ Continuous improvement mechanism
```

---

## 🚀 Quick Start

### Three Steps to Launch

#### 1️⃣ Clone Project

```bash
# GitHub
git clone https://github.com/jinhua10/omni-agent.git

# Or Gitee (faster in China)
git clone https://gitee.com/gnnu/omni-agent.git

cd omni-agent
```

#### 2️⃣ Build and Start Backend

```bash
# Clean and build project
mvn clean package \
    -pl omni-agent-example-basic \
    -am \
    -DskipTests

# Start backend service (using omni-agent-example-basic module)
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

### 🌐 Live Demo

**Website**: [https://yumbo.top](https://yumbo.top)

Visit the live demo to experience full functionality!

---

## 📦 System Architecture

### Backend Modules (25)

```
omni-agent/
├─ omni-agent-core               # Core foundation module (infrastructure and utilities)
├─ omni-agent-common             # Common utilities
├─ omni-agent-hope-api           # HOPE interface definitions (classification, persistence abstractions)
├─ omni-agent-hope-starter       # HOPE implementation (question classifier, HOPE system)
├─ omni-agent-orchestrator       # Service orchestration layer (query service, context management)
├─ omni-agent-ai-api             # AI service abstract interface
├─ omni-agent-ai-starter         # AI service implementation (Ollama/Online API/Vision LLM)
├─ omni-agent-rag-api            # RAG abstract interface
├─ omni-agent-rag-starter-adapter # RAG adapter (File/H2/SQLite/Redis/MongoDB/ES)
├─ omni-agent-chunking-api       # Chunking strategy interface
├─ omni-agent-chunking-starter   # Chunking strategy implementation (6 types)
├─ omni-agent-document-processor-api    # Document processing interface
├─ omni-agent-document-processor-starter # Document processor implementation
├─ omni-agent-document-storage-api      # Document storage interface
├─ omni-agent-document-storage-starter  # Document storage implementation
├─ omni-agent-knowledge-registry-api    # Knowledge registry interface
├─ omni-agent-knowledge-registry-starter # Knowledge network implementation
├─ omni-agent-ocr-starter-tesseract    # OCR recognition
├─ omni-agent-p2p-api            # P2P interface
├─ omni-agent-p2p-starter        # P2P implementation
├─ omni-agent-workflow           # Workflow engine
├─ omni-agent-marketplace        # Workflow marketplace
├─ omni-agent-web                # Web interface layer
├─ omni-agent-example-basic      # Basic example (startup entry)
└─ omni-agent-example-production # Production environment example
```

**Architecture Layering**:

```
Application Layer
├── omni-agent-web
├── omni-agent-example-basic
└── omni-agent-example-production
    ↓ depends on
Service Orchestration Layer
└── omni-agent-orchestrator
    ├── Query Service ✅
    ├── Context Management ✅
    └── Only depends on API interfaces ✅
    ↓ depends on
Starter Implementation Layer
├── omni-agent-hope-starter
│   ├── HOPE System Implementation ✅
│   ├── Question Classifier ✅
│   └── Depends on Caffeine ✅
├── omni-agent-rag-starter-adapter
│   └── Depends on Lucene ✅
└── omni-agent-document-processor-starter
    └── Depends on POI, PDFBox, Tika ✅
    ↓ depends on
API Interface Layer
├── omni-agent-hope-api
│   ├── HopePersistence Interface ✅
│   ├── QuestionClassifier Interface ✅
│   └── QuestionTypeConfig Model ✅
└── Other API Modules
    ↓ depends on
Core Layer
└── omni-agent-core
    ├── Infrastructure and utilities ✅
    ├── No dependency on specific implementation libraries ✅
    └── Clear responsibilities ✅
```

**Module Responsibilities**:
- **omni-agent-core**: Core foundation module, provides infrastructure and utility classes
- **omni-agent-hope-api**: HOPE interface definitions, includes question classification, persistence abstractions
- **omni-agent-hope-starter**: HOPE implementation, includes question classifier, HOPE system, etc.
- **omni-agent-orchestrator**: Service orchestration layer, handles query service, context management, business orchestration

### Frontend Tech Stack

- ⚛️ **React 18** - Modern UI framework
- 🎨 **Ant Design 5** - Enterprise component library
- 🎭 **Framer Motion** - Smooth animations
- 📊 **ECharts** - Data visualization
- 🔄 **React Router** - Routing management
- 🎨 **Custom Theme Engine** - Multi-theme switching

---

## 💡 Use Cases

### 1. 🏢 Enterprise Knowledge Management
- Internal document intelligent retrieval
- Technical documentation auto-Q&A
- Project knowledge sedimentation
- New employee training assistant

### 2. 💻 Development Assistant Tools
- Code repository intelligent analysis
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
- Literature correlation analysis
- Research achievement management
- Knowledge discovery

### 5. 🤖 Intelligent Customer Service
- Product documentation Q&A
- FAQ responses
- Multi-turn dialogue support
- Knowledge base management

### 6. 📊 Data Analysis
- Report auto-generation
- Data insight discovery
- Trend analysis & prediction
- Anomaly detection

---

## 🗺️ Development Roadmap

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
- 📋 More AI model integration
- 📋 Enterprise permission system
- 📋 Audit logging system
- 📋 SaaS cloud service version

---

## 👥 Contribution Guidelines

We welcome all forms of contributions!

### How to Contribute

1. 🐛 **Report Bugs** - [Issue Tracker](https://github.com/jinhua10/omni-agent/issues)
2. 💡 **Feature Requests** - Submit feature requests
3. 📝 **Improve Documentation** - Documentation is never perfect enough
4. 🔧 **Submit Code** - Pull Requests are welcome

### Development Process

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

## 🔮 Future Vision & Roadmap

### Why Open Source?

We firmly believe: **A complete enterprise-level AI service platform solution should be known and used by more people.**

Through our observation, **there is currently no truly complete solution in the industry that transforms from knowledge base to AI service platform**. The market offers either:
- 🔸 Traditional knowledge bases (query only, no generation)
- 🔸 AI tools (single function, no collaboration)
- 🔸 Skill-based Agents (high cost, unstable accuracy)
- 🔸 Commercial closed-source products (expensive, data security concerns)

**OmniAgent's Mission:**
```
Let every enterprise have its own AI service platform
Let every employee enjoy productivity improvements from AI
Let every developer build innovative applications on this foundation
```

### 📅 Development Roadmap

#### Near-term Plans (3-6 months)

**1. Agent Skill Capabilities** ⭐
```
✨ Under Development

Learning from mainstream solutions like Copilot:
├─ Agents can call external tools and APIs
├─ Support custom Skill plugins
├─ Skill marketplace (preset common skills)
└─ Deep integration with knowledge network (more accurate than Copilot)

Core Difference:
OmniAgent = Skill-based Agent + Knowledge Network
→ Both Skill flexibility and knowledge network accuracy
```

**2. Multimodal AI Services**
```
├─ Image understanding and generation (auto-parse charts in documents)
├─ Voice input/output (voice Q&A, meeting summarization)
├─ Video content analysis (auto-extract training video key points)
└─ Cross-modal retrieval (find images with text, find docs with images)
```

**3. Enterprise Feature Enhancements**
```
├─ Stronger permission system (fine-grained access control)
├─ Better monitoring & ops (complete observability)
├─ More integrations (DingTalk, WeCom, Feishu, etc.)
└─ Smarter recommendations (proactive knowledge push)
```

#### Mid-term Plans (6-12 months)

**1. Enhanced Agent Collaboration**
```
├─ Agent workflow orchestration (visual Agent collaboration config)
├─ Cross-enterprise Agent collaboration (secure knowledge sharing)
├─ Agent capability marketplace (preset professional Agent templates)
└─ Agent performance analysis (contribution assessment per Agent)
```

**2. Knowledge Graph Visualization**
```
├─ Enterprise knowledge map (global view of knowledge distribution)
├─ Knowledge correlation analysis (discover hidden connections)
├─ Knowledge gap identification (proactively find blind spots)
└─ Knowledge evolution tracking (how knowledge changes over time)
```

**3. Industry Solutions**
```
├─ Finance industry edition (compliance, risk control, investment research)
├─ Healthcare industry edition (medical records, diagnosis, research)
├─ Manufacturing edition (process, quality control, supply chain)
└─ More industries... (based on community demand)
```

#### Long-term Vision

```
Make OmniAgent:
✨ Infrastructure for enterprise AI services
✨ Operating system for Agent collaboration
✨ Industry standard for knowledge management
✨ Innovation platform for developers
```

### Current Challenges

**Honestly, we still have a lot to do:**

⚠️ **Feature-wise**
- Some advanced features still under development (e.g., Agent Skill)
- Accuracy in certain scenarios needs optimization
- UI/UX experience can be better

⚠️ **Ecosystem-wise**
- Need more use cases and best practices
- Need more comprehensive documentation and tutorials
- Need more active developer community

⚠️ **Business-wise**
- Enterprise service system under construction
- Partner network expanding
- Industry solutions need deepening

**But we believe:**
- ✅ Direction is right (from knowledge base to AI service platform)
- ✅ Architecture is solid (complete data closed-loop)
- ✅ Community will grow (build ecosystem together)

---

## 🌟 Support the Project

### ⭐ Give Us a Star

**If you appreciate this project's vision and direction, please give OmniAgent a Star!**

Your every Star is our greatest encouragement, helping to:
- 💪 Motivate the team to continue development and optimization
- 📢 Let more people discover this project
- 🚀 Promote enterprise AI service platform development
- 🌱 Foster open-source community ecosystem

[![GitHub stars](https://img.shields.io/github/stars/jinhua10/omni-agent?style=social)](https://github.com/jinhua10/omni-agent/stargazers)

### 🤝 Participate in Community Building

We sincerely invite you to participate in building OmniAgent:

**If you are a Developer 👨‍💻**
- 💡 Propose requirements and suggestions ([Issues](https://github.com/jinhua10/omni-agent/issues))
- 🐛 Report bugs and problems ([Issues](https://github.com/jinhua10/omni-agent/issues))
- 📝 Contribute code and features ([Pull Requests](https://github.com/jinhua10/omni-agent/pulls))
- 🔧 Develop plugins and extensions (based on framework)
- We provide: **Technical guidance, code review, recognition**

**If you are an Enterprise User 🏢**
- 📊 Share usage experiences and cases ([Discussions](https://github.com/jinhua10/omni-agent/discussions))
- 💬 Propose business needs and suggestions ([Issues](https://github.com/jinhua10/omni-agent/issues))
- 🤝 Become early customer, co-build industry solutions
- We provide: **Priority support, custom development, business cooperation**

**If you are a Content Contributor 📚**
- 📖 Improve project documentation and tutorials
- 🎬 Create video tutorials and demos
- 📢 Write blog articles and case analyses
- 🌍 Translate docs to other languages
- We provide: **Platform exposure, community honor, material rewards**

**If you are a Product Designer 💡**
- 🎨 Optimize UI design and user experience
- 🖼️ Design product icons and visual elements
- 📱 Provide interaction design suggestions
- We provide: **Design freedom, portfolio showcase, team collaboration**

### 💬 Join the Community

**Multiple ways to participate in discussions and exchanges:**

- 💬 [GitHub Discussions](https://github.com/jinhua10/omni-agent/discussions) - Feature discussions, help requests
- 🐛 [GitHub Issues](https://github.com/jinhua10/omni-agent/issues) - Bug reports, feature suggestions
- 📧 Email: 1015770492@qq.com
- 📝 CSDN Blog: [https://yumbo.blog.csdn.net/](https://yumbo.blog.csdn.net/)

### 🎁 Contributor Benefits

Thanks to every contributor! We provide active contributors with:

- 🏆 **Recognition**: Contributor list display, special thanks
- 📊 **Priority Access**: Priority testing and feedback on new features
- 🎓 **Technical Growth**: Participate in core technical discussions and decisions
- 🤝 **Career Opportunities**: Outstanding contributors can receive job recommendations
- 🎁 **Material Rewards**: Major contributors can receive commemorative items

**Let's make OmniAgent better together! 🚀**

---

## 📄 License

This project is licensed under **Apache License 2.0**.

See [LICENSE.txt](LICENSE.txt) file for details.

---

## 🙏 Acknowledgments

Thanks to the following open-source projects:

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

<div align="center">

**Let Agents Bloom Everywhere - The Year of Agent Officially Begins!**

**OmniAgent - Building Next-Generation Intelligent Agent Applications**

[🌐 Website](https://yumbo.top) • [📖 Docs](docs/) • [🐛 Feedback](https://github.com/jinhua10/omni-agent/issues) • [💬 Discussions](https://github.com/jinhua10/omni-agent/discussions)

Made with ❤️ by OmniAgent Team

</div>

