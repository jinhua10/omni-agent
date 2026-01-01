# OmniAgent - 全场景智能Agent框架 🚀

<div align="center">

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE.txt)
[![Java](https://img.shields.io/badge/Java-21-orange.svg)](https://openjdk.org/)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-3.4.1-brightgreen.svg)](https://spring.io/projects/spring-boot)
[![Build](https://img.shields.io/badge/build-passing-brightgreen.svg)]()

**让AI更智能、更可控、更实用的全场景Agent开发框架**

### 🌐 [**官网演示 →**](https://yumbo.top/) | 📖 [**快速开始**](#-快速开始) | 🎯 [**核心特性**](#-核心特性)

**中文 | [English](README_EN.md)**

**让Agent遍地开花，Agent元年正式启动！**

</div>

---

## 🎯 为什么选择 OmniAgent？

OmniAgent 是一个**全场景Agent开发框架**，专为解决传统RAG系统的根本性缺陷而设计，让您能够：

- 🏗️ **构建分布式企业级Agent平台** - 支持多实例、多策略、灾备冗余
- 🧠 **打造更智能的Agent应用** - HOPE自学习架构 + 知识网络系统  
- 🚀 **快速开发AI应用** - 完整的上下文管理 + 开箱即用的组件
- 📊 **构建专业知识服务** - 类似Copilot/Cursor的智能助手
- 🔧 **构建上下文更智能的任意AI应用** - 项目分析、自动化测试、代码生成等

### 📈 项目数据

| 指标 | 数值 |
|------|------|
| **代码量** | 85,144 行 Java 代码 |
| **后端模块** | 27 个功能模块 |
| **存储引擎** | 6 种（File/SQLite/H2/MongoDB/Redis/Elasticsearch） |
| **RAG策略** | 6+ 种智能分块策略 |
| **支持模型** | Ollama/在线API/ONNX本地模型 3种类型 |
| **文档格式** | Word/Excel/PPT/PDF + 所有文本格式 |
| **部署方式** | 本地/Docker/K8s/云服务器 |

---

## ⚠️ 传统RAG的根本性缺陷

### 1. 分块导致语义割裂
传统RAG将文档强制切分成固定大小的块，导致：
- 📄 **上下文断裂** - 重要信息被切断，完整语义无法保留
- 🔍 **检索不准确** - 相关内容分散在不同块中
- 💔 **语义完整性丢失** - 无法理解完整语境和逻辑关系

### 2. 单一索引混乱
所有文档混在一个向量空间：
- 🌀 **向量空间污染** - 不同领域文档互相干扰，降低检索精度
- 🎯 **无法针对性优化** - 技术文档和业务文档需要不同策略
- 📊 **扩展性差** - 数据量增长后性能急剧下降

### 3. 缺乏上下文记忆
传统RAG无法理解对话历史：
- 💬 **多轮对话失败** - 无法理解"它"、"这个"等指代词
- 🔄 **重复提问** - 每次都需要提供完整问题
- 🧠 **无学习能力** - 无法从交互中改进和优化

### 4. 无知识关联
文档之间缺乏语义连接：
- 🔗 **孤立的知识点** - 无法发现相关内容和引用关系
- 🕸️ **无知识图谱** - 缺少结构化的知识组织
- 📚 **检索单一维度** - 只能靠关键词简单匹配

---

## ✅ OmniAgent的架构创新

### 🎯 核心优势对比

| 特性 | 传统RAG | OmniAgent |
|------|---------|-----------|
| **分块策略** | 固定大小切分 | 6种智能策略（困惑度/语义/段落等） ⭐ |
| **存储方式** | 单一向量库 | 6种存储引擎异构冗余 + 灾备方案 |
| **RAG系统** | 单一实例 | 支持多套不同向量维度系统并行使用 |
| **知识组织** | 平面检索 | 域索引 + 知识网络 + HOPE自学习 |
| **上下文管理** | 无 | 完整的对话历史 + 意图分析 + 缺口检测 |
| **分布式** | 不支持 | P2P知识共享 + 连接码机制 |
| **模型支持** | 单一 | ONNX本地/Ollama/在线API 三种类型 |

### 🚀 创新架构详解

#### 1. 多策略智能分块

```
传统方式:
[固定500字] [固定500字] [固定500字]... ❌ 语义割裂
         
OmniAgent智能分块:
├─ 困惑度分块 (基于AI的语义边界识别) ⭐ 推荐
│  └─ 自动识别自然语义边界，保留完整上下文
├─ 语义分块 (基于向量相似度)
│  └─ 语义相近的内容聚合在一起
├─ 段落分块 (基于自然段落)
│  └─ 保持文档原始结构
├─ 滑动窗口 (重叠窗口)
│  └─ 保留上下文连续性
├─ 递归分块 (层次化)
│  └─ 大文档层次化处理
└─ 固定大小 (兼容模式)
   └─ 兼容传统RAG需求
```

#### 2. 异构冗余存储架构

```
同一份数据，6种存储方式，灾备保障:
┌─────────────────────────────────────────┐
│  File存储 ←→ SQLite ←→ MongoDB          │
│     ↕️          ↕️         ↕️               │
│  Redis缓存 ←→ H2内存 ←→ Elasticsearch   │
└─────────────────────────────────────────┘
✅ 灾备保障  ✅ 性能优化  ✅ 按需选择  ✅ 读写分离
```

**优势**：
- 📦 **File** - 简单快速，无需额外服务
- 💾 **SQLite/H2** - 嵌入式数据库，单文件部署
- 📊 **MongoDB** - 文档数据库，复杂查询
- ⚡ **Redis** - 高速缓存，毫秒级响应
- 🔍 **Elasticsearch** - 企业级搜索，海量数据

#### 3. 多维度RAG并行

```
同时运行多套不同维度的RAG系统:
┌──────────────────────────────────────────┐
│ RAG-768维  → 通用语义理解（快速检索）     │
│ RAG-1024维 → 专业领域精准匹配（高精度）   │  
│ RAG-512维  → 轻量级检索（低资源消耗）     │
└──────────────────────────────────────────┘
智能路由：根据问题类型自动选择最优RAG系统
```

#### 4. 知识网络系统

```
传统RAG：
文档1  文档2  文档3  文档4  文档5 (完全孤立) ❌
            
OmniAgent知识网络架构：
          [核心架构文档]
         /      |      \
    [API文档] [设计图] [代码库]
      /  \      |      /  \
[接口说明][测试用例][单元测试][集成测试]
      \    |    |    |    /
       [知识图谱自动关联]
```

**域索引组织**：
```
项目知识库
├─ 技术域
│  ├─ 架构文档
│  ├─ API文档
│  └─ 代码注释
├─ 业务域
│  ├─ 需求文档
│  ├─ 流程图
│  └─ 用户手册
└─ 测试域
   ├─ 测试用例
   ├─ 测试报告
   └─ BUG跟踪
```

#### 5. HOPE 自学习架构 ⭐

**HOPE = Hierarchical Omni-Agent Persistent Engine（分层智能持久化引擎）**

```
传统RAG：提问 → 检索 → 返回结果 (固定流程) ❌

OmniAgent HOPE 架构：
用户提问 → 问题分类 → 知识层级选择 → 智能检索
    ↓                                      ↓
反馈学习 ← 效果评估 ← 答案生成 ← 知识缺口检测
    ↓                                      ↓
策略优化 ← 模式识别 ← 补充知识 ← 自动改进
```

**三层知识结构**：

```
┌─────────────────────────────────────────┐
│  持久层 (Permanent Layer)               │
│  - 核心知识、系统文档、权威参考         │
│  - 长期稳定，手动管理                   │
├─────────────────────────────────────────┤
│  普通层 (Ordinary Layer)                │
│  - 一般性知识、业务文档、功能说明       │
│  - 动态更新，常规检索                   │
├─────────────────────────────────────────┤
│  高频层 (High Frequency Layer)          │
│  - 热点问题、最近问答、频繁访问知识     │
│  - 自动调整，优先检索                   │
└─────────────────────────────────────────┘
```

**自学习能力**：
- 🎓 **问题分类学习** - 基于关键词和模式自动分类
- 🔍 **知识缺口检测** - 发现知识库盲点
- 📈 **策略自动优化** - 根据访问频率动态调整层级
- 🔄 **持续改进** - 从每次交互中学习和优化

**详细文档**: [HOPE 系统设计文档](docs/core/HOPE_SYSTEM.md)

---

## 🎯 核心特性

### 1. 🤖 智能Agent构建
- ✅ 完整的对话历史管理
- ✅ 意图分析与理解
- ✅ 多轮对话支持
- ✅ 上下文自动保持
- ✅ 角色系统（支持多角色协作）

### 2. 📚 全面文档处理

**Office系列**：
- ✅ **Word** (.doc/.docx) - 表格转Markdown、样式保留
- ✅ **Excel** (.xls/.xlsx) - 公式计算、数据智能分段
- ✅ **PowerPoint** (.ppt/.pptx) - 幻灯片内容提取
- ✅ **PDF** - 逐页提取、页码标记、元数据

**所有文本格式**：
- ✅ 基础文本：.txt, .md, .log, .csv
- ✅ 配置文件：.yml, .json, .xml, .ini, .properties
- ✅ 编程语言：.java, .py, .js, .cpp, .go, .ts, .kt, .swift等
- ✅ **支持构建完整代码项目的独立知识库**

**高级功能**：
- ✅ **Vision LLM图片提取** - 使用AI理解图片内容（千问3-VL等）
- ✅ **OCR文字识别** - Tesseract光学识别
- ✅ **本地模型/Ollama/在线API** - 灵活选择提取方式

### 3. 🧠 先进RAG技术

**6种智能分块策略**：
- ✅ **困惑度智能分块** ⭐ 推荐 - AI驱动的语义边界识别
- ✅ **语义分块** - 基于向量相似度聚合
- ✅ **段落分块** - 保持自然段落结构
- ✅ **滑动窗口** - 重叠保留上下文
- ✅ **递归分块** - 层次化处理大文档
- ✅ **固定大小** - 兼容模式

**多维度向量化**：
- ✅ **ONNX本地模型** - bge-base-zh、bge-m3等
- ✅ **Ollama服务** - 本地部署，数据安全
- ✅ **在线向量API** - 千问、DeepSeek等
- ✅ **多套RAG并行** - 不同维度同时工作

### 4. 💾 异构冗余存储

**6种存储引擎**：
- ✅ **File** - 文件系统，零依赖
- ✅ **SQLite** - 嵌入式数据库，单文件
- ✅ **H2** - 内存数据库，高性能
- ✅ **MongoDB** - 文档数据库，灵活schema
- ✅ **Redis** - 缓存加速，毫秒响应
- ✅ **Elasticsearch** - 企业级搜索，亿级数据

**灾备保障**：
- ✅ 同一数据多存储备份
- ✅ 自动故障切换
- ✅ 数据一致性保证

### 5. 🕸️ 知识网络系统

**域索引**：
- ✅ 按领域分类组织知识
- ✅ 独立的向量空间
- ✅ 专业化检索策略
- ✅ 智能路由分发

**知识图谱**：
- ✅ 自动发现文档关联
- ✅ 引用关系追踪
- ✅ 语义相似度计算
- ✅ 智能推荐相关内容

**P2P知识共享**：
- ✅ 连接码机制
- ✅ 跨节点知识传递
- ✅ 分布式单体可用
- ✅ 企业内部知识网络

### 6. 🎓 HOPE自学习

- ✅ **问题分类** - 自动识别问题类型
- ✅ **知识缺口检测** - 发现知识盲点
- ✅ **策略自动优化** - 根据反馈改进
- ✅ **持续学习** - 从交互中进化
- ✅ **模式识别** - 发现常见问题模式

### 7. 🔄 工作流引擎

- ✅ 可视化流程设计
- ✅ 节点拖拽编排
- ✅ 条件分支控制
- ✅ 循环迭代支持
- ✅ 工作流市场（分享/导入）

### 8. 🌐 分布式架构

- ✅ P2P节点连接
- ✅ 去中心化设计
- ✅ 知识跨节点共享
- ✅ 单体可用保障
- ✅ 企业级部署支持

---

## 🏗️ 智能化全流程

### 完整的文档处理Pipeline

```
📄 文档上传
    ↓
📑 智能文本提取
    ├─ 本地模型提取
    ├─ Ollama服务提取  
    └─ 在线API提取（千问3-VL等）
    ↓
    支持格式：
    • Office: Word/Excel/PPT (.doc/.docx/.xls/.xlsx/.ppt/.pptx)
    • 文档: PDF
    • 文本: 所有文本格式（.txt/.md/.json/.xml/.log/.csv等）
    • 代码: 所有编程语言文件
    ↓
✂️ 智能分块
    ├─ 困惑度分块 (AI驱动) ⭐ 推荐
    ├─ 语义分块 (向量相似度)
    ├─ 段落分块 (自然段落)
    ├─ 滑动窗口 (重叠保留)
    ├─ 递归分块 (层次化)
    └─ 固定大小 (兼容模式)
    ↓
🔢 向量化
    ├─ ONNX本地模型 (bge-base-zh/bge-m3等)
    ├─ Ollama本地服务
    ├─ 在线向量API
    └─ 多套RAG系统并行支持
    ↓
💾 多元异构存储
    ├─ File (简单快速)
    ├─ SQLite (嵌入式)
    ├─ H2 (内存数据库)
    ├─ MongoDB (文档数据库)
    ├─ Redis (高速缓存)
    └─ Elasticsearch (企业搜索)
    ↓
🕸️ 知识网络构建
    ├─ 域索引组织
    ├─ 知识图谱自动构建
    ├─ 语义关联分析
    └─ P2P知识共享
    ↓
🤖 HOPE自学习
    ├─ 问题分类学习
    ├─ 知识缺口检测
    ├─ 策略自动优化
    └─ 持续改进机制
```

---

## 🚀 快速开始

### 三步启动

#### 1️⃣ 克隆项目

```bash
# GitHub
git clone https://github.com/jinhua10/omni-agent.git

# 或 Gitee（国内推荐，速度更快）
git clone https://gitee.com/gnnu/omni-agent.git

cd omni-agent
```

#### 2️⃣ 编译并启动后端

```bash
# 清理并编译项目
mvn clean package \
    -pl omni-agent-example-basic \
    -am \
    -DskipTests

# 启动后端服务（使用omni-agent-example-basic模块）
java -Dfile.encoding=UTF-8 \
     -Dsun.jnu.encoding=UTF-8 \
     -jar omni-agent-example-basic/target/omni-agent-example-basic-1.0.0.jar
```

**或使用启动脚本**：
```bash
# Windows
.\scripts\start.ps1

# Linux/Mac
chmod +x scripts/start.sh
./scripts/start.sh
```

#### 3️⃣ 启动前端

```bash
cd UI

# 安装依赖
npm install

# 启动开发服务器
npm run dev
```

访问 **http://localhost:3000** 开始使用！

### 📚 完整文档

- 📖 [快速开始指南](docs/core/QUICKSTART.md) - 详细的入门教程
- 🏗️ [完整系统架构](docs/core/ARCHITECTURE.md) - 包含 HOPE 系统的完整架构
- 🧠 [HOPE 自学习系统](docs/core/HOPE_SYSTEM.md) - 分层智能持久化引擎详解
- 🕸️ [知识网络架构](docs/core/KNOWLEDGE_NETWORK.md) - 知识图谱和域管理
- 📦 [模块架构详解](docs/core/MODULES.md) - 27个功能模块说明
- 📑 [文档索引](docs/core/README.md) - 所有文档的导航

### 🌐 在线演示

**官网地址**：[https://yumbo.top](https://yumbo.top)

立即访问在线演示，体验完整功能！

---

## 📦 系统架构

### 后端模块（27个）

```
omni-agent/
├─ omni-agent-core               # 核心基础模块（基础设施和工具）
├─ omni-agent-common             # 公共工具
├─ omni-agent-hope-api           # HOPE 接口定义（分类、持久化抽象）
├─ omni-agent-hope-starter       # HOPE 实现（问题分类器、HOPE 系统）
├─ omni-agent-orchestrator       # 服务编排层（查询服务、上下文管理）
├─ omni-agent-ai-api             # AI 服务抽象接口
├─ omni-agent-ai-starter         # AI 服务实现（Ollama/在线API/Vision LLM）
├─ omni-agent-rag-api            # RAG 抽象接口
├─ omni-agent-rag-starter-adapter # RAG 适配器（File/H2/SQLite/Redis/MongoDB/ES）
├─ omni-agent-chunking-api       # 分块策略接口
├─ omni-agent-chunking-starter   # 分块策略实现（6种）
├─ omni-agent-document-processor-api    # 文档处理接口
├─ omni-agent-document-processor-starter # 文档处理器实现
├─ omni-agent-document-storage-api      # 文档存储接口
├─ omni-agent-document-storage-starter  # 文档存储实现
├─ omni-agent-knowledge-registry-api    # 知识注册表接口
├─ omni-agent-knowledge-registry-starter # 知识网络实现
├─ omni-agent-ocr-starter-tesseract    # OCR 识别
├─ omni-agent-p2p-api            # P2P 接口
├─ omni-agent-p2p-starter        # P2P 实现
├─ omni-agent-workflow           # 工作流引擎
├─ omni-agent-marketplace        # 工作流市场
├─ omni-agent-web                # Web 接口层
├─ omni-agent-example-basic      # 基础示例（启动入口）
└─ omni-agent-example-production # 生产环境示例
```

**架构分层**：

```
应用层
├── omni-agent-web
├── omni-agent-example-basic
└── omni-agent-example-production
    ↓ 依赖
服务编排层
└── omni-agent-orchestrator
    ├── 查询服务 ✅
    ├── 上下文管理 ✅
    └── 只依赖 API 接口 ✅
    ↓ 依赖
Starter 实现层
├── omni-agent-hope-starter
│   ├── HOPE 系统实现 ✅
│   ├── 问题分类器 ✅
│   └── 依赖 Caffeine ✅
├── omni-agent-rag-starter-adapter
│   └── 依赖 Lucene ✅
└── omni-agent-document-processor-starter
    └── 依赖 POI, PDFBox, Tika ✅
    ↓ 依赖
API 接口层
├── omni-agent-hope-api
│   ├── HopePersistence 接口 ✅
│   ├── QuestionClassifier 接口 ✅
│   └── QuestionTypeConfig 模型 ✅
└── 其他 API 模块
    ↓ 依赖
核心层
└── omni-agent-core
    ├── 基础设施和工具 ✅
    ├── 不依赖具体实现库 ✅
    └── 职责清晰 ✅
```

**模块职责说明**：
- **omni-agent-core**: 核心基础模块，提供基础设施和工具类
- **omni-agent-hope-api**: HOPE 接口定义，包含问题分类、持久化等抽象接口
- **omni-agent-hope-starter**: HOPE 具体实现，包含问题分类器、HOPE 系统等
- **omni-agent-orchestrator**: 服务编排层，负责查询服务、上下文管理等业务编排

### 前端技术栈

- ⚛️ **React 18** - 现代UI框架
- 🎨 **Ant Design 5** - 企业级组件库
- 🎭 **Framer Motion** - 流畅动画
- 📊 **ECharts** - 数据可视化
- 🔄 **React Router** - 路由管理
- 🎨 **自定义主题引擎** - 多主题切换

---

## 💡 应用场景

### 1. 🏢 企业知识管理
- 内部文档智能检索
- 技术文档自动问答
- 项目知识沉淀
- 新人培训助手

### 2. 💻 开发辅助工具
- 代码库智能分析
- API文档自动生成
- 代码审查助手
- 项目架构分析

### 3. 🎓 教育培训
- 课程资料问答
- 学习进度跟踪
- 知识图谱构建
- 个性化学习路径

### 4. 🔬 研究助手
- 论文智能检索
- 文献关联分析
- 研究成果管理
- 知识发现

### 5. 🤖 智能客服
- 产品文档问答
- 常见问题解答
- 多轮对话支持
- 知识库管理

### 6. 📊 数据分析
- 报表自动生成
- 数据洞察发现
- 趋势分析预测
- 异常检测

---

## 🗺️ 开发路线图

### ✅ 已完成（v1.0.0）

- ✅ 核心架构设计
- ✅ 6种智能分块策略
- ✅ 6种存储引擎支持
- ✅ 多维度RAG系统
- ✅ 知识网络基础
- ✅ HOPE自学习框架
- ✅ 工作流引擎
- ✅ P2P知识共享
- ✅ Web管理界面
- ✅ Office文档全支持
- ✅ Vision LLM集成

### 🚧 进行中（v1.1.0）

- 🔄 知识图谱可视化
- 🔄 高级分析仪表盘
- 🔄 更多RAG策略
- 🔄 性能优化
- 🔄 Docker部署方案

### 📅 计划中（v2.0.0）

- 📋 多语言支持（Python SDK、Node.js SDK）
- 📋 云原生部署（K8s Operator）
- 📋 向量数据库优化
- 📋 更多AI模型集成
- 📋 企业级权限系统
- 📋 审计日志系统
- 📋 SaaS云服务版本

---

## 👥 贡献指南

我们欢迎所有形式的贡献！

### 贡献方式

1. 🐛 **提交Bug** - [Issue Tracker](https://github.com/jinhua10/omni-agent/issues)
2. 💡 **功能建议** - 提交Feature Request
3. 📝 **改进文档** - 文档永远不够完善
4. 🔧 **提交代码** - Pull Request欢迎

### 开发流程

```bash
# 1. Fork项目
# 2. 创建特性分支
git checkout -b feature/AmazingFeature

# 3. 提交更改
git commit -m 'Add some AmazingFeature'

# 4. 推送到分支
git push origin feature/AmazingFeature

# 5. 提交Pull Request
```

---

## 🔮 未来展望与规划

### 为什么开源？

我们坚信：**一套完整的企业级 AI 服务平台解决方案，应该让更多人了解和使用。**

经过我们的观察，**目前行业内没有一套真正完整的、从知识库到 AI 服务平台的解决方案**。市场上要么是：
- 🔸 传统知识库（只能查询，不能生成）
- 🔸 AI 工具（功能单一，无法协作）
- 🔸 Skill-based Agent（成本高，准确率不稳定）
- 🔸 商业闭源产品（价格昂贵，数据安全隐患）

**OmniAgent 的使命：**
```
让每个企业都能拥有自己的 AI 服务平台
让每个员工都能享受 AI 带来的生产力提升
让每个开发者都能基于此构建创新应用
```

### 📅 开发路线图

#### 近期规划（3-6 个月）

**1. Agent Skill 能力** ⭐
```
✨ 正在开发中

借鉴 Copilot 等主流方案的优势：
├─ Agent 可以调用外部工具和 API
├─ 支持自定义 Skill 插件
├─ Skill 市场（预置常用技能）
└─ 与知识网络深度融合（比 Copilot 更精准）

核心差异：
OmniAgent = Skill-based Agent + 知识网络
→ 既有 Skill 的灵活性，又有知识网络的准确性
```

**2. 多模态 AI 服务**
```
├─ 图片理解和生成（文档中的图表自动解析）
├─ 语音输入输出（语音问答、语音会议总结）
├─ 视频内容分析（培训视频自动提取要点）
└─ 跨模态检索（用文字找图片，用图片找文档）
```

**3. 企业级功能增强**
```
├─ 更强的权限体系（细粒度权限控制）
├─ 更好的监控运维（完整的可观测性）
├─ 更多的集成能力（钉钉、企微、飞书等）
└─ 更智能的推荐（主动推送相关知识）
```

#### 中期规划（6-12 个月）

**1. Agent 协作增强**
```
├─ Agent 工作流编排（可视化配置 Agent 协作流程）
├─ 跨企业 Agent 协作（企业间知识安全共享）
├─ Agent 能力市场（预置专业 Agent 模板）
└─ Agent 性能分析（每个 Agent 的贡献度评估）
```

**2. 知识图谱可视化**
```
├─ 企业知识地图（全局视角看知识分布）
├─ 知识关联分析（发现隐藏的知识关联）
├─ 知识缺口识别（主动发现知识盲区）
└─ 知识演化追踪（知识如何随时间变化）
```

**3. 行业解决方案**
```
├─ 金融行业版（合规、风控、投研）
├─ 医疗行业版（病历、诊断、科研）
├─ 制造业版（工艺、质检、供应链）
└─ 更多行业...（根据社区需求）
```

#### 长期愿景

```
让 OmniAgent 成为：
✨ 企业 AI 服务的基础设施
✨ Agent 协作的操作系统
✨ 知识管理的行业标准
✨ 开发者的创新平台
```

### 目前的挑战

**坦诚地说，我们还有很多事情要做：**

⚠️ **功能层面**
- 部分高级功能还在开发中（如 Agent Skill）
- 某些场景的准确率还需要优化
- UI/UX 体验还可以更好

⚠️ **生态层面**
- 需要更多的使用案例和最佳实践
- 需要更完善的文档和教程
- 需要更活跃的开发者社区

⚠️ **商业层面**
- 企业级服务体系还在建设中
- 合作伙伴网络正在拓展
- 行业解决方案需要深化

**但我们相信：**
- ✅ 方向是对的（从知识库到 AI 服务平台）
- ✅ 架构是扎实的（完整的数据闭环）
- ✅ 社区会壮大的（一起共建生态）

---

## 🌟 支持项目

### ⭐ 给我们一个 Star

**如果你认可这个项目的理念和方向，欢迎给 OmniAgent 点个 Star！**

你的每一个 Star 都是对我们最大的鼓励，能够：
- 💪 激励团队持续开发和优化
- 📢 让更多人发现这个项目
- 🚀 推动企业 AI 服务平台的发展
- 🌱 促进开源社区生态建设

[![GitHub stars](https://img.shields.io/github/stars/jinhua10/omni-agent?style=social)](https://github.com/jinhua10/omni-agent/stargazers)

### 🤝 参与社区共建

我们诚挚邀请你参与到 OmniAgent 的建设中来：

**如果你是开发者 👨‍💻**
- 💡 提出需求和建议（[Issues](https://github.com/jinhua10/omni-agent/issues)）
- 🐛 报告 Bug 和问题（[Issues](https://github.com/jinhua10/omni-agent/issues)）
- 📝 贡献代码和功能（[Pull Requests](https://github.com/jinhua10/omni-agent/pulls)）
- 🔧 开发插件和扩展（基于框架）
- 我们提供：**技术指导、代码审查、荣誉认可**

**如果你是企业用户 🏢**
- 📊 分享使用经验和案例（[Discussions](https://github.com/jinhua10/omni-agent/discussions)）
- 💬 提出业务需求和建议（[Issues](https://github.com/jinhua10/omni-agent/issues)）
- 🤝 成为早期客户，共建行业方案
- 我们提供：**优先支持、定制开发、商业合作**

**如果你是内容贡献者 📚**
- 📖 完善项目文档和教程
- 🎬 制作视频教程和演示
- 📢 撰写博客文章和案例分析
- 🌍 翻译文档到其他语言
- 我们提供：**平台曝光、社区荣誉、物质奖励**

**如果你是产品设计师 💡**
- 🎨 优化界面设计和用户体验
- 🖼️ 设计产品图标和视觉元素
- 📱 提供交互设计建议
- 我们提供：**设计自由、作品展示、团队协作**

### 💬 加入社区

**多种方式参与讨论和交流：**

- 💬 [GitHub Discussions](https://github.com/jinhua10/omni-agent/discussions) - 功能讨论、问题求助
- 🐛 [GitHub Issues](https://github.com/jinhua10/omni-agent/issues) - Bug反馈、功能建议
- 📧 邮件联系：1015770492@qq.com
- 📝 CSDN博客：[https://yumbo.blog.csdn.net/](https://yumbo.blog.csdn.net/)

### 🎁 贡献者福利

感谢每一位贡献者！我们为活跃贡献者提供：

- 🏆 **荣誉认可**：贡献者名单展示、特别感谢
- 📊 **优先体验**：新功能优先测试和反馈
- 🎓 **技术成长**：参与核心技术讨论和决策
- 🤝 **职业机会**：优秀贡献者可获得工作推荐
- 🎁 **实物奖励**：重大贡献者可获得纪念品

**一起让 OmniAgent 变得更好！🚀**

---

## 📄 许可证

本项目采用 **Apache License 2.0** 开源协议。

详见 [LICENSE.txt](LICENSE.txt) 文件。

---

## 🙏 致谢

感谢以下开源项目：

- [Spring Boot](https://spring.io/projects/spring-boot) - 应用框架
- [Apache Lucene](https://lucene.apache.org/) - 全文检索
- [ONNX Runtime](https://onnxruntime.ai/) - 模型推理
- [React](https://react.dev/) - 前端框架
- [Ant Design](https://ant.design/) - UI组件库
- [Ollama](https://ollama.ai/) - 本地LLM服务

---

## 📞 联系方式

- 📧 **邮箱**: 1015770492@qq.com
- 💬 **CSDN博客**: [https://yumbo.blog.csdn.net/](https://yumbo.blog.csdn.net/)
- 🐙 **GitHub**: [https://github.com/jinhua10](https://github.com/jinhua10)
- 🦊 **Gitee**: [https://gitee.com/gnnu](https://gitee.com/gnnu)
- 🌐 **官网**: [https://yumbo.top](https://yumbo.top)

---

## ⭐ Star History

如果这个项目对您有帮助，请给我们一个Star！⭐

[![Star History Chart](https://api.star-history.com/svg?repos=jinhua10/omni-agent&type=Date)](https://star-history.com/#jinhua10/omni-agent&Date)

---

## 💝 联系我们 & 赞助支持

<div align="center">

<table>
<tr>
<td align="center">
  <h3>📱 联系方式</h3>
  <img src="UI/src/assets/images/Connect Me.png" alt="联系我二维码" width="200"/>
  <p><b>扫码添加微信<br/>加入技术交流群</b></p>
</td>
<td align="center">
  <h3>☕ 赞助支持</h3>
  <img src="UI/src/assets/images/Payment QR Code.png" alt="赞助二维码" width="200"/>
  <p><b>请我喝杯咖啡 ☕<br/>您的支持是我们持续开发的动力！💪</b></p>
</td>
</tr>
</table>

</div>

---

<div align="center">

**让Agent遍地开花，Agent元年正式启动！**

**OmniAgent - 构建下一代智能Agent应用**

[🌐 官网](https://yumbo.top) • [📖 核心文档](docs/core/) • [🐛 反馈](https://github.com/jinhua10/omni-agent/issues) • [💬 讨论](https://github.com/jinhua10/omni-agent/discussions)

Made with ❤️ by OmniAgent Team

</div>

