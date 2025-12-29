# OmniAgent - 全场景智能Agent框架 🚀

<div align="center">

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE.txt)
[![Java](https://img.shields.io/badge/Java-21-orange.svg)](https://openjdk.org/)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-3.4.1-brightgreen.svg)](https://spring.io/projects/spring-boot)
[![Build](https://img.shields.io/badge/build-passing-brightgreen.svg)]()

**让AI更智能、更可控、更实用的全场景Agent开发框架**

### 🌐 [**官网演示 →**](https://yumbo.top/) | 📖 [**快速开始**](#-快速开始) | 🎯 [**核心特性**](#-核心特性)

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
| **后端模块** | 22 个功能模块 |
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

#### 5. HOPE自学习架构

```
传统RAG：提问 → 检索 → 返回结果 (固定流程) ❌

OmniAgent HOPE架构：
用户提问 → 意图分析 → 问题分类 → 知识检索
    ↓                                      ↓
反馈学习 ← 效果评估 ← 答案生成 ← 知识缺口检测
    ↓                                      ↓
策略优化 ← 模式识别 ← 补充知识 ← 自动改进
```

**自学习能力**：
- 🎓 **问题分类学习** - 自动识别问题类型模式
- 🔍 **知识缺口检测** - 发现知识库盲点
- 📈 **策略自动优化** - 根据效果调整检索策略
- 🔄 **持续改进** - 从每次交互中学习

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

### 🌐 在线演示

**官网地址**：[https://yumbo.top](https://yumbo.top)

立即访问在线演示，体验完整功能！

---

## 📦 系统架构

### 后端模块（22个）

```
omni-agent/
├─ omni-agent-core               # 核心模块（HOPE/查询/分类）
├─ omni-agent-common             # 公共工具
├─ omni-agent-ai-api             # AI服务抽象接口
├─ omni-agent-ai-starter         # AI服务实现（Ollama/在线API/Vision LLM）
├─ omni-agent-rag-api            # RAG抽象接口
├─ omni-agent-rag-starter-adapter # RAG适配器（File/H2/SQLite/Redis/MongoDB/ES）
├─ omni-agent-chunking-api       # 分块策略接口
├─ omni-agent-chunking-starter   # 分块策略实现（6种）
├─ omni-agent-document-processor-api    # 文档处理接口
├─ omni-agent-document-processor-starter # 文档处理器实现
├─ omni-agent-document-storage-api      # 文档存储接口
├─ omni-agent-document-storage-starter  # 文档存储实现
├─ omni-agent-knowledge-registry-api    # 知识注册表接口
├─ omni-agent-knowledge-registry-starter # 知识网络实现
├─ omni-agent-ocr-starter-tesseract    # OCR识别
├─ omni-agent-p2p-api            # P2P接口
├─ omni-agent-p2p-starter        # P2P实现
├─ omni-agent-workflow           # 工作流引擎
├─ omni-agent-marketplace        # 工作流市场
├─ omni-agent-web                # Web接口层
├─ omni-agent-example-basic      # 基础示例（启动入口）
└─ omni-agent-example-production # 生产环境示例
```

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

<div align="center">

**让Agent遍地开花，Agent元年正式启动！**

**OmniAgent - 构建下一代智能Agent应用**

[🌐 官网](https://yumbo.top) • [📖 文档](docs/) • [🐛 反馈](https://github.com/jinhua10/omni-agent/issues) • [💬 讨论](https://github.com/jinhua10/omni-agent/discussions)

Made with ❤️ by OmniAgent Team

</div>

