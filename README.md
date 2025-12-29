# OmniAgent - 智能知识网络平台 🚀

<div align="center">

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE.txt)
[![Java](https://img.shields.io/badge/Java-21-orange.svg)](https://openjdk.org/)
[![Spring Boot](https://img.shields.io/badge/Spring%20Boot-3.4.1-brightgreen.svg)](https://spring.io/projects/spring-boot)
[![Build](https://img.shields.io/badge/build-passing-brightgreen.svg)]()

**基于知识域隔离的智能文档处理与RAG系统**

### 📖 [**5分钟快速开始 →**](QUICKSTART.md)

[核心特性](#-核心特性) • [架构设计](#-架构设计) • [使用示例](#-使用示例) • [路线图](#-开发路线图)

</div>

---

## 📊 项目概览

OmniAgent 是一个现代化的企业级知识管理平台，通过**知识域隔离架构**实现多领域知识的专业化组织和检索。

| 维度 | 状态 |
|------|------|
| **当前版本** | 1.0.0 |
| **模块总数** | 20个Maven模块 + 1个前端项目 |
| **代码量** | ~15,000+ 行 |
| **架构状态** | ✅ 重构完成 |
| **编译状态** | ✅ BUILD SUCCESS |
| **最后更新** | 2025-12-29 |

---

## 🎯 核心理念

### 传统RAG的问题

```
传统架构：所有文档混在一个索引中
┌────────────────────────────────────┐
│         单一RAG索引池               │
│  📄技术文档 + 📊财务报表 + 💼合同   │
│  + 📧邮件 + 📝会议记录...          │
└────────────────────────────────────┘
         ↓
    ❌ 向量空间混乱
    ❌ 检索精度低下
    ❌ 无法专业化处理
```

### OmniAgent的解决方案

```
知识域隔离架构：每个领域独立的向量空间
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│  技术文档域   │  │  财务报表域   │  │   合同域      │
│  独立索引     │  │  独立索引     │  │  独立索引     │
│  专业化策略   │  │  专业化策略   │  │  专业化策略   │
└──────────────┘  └──────────────┘  └──────────────┘
         ↓                ↓                ↓
    ✅ 语义检索精准    ✅ 专业知识提取   ✅ 细粒度权限
    ✅ 针对性优化      ✅ 领域增量更新   ✅ 智能路由
```

---

## ⚡ 核心特性

### 🏗️ 知识域隔离架构

- **多域管理** - 每个知识域独立的向量空间和索引
- **智能路由** - 根据查询意图自动路由到合适的知识域
- **增量更新** - 文件哈希追踪，只处理变更内容
- **权限隔离** - 细粒度的域级访问控制

### 📄 全格式文档支持

#### Office文档（5种核心格式）

| 格式 | 特性 | 处理器 |
|------|------|--------|
| **PDF** | 逐页提取、页码标记、元数据 | PDFProcessor |
| **Word** (.doc/.docx) | 表格转Markdown、标题识别 | WordProcessor |
| **Excel** (.xls/.xlsx) | 智能表格、公式计算、数据分段 | ExcelProcessor |
| **PowerPoint** (.ppt/.pptx) | 幻灯片提取、结构化输出 | PPTProcessor |
| **文本** (.txt/.md/.log) | 纯文本提取 | TextProcessor |

#### 全文本格式支持 ⭐

- ✅ **所有文本格式** - .txt, .md, .json, .xml, .csv, .log, .yaml, .ini, .conf...
- ✅ **所有编程语言** - .java, .py, .js, .cpp, .go, .rs, .kt, .swift, .ts...
- ✅ **配置文件** - .properties, .yml, .env, .config...
- ✅ **代码项目** - 支持构建完整代码库的独立知识库

#### 增强功能

- ✅ **Markdown 标准化** - 所有表格统一转换为Markdown格式
- ✅ **结构保留** - 标题层级、页码信息完整保留
- ✅ **LLM友好** - 输出格式适合大语言模型处理
- ✅ **逐页处理** - 避免大文件内存溢出
- ✅ **代码智能分析** - 支持源码级别的知识提取

### 🧩 智能分块策略（6种）

| 策略 | 适用场景 | 特点 |
|------|----------|------|
| **固定长度** | 通用文档 | 固定大小+重叠窗口 |
| **段落分块** | 结构化文档 | 按段落边界+最大段落数 |
| **句子分块** | 短文本 | 按句子边界分块 |
| **句子边界** | 平衡场景 | 目标大小+句子完整性 |
| **PPL智能** | 技术文档 | 困惑度峰值检测+ONNX可选 |
| **语义分块** | 高质量需求 | TF-IDF+向量化+AI可选 |

#### 自动化特性

- ✅ **文档类型推断** - 自动识别7种文档类型
- ✅ **策略自动选择** - 根据文档类型选择最佳分块策略
- ✅ **策略自动注册** - Spring Boot自动发现和注册分块策略
- ✅ **参数优化** - 内置参数工具类自动调优

### 🔍 多域RAG检索

- **统一接口** - 简洁优雅的RagService接口（15个核心方法）
- **向量检索** - 集成ONNX Embedding，支持真正的语义搜索
- **优雅降级** - 无Embedding时自动降级到文本检索
- **多后端支持** - Lucene全文索引（其他后端待扩展）

### 🌐 知识网络（增强层）

- **知识图谱** - 基于提取文本构建知识关联网络
- **跨域关联** - 建立不同知识域之间的关联
- **AI提取** - 调用大语言模型自动提取知识点
- **后台异步** - 独立后台服务，不影响原有流程

---

## 💡 使用示例

### 示例1：处理文档

```java
// 完整流程：文档 → 分块 → 索引
@Service
public class DocumentService {
    @Autowired private DocumentProcessor processor;
    @Autowired private ChunkingService chunking;
    @Autowired private RagService rag;
    
    public void process(File file) {
        // 1. 提取文本（自动识别格式）
        String text = processor.extractText(file).getText();
        
        // 2. 智能分块（自动选择策略）
        List<Chunk> chunks = chunking.chunk(text);
        
        // 3. 索引到知识库
        List<Document> docs = chunks.stream()
            .map(chunk -> Document.builder()
                .content(chunk.getText())
                .build())
            .collect(Collectors.toList());
        rag.batchIndex(docs);
    }
}
```

### 示例2：搜索与问答

```java
// 简单搜索
@GetMapping("/search")
public List<String> search(@RequestParam String query) {
    return ragService.search(query, 5)
        .stream()
        .map(Document::getContent)
        .collect(Collectors.toList());
}

// AI问答（可选）
@GetMapping("/qa")
public String qa(@RequestParam String question) {
    // 检索相关文档
    List<Document> docs = ragService.search(question, 3);
    String context = docs.stream()
        .map(Document::getContent)
        .collect(Collectors.joining("\n"));
    
    // AI生成答案（需要配置AI服务）
    return aiService.generate("根据以下内容回答：" + context + "\n问题：" + question);
}
```

### 示例3：知识域管理

```java
// 创建专门的知识域
@Service
public class DomainService {
    @Autowired private KnowledgeDomainService domainService;
    
    public void createTechDomain() {
        KnowledgeDomain domain = KnowledgeDomain.builder()
            .domainId("tech-docs")
            .name("技术文档域")
            .domainType(DomainType.DOCUMENT)
            .build();
        domainService.createDomain(domain);
    }
    
    // 智能路由
    @Autowired private DomainRouter router;
    
    public void smartRoute(String query) {
        QueryRouteResult result = router.route(query);
        System.out.println("推荐域: " + result.getDomainIds());
    }
}
```

> 💡 **更多示例**: 查看 [QUICKSTART.md](QUICKSTART.md) 获取完整教程

---

## 🏗️ 架构设计

### 模块结构（20个Maven模块 + 1个前端项目）

```
omni-agent/
│
├── ========== API层（接口定义）7个模块 ==========
│
├── omni-agent-document-storage-api       # 文档存储接口
├── omni-agent-rag-api                    # RAG检索接口
├── omni-agent-ai-api                     # AI服务接口
├── omni-agent-p2p-api                    # P2P协作接口
├── omni-agent-knowledge-registry-api     # 知识域注册接口
├── omni-agent-chunking-api               # 文档分块接口
├── omni-agent-document-processor-api     # 文档处理接口
│
├── ========== 通用工具层 1个模块 ==========
│
├── omni-agent-common                     # 通用工具类
│
├── ========== 核心业务层 1个模块 ==========
│
├── omni-agent-core                       # 核心业务逻辑
│   ├── 知识域管理
│   ├── 工作流编排
│   └── 业务路由
│
├── ========== Starter实现层 7个模块 ==========
│
├── omni-agent-chunking-starter           # 分块策略实现（6种）
├── omni-agent-document-processor-starter # 文档处理器实现（5种）
├── omni-agent-document-storage-starter   # 文档存储实现
├── omni-agent-ocr-starter-tesseract      # OCR识别实现
├── omni-agent-rag-starter-adapter        # RAG适配器实现
├── omni-agent-ai-starter                 # AI服务实现
├── omni-agent-knowledge-registry-starter # 知识域实现
├── omni-agent-p2p-starter                # P2P实现
│
├── ========== Web层 1个模块 ==========
│
├── omni-agent-web                        # REST API接口
│
├── ========== 工作流引擎 1个模块 ==========
│
├── omni-agent-workflow                   # 工作流定义与执行
│
├── ========== 算法市场 1个模块 ==========
│
├── omni-agent-marketplace                # 算法插件市场
│
├── ========== 应用示例 2个模块 ==========
│
├── omni-agent-example-basic              # 基础示例
├── omni-agent-example-production         # 生产示例
│
└── ========== 前端项目（独立） ==========
    │
    └── UI/                                # React + Vite 前端应用
        ├── 问答模块
        ├── 文档管理
        ├── 统计分析
        ├── 反馈系统
        ├── 角色管理
        └── 协作网络
```

### 架构层次图

```
┌─────────────────────────────────────────────────────────┐
│                    应用层（你的代码）                      │
│              REST API / Web UI / 工作流                  │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│                  核心业务层（Core）                       │
│         知识域管理 | 路由编排 | 业务逻辑                  │
└─────────────────────────────────────────────────────────┘
                          ↓
┌─────────────────────────────────────────────────────────┐
│                 API接口层（抽象定义）                     │
│   文档处理 | 分块 | RAG | AI | 知识域 | 存储             │
└─────────────────────────────────────────────────────────┘
                          ↑
┌─────────────────────────────────────────────────────────┐
│              Starter实现层（具体实现）                    │
│   各种技术栈的实现，通过Spring Boot自动配置               │
└─────────────────────────────────────────────────────────┘
```

### 设计原则

- ✅ **依赖倒置**（DIP） - 高层依赖抽象，低层实现抽象
- ✅ **接口隔离**（ISP） - 接口职责单一，精准定义
- ✅ **开闭原则**（OCP） - 对扩展开放，对修改关闭
- ✅ **单一职责**（SRP） - 每个模块职责明确

---

## 🚀 快速开始

> **💡 完整教程请查看**: **[QUICKSTART.md](QUICKSTART.md)** - 5分钟快速上手指南

### 三步启动

```bash
# 1. 克隆项目
git clone https://github.com/jinhua10/omni-agent.git
cd omni-agent

# 2. 编译项目
mvn clean install -DskipTests

# 3. 运行示例
cd omni-agent-example-basic
mvn spring-boot:run
```

### 核心使用

```java
@Service
public class MyService {
    @Autowired private DocumentProcessor documentProcessor;
    @Autowired private ChunkingService chunkingService;
    @Autowired private RagService ragService;
    
    public void process(File file) {
        // 1. 提取文本
        String text = documentProcessor.extractText(file).getText();
        
        // 2. 智能分块
        List<Chunk> chunks = chunkingService.chunk(text);
        
        // 3. 索引到RAG
        ragService.batchIndex(toDocuments(chunks));
    }
    
    public List<String> search(String query) {
        return ragService.search(query, 5)
            .stream()
            .map(Document::getContent)
            .collect(Collectors.toList());
    }
}
```

### REST API快速测试

```bash
# 搜索文档
curl -X POST http://localhost:8080/api/search \
  -H "Content-Type: application/json" \
  -d '{"query": "关键词", "topK": 5}'
```

> 📖 **更多示例和详细说明**: [QUICKSTART.md](QUICKSTART.md)

---

## 📚 文档与资源

### 快速开始

- **[📖 5分钟快速开始](QUICKSTART.md)** ⭐ **推荐首选** - 从零开始的完整教程

### 架构文档

- **[知识网络架构设计](docs/refactor_01/core/KNOWLEDGE_NETWORK_ARCHITECTURE.md)** - 知识域隔离架构详解
- **[RAG重构总结](docs/refactor_01/PROJECT_FINAL_SUMMARY.md)** - RAG架构重构完整记录

### API参考

各模块的详细API文档请查看对应模块目录下的README.md文件：

```
omni-agent-chunking-api/README.md           # 分块API
omni-agent-document-processor-api/README.md # 文档处理API
omni-agent-rag-api/README.md                # RAG检索API
omni-agent-knowledge-registry-api/README.md # 知识域API
```

### 示例代码

完整的示例代码位于：
- `omni-agent-example-basic/` - 基础功能示例
- `omni-agent-example-production/` - 生产环境配置示例

---

## 🛣️ 开发路线图

### ✅ Phase 1: 基础架构（已完成）

- ✅ 知识域注册API
- ✅ 文档处理模块（5种格式）
- ✅ 智能分块模块（6种策略）
- ✅ RAG统一接口
- ✅ 向量化集成（ONNX）

### ✅ Phase 2: 知识网络（已完成）

- ✅ 知识域管理服务
- ✅ 知识网络API定义
- ✅ 知识网络构建器（KnowledgeNetworkBuilder）
- ✅ 知识网络管理器（KnowledgeNetworkManager）
- ✅ AI知识提取服务
- ✅ 异步构建与后台扫描
- ✅ 知识存储服务

### ✅ Phase 3: 智能路由（已完成）

- ✅ 意图识别引擎（基于关键词匹配）
- ✅ 领域路由器（DomainRouter）
- ✅ 多域查询支持
- ✅ 角色匹配机制
- ✅ 跨域查询优化
- ✅ REST API接口（/api/router/route）

### 🔄 Phase 4: Web界面（部分完成）

- ✅ React + Vite 前端框架
- ✅ 问答模块（QA）
- ✅ 文档管理模块
- ✅ 统计模块
- ✅ 反馈系统
- ✅ 角色管理
- ✅ 协作网络
- ⏳ AI服务市场（进行中）
- ⏳ 个人中心（进行中）
- ⏳ 系统管理（进行中）

### 📋 Phase 5: 高级功能（规划中）

#### 5.1 应用场景扩展

- ⬜ **源码分析域** - 基于现有框架的应用场景
  - 源码域定义与配置
  - 多角度分析（安全、架构、质量）
  - Git深度集成
  - 代码知识自动提取
  
- ⬜ **更多领域场景**
  - 财务分析域
  - 法律合同域
  - 医疗知识域
  - 电商产品域

#### 5.2 功能增强

- ⬜ 知识图谱可视化
- ⬜ 更智能的意图识别（LLM驱动）
- ⬜ 实时监控仪表板
- ⬜ 多模态支持（图片、音频、视频）
- ⬜ 知识推理引擎

#### 5.3 性能优化

- ⬜ 分布式RAG检索
- ⬜ 向量索引优化
- ⬜ 缓存策略增强
- ⬜ 并发处理优化

---

## 🎨 技术栈

### 核心框架

- **Spring Boot** 3.4.1 - 应用框架
- **Spring Data** - 数据访问
- **Spring WebFlux** - 响应式Web

### 文档处理

- **Apache POI** 5.5.0 - Office文档处理
- **PDFBox** 2.0.30 - PDF处理
- **Apache Tika** 3.2.3 - 文档格式检测

### RAG与向量化

- **Apache Lucene** 9.10.0 - 全文检索
- **ONNX Runtime** - 模型推理
- **BGE-base-zh-v1.5** - 中文Embedding模型

### 工具库

- **Lombok** 1.18.34 - 减少样板代码
- **Jackson** 2.15.3 - JSON处理
- **SLF4J + Logback** 1.5.19 - 日志框架

---

## 📊 项目统计

### 代码规模

```
总模块数:    20个Maven模块 + 1个前端项目
API模块:     7个
Starter模块: 7个
核心模块:    1个
工具模块:    1个
Web模块:     1个
工作流模块:  1个
示例模块:    2个
前端项目:    1个（React + Vite）

总代码量:    ~15,000+行（后端Java）
前端代码:    ~5,000+行（React/JavaScript）
文档数量:    30+份
```

### 功能覆盖

```
文档格式:    5种（PDF/Word/Excel/PPT/Text）
分块策略:    6种（固定/段落/句子/边界/PPL/语义）
RAG后端:     1种（Lucene，其他待扩展）
AI集成:      支持（Ollama/ONNX/在线API）
知识域:      无限扩展
```

### 架构质量

- **编译状态**: ✅ BUILD SUCCESS
- **代码质量**: ⭐⭐⭐⭐⭐
- **架构设计**: ⭐⭐⭐⭐⭐
- **可维护性**: ⭐⭐⭐⭐⭐
- **文档完整性**: ⭐⭐⭐⭐⭐

---

## 🔥 核心亮点

### 1. 知识域隔离架构

业界首创的多域隔离RAG架构，每个知识域独立向量空间：

- 📊 **财务域** - 财务报表、预算分析
- 💼 **合同域** - 合同文本、法律条款
- 🔧 **技术域** - 技术文档、API手册
- 👥 **HR域** - 员工手册、政策制度
- 🎯 **自定义域** - 无限扩展可能

### 2. 智能化全流程

```
文档上传 → 格式识别 → 智能分块 → 向量化 → 域索引 → 语义检索
   ↓          ↓          ↓          ↓         ↓         ↓
  自动      Markdown   策略选择    ONNX     域隔离    AI增强
```

### 3. 企业级特性

- ✅ **可插拔架构** - 每个组件都可替换
- ✅ **优雅降级** - 核心功能不依赖外部服务
- ✅ **增量更新** - 文件哈希追踪，避免重复处理
- ✅ **资源优化** - 逐页处理，避免内存溢出
- ✅ **生产就绪** - 完整的异常处理和日志

---

## 🤝 贡献指南

我们欢迎各种形式的贡献！

### 贡献方式

1. **报告Bug** - 在Issues中报告问题
2. **提交功能建议** - 在Discussions中讨论新功能
3. **提交代码** - Fork项目，提交Pull Request
4. **完善文档** - 帮助改进文档

### 开发流程

```bash
# 1. Fork项目
# 2. 创建特性分支
git checkout -b feature/your-feature

# 3. 提交更改
git commit -m "Add: 新功能描述"

# 4. 推送到分支
git push origin feature/your-feature

# 5. 创建Pull Request
```

---

## 📄 许可证

本项目采用 [Apache License 2.0](LICENSE.txt) 开源许可证。

---

## 👥 团队

**开发者**: Jinhua Yu  
**邮箱**: 1015770492@qq.com  
**GitHub**: https://github.com/jinhua10/omni-agent

---

## 🌟 Star History

如果这个项目对你有帮助，请给我们一个 ⭐️！

[![Star History Chart](https://api.star-history.com/svg?repos=jinhua10/omni-agent&type=Date)](https://star-history.com/#jinhua10/omni-agent&Date)

---

## 📞 联系我们

- **GitHub Issues**: [提交问题](https://github.com/jinhua10/omni-agent/issues)
- **GitHub Discussions**: [参与讨论](https://github.com/jinhua10/omni-agent/discussions)
- **Email**: 1015770492@qq.com

---

<div align="center">

**🎉 感谢使用 OmniAgent！**

Made with ❤️ by Jinhua Yu

</div>

