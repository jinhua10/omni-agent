# Omni-Agent 知识网络架构重构方案

> **文档创建时间：** 2025-12-27  
> **目标：** 构建专业化的知识网络系统，支持多领域独立知识库和源码深度分析  
> **作者：** 系统架构设计

---

## 📋 目录

1. [当前系统问题分析](#当前系统问题分析)
2. [新架构设计](#新架构设计)
3. [核心概念：知识网络](#核心概念知识网络)
4. [技术实现方案](#技术实现方案)
5. [源码分析专项功能](#源码分析专项功能)
6. [增量更新机制](#增量更新机制)
7. [实施路线图](#实施路线图)

---

## 当前系统问题分析

### 🔴 核心问题

#### 1. 向量混乱问题
```
当前架构：
┌──────────────────────────────────────┐
│      Single RAG Index                │
├──────────────────────────────────────┤
│  • 所有文档混在一起                   │
│  • 向量空间不隔离                     │
│  • 语义检索不专业                     │
│  • 知识召回不精准                     │
└──────────────────────────────────────┘

问题示例：
用户问："分析这个Java项目的安全漏洞"
系统可能召回：
- ❌ Python项目的配置文件
- ❌ 用户上传的技术文档
- ❌ 无关的代码片段
```

#### 2. 知识库无专业分类
```
data/
├── storage/
│   ├── chunks/         ← 所有文档的分块混在一起
│   ├── documents/      ← 所有文档混在一起
│   └── extracted/      ← 所有提取内容混在一起
└── rag-index/          ← 单一Lucene索引
```

#### 3. 缺少增量更新机制
- 文件变更后需要重新处理整个项目
- 没有版本控制和变更追踪
- 资源浪费严重

---

## 新架构设计

### 🎯 设计目标

1. **知识隔离**：每个知识库独立的向量空间
2. **专业分类**：按领域/角色/项目组织知识
3. **智能路由**：根据查询意图路由到专业知识库
4. **增量更新**：只处理变更的文件
5. **可扩展性**：支持多种知识源（文档、源码、API文档等）

### 🏗️ 整体架构

```
┌─────────────────────────────────────────────────────────────────────┐
│                         Omni-Agent Core                             │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌───────────────────────────────────────────────────────────────┐ │
│  │             Knowledge Network Manager                         │ │
│  │  (知识网络管理器 - 总控制中心)                                  │ │
│  └───────────┬──────────────────────────────────────────────────┘ │
│              │                                                     │
│              ├─────────────┬─────────────┬──────────────┐         │
│              ▼             ▼             ▼              ▼         │
│  ┌───────────────┐ ┌──────────────┐ ┌──────────────┐ ┌────────┐ │
│  │  Knowledge    │ │  Knowledge   │ │  Knowledge   │ │  ...   │ │
│  │  Domain 1     │ │  Domain 2    │ │  Domain 3    │ │        │ │
│  │  (文档知识库) │ │ (源码知识库) │ │ (角色知识库) │ │        │ │
│  └───────┬───────┘ └──────┬───────┘ └──────┬───────┘ └────────┘ │
│          │                │                │                      │
│  ┌───────▼────────────────▼────────────────▼────────────────────┐ │
│  │              Domain Router (领域路由器)                       │ │
│  │  • 意图识别                                                   │ │
│  │  • 领域匹配                                                   │ │
│  │  • 跨域查询                                                   │ │
│  └───────────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────────────┘
```

### 📊 数据组织结构

```
data/
├── knowledge-network/              # 知识网络根目录
│   ├── domains/                    # 知识域目录
│   │   ├── domain-1-docs/          # 文档知识域
│   │   │   ├── metadata.json       # 域元数据
│   │   │   ├── rag-index/          # 独立的RAG索引
│   │   │   ├── storage/            # 独立的存储空间
│   │   │   │   ├── documents/
│   │   │   │   ├── chunks/
│   │   │   │   └── extracted/
│   │   │   └── config.json         # 域配置
│   │   │
│   │   ├── domain-2-source-code/   # 源码知识域
│   │   │   ├── metadata.json
│   │   │   ├── projects/           # 项目列表
│   │   │   │   ├── project-1/      # 项目1
│   │   │   │   │   ├── git-sync/   # Git同步信息
│   │   │   │   │   ├── rag-index/  # 项目的RAG索引
│   │   │   │   │   ├── analysis/   # 分析结果
│   │   │   │   │   │   ├── security/      # 安全分析
│   │   │   │   │   │   ├── architecture/  # 架构分析
│   │   │   │   │   │   ├── quality/       # 代码质量
│   │   │   │   │   │   └── dependency/    # 依赖分析
│   │   │   │   │   └── incremental/       # 增量追踪
│   │   │   │   │       ├── file-hashes.json
│   │   │   │   │       └── change-log.json
│   │   │   │   └── project-2/
│   │   │   └── config.json
│   │   │
│   │   └── domain-3-role-kb/       # 角色知识库域
│   │       ├── metadata.json
│   │       ├── roles/              # 角色列表
│   │       │   ├── security-analyst/   # 安全分析师角色
│   │       │   │   ├── profile.json    # 角色档案
│   │       │   │   ├── responsibilities.md  # 职责说明
│   │       │   │   ├── rag-index/      # 角色的知识索引
│   │       │   │   ├── learned-knowledge/  # 学习到的知识
│   │       │   │   └── reports/        # 生成的报告
│   │       │   └── architect/          # 架构师角色
│   │       └── config.json
│   │
│   ├── network-config.json         # 网络配置
│   └── routing-rules.json          # 路由规则
│
├── omni-agent.db                   # 系统数据库
└── workflows/                       # 工作流定义
```

---

## 核心概念：知识网络

### 1. 知识域 (Knowledge Domain)

**定义：** 一个独立的、专业化的知识空间

**特性：**
- 独立的向量索引
- 独立的存储空间
- 专属的配置和策略
- 可以是：文档库、源码库、角色知识库等

**数据模型：**

```java
@Data
@Entity
@Table(name = "knowledge_domains")
public class KnowledgeDomain {
    
    @Id
    private String domainId;              // 域ID
    
    private String domainName;            // 域名称
    
    @Enumerated(EnumType.STRING)
    private DomainType domainType;        // 域类型
    
    private String description;           // 描述
    
    private String storagePath;           // 存储路径
    
    private String ragIndexPath;          // RAG索引路径
    
    @Embedded
    private DomainConfig config;          // 配置
    
    @Enumerated(EnumType.STRING)
    private DomainStatus status;          // 状态
    
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;
    
    // 关联的角色（如果是角色知识库）
    private String linkedRoleId;
    
    // 关联的项目（如果是源码库）
    private String linkedProjectId;
}

public enum DomainType {
    DOCUMENT,           // 文档知识域
    SOURCE_CODE,        // 源码知识域
    ROLE_KNOWLEDGE,     // 角色知识域
    API_DOCUMENTATION,  // API文档域
    MIXED               // 混合域
}
```

### 2. 角色知识库 (Role Knowledge Base)

**定义：** 与特定角色/职责绑定的专业知识库

**角色模型：**

```java
@Data
@Entity
@Table(name = "knowledge_roles")
public class KnowledgeRole {
    
    @Id
    private String roleId;
    
    private String roleName;              // 角色名称
    
    private String roleType;              // 角色类型
    
    @Column(length = 2000)
    private String responsibilities;      // 职责描述
    
    @Column(length = 5000)
    private String expertise;             // 专业领域
    
    // 关联的知识域
    private String knowledgeDomainId;
    
    // 学习源（从哪些域学习）
    @ElementCollection
    private List<String> learningSourceDomainIds;
    
    // 使用的AI模型
    @Embedded
    private AIModelConfig modelConfig;
    
    private LocalDateTime createdAt;
    private LocalDateTime lastLearnedAt;
}

// 示例：安全分析师角色
{
  "roleId": "security-analyst-001",
  "roleName": "源码安全分析师",
  "roleType": "SOURCE_CODE_ANALYZER",
  "responsibilities": "分析Java/Python项目源码，识别安全漏洞包括SQL注入、XSS、CSRF、敏感信息泄露等",
  "expertise": "OWASP Top 10, 静态代码分析, 依赖漏洞扫描",
  "knowledgeDomainId": "domain-role-security-analyst",
  "learningSourceDomainIds": [
    "domain-source-code-project-1",
    "domain-docs-security-best-practices"
  ],
  "modelConfig": {
    "modelType": "LOCAL",  // 本地模型
    "modelName": "qwen2.5-coder-7b",
    "apiEndpoint": "http://localhost:11434"
  }
}
```

### 3. 领域路由器 (Domain Router)

**功能：** 智能分析用户查询，路由到合适的知识域

**路由策略：**

```python
class DomainRouter:
    """
    领域路由器 - 将查询路由到最合适的知识域
    """
    
    def route_query(self, query: str, context: Dict) -> List[str]:
        """
        路由查询到相关的知识域
        
        Args:
            query: 用户查询
            context: 上下文信息
            
        Returns:
            相关知识域ID列表
        """
        # 1. 意图识别
        intent = self.analyze_intent(query)
        
        # 2. 实体提取
        entities = self.extract_entities(query)
        
        # 3. 领域匹配
        candidate_domains = []
        
        # 规则匹配
        if "源码" in query or "代码" in query or "项目" in query:
            candidate_domains.extend(
                self.get_domains_by_type(DomainType.SOURCE_CODE)
            )
        
        if "安全" in query or "漏洞" in query:
            candidate_domains.extend(
                self.get_domains_by_role("security-analyst")
            )
        
        # 向量相似度匹配
        domain_embeddings = self.get_all_domain_embeddings()
        query_embedding = self.embed(query)
        
        similar_domains = self.find_similar_domains(
            query_embedding, 
            domain_embeddings, 
            top_k=3
        )
        candidate_domains.extend(similar_domains)
        
        # 4. 去重和排序
        return self.deduplicate_and_rank(candidate_domains, query)
    
    def analyze_intent(self, query: str) -> str:
        """分析查询意图"""
        intents = {
            "code_analysis": ["分析", "检查", "审查", "扫描"],
            "knowledge_query": ["什么是", "如何", "为什么", "解释"],
            "code_generation": ["生成", "创建", "实现", "写"],
            "bug_fix": ["修复", "解决", "调试", "错误"]
        }
        
        for intent, keywords in intents.items():
            if any(kw in query for kw in keywords):
                return intent
        
        return "general_query"
```

---

## 技术实现方案

### 1. 多RAG索引管理

**新增接口：**

```java
/**
 * 知识域管理服务
 */
public interface KnowledgeDomainService {
    
    /**
     * 创建知识域
     */
    KnowledgeDomain createDomain(CreateDomainRequest request);
    
    /**
     * 获取域的RAG服务
     */
    RAGService getDomainRAGService(String domainId);
    
    /**
     * 跨域查询
     */
    List<SearchResult> crossDomainSearch(
        String query, 
        List<String> domainIds, 
        int topK
    );
    
    /**
     * 域间知识迁移
     */
    void transferKnowledge(String sourceDomainId, String targetDomainId);
}

/**
 * RAG服务工厂 - 为每个域创建独立的RAG实例
 */
@Component
public class RAGServiceFactory {
    
    private final Map<String, RAGService> domainRAGServices = new ConcurrentHashMap<>();
    
    /**
     * 获取或创建域的RAG服务
     */
    public RAGService getOrCreateRAGService(String domainId, DomainConfig config) {
        return domainRAGServices.computeIfAbsent(domainId, id -> {
            return createRAGService(id, config);
        });
    }
    
    private RAGService createRAGService(String domainId, DomainConfig config) {
        // 根据配置创建相应的RAG实现
        String indexPath = config.getRagIndexPath();
        
        switch (config.getBackendType()) {
            case LUCENE:
                return new FileRAGService(indexPath);
            case MONGODB:
                return new MongoDBRAGService(config.getMongoConfig());
            case ELASTICSEARCH:
                return new ElasticsearchRAGService(config.getEsConfig());
            default:
                throw new IllegalArgumentException("Unsupported backend: " + config.getBackendType());
        }
    }
}
```

### 2. 角色知识库实现

```java
/**
 * 角色知识库服务
 */
@Service
@Slf4j
public class RoleKnowledgeService {
    
    private final KnowledgeDomainService domainService;
    private final RAGServiceFactory ragServiceFactory;
    private final AIModelService aiModelService;
    
    /**
     * 创建角色
     */
    public KnowledgeRole createRole(CreateRoleRequest request) {
        // 1. 创建角色实体
        KnowledgeRole role = new KnowledgeRole();
        role.setRoleId(UUID.randomUUID().toString());
        role.setRoleName(request.getRoleName());
        role.setResponsibilities(request.getResponsibilities());
        
        // 2. 创建角色专属的知识域
        CreateDomainRequest domainRequest = CreateDomainRequest.builder()
            .domainName(request.getRoleName() + " Knowledge Base")
            .domainType(DomainType.ROLE_KNOWLEDGE)
            .linkedRoleId(role.getRoleId())
            .build();
        
        KnowledgeDomain domain = domainService.createDomain(domainRequest);
        role.setKnowledgeDomainId(domain.getDomainId());
        
        // 3. 保存角色
        return roleRepository.save(role);
    }
    
    /**
     * 角色学习 - 从指定的源域学习知识
     */
    public void learnFromDomains(String roleId, List<String> sourceDomainIds) {
        KnowledgeRole role = roleRepository.findById(roleId)
            .orElseThrow(() -> new NotFoundException("Role not found"));
        
        RAGService roleRAG = ragServiceFactory.getOrCreateRAGService(
            role.getKnowledgeDomainId(), 
            getDomainConfig(role.getKnowledgeDomainId())
        );
        
        // 遍历源域，提取相关知识
        for (String sourceDomainId : sourceDomainIds) {
            log.info("角色 {} 正在从域 {} 学习...", role.getRoleName(), sourceDomainId);
            
            RAGService sourceRAG = ragServiceFactory.getOrCreateRAGService(
                sourceDomainId, 
                getDomainConfig(sourceDomainId)
            );
            
            // 根据角色职责筛选相关文档
            List<Document> relevantDocs = filterRelevantDocuments(
                sourceRAG.getAllDocuments(0, 1000),
                role.getResponsibilities()
            );
            
            // 使用AI模型提炼知识
            for (Document doc : relevantDocs) {
                String refinedKnowledge = refineKnowledge(doc, role);
                
                // 存储到角色知识库
                Document knowledgeDoc = Document.builder()
                    .id(UUID.randomUUID().toString())
                    .content(refinedKnowledge)
                    .metadata(Map.of(
                        "source_domain", sourceDomainId,
                        "source_doc", doc.getId(),
                        "learned_at", LocalDateTime.now().toString()
                    ))
                    .build();
                
                roleRAG.indexDocument(knowledgeDoc);
            }
        }
        
        role.setLastLearnedAt(LocalDateTime.now());
        roleRepository.save(role);
    }
    
    /**
     * 使用AI模型提炼知识
     */
    private String refineKnowledge(Document doc, KnowledgeRole role) {
        String prompt = String.format(
            "你是一个%s，职责是：%s\n\n" +
            "请从以下文档中提炼出与你职责相关的关键知识点：\n\n%s\n\n" +
            "要求：\n" +
            "1. 只提取与职责直接相关的内容\n" +
            "2. 用专业术语总结\n" +
            "3. 结构化输出（使用Markdown）\n",
            role.getRoleName(),
            role.getResponsibilities(),
            doc.getContent()
        );
        
        return aiModelService.generate(
            role.getModelConfig(), 
            prompt
        );
    }
}
```

---

## 源码分析专项功能

### 1. 源码域模型

```java
@Data
@Entity
@Table(name = "source_code_projects")
public class SourceCodeProject {
    
    @Id
    private String projectId;
    
    private String projectName;
    
    private String projectPath;          // 本地路径
    
    private String gitRepository;        // Git仓库URL
    
    private String gitBranch;            // Git分支
    
    @Enumerated(EnumType.STRING)
    private ProjectLanguage primaryLanguage;
    
    @ElementCollection
    private List<String> languages;      // 项目使用的所有语言
    
    // 关联的知识域
    private String knowledgeDomainId;
    
    // 增量追踪
    @Embedded
    private IncrementalTracker incrementalTracker;
    
    // 分析状态
    @OneToMany(mappedBy = "project", cascade = CascadeType.ALL)
    private List<AnalysisReport> analysisReports;
    
    private LocalDateTime createdAt;
    private LocalDateTime lastAnalyzedAt;
}
```

### 2. 源码分析流程

```java
/**
 * 源码分析服务
 */
@Service
@Slf4j
public class SourceCodeAnalysisService {
    
    /**
     * 分析项目
     */
    public void analyzeProject(String projectId, List<String> analysisTypes) {
        SourceCodeProject project = projectRepository.findById(projectId)
            .orElseThrow();
        
        // 1. 检测变更（增量分析）
        List<FileChange> changes = detectChanges(project);
        
        if (changes.isEmpty()) {
            log.info("项目无变更，跳过分析");
            return;
        }
        
        // 2. 为每种分析类型分配角色
        for (String analysisType : analysisTypes) {
            KnowledgeRole analyst = getRoleForAnalysisType(analysisType);
            
            // 3. 执行分析
            AnalysisReport report = performAnalysis(
                project, 
                changes, 
                analyst, 
                analysisType
            );
            
            // 4. 保存报告到角色知识库
            saveReportToRoleKB(analyst, report);
        }
        
        // 5. 更新项目状态
        project.setLastAnalyzedAt(LocalDateTime.now());
        projectRepository.save(project);
    }
    
    /**
     * 执行具体的分析
     */
    private AnalysisReport performAnalysis(
        SourceCodeProject project,
        List<FileChange> changes,
        KnowledgeRole analyst,
        String analysisType
    ) {
        AnalysisReport report = new AnalysisReport();
        report.setProjectId(project.getProjectId());
        report.setAnalysisType(analysisType);
        report.setAnalyzedBy(analyst.getRoleId());
        
        List<Finding> findings = new ArrayList<>();
        
        // 只分析变更的文件（增量）
        for (FileChange change : changes) {
            String fileContent = readFile(change.getFilePath());
            
            // 构建分析提示词
            String prompt = buildAnalysisPrompt(
                analysisType, 
                analyst.getResponsibilities(),
                change.getFilePath(),
                fileContent
            );
            
            // 调用AI模型分析
            String analysis = aiModelService.generate(
                analyst.getModelConfig(),
                prompt
            );
            
            // 解析分析结果
            List<Finding> fileFindigs = parseAnalysisResult(analysis);
            findings.addAll(fileFindigs);
        }
        
        report.setFindings(findings);
        report.setGeneratedAt(LocalDateTime.now());
        
        return report;
    }
    
    /**
     * 构建分析提示词
     */
    private String buildAnalysisPrompt(
        String analysisType,
        String responsibilities,
        String filePath,
        String fileContent
    ) {
        switch (analysisType) {
            case "security":
                return String.format(
                    "作为安全分析师，你的职责是：%s\n\n" +
                    "请分析以下代码文件的安全问题：\n" +
                    "文件路径：%s\n\n" +
                    "```\n%s\n```\n\n" +
                    "请以JSON格式输出发现的问题：\n" +
                    "{\n" +
                    "  \"findings\": [\n" +
                    "    {\n" +
                    "      \"type\": \"漏洞类型\",\n" +
                    "      \"severity\": \"HIGH|MEDIUM|LOW\",\n" +
                    "      \"location\": \"行号\",\n" +
                    "      \"description\": \"问题描述\",\n" +
                    "      \"recommendation\": \"修复建议\"\n" +
                    "    }\n" +
                    "  ]\n" +
                    "}",
                    responsibilities,
                    filePath,
                    fileContent
                );
            
            case "architecture":
                return String.format(
                    "作为架构师，你的职责是：%s\n\n" +
                    "请分析以下代码的架构设计：\n" +
                    "文件路径：%s\n\n" +
                    "```\n%s\n```\n\n" +
                    "请分析：\n" +
                    "1. 设计模式使用\n" +
                    "2. 模块职责是否清晰\n" +
                    "3. 依赖关系是否合理\n" +
                    "4. 改进建议",
                    responsibilities,
                    filePath,
                    fileContent
                );
            
            default:
                throw new IllegalArgumentException("Unknown analysis type: " + analysisType);
        }
    }
}
```

---

## 增量更新机制

### 1. 文件变更追踪

```java
/**
 * 增量追踪器
 */
@Embeddable
@Data
public class IncrementalTracker {
    
    // 文件哈希映射（文件路径 -> SHA256哈希）
    @Column(length = 10000)
    private String fileHashesJson;
    
    // 最后一次完整扫描时间
    private LocalDateTime lastFullScanAt;
    
    // 最后一次增量扫描时间
    private LocalDateTime lastIncrementalScanAt;
    
    /**
     * 获取文件哈希映射
     */
    public Map<String, String> getFileHashes() {
        if (fileHashesJson == null) {
            return new HashMap<>();
        }
        try {
            return new ObjectMapper().readValue(
                fileHashesJson, 
                new TypeReference<Map<String, String>>() {}
            );
        } catch (Exception e) {
            return new HashMap<>();
        }
    }
    
    /**
     * 设置文件哈希映射
     */
    public void setFileHashes(Map<String, String> hashes) {
        try {
            this.fileHashesJson = new ObjectMapper().writeValueAsString(hashes);
        } catch (Exception e) {
            throw new RuntimeException("Failed to serialize file hashes", e);
        }
    }
}

/**
 * 文件变更检测服务
 */
@Service
@Slf4j
public class FileChangeDetector {
    
    /**
     * 检测项目变更
     */
    public List<FileChange> detectChanges(SourceCodeProject project) {
        String projectPath = project.getProjectPath();
        Map<String, String> oldHashes = project.getIncrementalTracker().getFileHashes();
        Map<String, String> newHashes = new HashMap<>();
        
        List<FileChange> changes = new ArrayList<>();
        
        // 扫描项目目录
        Files.walk(Paths.get(projectPath))
            .filter(Files::isRegularFile)
            .filter(this::isSourceFile)  // 只处理源码文件
            .forEach(path -> {
                String relativePath = projectPath.relativize(path).toString();
                String newHash = calculateFileHash(path);
                
                newHashes.put(relativePath, newHash);
                
                String oldHash = oldHashes.get(relativePath);
                
                if (oldHash == null) {
                    // 新增文件
                    changes.add(FileChange.added(relativePath, path));
                } else if (!oldHash.equals(newHash)) {
                    // 修改文件
                    changes.add(FileChange.modified(relativePath, path));
                }
            });
        
        // 检测删除的文件
        for (String oldPath : oldHashes.keySet()) {
            if (!newHashes.containsKey(oldPath)) {
                changes.add(FileChange.deleted(oldPath));
            }
        }
        
        // 更新哈希映射
        project.getIncrementalTracker().setFileHashes(newHashes);
        project.getIncrementalTracker().setLastIncrementalScanAt(LocalDateTime.now());
        
        log.info("检测到 {} 个文件变更", changes.size());
        return changes;
    }
    
    /**
     * 计算文件SHA256哈希
     */
    private String calculateFileHash(Path file) {
        try {
            MessageDigest digest = MessageDigest.getInstance("SHA-256");
            byte[] fileBytes = Files.readAllBytes(file);
            byte[] hash = digest.digest(fileBytes);
            return Base64.getEncoder().encodeToString(hash);
        } catch (Exception e) {
            throw new RuntimeException("Failed to calculate file hash", e);
        }
    }
    
    /**
     * 判断是否为源码文件
     */
    private boolean isSourceFile(Path path) {
        String fileName = path.getFileName().toString();
        return fileName.endsWith(".java") ||
               fileName.endsWith(".py") ||
               fileName.endsWith(".js") ||
               fileName.endsWith(".ts") ||
               fileName.endsWith(".go") ||
               fileName.endsWith(".rs") ||
               fileName.endsWith(".cpp") ||
               fileName.endsWith(".c") ||
               fileName.endsWith(".h");
    }
}

@Data
@AllArgsConstructor
public class FileChange {
    private ChangeType type;
    private String relativePath;
    private Path absolutePath;
    
    public static FileChange added(String relativePath, Path absolutePath) {
        return new FileChange(ChangeType.ADDED, relativePath, absolutePath);
    }
    
    public static FileChange modified(String relativePath, Path absolutePath) {
        return new FileChange(ChangeType.MODIFIED, relativePath, absolutePath);
    }
    
    public static FileChange deleted(String relativePath) {
        return new FileChange(ChangeType.DELETED, relativePath, null);
    }
    
    public enum ChangeType {
        ADDED, MODIFIED, DELETED
    }
}
```

### 2. Git集成

```java
/**
 * Git同步服务
 */
@Service
@Slf4j
public class GitSyncService {
    
    /**
     * 从Git拉取项目
     */
    public SourceCodeProject cloneOrPullProject(String gitUrl, String branch) {
        String projectName = extractProjectName(gitUrl);
        String localPath = "data/knowledge-network/source-code/" + projectName;
        
        Path projectPath = Paths.get(localPath);
        
        if (Files.exists(projectPath)) {
            // 已存在，执行 git pull
            pullLatestChanges(projectPath, branch);
        } else {
            // 不存在，执行 git clone
            cloneRepository(gitUrl, projectPath, branch);
        }
        
        // 创建或更新项目记录
        SourceCodeProject project = projectRepository.findByGitRepository(gitUrl)
            .orElse(new SourceCodeProject());
        
        project.setProjectName(projectName);
        project.setProjectPath(localPath);
        project.setGitRepository(gitUrl);
        project.setGitBranch(branch);
        
        return projectRepository.save(project);
    }
    
    /**
     * 使用JGit拉取最新代码
     */
    private void pullLatestChanges(Path projectPath, String branch) {
        try {
            Git git = Git.open(projectPath.toFile());
            
            // 切换分支
            git.checkout().setName(branch).call();
            
            // 拉取最新代码
            PullResult result = git.pull().call();
            
            if (result.isSuccessful()) {
                log.info("成功拉取最新代码: {}", projectPath);
            } else {
                log.warn("拉取代码失败: {}", result.getMergeResult().getMergeStatus());
            }
            
            git.close();
        } catch (Exception e) {
            throw new RuntimeException("Failed to pull from git", e);
        }
    }
    
    /**
     * 克隆仓库
     */
    private void cloneRepository(String gitUrl, Path targetPath, String branch) {
        try {
            Git.cloneRepository()
                .setURI(gitUrl)
                .setDirectory(targetPath.toFile())
                .setBranch(branch)
                .call()
                .close();
            
            log.info("成功克隆仓库: {} -> {}", gitUrl, targetPath);
        } catch (Exception e) {
            throw new RuntimeException("Failed to clone repository", e);
        }
    }
}
```

---

## 实施路线图

### Phase 1: 基础架构重构（2周）

**目标：** 实现多知识域的基础架构

**任务：**
1. ✅ 设计并实现 `KnowledgeDomain` 实体和数据库表
2. ✅ 实现 `RAGServiceFactory` - 支持多RAG实例管理
3. ✅ 重构 `data` 目录结构
4. ✅ 实现 `KnowledgeDomainService` 基础API
5. ✅ 数据迁移工具 - 将现有数据迁移到新结构

**交付物：**
- 新的数据库表结构
- 多RAG实例管理器
- 数据迁移脚本

### Phase 2: 角色知识库系统（2周）

**目标：** 实现角色创建、学习和知识管理

**任务：**
1. ✅ 实现 `KnowledgeRole` 实体
2. ✅ 实现角色创建和管理API
3. ✅ 实现角色学习功能
4. ✅ 实现领域路由器
5. ✅ 前端UI - 角色管理界面

**交付物：**
- 角色管理API
- 角色学习引擎
- 角色管理UI

### Phase 3: 源码分析功能（3周）

**目标：** 实现源码项目导入和分析

**任务：**
1. ✅ 实现 `SourceCodeProject` 实体
2. ✅ 实现文件变更检测器
3. ✅ 实现Git集成
4. ✅ 实现源码分析服务
5. ✅ 集成本地AI模型（Ollama）
6. ✅ 实现分析报告生成
7. ✅ 前端UI - 源码项目管理

**交付物：**
- 源码项目管理API
- 增量分析引擎
- 分析报告系统
- 源码项目管理UI

### Phase 4: 知识网络与智能路由（2周）

**目标：** 实现跨域查询和知识关联

**任务：**
1. ✅ 实现跨域查询功能
2. ✅ 实现知识关联和迁移
3. ✅ 优化领域路由算法
4. ✅ 实现知识网络可视化
5. ✅ 性能优化

**交付物：**
- 知识网络查询引擎
- 知识网络可视化UI

### Phase 5: 综合报告与评估（1周）

**目标：** 实现多角度分析报告汇总

**任务：**
1. ✅ 实现报告聚合引擎
2. ✅ 实现综合评估算法
3. ✅ 实现报告导出（PDF/Markdown）
4. ✅ 前端UI - 综合报告展示

**交付物：**
- 综合报告生成器
- 报告展示UI

---

## 技术栈选择

### 后端
- **Java 17** + Spring Boot 3.x
- **JGit** - Git操作
- **Ollama** - 本地AI模型
- **Lucene** - 全文检索（每个域独立索引）
- **MongoDB** / **Redis** - 可选的向量存储后端

### 前端
- **React** + TypeScript
- **Ant Design** - UI组件
- **D3.js** / **Cytoscape.js** - 知识网络可视化
- **Monaco Editor** - 代码查看器

### AI模型
- **本地模型**：
  - Qwen2.5-Coder (7B/14B) - 代码分析
  - Deepseek-Coder - 代码理解
- **在线API**：
  - Claude 3.5 Sonnet - 复杂推理
  - GPT-4 - 高级分析

---

## 总结

### 🎯 核心创新点

1. **知识网络架构**
   - 多知识域隔离，向量空间专业化
   - 智能领域路由，精准知识召回

2. **角色知识库**
   - 职责驱动的知识组织
   - 主动学习和知识提炼

3. **增量分析**
   - 文件哈希追踪
   - 只处理变更，节约资源

4. **源码深度分析**
   - 多角度分析（安全、架构、质量）
   - 支持本地模型，降低成本

5. **Git深度集成**
   - 自动同步代码
   - 为未来CI/CD集成打基础

### 📈 预期效果

- **准确率提升**：知识召回准确率提升 50%+
- **成本降低**：使用本地模型，成本降低 80%+
- **效率提升**：增量分析，处理速度提升 10x
- **可扩展性**：支持无限扩展知识域

---

**下一步行动：** 开始 Phase 1 实施！


