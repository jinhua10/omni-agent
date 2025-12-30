# 📦 新模块设计方案

**设计原则：** 简洁实用，避免过度拆分  
**创建时间：** 2025-12-28

---

## 🎯 模块列表

### 1. 分块模块

```
omni-agent-chunking-api/               # 分块 API
omni-agent-chunking-starter/           # 分块实现（统一）
```

### 2. 文档处理模块

```
omni-agent-document-processor-api/     # 文档处理 API
omni-agent-document-processor-starter/ # 文档处理实现（统一）
```

---

## 📋 模块详细设计

### Module 1: omni-agent-chunking-api

**职责：** 定义文档分块的接口和模型

**包结构：**
```
omni-agent-chunking-api/
└── src/main/java/top/yumbo/ai/omni/chunking/
    ├── ChunkingService.java          # 分块服务接口
    ├── Chunk.java                    # 分块模型
    ├── ChunkingStrategy.java         # 分块策略枚举
    └── ChunkingConfig.java           # 分块配置
```

**核心接口：**
```java
public interface ChunkingService {
    /**
     * 分块文档
     */
    List<Chunk> chunk(String documentId, String content, ChunkingConfig config);
    
    /**
     * 获取支持的策略
     */
    List<ChunkingStrategy> getSupportedStrategies();
}

public enum ChunkingStrategy {
    PPL,           // PPL 智能分块
    FIXED_LENGTH,  // 固定长度分块
    SEMANTIC,      // 语义分块
    PARAGRAPH,     // 段落分块
    SENTENCE       // 句子分块
}
```

---

### Module 2: omni-agent-chunking-starter

**职责：** 实现所有分块算法

**包结构：**
```
omni-agent-chunking-starter/
└── src/main/java/top/yumbo/ai/omni/chunking/starter/
    ├── DefaultChunkingService.java       # 默认实现
    ├── strategy/
    │   ├── PPLChunkingStrategy.java      # PPL 分块
    │   ├── FixedLengthStrategy.java      # 固定长度
    │   ├── SemanticStrategy.java         # 语义分块
    │   ├── ParagraphStrategy.java        # 段落分块
    │   └── SentenceStrategy.java         # 句子分块
    ├── config/
    │   └── ChunkingAutoConfiguration.java # 自动配置
    └── properties/
        └── ChunkingProperties.java        # 配置属性
```

**自动配置：**
```java
@Configuration
@EnableConfigurationProperties(ChunkingProperties.class)
public class ChunkingAutoConfiguration {
    
    @Bean
    @ConditionalOnMissingBean
    public ChunkingService chunkingService(ChunkingProperties properties) {
        return new DefaultChunkingService(properties);
    }
}
```

**配置示例：**
```yaml
omni-agent:
  chunking:
    strategy: PPL              # 默认策略
    fixed-length-size: 512     # 固定长度
    semantic-threshold: 0.7    # 语义阈值
```

---

### Module 3: omni-agent-document-processor-api

**职责：** 定义文档处理的接口和模型

**包结构：**
```
omni-agent-document-processor-api/
└── src/main/java/top/yumbo/ai/omni/document/processor/
    ├── DocumentProcessor.java         # 文档处理器接口
    ├── ProcessedDocument.java         # 处理结果模型
    ├── DocumentType.java              # 文档类型枚举
    └── ProcessorException.java        # 异常类
```

**核心接口：**
```java
public interface DocumentProcessor {
    /**
     * 处理文档，提取文本
     */
    ProcessedDocument process(String documentId, InputStream input);
    
    /**
     * 支持的文档类型
     */
    List<DocumentType> getSupportedTypes();
    
    /**
     * 是否支持该类型
     */
    boolean supports(DocumentType type);
}

public enum DocumentType {
    PDF,
    WORD,      // .doc, .docx
    EXCEL,     // .xls, .xlsx
    PPT,       // .ppt, .pptx
    TEXT,      // .txt, .md
    HTML,
    XML,
    JSON
}
```

---

### Module 4: omni-agent-document-processor-starter

**职责：** 实现所有文档格式的处理器

**包结构：**
```
omni-agent-document-processor-starter/
└── src/main/java/top/yumbo/ai/omni/document/processor/starter/
    ├── CompositeDocumentProcessor.java   # 组合处理器
    ├── processor/
    │   ├── PDFProcessor.java             # PDF 处理器
    │   ├── WordProcessor.java            # Word 处理器
    │   ├── ExcelProcessor.java           # Excel 处理器
    │   ├── PPTProcessor.java             # PPT 处理器
    │   ├── TextProcessor.java            # 文本处理器
    │   ├── HTMLProcessor.java            # HTML 处理器
    │   └── ... (后续扩展)
    ├── config/
    │   └── DocumentProcessorAutoConfiguration.java
    └── properties/
        └── DocumentProcessorProperties.java
```

**自动配置：**
```java
@Configuration
@EnableConfigurationProperties(DocumentProcessorProperties.class)
public class DocumentProcessorAutoConfiguration {
    
    @Bean
    @ConditionalOnMissingBean
    public DocumentProcessor documentProcessor(
            List<DocumentProcessor> processors) {
        return new CompositeDocumentProcessor(processors);
    }
    
    @Bean
    public PDFProcessor pdfProcessor() {
        return new PDFProcessor();
    }
    
    @Bean
    public WordProcessor wordProcessor() {
        return new WordProcessor();
    }
    
    // ... 其他处理器
}
```

**配置示例：**
```yaml
omni-agent:
  document-processor:
    enabled: true
    pdf:
      extract-images: true
      ocr-enabled: true
    word:
      preserve-formatting: false
    excel:
      max-rows: 10000
```

---

## 🎨 使用示例

### 1. 用户引入依赖

```xml
<dependencies>
    <!-- 分块功能 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-chunking-starter</artifactId>
    </dependency>
    
    <!-- 文档处理功能 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-document-processor-starter</artifactId>
    </dependency>
</dependencies>
```

### 2. 配置

```yaml
omni-agent:
  # 分块配置
  chunking:
    strategy: PPL
    fixed-length-size: 512
  
  # 文档处理配置
  document-processor:
    enabled: true
    pdf:
      extract-images: true
```

### 3. 使用代码

```java
@Service
public class DocumentService {
    
    @Autowired
    private DocumentProcessor documentProcessor;
    
    @Autowired
    private ChunkingService chunkingService;
    
    public void processDocument(String documentId, InputStream input) {
        // 1. 处理文档，提取文本
        ProcessedDocument doc = documentProcessor.process(documentId, input);
        
        // 2. 分块
        List<Chunk> chunks = chunkingService.chunk(
            documentId, 
            doc.getText(), 
            ChunkingConfig.defaults()
        );
        
        // 3. 后续处理...
    }
}
```

---

## ✅ 设计优势

### 1. 简洁性
- ✅ 只有 4 个模块（2个 API + 2个 Starter）
- ✅ 用户只需引入 2 个依赖
- ✅ 配置简单明了

### 2. 扩展性
```java
// 新增分块算法
@Component
public class CustomChunkingStrategy implements ChunkingStrategy {
    // 实现自定义算法
}

// 新增文档处理器
@Component
public class MarkdownProcessor implements DocumentProcessor {
    // 实现 Markdown 处理
}

// Spring Boot 自动发现并集成
```

### 3. 灵活性
```yaml
# 可以通过配置选择策略
omni-agent:
  chunking:
    strategy: SEMANTIC  # 切换策略
```

### 4. 维护性
- ✅ 相关代码集中在一起
- ✅ 减少跨模块依赖
- ✅ 便于统一升级

---

## 🚫 为什么不过度拆分？

### ❌ 过度拆分的问题

```
omni-agent-chunking-starter-ppl/
omni-agent-chunking-starter-fixed/
omni-agent-chunking-starter-semantic/
omni-agent-chunking-starter-paragraph/
```

**问题：**
1. **模块爆炸**：每个算法一个模块，太多了
2. **用户困惑**：不知道该选哪个，需要了解每个算法
3. **维护成本**：每个模块都需要 pom.xml、配置、文档
4. **代码分散**：相似的代码分散在多个模块
5. **依赖复杂**：用户可能需要引入多个依赖

### ✅ 统一实现的优势

```
omni-agent-chunking-starter/         # 一个模块包含所有
  ├─ PPLChunkingStrategy
  ├─ FixedLengthStrategy
  ├─ SemanticStrategy
  └─ ... (所有策略)
```

**优势：**
1. **简洁**：一个模块，一个依赖
2. **灵活**：通过配置切换策略
3. **易维护**：代码集中，统一管理
4. **易扩展**：新增策略只需加一个类
5. **用户友好**：不需要了解内部实现

---

## 📊 对比表

| 维度 | 过度拆分 | 统一实现 |
|------|---------|---------|
| 模块数量 | 10+ | 4 |
| 用户依赖 | 多个 | 2个 |
| 配置复杂度 | 高 | 低 |
| 维护成本 | 高 | 低 |
| 扩展难度 | 高（需建新模块）| 低（加新类）|
| 用户学习成本 | 高 | 低 |

---

## 🎯 实施计划

### Phase 1: 创建 API 模块
- [ ] 创建 `omni-agent-chunking-api`
- [ ] 创建 `omni-agent-document-processor-api`

### Phase 2: 创建 Starter 模块
- [ ] 创建 `omni-agent-chunking-starter`
- [ ] 创建 `omni-agent-document-processor-starter`

### Phase 3: 迁移代码
- [ ] 从 `core/old/chunking/` 迁移分块代码
- [ ] 从 `core/old/document/` 迁移文档处理代码

### Phase 4: 测试验证
- [ ] 编写单元测试
- [ ] 编写集成测试
- [ ] 更新文档

---

**创建时间：** 2025-12-28  
**状态：** 设计完成，待实施  
**原则：** 简洁实用，避免过度设计

