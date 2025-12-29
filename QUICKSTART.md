# OmniAgent 快速开始指南 🚀

> **5分钟快速上手** - 从零开始使用OmniAgent构建智能知识管理系统

---

## 📋 目录

- [环境准备](#环境准备)
- [快速安装](#快速安装)
- [第一个示例](#第一个示例)
- [核心功能使用](#核心功能使用)
- [常见问题](#常见问题)

---

## 环境准备

### 必需环境

```bash
# 检查Java版本（需要21+）
java -version

# 检查Maven版本（需要3.8+）
mvn -version
```

### 推荐配置

- **内存**: 4GB+
- **硬盘**: 10GB+
- **操作系统**: Windows/Linux/macOS

---

## 快速安装

### 1. 克隆项目

```bash
git clone https://github.com/jinhua10/omni-agent.git
cd omni-agent
```

### 2. 编译项目

```bash
# 完整编译（首次运行）
mvn clean install -DskipTests

# 编译成功后会看到：
# [INFO] BUILD SUCCESS
```

### 3. 准备配置文件

在项目根目录创建 `application.yml`：

```yaml
# 最小配置 - 开箱即用
omni:
  # 文档存储配置
  document-storage:
    file:
      base-path: ./data/storage
      
  # RAG检索配置
  rag:
    file:
      enabled: true
      index-path: ./data/rag/lucene
      
  # 分块配置
  chunking:
    default-strategy: sentence-boundary
```

### 4. 启动应用

```bash
cd omni-agent-example-basic
mvn spring-boot:run
```

看到以下输出说明启动成功：
```
Started OmniAgentApplication in 5.123 seconds
```

---

## 第一个示例

### 示例1：上传并处理文档

创建一个简单的测试类：

```java
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.processor.api.DocumentProcessor;
import top.yumbo.ai.omni.chunking.api.ChunkingService;
import top.yumbo.ai.omni.rag.api.RagService;

@Service
public class QuickStartService {
    
    @Autowired
    private DocumentProcessor documentProcessor;
    
    @Autowired
    private ChunkingService chunkingService;
    
    @Autowired
    private RagService ragService;
    
    /**
     * 完整流程：处理文档 → 分块 → 索引
     */
    public void processDocument(File file) {
        // 步骤1：提取文本
        ExtractionResult result = documentProcessor.extractText(file);
        String text = result.getText();
        System.out.println("提取到文本: " + text.length() + " 字符");
        
        // 步骤2：智能分块
        List<Chunk> chunks = chunkingService.chunk(text);
        System.out.println("分块数量: " + chunks.size());
        
        // 步骤3：索引到RAG
        List<Document> docs = new ArrayList<>();
        for (Chunk chunk : chunks) {
            Document doc = Document.builder()
                .id(UUID.randomUUID().toString())
                .content(chunk.getText())
                .build();
            docs.add(doc);
        }
        ragService.batchIndex(docs);
        System.out.println("索引完成！");
    }
}
```

### 示例2：搜索文档

```java
@Service
public class SearchService {
    
    @Autowired
    private RagService ragService;
    
    /**
     * 简单搜索
     */
    public void search(String query) {
        // 执行搜索
        List<Document> results = ragService.search(query, 5);
        
        // 打印结果
        System.out.println("找到 " + results.size() + " 个结果：");
        for (int i = 0; i < results.size(); i++) {
            Document doc = results.get(i);
            System.out.println((i+1) + ". " + doc.getContent().substring(0, 100) + "...");
        }
    }
}
```

### 示例3：使用REST API

启动应用后，可以直接使用REST API：

```bash
# 1. 上传文档
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@your-document.pdf" \
  -F "domainId=default"

# 2. 搜索文档
curl -X POST http://localhost:8080/api/search \
  -H "Content-Type: application/json" \
  -d '{"query": "你的搜索关键词", "topK": 5}'

# 3. 查看统计
curl http://localhost:8080/api/statistics
```

---

## 核心功能使用

### 功能1：文档处理（支持5种格式）

```java
// 处理PDF
File pdfFile = new File("document.pdf");
ExtractionResult result = documentProcessor.extractText(pdfFile);

// 处理Word
File wordFile = new File("document.docx");
ExtractionResult result = documentProcessor.extractText(wordFile);

// 处理Excel
File excelFile = new File("data.xlsx");
ExtractionResult result = documentProcessor.extractText(excelFile);

// 自动识别格式
File anyFile = new File("unknown.doc");
ExtractionResult result = documentProcessor.extractText(anyFile);
```

### 功能2：智能分块（6种策略）

```java
// 使用默认策略（推荐）
List<Chunk> chunks = chunkingService.chunk(text);

// 使用固定长度策略
ChunkingParams params = ChunkingParams.builder()
    .strategy("fixed-length")
    .maxChunkSize(512)
    .overlap(50)
    .build();
List<Chunk> chunks = chunkingService.chunk(text, params);

// 使用段落分块策略
ChunkingParams params = ChunkingParams.builder()
    .strategy("paragraph")
    .maxParagraphs(3)
    .build();
List<Chunk> chunks = chunkingService.chunk(text, params);

// 自动选择策略（根据文档类型）
ChunkingParams params = ChunkingParams.forDocType(DocumentType.TECHNICAL);
List<Chunk> chunks = chunkingService.chunk(text, params);
```

### 功能3：知识域管理

```java
@Autowired
private KnowledgeDomainService domainService;

// 创建知识域
KnowledgeDomain domain = KnowledgeDomain.builder()
    .domainId("tech-docs")
    .name("技术文档域")
    .description("存储所有技术文档")
    .domainType(DomainType.DOCUMENT)
    .build();
domainService.createDomain(domain);

// 查询知识域
KnowledgeDomain domain = domainService.getDomain("tech-docs");

// 列出所有域
List<KnowledgeDomain> domains = domainService.listAllDomains();
```

### 功能4：智能路由

```java
@Autowired
private DomainRouter domainRouter;

// 自动路由查询
String query = "如何配置Spring Boot？";
QueryRouteResult result = domainRouter.route(query);

System.out.println("推荐域: " + result.getDomainIds());
System.out.println("推荐角色: " + result.getRoleIds());
System.out.println("跨域查询: " + result.isCrossDomain());
```

---

## 常见问题

### Q1: 启动报错 "java.lang.OutOfMemoryError"

**解决方案**：增加JVM内存

```bash
# 在启动命令中添加内存参数
mvn spring-boot:run -Dspring-boot.run.jvmArguments="-Xmx2g"
```

### Q2: 文档处理失败

**可能原因**：
1. 文件格式不支持
2. 文件损坏
3. 编码问题

**解决方案**：
```java
try {
    ExtractionResult result = documentProcessor.extractText(file);
} catch (UnsupportedFormatException e) {
    System.out.println("不支持的格式: " + e.getMessage());
} catch (Exception e) {
    System.out.println("处理失败: " + e.getMessage());
}
```

### Q3: 搜索结果为空

**检查清单**：
1. ✅ 是否已索引文档？
2. ✅ 查询关键词是否存在？
3. ✅ 知识域ID是否正确？

**调试代码**：
```java
// 检查索引数量
IndexStatistics stats = ragService.getStatistics();
System.out.println("已索引文档数: " + stats.getDocumentCount());

// 使用更宽松的搜索
List<Document> results = ragService.search(query, 20); // 增加返回数量
```

### Q4: 如何启用AI功能（可选）

需要额外配置AI服务：

```yaml
# 方案1：使用本地Ollama
omni:
  ai:
    ollama:
      enabled: true
      base-url: http://localhost:11434
      model: qwen2.5:0.5b

# 方案2：使用在线API
omni:
  ai:
    online:
      enabled: true
      api-key: your-api-key
      provider: openai
      model: gpt-3.5-turbo
```

### Q5: 如何启用向量检索（可选）

需要配置ONNX Embedding：

```yaml
embedding:
  onnx:
    enabled: true
    model-path: ./models/bge-base-zh-v1.5/model.onnx
    max-sequence-length: 512
```

下载模型（约200MB）：
```bash
# 创建模型目录
mkdir -p models/bge-base-zh-v1.5

# 下载模型文件（示例URL）
# wget https://huggingface.co/BAAI/bge-base-zh-v1.5/resolve/main/onnx/model.onnx \
#   -O models/bge-base-zh-v1.5/model.onnx
```

---

## 🎯 下一步

### 学习更多

- **架构设计** - 了解知识域隔离架构
- **高级配置** - 自定义分块策略、RAG参数
- **生产部署** - 使用Docker、K8s部署
- **性能优化** - 索引优化、缓存配置

### 实战项目

1. **企业知识库** - 管理公司文档
2. **技术文档助手** - API文档问答
3. **合同分析系统** - 合同审核助手
4. **代码知识库** - 源码分析与检索

### 获取帮助

- **GitHub Issues**: [提交问题](https://github.com/jinhua10/omni-agent/issues)
- **Email**: 1015770492@qq.com

---

## 📝 完整示例代码

将以下代码保存为 `QuickStartExample.java`：

```java
package com.example.quickstart;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import top.yumbo.ai.omni.chunking.api.ChunkingService;
import top.yumbo.ai.omni.chunking.api.model.Chunk;
import top.yumbo.ai.omni.processor.api.DocumentProcessor;
import top.yumbo.ai.omni.processor.api.model.ExtractionResult;
import top.yumbo.ai.omni.rag.api.RagService;
import top.yumbo.ai.omni.rag.api.model.Document;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Slf4j
@SpringBootApplication
public class QuickStartExample implements CommandLineRunner {

    @Autowired
    private DocumentProcessor documentProcessor;
    
    @Autowired
    private ChunkingService chunkingService;
    
    @Autowired
    private RagService ragService;

    public static void main(String[] args) {
        SpringApplication.run(QuickStartExample.class, args);
    }

    @Override
    public void run(String... args) throws Exception {
        log.info("=== OmniAgent 快速开始示例 ===");
        
        // 示例1：处理文档
        processDocumentExample();
        
        // 示例2：搜索文档
        searchExample();
        
        log.info("=== 示例运行完成 ===");
    }
    
    /**
     * 示例1：处理文档
     */
    private void processDocumentExample() {
        try {
            // 准备测试文本（实际使用中替换为真实文件）
            String testText = "这是一个测试文档。\n\n" +
                "OmniAgent是一个智能知识管理平台。\n" +
                "它支持多种文档格式，包括PDF、Word、Excel等。\n\n" +
                "系统采用知识域隔离架构，每个领域独立管理。";
            
            log.info("步骤1：提取文本 (模拟)");
            log.info("文本长度: {} 字符", testText.length());
            
            log.info("步骤2：智能分块");
            List<Chunk> chunks = chunkingService.chunk(testText);
            log.info("分块数量: {}", chunks.size());
            
            log.info("步骤3：索引到RAG");
            List<Document> docs = new ArrayList<>();
            for (int i = 0; i < chunks.size(); i++) {
                Chunk chunk = chunks.get(i);
                Document doc = Document.builder()
                    .id("doc-" + i)
                    .content(chunk.getText())
                    .build();
                docs.add(doc);
                log.info("分块 {}: {}", i+1, chunk.getText().substring(0, 
                    Math.min(50, chunk.getText().length())) + "...");
            }
            ragService.batchIndex(docs);
            log.info("✅ 索引完成！");
            
        } catch (Exception e) {
            log.error("处理失败", e);
        }
    }
    
    /**
     * 示例2：搜索文档
     */
    private void searchExample() {
        try {
            String query = "知识管理";
            log.info("搜索关键词: {}", query);
            
            List<Document> results = ragService.search(query, 3);
            log.info("找到 {} 个结果", results.size());
            
            for (int i = 0; i < results.size(); i++) {
                Document doc = results.get(i);
                String preview = doc.getContent().substring(0, 
                    Math.min(100, doc.getContent().length()));
                log.info("结果 {}: {}...", i+1, preview);
            }
            
        } catch (Exception e) {
            log.error("搜索失败", e);
        }
    }
}
```

运行示例：
```bash
mvn spring-boot:run
```

---

<div align="center">

**🎉 开始你的OmniAgent之旅！**

有问题？查看 [GitHub Issues](https://github.com/jinhua10/omni-agent/issues)

</div>

