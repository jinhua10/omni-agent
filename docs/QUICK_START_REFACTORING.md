# Omni-Agent 知识网络重构 - 快速开始指南

> 从当前单一RAG系统迁移到知识网络架构的实施手册

---

## 🚀 Phase 1: 基础架构重构（立即开始）

### Step 1: 创建新的实体类

#### 1.1 KnowledgeDomain 实体

**位置：** `omni-agent-core/src/main/java/top/yumbo/ai/core/domain/`

```java
package top.yumbo.ai.core.domain;

import lombok.Data;
import jakarta.persistence.*;
import java.time.LocalDateTime;
import java.util.Map;

@Data
@Entity
@Table(name = "knowledge_domains")
public class KnowledgeDomain {
    
    @Id
    private String domainId;
    
    @Column(nullable = false)
    private String domainName;
    
    @Enumerated(EnumType.STRING)
    @Column(nullable = false)
    private DomainType domainType;
    
    @Column(length = 1000)
    private String description;
    
    @Column(nullable = false)
    private String storagePath;
    
    @Column(nullable = false)
    private String ragIndexPath;
    
    @Column(columnDefinition = "TEXT")
    private String configJson;  // JSON格式的配置
    
    @Enumerated(EnumType.STRING)
    private DomainStatus status = DomainStatus.ACTIVE;
    
    private String linkedEntityId;  // 关联的实体ID（角色/项目）
    
    @Column(nullable = false, updatable = false)
    private LocalDateTime createdAt = LocalDateTime.now();
    
    private LocalDateTime updatedAt = LocalDateTime.now();
    
    @PreUpdate
    protected void onUpdate() {
        this.updatedAt = LocalDateTime.now();
    }
}
```

#### 1.2 枚举类型

```java
public enum DomainType {
    DOCUMENT("文档知识域"),
    SOURCE_CODE("源码知识域"),
    ROLE_KNOWLEDGE("角色知识域"),
    API_DOCUMENTATION("API文档域"),
    MIXED("混合域");
    
    private final String description;
    
    DomainType(String description) {
        this.description = description;
    }
    
    public String getDescription() {
        return description;
    }
}

public enum DomainStatus {
    ACTIVE,      // 活跃
    INACTIVE,    // 非活跃
    ARCHIVED,    // 已归档
    ERROR        // 错误状态
}
```

### Step 2: 创建知识域服务

#### 2.1 Repository

```java
package top.yumbo.ai.core.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import top.yumbo.ai.core.domain.KnowledgeDomain;
import top.yumbo.ai.core.domain.DomainType;
import java.util.List;

public interface KnowledgeDomainRepository extends JpaRepository<KnowledgeDomain, String> {
    
    List<KnowledgeDomain> findByDomainType(DomainType domainType);
    
    List<KnowledgeDomain> findByStatus(DomainStatus status);
    
    List<KnowledgeDomain> findByLinkedEntityId(String linkedEntityId);
}
```

#### 2.2 Service 接口

```java
package top.yumbo.ai.core.service;

import top.yumbo.ai.core.domain.KnowledgeDomain;
import top.yumbo.ai.core.domain.DomainType;
import top.yumbo.ai.rag.api.RAGService;
import java.util.List;

public interface KnowledgeDomainService {
    
    /**
     * 创建知识域
     */
    KnowledgeDomain createDomain(CreateDomainRequest request);
    
    /**
     * 获取知识域
     */
    KnowledgeDomain getDomain(String domainId);
    
    /**
     * 列出所有知识域
     */
    List<KnowledgeDomain> listDomains(DomainType type);
    
    /**
     * 获取域的RAG服务实例
     */
    RAGService getDomainRAGService(String domainId);
    
    /**
     * 删除知识域
     */
    void deleteDomain(String domainId);
}
```

#### 2.3 Request DTO

```java
package top.yumbo.ai.core.dto;

import lombok.Data;
import lombok.Builder;
import top.yumbo.ai.core.domain.DomainType;

@Data
@Builder
public class CreateDomainRequest {
    
    private String domainName;
    
    private DomainType domainType;
    
    private String description;
    
    private String linkedEntityId;  // 可选：关联的角色ID或项目ID
    
    // RAG配置
    private String ragBackend = "lucene";  // lucene, mongodb, elasticsearch
    
    private Map<String, Object> ragConfig;
}
```

### Step 3: 实现RAG服务工厂

```java
package top.yumbo.ai.core.service;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.file.FileRAGService;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

@Slf4j
@Component
public class RAGServiceFactory {
    
    private final Map<String, RAGService> domainRAGServices = new ConcurrentHashMap<>();
    
    /**
     * 获取或创建域的RAG服务
     */
    public RAGService getOrCreate(String domainId, String indexPath, String backend) {
        return domainRAGServices.computeIfAbsent(domainId, id -> {
            log.info("为域 {} 创建 RAG 服务，后端：{}", domainId, backend);
            return createRAGService(indexPath, backend);
        });
    }
    
    /**
     * 创建RAG服务实例
     */
    private RAGService createRAGService(String indexPath, String backend) {
        switch (backend.toLowerCase()) {
            case "lucene":
            case "file":
                return new FileRAGService(indexPath);
            
            // 后续添加其他后端
            // case "mongodb":
            //     return new MongoDBRAGService(config);
            
            default:
                throw new IllegalArgumentException("Unsupported RAG backend: " + backend);
        }
    }
    
    /**
     * 移除域的RAG服务
     */
    public void remove(String domainId) {
        RAGService service = domainRAGServices.remove(domainId);
        if (service != null) {
            log.info("移除域 {} 的 RAG 服务", domainId);
        }
    }
}
```

### Step 4: 创建目录结构

#### 4.1 目录初始化脚本

```bash
# 创建知识网络根目录
mkdir -p data/knowledge-network/domains

# 创建默认的文档域（迁移现有数据）
mkdir -p data/knowledge-network/domains/default-docs-domain/rag-index
mkdir -p data/knowledge-network/domains/default-docs-domain/storage/{documents,chunks,extracted}

# 创建域配置文件
cat > data/knowledge-network/domains/default-docs-domain/metadata.json << 'EOF'
{
  "domainId": "default-docs-domain",
  "domainName": "默认文档知识域",
  "domainType": "DOCUMENT",
  "description": "系统默认的文档知识库",
  "createdAt": "2025-12-27T00:00:00",
  "ragBackend": "lucene"
}
EOF

# 创建网络配置
cat > data/knowledge-network/network-config.json << 'EOF'
{
  "version": "1.0.0",
  "createdAt": "2025-12-27T00:00:00",
  "domains": [
    {
      "domainId": "default-docs-domain",
      "enabled": true
    }
  ]
}
EOF
```

### Step 5: 数据迁移

#### 5.1 迁移服务

```java
package top.yumbo.ai.core.migration;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import java.nio.file.*;
import java.io.IOException;

@Slf4j
@Service
public class DataMigrationService {
    
    /**
     * 迁移现有RAG数据到默认域
     */
    public void migrateExistingData() {
        log.info("开始迁移现有数据到知识网络架构...");
        
        try {
            // 1. 迁移RAG索引
            Path oldIndex = Paths.get("data/rag-index");
            Path newIndex = Paths.get("data/knowledge-network/domains/default-docs-domain/rag-index");
            
            if (Files.exists(oldIndex)) {
                copyDirectory(oldIndex, newIndex);
                log.info("✅ RAG索引迁移完成");
            }
            
            // 2. 迁移storage
            Path oldStorage = Paths.get("data/storage");
            Path newStorage = Paths.get("data/knowledge-network/domains/default-docs-domain/storage");
            
            if (Files.exists(oldStorage)) {
                copyDirectory(oldStorage, newStorage);
                log.info("✅ Storage迁移完成");
            }
            
            // 3. 创建默认域记录
            createDefaultDomain();
            
            log.info("✅ 数据迁移完成！");
            
        } catch (Exception e) {
            log.error("数据迁移失败", e);
            throw new RuntimeException("Migration failed", e);
        }
    }
    
    private void copyDirectory(Path source, Path target) throws IOException {
        Files.walk(source)
            .forEach(src -> {
                try {
                    Path dest = target.resolve(source.relativize(src));
                    if (Files.isDirectory(src)) {
                        Files.createDirectories(dest);
                    } else {
                        Files.copy(src, dest, StandardCopyOption.REPLACE_EXISTING);
                    }
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
    }
    
    private void createDefaultDomain() {
        // 在数据库中创建默认域记录
        KnowledgeDomain domain = new KnowledgeDomain();
        domain.setDomainId("default-docs-domain");
        domain.setDomainName("默认文档知识域");
        domain.setDomainType(DomainType.DOCUMENT);
        domain.setDescription("系统默认的文档知识库（从旧架构迁移）");
        domain.setStoragePath("data/knowledge-network/domains/default-docs-domain/storage");
        domain.setRagIndexPath("data/knowledge-network/domains/default-docs-domain/rag-index");
        domain.setStatus(DomainStatus.ACTIVE);
        
        domainRepository.save(domain);
        log.info("✅ 默认域记录已创建");
    }
}
```

### Step 6: 更新现有API

#### 6.1 兼容性适配器

```java
package top.yumbo.ai.core.adapter;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.rag.api.RAGService;

/**
 * RAG服务适配器 - 保持向后兼容
 */
@Slf4j
@Service
public class RAGServiceAdapter {
    
    private final KnowledgeDomainService domainService;
    private final String DEFAULT_DOMAIN_ID = "default-docs-domain";
    
    /**
     * 获取默认的RAG服务（兼容旧API）
     */
    public RAGService getDefaultRAGService() {
        return domainService.getDomainRAGService(DEFAULT_DOMAIN_ID);
    }
    
    /**
     * 智能路由到合适的域
     */
    public RAGService getRAGServiceForQuery(String query) {
        // 简单实现：先使用默认域
        // 后续可以添加智能路由逻辑
        return getDefaultRAGService();
    }
}
```

### Step 7: 添加管理API

```java
package top.yumbo.ai.omni.web.controller;

import lombok.RequiredArgsConstructor;
import org.springframework.web.bind.annotation.*;
import top.yumbo.ai.core.domain.KnowledgeDomain;
import top.yumbo.ai.core.service.KnowledgeDomainService;

@RestController
@RequestMapping("/api/knowledge-domains")
@RequiredArgsConstructor
public class KnowledgeDomainController {
    
    private final KnowledgeDomainService domainService;
    
    /**
     * 创建知识域
     */
    @PostMapping
    public KnowledgeDomain createDomain(@RequestBody CreateDomainRequest request) {
        return domainService.createDomain(request);
    }
    
    /**
     * 列出所有知识域
     */
    @GetMapping
    public List<KnowledgeDomain> listDomains(
        @RequestParam(required = false) DomainType type
    ) {
        return domainService.listDomains(type);
    }
    
    /**
     * 获取知识域详情
     */
    @GetMapping("/{domainId}")
    public KnowledgeDomain getDomain(@PathVariable String domainId) {
        return domainService.getDomain(domainId);
    }
    
    /**
     * 删除知识域
     */
    @DeleteMapping("/{domainId}")
    public void deleteDomain(@PathVariable String domainId) {
        domainService.deleteDomain(domainId);
    }
}
```

---

## 🧪 测试计划

### 测试1: 创建知识域

```bash
curl -X POST http://localhost:8080/api/knowledge-domains \
  -H "Content-Type: application/json" \
  -d '{
    "domainName": "测试文档域",
    "domainType": "DOCUMENT",
    "description": "用于测试的文档知识域",
    "ragBackend": "lucene"
  }'
```

### 测试2: 列出知识域

```bash
curl http://localhost:8080/api/knowledge-domains
```

### 测试3: 向新域索引文档

```bash
curl -X POST http://localhost:8080/api/knowledge-domains/{domainId}/index \
  -H "Content-Type: application/json" \
  -d '{
    "documentId": "test-doc-1",
    "content": "这是测试文档的内容",
    "metadata": {
      "title": "测试文档"
    }
  }'
```

---

## 📋 检查清单

- [ ] 数据库添加 `knowledge_domains` 表
- [ ] 创建 `KnowledgeDomain` 实体
- [ ] 创建 `KnowledgeDomainRepository`
- [ ] 实现 `RAGServiceFactory`
- [ ] 实现 `KnowledgeDomainService`
- [ ] 创建 `data/knowledge-network` 目录结构
- [ ] 实现数据迁移脚本
- [ ] 添加管理API接口
- [ ] 更新前端UI（添加知识域管理页面）
- [ ] 编写单元测试
- [ ] 编写集成测试
- [ ] 更新文档

---

## 🎯 下一步（Phase 2）

完成Phase 1后，继续：

1. **角色知识库系统**
   - 创建 `KnowledgeRole` 实体
   - 实现角色创建和管理
   - 实现角色学习功能

2. **前端UI**
   - 知识域管理界面
   - 可视化知识网络
   - 角色管理界面

3. **智能路由**
   - 实现领域路由器
   - 意图识别
   - 跨域查询

---

## 💡 提示

1. **渐进式迁移**：保持向后兼容，逐步迁移功能
2. **数据备份**：迁移前备份 `data` 目录
3. **测试优先**：每个功能先写测试
4. **文档同步**：及时更新API文档

---

**准备好了吗？开始Phase 1的实施！** 🚀

