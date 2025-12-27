# Omni-Agent 知识网络重构 - 快速开始指南

> 从当前单一RAG系统迁移到知识网络架构的实施手册

---

## 🚀 Phase 1: 基础架构重构（立即开始）

### Step 1: 创建新的实体类和存储接口

#### 1.1 KnowledgeDomain 实体（纯POJO，无存储依赖）

**位置：** `omni-agent-core/src/main/java/top/yumbo/ai/core/domain/`

```java
package top.yumbo.ai.core.domain;

import lombok.Data;
import lombok.Builder;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.io.Serializable;
import java.time.LocalDateTime;
import java.util.Map;

/**
 * 知识域实体
 * 注意：这是一个纯 POJO，不依赖任何特定的存储实现
 * 可以存储在 File, MongoDB, Redis, Elasticsearch 等任何后端
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class KnowledgeDomain implements Serializable {
    
    private static final long serialVersionUID = 1L;
    
    /**
     * 域ID（主键）
     */
    private String domainId;
    
    /**
     * 域名称
     */
    private String domainName;
    
    /**
     * 域类型
     */
    private DomainType domainType;
    
    /**
     * 描述
     */
    private String description;
    
    /**
     * 存储路径（文件系统路径或逻辑路径）
     */
    private String storagePath;
    
    /**
     * RAG索引路径
     */
    private String ragIndexPath;
    
    /**
     * 配置信息（JSON格式）
     */
    private Map<String, Object> config;
    
    /**
     * 状态
     */
    private DomainStatus status;
    
    /**
     * 关联的实体ID（角色/项目）
     */
    private String linkedEntityId;
    
    /**
     * 创建时间
     */
    private LocalDateTime createdAt;
    
    /**
     * 更新时间
     */
    private LocalDateTime updatedAt;
    
    /**
     * 创建前设置默认值
     */
    public void prePersist() {
        if (createdAt == null) {
            createdAt = LocalDateTime.now();
        }
        if (updatedAt == null) {
            updatedAt = LocalDateTime.now();
        }
        if (status == null) {
            status = DomainStatus.ACTIVE;
        }
    }
    
    /**
     * 更新前设置更新时间
     */
    public void preUpdate() {
        updatedAt = LocalDateTime.now();
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

#### 1.3 存储接口抽象

**位置：** `omni-agent-core/src/main/java/top/yumbo/ai/core/persistence/`

```java
package top.yumbo.ai.core.persistence;

import top.yumbo.ai.core.domain.KnowledgeDomain;
import top.yumbo.ai.core.domain.DomainType;
import top.yumbo.ai.core.domain.DomainStatus;
import java.util.List;
import java.util.Optional;

/**
 * 知识域持久化接口
 * 
 * 这是一个存储无关的抽象接口，可以有多种实现：
 * - KnowledgeDomainFileStorage（基于文件存储）
 * - KnowledgeDomainMongoStorage（基于MongoDB）
 * - KnowledgeDomainRedisStorage（基于Redis）
 * - KnowledgeDomainElasticsearchStorage（基于ES）
 * 
 * 通过 Spring Boot Starter 模式选择具体实现
 */
public interface KnowledgeDomainPersistence {
    
    /**
     * 保存知识域
     */
    String save(KnowledgeDomain domain);
    
    /**
     * 根据ID查找
     */
    Optional<KnowledgeDomain> findById(String domainId);
    
    /**
     * 根据类型查找
     */
    List<KnowledgeDomain> findByType(DomainType type);
    
    /**
     * 根据状态查找
     */
    List<KnowledgeDomain> findByStatus(DomainStatus status);
    
    /**
     * 根据关联实体ID查找
     */
    List<KnowledgeDomain> findByLinkedEntityId(String linkedEntityId);
    
    /**
     * 查找所有
     */
    List<KnowledgeDomain> findAll();
    
    /**
     * 更新知识域
     */
    boolean update(KnowledgeDomain domain);
    
    /**
     * 删除知识域
     */
    boolean delete(String domainId);
    
    /**
     * 检查域是否存在
     */
    boolean exists(String domainId);
    
    /**
     * 统计域数量
     */
    long count();
}
```

#### 1.4 默认实现：基于文件的存储

**位置：** `omni-agent-core/src/main/java/top/yumbo/ai/core/persistence/impl/`

```java
package top.yumbo.ai.core.persistence.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.core.domain.KnowledgeDomain;
import top.yumbo.ai.core.domain.DomainType;
import top.yumbo.ai.core.domain.DomainStatus;
import top.yumbo.ai.core.persistence.KnowledgeDomainPersistence;

import java.io.File;
import java.io.IOException;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 基于文件的知识域存储实现
 * 
 * 存储结构：
 * data/knowledge-network/
 *   ├── registry/
 *   │   ├── domain-1.json
 *   │   ├── domain-2.json
 *   │   └── ...
 *   └── domains/
 *       ├── domain-1/
 *       └── domain-2/
 */
@Slf4j
public class FileKnowledgeDomainPersistence implements KnowledgeDomainPersistence {
    
    private final String registryPath;
    private final ObjectMapper objectMapper;
    
    public FileKnowledgeDomainPersistence(String registryPath) {
        this.registryPath = registryPath;
        this.objectMapper = new ObjectMapper();
        this.objectMapper.registerModule(new JavaTimeModule());
        
        // 确保目录存在
        try {
            Files.createDirectories(Paths.get(registryPath));
        } catch (IOException e) {
            throw new RuntimeException("Failed to create registry directory", e);
        }
    }
    
    @Override
    public String save(KnowledgeDomain domain) {
        domain.prePersist();
        
        String fileName = domain.getDomainId() + ".json";
        Path filePath = Paths.get(registryPath, fileName);
        
        try {
            objectMapper.writerWithDefaultPrettyPrinter()
                .writeValue(filePath.toFile(), domain);
            log.info("✅ 保存知识域: {}", domain.getDomainId());
            return domain.getDomainId();
        } catch (IOException e) {
            log.error("保存知识域失败: {}", domain.getDomainId(), e);
            throw new RuntimeException("Failed to save domain", e);
        }
    }
    
    @Override
    public Optional<KnowledgeDomain> findById(String domainId) {
        Path filePath = Paths.get(registryPath, domainId + ".json");
        
        if (!Files.exists(filePath)) {
            return Optional.empty();
        }
        
        try {
            KnowledgeDomain domain = objectMapper.readValue(
                filePath.toFile(), 
                KnowledgeDomain.class
            );
            return Optional.of(domain);
        } catch (IOException e) {
            log.error("读取知识域失败: {}", domainId, e);
            return Optional.empty();
        }
    }
    
    @Override
    public List<KnowledgeDomain> findByType(DomainType type) {
        return findAll().stream()
            .filter(d -> d.getDomainType() == type)
            .collect(Collectors.toList());
    }
    
    @Override
    public List<KnowledgeDomain> findByStatus(DomainStatus status) {
        return findAll().stream()
            .filter(d -> d.getStatus() == status)
            .collect(Collectors.toList());
    }
    
    @Override
    public List<KnowledgeDomain> findByLinkedEntityId(String linkedEntityId) {
        return findAll().stream()
            .filter(d -> linkedEntityId.equals(d.getLinkedEntityId()))
            .collect(Collectors.toList());
    }
    
    @Override
    public List<KnowledgeDomain> findAll() {
        try {
            return Files.list(Paths.get(registryPath))
                .filter(p -> p.toString().endsWith(".json"))
                .map(p -> {
                    try {
                        return objectMapper.readValue(
                            p.toFile(), 
                            KnowledgeDomain.class
                        );
                    } catch (IOException e) {
                        log.warn("读取域文件失败: {}", p, e);
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
        } catch (IOException e) {
            log.error("列出知识域失败", e);
            return Collections.emptyList();
        }
    }
    
    @Override
    public boolean update(KnowledgeDomain domain) {
        domain.preUpdate();
        
        Path filePath = Paths.get(registryPath, domain.getDomainId() + ".json");
        
        if (!Files.exists(filePath)) {
            log.warn("域不存在，无法更新: {}", domain.getDomainId());
            return false;
        }
        
        try {
            objectMapper.writerWithDefaultPrettyPrinter()
                .writeValue(filePath.toFile(), domain);
            log.info("✅ 更新知识域: {}", domain.getDomainId());
            return true;
        } catch (IOException e) {
            log.error("更新知识域失败: {}", domain.getDomainId(), e);
            return false;
        }
    }
    
    @Override
    public boolean delete(String domainId) {
        Path filePath = Paths.get(registryPath, domainId + ".json");
        
        try {
            boolean deleted = Files.deleteIfExists(filePath);
            if (deleted) {
                log.info("✅ 删除知识域: {}", domainId);
            }
            return deleted;
        } catch (IOException e) {
            log.error("删除知识域失败: {}", domainId, e);
            return false;
        }
    }
    
    @Override
    public boolean exists(String domainId) {
        return Files.exists(Paths.get(registryPath, domainId + ".json"));
    }
    
    @Override
    public long count() {
        try {
            return Files.list(Paths.get(registryPath))
                .filter(p -> p.toString().endsWith(".json"))
                .count();
        } catch (IOException e) {
            log.error("统计知识域数量失败", e);
            return 0;
        }
    }
}
```

### Step 2: 创建知识域服务

#### 2.1 Service 接口

**位置：** `omni-agent-core/src/main/java/top/yumbo/ai/core/service/`

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
     * 更新知识域
     */
    KnowledgeDomain updateDomain(String domainId, UpdateDomainRequest request);
    
    /**
     * 删除知识域
     */
    void deleteDomain(String domainId);
}
```

#### 2.2 Service 实现

```java
package top.yumbo.ai.core.service.impl;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.core.domain.*;
import top.yumbo.ai.core.dto.*;
import top.yumbo.ai.core.persistence.KnowledgeDomainPersistence;
import top.yumbo.ai.core.service.KnowledgeDomainService;
import top.yumbo.ai.rag.api.RAGService;

import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.List;
import java.util.UUID;

@Slf4j
@Service
@RequiredArgsConstructor
public class KnowledgeDomainServiceImpl implements KnowledgeDomainService {
    
    private final KnowledgeDomainPersistence domainPersistence;
    private final RAGServiceFactory ragServiceFactory;
    
    @Override
    public KnowledgeDomain createDomain(CreateDomainRequest request) {
        log.info("创建知识域: {}", request.getDomainName());
        
        // 生成域ID
        String domainId = UUID.randomUUID().toString();
        
        // 构建存储路径
        String basePath = "data/knowledge-network/domains/" + domainId;
        String storagePath = basePath + "/storage";
        String ragIndexPath = basePath + "/rag-index";
        
        // 创建目录结构
        try {
            Files.createDirectories(Paths.get(storagePath + "/documents"));
            Files.createDirectories(Paths.get(storagePath + "/chunks"));
            Files.createDirectories(Paths.get(storagePath + "/extracted"));
            Files.createDirectories(Paths.get(ragIndexPath));
        } catch (Exception e) {
            throw new RuntimeException("Failed to create domain directories", e);
        }
        
        // 创建域实体
        KnowledgeDomain domain = KnowledgeDomain.builder()
            .domainId(domainId)
            .domainName(request.getDomainName())
            .domainType(request.getDomainType())
            .description(request.getDescription())
            .storagePath(storagePath)
            .ragIndexPath(ragIndexPath)
            .config(request.getRagConfig())
            .status(DomainStatus.ACTIVE)
            .linkedEntityId(request.getLinkedEntityId())
            .build();
        
        // 保存到存储
        domainPersistence.save(domain);
        
        log.info("✅ 知识域创建成功: {} ({})", domain.getDomainName(), domainId);
        return domain;
    }
    
    @Override
    public KnowledgeDomain getDomain(String domainId) {
        return domainPersistence.findById(domainId)
            .orElseThrow(() -> new NotFoundException("Domain not found: " + domainId));
    }
    
    @Override
    public List<KnowledgeDomain> listDomains(DomainType type) {
        if (type == null) {
            return domainPersistence.findAll();
        }
        return domainPersistence.findByType(type);
    }
    
    @Override
    public RAGService getDomainRAGService(String domainId) {
        KnowledgeDomain domain = getDomain(domainId);
        
        String backend = (String) domain.getConfig()
            .getOrDefault("ragBackend", "lucene");
        
        return ragServiceFactory.getOrCreate(
            domainId, 
            domain.getRagIndexPath(), 
            backend
        );
    }
    
    @Override
    public KnowledgeDomain updateDomain(String domainId, UpdateDomainRequest request) {
        KnowledgeDomain domain = getDomain(domainId);
        
        if (request.getDomainName() != null) {
            domain.setDomainName(request.getDomainName());
        }
        if (request.getDescription() != null) {
            domain.setDescription(request.getDescription());
        }
        if (request.getStatus() != null) {
            domain.setStatus(request.getStatus());
        }
        
        domainPersistence.update(domain);
        log.info("✅ 知识域更新成功: {}", domainId);
        
        return domain;
    }
    
    @Override
    public void deleteDomain(String domainId) {
        // 移除RAG服务
        ragServiceFactory.remove(domainId);
        
        // 删除存储记录
        domainPersistence.delete(domainId);
        
        // TODO: 可选，删除文件系统中的数据
        
        log.info("✅ 知识域删除成功: {}", domainId);
    }
}

class NotFoundException extends RuntimeException {
    public NotFoundException(String message) {
        super(message);
    }
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

### Phase 1: 基础架构（File Starter）

- [ ] 创建 `KnowledgeDomain` 实体（纯POJO）
- [ ] 创建 `DomainType` 和 `DomainStatus` 枚举
- [ ] 创建 `KnowledgeDomainPersistence` 接口
- [ ] 实现 `FileKnowledgeDomainPersistence`（基于JSON文件）
- [ ] 创建 `omni-agent-knowledge-domain-starter-file` 模块
- [ ] 实现 `FileKnowledgeDomainAutoConfiguration`
- [ ] 实现 `RAGServiceFactory`
- [ ] 实现 `KnowledgeDomainService`
- [ ] 创建 `data/knowledge-network` 目录结构
- [ ] 实现数据迁移脚本 `DataMigrationService`
- [ ] 添加管理API接口 `KnowledgeDomainController`
- [ ] 编写单元测试
- [ ] 编写集成测试
- [ ] 更新文档

### Phase 2: 扩展存储后端（可选）

- [ ] 实现 `omni-agent-knowledge-domain-starter-mongodb`
- [ ] 实现 `omni-agent-knowledge-domain-starter-redis`
- [ ] 实现 `omni-agent-knowledge-domain-starter-elasticsearch`
- [ ] 实现 `omni-agent-knowledge-domain-starter-sqlite`
- [ ] 实现 `omni-agent-knowledge-domain-starter-h2`

### UI 和前端

- [ ] 更新前端UI（添加知识域管理页面）
- [ ] 实现知识网络可视化

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

