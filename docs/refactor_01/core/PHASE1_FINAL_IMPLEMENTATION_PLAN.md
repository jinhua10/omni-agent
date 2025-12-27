# Phase 1 实施方案 - 最终版

> 创建新的 Starter 模块，保持架构清晰，便于后续优化

---

## 🎯 策略调整

### 原因

虽然复用现有 Persistence 可以节省模块数量，但考虑到：
1. **影响范围小**：新模块独立，不影响现有代码
2. **便于迁移**：后续可以逐步将现有代码迁移过来
3. **职责清晰**：知识域存储有专门的模块，语义更清晰
4. **可以删除**：如果后续发现不需要，可以轻松删除

### 决策

✅ **创建新的 Starter 模块**（但采用优雅的设计）

---

## 📦 新模块规划

### 优先级 P0: 基础模块（立即创建）

#### 1. omni-agent-knowledge-registry-api

**作用：** 定义知识注册表的统一接口

```
omni-agent-knowledge-registry-api/
├── pom.xml
└── src/main/java/top/yumbo/ai/knowledge/registry/
    ├── KnowledgeRegistry.java              # 核心接口
    ├── model/
    │   ├── KnowledgeDomain.java           # 知识域实体
    │   ├── KnowledgeRole.java             # 知识角色实体
    │   ├── SourceProject.java             # 源码项目实体
    │   ├── DomainType.java                # 域类型枚举
    │   └── DomainStatus.java              # 域状态枚举
    └── exception/
        └── KnowledgeRegistryException.java
```

**核心接口：**
```java
package top.yumbo.ai.knowledge.registry;

import java.util.List;
import java.util.Optional;

/**
 * 知识注册表接口
 * 用于存储和管理知识网络中的元数据
 */
public interface KnowledgeRegistry {
    
    // ========== 知识域管理 ==========
    
    /**
     * 保存知识域
     */
    String saveDomain(KnowledgeDomain domain);
    
    /**
     * 查找知识域
     */
    Optional<KnowledgeDomain> findDomainById(String domainId);
    
    /**
     * 列出所有知识域
     */
    List<KnowledgeDomain> findAllDomains();
    
    /**
     * 根据类型查找域
     */
    List<KnowledgeDomain> findDomainsByType(DomainType type);
    
    /**
     * 更新知识域
     */
    boolean updateDomain(KnowledgeDomain domain);
    
    /**
     * 删除知识域
     */
    boolean deleteDomain(String domainId);
    
    // ========== 知识角色管理 ==========
    
    String saveRole(KnowledgeRole role);
    Optional<KnowledgeRole> findRoleById(String roleId);
    List<KnowledgeRole> findAllRoles();
    boolean updateRole(KnowledgeRole role);
    boolean deleteRole(String roleId);
    
    // ========== 源码项目管理 ==========
    
    String saveProject(SourceProject project);
    Optional<SourceProject> findProjectById(String projectId);
    List<SourceProject> findAllProjects();
    boolean updateProject(SourceProject project);
    boolean deleteProject(String projectId);
    
    // ========== 通用方法 ==========
    
    /**
     * 检查是否存在
     */
    boolean exists(String entityType, String entityId);
    
    /**
     * 统计数量
     */
    long count(String entityType);
}
```

#### 2. omni-agent-knowledge-registry-starter-file

**作用：** 基于 JSON 文件的默认实现

```
omni-agent-knowledge-registry-starter-file/
├── pom.xml
└── src/main/java/top/yumbo/ai/knowledge/registry/file/
    ├── FileKnowledgeRegistry.java
    ├── FileKnowledgeRegistryProperties.java
    └── FileKnowledgeRegistryAutoConfiguration.java
```

**实现：**

```java
package top.yumbo.ai.knowledge.registry.file;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;

import java.io.File;
import java.nio.file.*;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public class FileKnowledgeRegistry implements KnowledgeRegistry {

    private final String basePath;
    private final ObjectMapper objectMapper;

    public FileKnowledgeRegistry(String basePath) {
        this.basePath = basePath;
        this.objectMapper = new ObjectMapper();
        this.objectMapper.findAndRegisterModules();

        // 初始化目录
        initDirectories();
    }

    private void initDirectories() {
        try {
            Files.createDirectories(Paths.get(basePath, "domains"));
            Files.createDirectories(Paths.get(basePath, "roles"));
            Files.createDirectories(Paths.get(basePath, "projects"));
        } catch (Exception e) {
            throw new RuntimeException("Failed to create directories", e);
        }
    }

    // ========== 知识域管理 ==========

    @Override
    public String saveDomain(KnowledgeDomain domain) {
        Path filePath = Paths.get(basePath, "domains", domain.getDomainId() + ".json");

        try {
            objectMapper.writerWithDefaultPrettyPrinter()
                    .writeValue(filePath.toFile(), domain);
            log.info("✅ 保存知识域: {}", domain.getDomainName());
            return domain.getDomainId();
        } catch (Exception e) {
            log.error("保存知识域失败: {}", domain.getDomainId(), e);
            throw new RuntimeException("Failed to save domain", e);
        }
    }

    @Override
    public Optional<KnowledgeDomain> findDomainById(String domainId) {
        Path filePath = Paths.get(basePath, "domains", domainId + ".json");

        if (!Files.exists(filePath)) {
            return Optional.empty();
        }

        try {
            KnowledgeDomain domain = objectMapper.readValue(
                    filePath.toFile(),
                    KnowledgeDomain.class
            );
            return Optional.of(domain);
        } catch (Exception e) {
            log.error("读取知识域失败: {}", domainId, e);
            return Optional.empty();
        }
    }

    @Override
    public List<KnowledgeDomain> findAllDomains() {
        try {
            Path domainsDir = Paths.get(basePath, "domains");

            if (!Files.exists(domainsDir)) {
                return Collections.emptyList();
            }

            return Files.list(domainsDir)
                    .filter(p -> p.toString().endsWith(".json"))
                    .map(p -> {
                        try {
                            return objectMapper.readValue(
                                    p.toFile(),
                                    KnowledgeDomain.class
                            );
                        } catch (Exception e) {
                            log.warn("读取域文件失败: {}", p, e);
                            return null;
                        }
                    })
                    .filter(Objects::nonNull)
                    .collect(Collectors.toList());
        } catch (Exception e) {
            log.error("列出知识域失败", e);
            return Collections.emptyList();
        }
    }

    @Override
    public List<KnowledgeDomain> findDomainsByType(DomainType type) {
        return findAllDomains().stream()
                .filter(d -> d.getDomainType() == type)
                .collect(Collectors.toList());
    }

    @Override
    public boolean updateDomain(KnowledgeDomain domain) {
        return saveDomain(domain) != null;
    }

    @Override
    public boolean deleteDomain(String domainId) {
        Path filePath = Paths.get(basePath, "domains", domainId + ".json");

        try {
            boolean deleted = Files.deleteIfExists(filePath);
            if (deleted) {
                log.info("✅ 删除知识域: {}", domainId);
            }
            return deleted;
        } catch (Exception e) {
            log.error("删除知识域失败: {}", domainId, e);
            return false;
        }
    }

    // ========== 角色和项目管理（类似实现）==========

    @Override
    public String saveRole(KnowledgeRole role) {
        // 类似 saveDomain 的实现
        return null;
    }

    @Override
    public Optional<KnowledgeRole> findRoleById(String roleId) {
        // 类似 findDomainById 的实现
        return Optional.empty();
    }

    // ... 其他方法

    @Override
    public boolean exists(String entityType, String entityId) {
        Path filePath = Paths.get(basePath, entityType + "s", entityId + ".json");
        return Files.exists(filePath);
    }

    @Override
    public long count(String entityType) {
        try {
            Path dir = Paths.get(basePath, entityType + "s");
            if (!Files.exists(dir)) {
                return 0;
            }
            return Files.list(dir)
                    .filter(p -> p.toString().endsWith(".json"))
                    .count();
        } catch (Exception e) {
            return 0;
        }
    }
}
```

**自动配置：**
```java
package top.yumbo.ai.knowledge.registry.file;

import org.springframework.boot.autoconfigure.condition.ConditionalOnMissingBean;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;

@Configuration
@ConditionalOnProperty(
    prefix = "omni-agent.knowledge-registry",
    name = "type",
    havingValue = "file",
    matchIfMissing = true  // 默认使用 file
)
@EnableConfigurationProperties(FileKnowledgeRegistryProperties.class)
public class FileKnowledgeRegistryAutoConfiguration {
    
    @Bean
    @ConditionalOnMissingBean(KnowledgeRegistry.class)
    public KnowledgeRegistry knowledgeRegistry(FileKnowledgeRegistryProperties properties) {
        return new FileKnowledgeRegistry(properties.getBasePath());
    }
}
```

**配置类：**
```java
package top.yumbo.ai.knowledge.registry.file;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;

@Data
@ConfigurationProperties(prefix = "omni-agent.knowledge-registry.file")
public class FileKnowledgeRegistryProperties {
    
    /**
     * 注册表文件存储路径
     */
    private String basePath = "data/knowledge-network/registry";
}
```

**spring.factories：**
```properties
# src/main/resources/META-INF/spring.factories
org.springframework.boot.autoconfigure.EnableAutoConfiguration=\
  top.yumbo.ai.omni.knowledge.registry.file.FileKnowledgeRegistryAutoConfiguration
```

---

## 🔧 使用方式

### 1. 添加依赖

```xml
<!-- API -->
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-knowledge-registry-api</artifactId>
</dependency>

<!-- File Starter（默认实现）-->
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-knowledge-registry-starter-file</artifactId>
</dependency>
```

### 2. 配置

```yaml
# application.yml
omni-agent:
  knowledge-registry:
    type: file  # 可选：file, mongodb, redis（后续）
    file:
      base-path: data/knowledge-network/registry
```

### 3. 使用

```java
@Service
@RequiredArgsConstructor
public class KnowledgeDomainService {
    
    private final KnowledgeRegistry knowledgeRegistry;  // 自动注入
    
    public KnowledgeDomain createDomain(CreateDomainRequest request) {
        KnowledgeDomain domain = KnowledgeDomain.builder()
            .domainId(UUID.randomUUID().toString())
            .domainName(request.getDomainName())
            .domainType(request.getDomainType())
            .description(request.getDescription())
            .build();
        
        knowledgeRegistry.saveDomain(domain);
        
        return domain;
    }
    
    public KnowledgeDomain getDomain(String domainId) {
        return knowledgeRegistry.findDomainById(domainId)
            .orElseThrow(() -> new NotFoundException("Domain not found"));
    }
}
```

---

## 📋 实施清单

### Phase 1.1: 创建 API 模块（2天）

- [ ] 创建 `omni-agent-knowledge-registry-api` 模块
- [ ] 定义 `KnowledgeRegistry` 接口
- [ ] 创建实体类（KnowledgeDomain, KnowledgeRole, SourceProject）
- [ ] 创建枚举类（DomainType, DomainStatus）
- [ ] 编写 JavaDoc

### Phase 1.2: 创建 File Starter（2天）

- [ ] 创建 `omni-agent-knowledge-registry-starter-file` 模块
- [ ] 实现 `FileKnowledgeRegistry`
- [ ] 实现 `FileKnowledgeRegistryAutoConfiguration`
- [ ] 创建 `FileKnowledgeRegistryProperties`
- [ ] 添加 `spring.factories`
- [ ] 编写单元测试

### Phase 1.3: 集成到 Core（1天）

- [ ] 在 `omni-agent-core` 中引入依赖
- [ ] 创建 `KnowledgeDomainService`
- [ ] 创建 `KnowledgeDomainController`
- [ ] 编写集成测试

### Phase 1.4: 文档和示例（1天）

- [ ] 更新 README
- [ ] 编写使用文档
- [ ] 创建配置示例

**总计：6 天**

---

## 🎁 优势

### 1. 职责清晰

```
omni-agent-knowledge-registry-*  ← 专门用于知识注册表
omni-agent-persistence-*         ← 专门用于问题分类等配置
```

### 2. 语义明确

```java
KnowledgeRegistry knowledgeRegistry;  // ✅ 一目了然
knowledgeRegistry.saveDomain(domain);  // ✅ 语义清晰
```

### 3. 独立演化

- 新模块不影响现有代码
- 后续可以优化或重构
- 不需要的话可以直接删除

### 4. 便于迁移

- 现有 Persistence 代码可以逐步迁移
- 或者继续保持两套并存
- 灵活选择

---

## 🔄 后续优化路径

### 选项 1: 保持独立

```
继续维护两套模块：
- omni-agent-knowledge-registry-*  （知识网络）
- omni-agent-persistence-*         （问题分类等）
```

### 选项 2: 逐步迁移

```
将 Persistence 的代码逐步迁移到 KnowledgeRegistry：
1. 问题分类也使用 KnowledgeRegistry
2. 统一接口
3. 删除旧模块
```

### 选项 3: 两种方式并存

```
根据场景选择：
- 知识网络：使用 KnowledgeRegistry
- 传统配置：使用 Persistence
```

**灵活选择，后续可以调整！**

---

## 📊 模块数量

```
当前：44 个模块
新增：2 个模块
  ├── omni-agent-knowledge-registry-api
  └── omni-agent-knowledge-registry-starter-file

总计：46 个模块  ← 可接受的增长
```

如果后续扩展：
```
可选扩展：
  ├── omni-agent-knowledge-registry-starter-mongodb
  ├── omni-agent-knowledge-registry-starter-redis
  └── omni-agent-knowledge-registry-starter-elasticsearch

最多：46 + 3 = 49 个模块
```

**但可以按需创建，不必一次性全部实现！**

---

## ✅ 总结

### 决策

✅ **创建新的 Knowledge Registry 模块**

### 理由

1. **职责清晰**：专门用于知识网络
2. **语义明确**：`KnowledgeRegistry` 一目了然
3. **影响可控**：独立模块，不影响现有代码
4. **便于管理**：后续可以删除或优化
5. **便于迁移**：可以逐步将现有代码迁移过来

### 实施

- **Phase 1.1-1.2**：创建 API 和 File Starter（4天）
- **Phase 1.3-1.4**：集成和文档（2天）
- **总计**：6 天

---

**准备好开始实施了吗？** 🚀

**更新时间：** 2025-12-27  
**决策：** 创建独立的 Knowledge Registry 模块  
**作者：** OmniAgent 架构最终方案

