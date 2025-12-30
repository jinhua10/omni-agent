# 🏗️ 知识模块独立化重构方案

## 📋 问题分析

### 当前架构问题

**omni-agent-core 模块混杂了太多知识库相关的代码：**

```
omni-agent-core/
├── src/main/java/top/yumbo/ai/omni/core/
│   ├── model/
│   │   ├── RefinedKnowledge.java           ❌ 应该独立
│   │   └── KnowledgeDocument.java          ❌ 应该独立
│   ├── service/knowledge/                   ❌ 应该独立
│   │   ├── KnowledgeStorageService.java
│   │   ├── KnowledgeRefinementService.java
│   │   ├── KnowledgeExtractionService.java
│   │   └── KnowledgeAssociationService.java
│   ├── knowledge/                           ❌ 应该独立
│   │   ├── KnowledgeLoader.java
│   │   └── network/
│   │       ├── KnowledgeNetworkManager.java
│   │       └── KnowledgeNetworkBuilder.java
│   └── service/
│       ├── domain/KnowledgeDomainService.java  ✅ 可以保留
│       └── role/RoleLearningService.java       ✅ 可以保留
```

### 设计原则违背

1. **Core 模块应该只包含核心业务编排**
2. **知识库功能应该作为独立的可插拔模块**
3. **违反了单一职责原则**

---

## 🎯 重构目标

### 1. 使用已有的 knowledge-registry-api 模块

**新增包结构：**
```
omni-agent-knowledge-registry-api/
└── src/main/java/top/yumbo/ai/omni/knowledge/registry/
    ├── model/                              # 已有：域、角色模型
    ├── network/                            # 已有：知识网络
    ├── enhancement/                        # ⭐ 新增：知识增强
    │   ├── model/
    │   │   ├── RefinedKnowledge.java      # 从 core 迁移
    │   │   └── KnowledgeDocument.java     # 从 core 迁移
    │   └── service/
    │       ├── KnowledgeRefinementService.java     # 接口
    │       ├── KnowledgeExtractionService.java     # 接口
    │       ├── KnowledgeStorageService.java        # 接口
    │       └── KnowledgeAssociationService.java    # 接口
    └── KnowledgeRegistry.java             # 已有
```

### 2. Core 模块实现接口

Core 模块提供这些接口的默认实现

---

## 📦 新模块结构

### Module 1: omni-agent-knowledge-enhancement-api

**职责：** 定义知识增强的接口和模型

```
omni-agent-knowledge-enhancement-api/
└── src/main/java/top/yumbo/ai/omni/knowledge/enhancement/
    ├── model/
    │   ├── RefinedKnowledge.java              # 从 core 迁移
    │   ├── KnowledgeDocument.java             # 从 core 迁移
    │   └── KnowledgeAssociation.java          # 新增
    ├── service/
    │   ├── KnowledgeRefinementService.java    # 接口定义
    │   ├── KnowledgeExtractionService.java    # 接口定义
    │   ├── KnowledgeStorageService.java       # 接口定义
    │   └── KnowledgeAssociationService.java   # 接口定义
    └── network/
        └── KnowledgeNetworkService.java       # 已在 registry-api 中
```

### Module 2: omni-agent-knowledge-enhancement-core

**职责：** 实现知识增强的核心逻辑

```
omni-agent-knowledge-enhancement-core/
└── src/main/java/top/yumbo/ai/omni/knowledge/enhancement/
    ├── service/impl/
    │   ├── DefaultKnowledgeRefinementService.java
    │   ├── DefaultKnowledgeExtractionService.java
    │   ├── DefaultKnowledgeStorageService.java
    │   └── DefaultKnowledgeAssociationService.java
    ├── network/
    │   ├── KnowledgeNetworkManager.java       # 从 core 迁移
    │   └── KnowledgeNetworkBuilder.java       # 从 core 迁移
    └── loader/
        └── KnowledgeLoader.java               # 从 core 迁移
```

### Module 3: omni-agent-knowledge-enhancement-starter

**职责：** Spring Boot 自动配置

```
omni-agent-knowledge-enhancement-starter/
└── src/main/java/top/yumbo/ai/omni/knowledge/enhancement/starter/
    ├── KnowledgeEnhancementAutoConfiguration.java
    ├── KnowledgeEnhancementProperties.java
    └── KnowledgeEnhancementHealthIndicator.java
```

---

## 🔄 迁移步骤

### Phase 1: 创建新模块（第1-2天）

#### 1.1 创建 API 模块

```bash
# 创建目录
mkdir -p omni-agent-knowledge-enhancement-api/src/main/java/top/yumbo/ai/omni/knowledge/enhancement/{model,service,network}

# 创建 pom.xml
```

**pom.xml 配置：**
```xml
<artifactId>omni-agent-knowledge-enhancement-api</artifactId>
<name>OmniAgent Knowledge Enhancement API</name>
<description>知识增强 API - 接口定义</description>

<dependencies>
    <!-- 只依赖基础库 -->
    <dependency>
        <groupId>org.projectlombok</groupId>
        <artifactId>lombok</artifactId>
    </dependency>
</dependencies>
```

#### 1.2 创建 Core 实现模块

```xml
<artifactId>omni-agent-knowledge-enhancement-core</artifactId>
<name>OmniAgent Knowledge Enhancement Core</name>

<dependencies>
    <!-- API 层 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-knowledge-enhancement-api</artifactId>
    </dependency>
    
    <!-- 知识注册表 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-knowledge-registry-api</artifactId>
    </dependency>
    
    <!-- AI 服务 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-ai-api</artifactId>
    </dependency>
    
    <!-- 文档存储 -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-document-storage-api</artifactId>
    </dependency>
    
    <!-- RAG -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-rag-api</artifactId>
    </dependency>
</dependencies>
```

#### 1.3 创建 Starter 模块

```xml
<artifactId>omni-agent-knowledge-enhancement-starter</artifactId>
<name>OmniAgent Knowledge Enhancement Starter</name>

<dependencies>
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-knowledge-enhancement-core</artifactId>
    </dependency>
    
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-starter</artifactId>
    </dependency>
    
    <dependency>
        <groupId>org.springframework.boot</groupId>
        <artifactId>spring-boot-autoconfigure</artifactId>
    </dependency>
</dependencies>
```

---

### Phase 2: 迁移代码（第3-5天）

#### 2.1 迁移模型类

**从：** `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/model/`  
**到：** `omni-agent-knowledge-enhancement-api/.../model/`

迁移文件：
- ✅ RefinedKnowledge.java
- ✅ KnowledgeDocument.java

#### 2.2 迁移服务接口

**从：** `omni-agent-core/.../service/knowledge/`  
**到：** `omni-agent-knowledge-enhancement-api/.../service/`

转换为接口：
- ✅ KnowledgeRefinementService → 接口
- ✅ KnowledgeExtractionService → 接口
- ✅ KnowledgeStorageService → 接口
- ✅ KnowledgeAssociationService → 接口

#### 2.3 迁移实现类

**从：** `omni-agent-core/.../service/knowledge/`  
**到：** `omni-agent-knowledge-enhancement-core/.../service/impl/`

重命名：
- KnowledgeRefinementService → DefaultKnowledgeRefinementService
- KnowledgeExtractionService → DefaultKnowledgeExtractionService
- KnowledgeStorageService → DefaultKnowledgeStorageService
- KnowledgeAssociationService → DefaultKnowledgeAssociationService

#### 2.4 迁移知识网络

**从：** `omni-agent-core/.../knowledge/network/`  
**到：** `omni-agent-knowledge-enhancement-core/.../network/`

迁移文件：
- ✅ KnowledgeNetworkManager.java
- ✅ KnowledgeNetworkBuilder.java

#### 2.5 迁移工具类

**从：** `omni-agent-core/.../knowledge/`  
**到：** `omni-agent-knowledge-enhancement-core/.../loader/`

迁移文件：
- ✅ KnowledgeLoader.java

---

### Phase 3: 创建自动配置（第6天）

#### 3.1 创建 AutoConfiguration

```java
@Configuration
@ConditionalOnProperty(
    prefix = "omni-agent.knowledge-enhancement",
    name = "enabled",
    havingValue = "true",
    matchIfMissing = true
)
@EnableConfigurationProperties(KnowledgeEnhancementProperties.class)
public class KnowledgeEnhancementAutoConfiguration {
    
    @Bean
    @ConditionalOnMissingBean
    public KnowledgeRefinementService knowledgeRefinementService(
            AIService aiService) {
        return new DefaultKnowledgeRefinementService(aiService);
    }
    
    @Bean
    @ConditionalOnMissingBean
    public KnowledgeExtractionService knowledgeExtractionService(
            KnowledgeRegistry registry) {
        return new DefaultKnowledgeExtractionService(registry);
    }
    
    @Bean
    @ConditionalOnMissingBean
    public KnowledgeStorageService knowledgeStorageService(
            RagService ragService,
            DocumentStorageService documentStorage) {
        return new DefaultKnowledgeStorageService(ragService, documentStorage);
    }
    
    @Bean
    @ConditionalOnMissingBean
    public KnowledgeNetworkService knowledgeNetworkService(
            KnowledgeDomainService domainService,
            DocumentStorageService documentStorage) {
        return new KnowledgeNetworkManager(domainService, documentStorage);
    }
}
```

#### 3.2 创建配置属性

```java
@Data
@ConfigurationProperties(prefix = "omni-agent.knowledge-enhancement")
public class KnowledgeEnhancementProperties {
    
    /** 是否启用知识增强 */
    private boolean enabled = true;
    
    /** 知识网络配置 */
    private NetworkConfig network = new NetworkConfig();
    
    /** AI 提炼配置 */
    private RefinementConfig refinement = new RefinementConfig();
    
    @Data
    public static class NetworkConfig {
        /** 是否自动构建知识网络 */
        private boolean autoScan = true;
        
        /** 扫描间隔（毫秒） */
        private long scanInterval = 300000;
        
        /** 批处理大小 */
        private int batchSize = 10;
    }
    
    @Data
    public static class RefinementConfig {
        /** 是否使用 AI 提炼 */
        private boolean useAI = true;
        
        /** AI 模型名称 */
        private String aiModel = "default";
    }
}
```

---

### Phase 4: 更新 Core 模块（第7天）

#### 4.1 更新 Core 依赖

```xml
<!-- omni-agent-core/pom.xml -->

<!-- 移除旧的知识相关代码，添加新模块依赖 -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-knowledge-enhancement-api</artifactId>
    <version>${project.version}</version>
</dependency>
```

#### 4.2 删除已迁移的代码

```bash
# 删除已迁移的文件
rm -rf omni-agent-core/src/main/java/top/yumbo/ai/omni/core/model/RefinedKnowledge.java
rm -rf omni-agent-core/src/main/java/top/yumbo/ai/omni/core/model/KnowledgeDocument.java
rm -rf omni-agent-core/src/main/java/top/yumbo/ai/omni/core/service/knowledge/
rm -rf omni-agent-core/src/main/java/top/yumbo/ai/omni/core/knowledge/network/
rm -rf omni-agent-core/src/main/java/top/yumbo/ai/omni/core/knowledge/KnowledgeLoader.java
```

#### 4.3 更新导入路径

在 Core 模块中，更新所有引用：
```java
// 旧的导入
import top.yumbo.ai.omni.core.model.RefinedKnowledge;
import top.yumbo.ai.omni.core.service.knowledge.KnowledgeStorageService;

// 新的导入
import top.yumbo.ai.omni.knowledge.enhancement.model.RefinedKnowledge;
import top.yumbo.ai.omni.knowledge.enhancement.service.KnowledgeStorageService;
```

---

### Phase 5: 更新应用模块（第8天）

#### 5.1 更新 omni-agent-web 依赖

```xml
<!-- 移除对 core 中知识模块的直接依赖 -->
<!-- 添加 knowledge-enhancement-starter -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-knowledge-enhancement-starter</artifactId>
    <version>${project.version}</version>
</dependency>
```

#### 5.2 更新配置文件

```yaml
# application.yml
omni-agent:
  knowledge-enhancement:
    enabled: true
    network:
      auto-scan: true
      scan-interval: 300000
      batch-size: 10
    refinement:
      use-ai: true
      ai-model: "gpt-4"
```

---

## ✅ 验证清单

### 编译检查
- [ ] 所有新模块编译通过
- [ ] Core 模块编译通过
- [ ] Web 模块编译通过
- [ ] 示例模块编译通过

### 功能测试
- [ ] 知识提炼功能正常
- [ ] 知识提取功能正常
- [ ] 知识存储功能正常
- [ ] 知识网络构建正常
- [ ] 角色学习功能正常

### 集成测试
- [ ] Spring Boot 自动配置生效
- [ ] 配置属性正确加载
- [ ] 依赖注入正常工作
- [ ] 所有测试用例通过

---

## 📊 重构前后对比

### 重构前

```
omni-agent-core (臃肿，职责不清)
├── 核心业务编排
├── 知识处理逻辑 ❌
├── 知识网络构建 ❌
└── 知识模型定义 ❌
```

### 重构后

```
omni-agent-core (纯粹，职责清晰)
├── 核心业务编排
└── 领域服务协调

omni-agent-knowledge-enhancement (独立可插拔) ⭐
├── API 层（接口定义）
├── Core 层（核心实现）
└── Starter 层（自动配置）
```

---

## 🎯 重构收益

### 1. 模块职责清晰
- ✅ Core 模块专注核心业务
- ✅ 知识增强功能独立
- ✅ 易于理解和维护

### 2. 可插拔架构
- ✅ 用户可选择是否启用知识增强
- ✅ 支持自定义实现
- ✅ 符合开闭原则

### 3. 依赖管理清晰
- ✅ 单向依赖
- ✅ 避免循环依赖
- ✅ 易于测试

### 4. 扩展性强
- ✅ 易于添加新的知识处理算法
- ✅ 支持多种存储后端
- ✅ 支持多种 AI 模型

---

## 📋 实施时间表

| 阶段 | 任务 | 时间 | 负责人 |
|------|------|------|--------|
| Phase 1 | 创建新模块 | 1-2天 | - |
| Phase 2 | 迁移代码 | 3-5天 | - |
| Phase 3 | 自动配置 | 1天 | - |
| Phase 4 | 更新 Core | 1天 | - |
| Phase 5 | 更新应用 | 1天 | - |
| **总计** | | **8天** | |

---

## 🚨 风险与应对

### 风险1：导入路径变更导致编译错误
**应对：** 使用 IDE 的全局替换功能，逐步更新

### 风险2：依赖注入失效
**应对：** 确保 AutoConfiguration 正确配置，添加详细日志

### 风险3：功能回归
**应对：** 保留完整的测试用例，逐步验证

---

## 📚 相关文档

- [知识网络架构设计](KNOWLEDGE_NETWORK_ARCHITECTURE.md)
- [知识网络 API 总结](KNOWLEDGE_NETWORK_API_SUMMARY.md)
- [Phase 1 实施计划](PHASE1_FINAL_IMPLEMENTATION_PLAN.md)

---

**创建时间：** 2025-12-28  
**版本：** 1.0.0  
**状态：** 待实施

