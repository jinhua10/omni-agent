# HOPE 模块重构指南

## 📦 新建的模块

根据批次1分析报告的建议，已创建以下3个新模块：

### 1. omni-agent-hope-api
**位置：** `D:\Jetbrains\omni-agent\omni-agent-hope-api`  
**用途：** HOPE 系统接口层（API定义）  
**包路径：** `top.yumbo.ai.omni.hope.api`

**目录结构：**
```
omni-agent-hope-api/
├── pom.xml
└── src/
    ├── main/
    │   ├── java/top/yumbo/ai/omni/hope/api/
    │   │   ├── model/          # 数据模型
    │   │   ├── service/        # 服务接口
    │   │   └── persistence/    # 持久化接口
    │   └── resources/
    └── test/java/
```

### 2. omni-agent-hope-starter
**位置：** `D:\Jetbrains\omni-agent\omni-agent-hope-starter`  
**用途：** HOPE 系统实现层（Spring Boot Starter）  
**包路径：** `top.yumbo.ai.omni.hope.starter`

**目录结构：**
```
omni-agent-hope-starter/
├── pom.xml
└── src/
    ├── main/
    │   ├── java/top/yumbo/ai/omni/hope/starter/
    │   │   ├── impl/           # 服务实现
    │   │   ├── config/         # 自动配置
    │   │   └── persistence/    # 持久化实现
    │   └── resources/
    │       ├── META-INF/spring/
    │       │   └── org.springframework.boot.autoconfigure.AutoConfiguration.imports
    │       └── application-hope.properties
    └── test/java/
```

### 3. omni-agent-orchestrator
**位置：** `D:\Jetbrains\omni-agent\omni-agent-orchestrator`  
**用途：** 服务编排器（协调各个服务组件）  
**包路径：** `top.yumbo.ai.omni.orchestrator`

**目录结构：**
```
omni-agent-orchestrator/
├── pom.xml
└── src/
    ├── main/
    │   ├── java/top/yumbo/ai/omni/orchestrator/
    │   │   ├── service/        # 编排服务
    │   │   ├── workflow/       # 工作流定义
    │   │   └── config/         # 配置
    │   └── resources/
    └── test/java/
```

---

## 🚚 代码迁移指南

### 步骤1：从 omni-agent-core 迁移到 omni-agent-hope-api

**需要迁移的接口和模型：**

从 `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/hope/` 迁移到 `omni-agent-hope-api/src/main/java/top/yumbo/ai/omni/hope/api/`

| 源文件 | 目标位置 | 说明 |
|--------|---------|------|
| `hope/persistence/HopePersistence.java` | `api/persistence/HopePersistence.java` | 持久化接口 |
| `hope/model/QuestionTypeConfig.java` | `api/model/QuestionTypeConfig.java` | 问题类型配置模型 |

**迁移操作（在IDEA中）：**
1. 打开 `omni-agent-core` 模块
2. 选中 `hope/persistence/HopePersistence.java`
3. 右键 → Refactor → Move → 选择 `omni-agent-hope-api` 的 `api.persistence` 包
4. 对 `QuestionTypeConfig.java` 重复相同操作

**注意事项：**
- ✅ 只迁移接口和模型（纯定义，无实现）
- ✅ 更新包名：`top.yumbo.ai.omni.core.hope` → `top.yumbo.ai.omni.hope.api`
- ✅ IDEA会自动更新所有引用

---

### 步骤2：从 omni-agent-core 迁移到 omni-agent-hope-starter

**需要迁移的实现类：**

从 `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/hope/` 迁移到 `omni-agent-hope-starter/src/main/java/top/yumbo/ai/omni/hope/starter/`

| 源文件 | 目标位置 | 说明 |
|--------|---------|------|
| `hope/HOPEKnowledgeManager.java` | `starter/impl/HOPEKnowledgeManager.java` | HOPE知识管理器 |
| `hope/QuestionClassifier.java` | `starter/impl/QuestionClassifier.java` | 问题分类器 |
| `hope/persistence/impl/InMemoryHopePersistence.java` | `starter/persistence/InMemoryHopePersistence.java` | 内存持久化实现 |
| `hope/persistence/impl/KnowledgeRegistryHopePersistence.java` | `starter/persistence/KnowledgeRegistryHopePersistence.java` | 知识注册表持久化实现 |
| `hope/config/HopePersistenceAutoConfiguration.java` | `starter/config/HopeAutoConfiguration.java` | 自动配置类 |

**迁移操作（在IDEA中）：**
1. 选中上述实现类文件
2. 右键 → Refactor → Move → 选择 `omni-agent-hope-starter` 的对应包
3. 更新自动配置文件名为 `HopeAutoConfiguration.java`
4. 更新 `META-INF/spring/org.springframework.boot.autoconfigure.AutoConfiguration.imports` 中的类名

**注意事项：**
- ✅ 迁移所有实现类
- ✅ 更新包名：`top.yumbo.ai.omni.core.hope` → `top.yumbo.ai.omni.hope.starter`
- ✅ 确保自动配置文件正确

---

### 步骤3：从 omni-agent-core 迁移到 omni-agent-orchestrator

**需要迁移的服务：**

从 `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/query/` 迁移到 `omni-agent-orchestrator/src/main/java/top/yumbo/ai/omni/orchestrator/`

| 源文件 | 目标位置 | 说明 |
|--------|---------|------|
| `query/QueryService.java` | `orchestrator/service/QueryService.java` | 查询服务 |
| `query/cache/QueryExpansionCacheService.java` | `orchestrator/service/QueryExpansionCacheService.java` | 查询缓存 |
| `query/model/*` | `orchestrator/model/*` | 查询相关模型 |

**迁移操作（在IDEA中）：**
1. 选中 `query` 包下的文件
2. 右键 → Refactor → Move → 选择 `omni-agent-orchestrator` 的对应包
3. 更新包名

**注意事项：**
- ✅ QueryService 是编排服务，负责协调多个组件
- ✅ 更新包名：`top.yumbo.ai.omni.core.query` → `top.yumbo.ai.omni.orchestrator.service`

---

### 步骤4：清理 omni-agent-core

**完成上述迁移后，从 omni-agent-core 中删除：**

1. `hope/` 整个目录
2. `query/` 整个目录（除非有其他依赖）
3. `hope` 相关的配置和资源文件

**保留在 omni-agent-core 中的：**
- `config/ThreadPoolConfiguration.java`（需要重新评估位置）
- `config/MediaProcessingConfig.java`（需要重新评估位置）

---

## 🔧 更新依赖关系

### 步骤5：更新 omni-agent-core 的 pom.xml

**移除依赖：**
```xml
<!-- 移除这些依赖，它们应该在对应的 Starter 中 -->
<!-- Lucene → 移到 rag-starter-adapter -->
<!-- POI → 移到 document-processor-starter -->
<!-- PDFBox → 移到 document-processor-starter -->
<!-- Tika → 移到 document-processor-starter -->
<!-- Caffeine → 移到 hope-starter -->
```

**添加依赖：**
```xml
<!-- 添加新的模块依赖 -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-hope-api</artifactId>
    <version>${project.version}</version>
</dependency>

<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-orchestrator</artifactId>
    <version>${project.version}</version>
</dependency>
```

---

### 步骤6：更新其他模块的依赖

**需要更新的模块：**
- `omni-agent-web` - 添加 `omni-agent-hope-starter` 和 `omni-agent-orchestrator` 依赖
- `omni-agent-example-basic` - 添加 `omni-agent-hope-starter` 依赖
- `omni-agent-example-production` - 添加 `omni-agent-hope-starter` 依赖

**示例（在 web 模块的 pom.xml 中）：**
```xml
<dependencies>
    <!-- HOPE Starter -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-hope-starter</artifactId>
    </dependency>

    <!-- Orchestrator -->
    <dependency>
        <groupId>top.yumbo.ai.omni</groupId>
        <artifactId>omni-agent-orchestrator</artifactId>
    </dependency>
</dependencies>
```

---

## ✅ 验证清单

完成迁移后，请验证以下内容：

- [ ] 所有文件已从 `omni-agent-core` 正确迁移到新模块
- [ ] 包名已正确更新
- [ ] 所有 import 语句已自动更新（IDEA会处理）
- [ ] `omni-agent-hope-api` 只包含接口和模型（无实现）
- [ ] `omni-agent-hope-starter` 包含所有实现类
- [ ] `omni-agent-orchestrator` 包含服务编排逻辑
- [ ] 自动配置文件已正确更新
- [ ] 父 pom.xml 已包含3个新模块
- [ ] 所有依赖关系已正确更新
- [ ] 执行 `mvn clean install` 构建成功
- [ ] 所有测试通过

---

## 🎯 迁移后的架构

```
┌─────────────────────────────────────────────┐
│  应用层                                      │
│  - omni-agent-web                           │
│  - omni-agent-example-*                     │
└─────────────────────────────────────────────┘
                ↓ 依赖
┌─────────────────────────────────────────────┐
│  服务编排层 (新增)                           │
│  - omni-agent-orchestrator                  │
│    协调各个服务组件                          │
└─────────────────────────────────────────────┘
                ↓ 依赖
┌──────────────────┬──────────────────────────┐
│ omni-agent-core  │  Starter 实现层          │
│ (精简后)         │  - hope-starter          │
│                  │  - rag-starter           │
│                  │  - ai-starter            │
│                  │  - ...                   │
└──────────────────┴──────────────────────────┘
                ↓ 依赖
┌─────────────────────────────────────────────┐
│  API 接口层                                  │
│  - hope-api                                  │
│  - rag-api                                   │
│  - ai-api                                    │
│  - ...                                       │
└─────────────────────────────────────────────┘
                ↓ 依赖
┌─────────────────────────────────────────────┐
│  通用层                                      │
│  - omni-agent-common                        │
└─────────────────────────────────────────────┘
```

---

## 📝 注意事项

1. **使用IDEA的重构功能**
   - 不要手动复制粘贴文件
   - 使用 Refactor → Move 功能
   - IDEA会自动更新所有引用

2. **分步骤进行**
   - 先迁移接口和模型（步骤1）
   - 再迁移实现类（步骤2-3）
   - 最后更新依赖关系（步骤4-6）

3. **及时测试**
   - 每完成一个步骤，执行 `mvn clean compile`
   - 确保没有编译错误
   - 检查IDE的错误提示

4. **提交版本控制**
   - 每完成一个大步骤，提交一次Git
   - 便于回滚和追踪变更

---

**创建时间：** 2025-12-31  
**参考文档：** `docs/analysis/BATCH_01_FOUNDATION_ANALYSIS.md`

