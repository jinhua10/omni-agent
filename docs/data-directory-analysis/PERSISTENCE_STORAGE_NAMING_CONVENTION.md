# 持久化层命名规范文档

## 📋 命名规范总则

为了清晰区分 **Persistence 层** 和 **Storage 层**，制定以下命名规范。

---

## 🎯 核心原则

### 1. Persistence 层 = 配置持久化
**职责**: 系统的"大脑" - 管理配置、规则、元数据

**命名关键词**:
- `Persistence` - 持久化
- `Config` / `Configuration` - 配置
- `Metadata` - 元数据
- `Rule` - 规则

**示例**:
- ✅ `QuestionClassifierPersistence` - 问题分类器持久化
- ✅ `SystemConfigurationPersistence` - 系统配置持久化
- ✅ `MetadataPersistence` - 元数据持久化

---

### 2. Storage 层 = 内容存储
**职责**: 系统的"仓库" - 管理文档、内容、数据

**命名关键词**:
- `Storage` - 存储
- `Document` - 文档
- `Content` - 内容
- `Data` - 数据

**示例**:
- ✅ `DocumentStorageService` - 文档存储服务
- ✅ `ContentStorageService` - 内容存储服务
- ✅ `DataStorageService` - 数据存储服务

---

## 📦 模块命名规范

### Persistence 模块

```
omni-agent-persistence-api               ← API定义
omni-agent-persistence-starter-*         ← Starter实现
  ├─ omni-agent-persistence-starter-file
  ├─ omni-agent-persistence-starter-h2
  ├─ omni-agent-persistence-starter-sqlite
  ├─ omni-agent-persistence-starter-redis
  ├─ omni-agent-persistence-starter-mongodb
  └─ omni-agent-persistence-starter-elasticsearch
```

### Storage 模块

```
omni-agent-document-storage-api          ← API定义（保持现状）
omni-agent-document-storage-starter-*    ← Starter实现
  ├─ omni-agent-document-storage-starter-file
  ├─ omni-agent-document-storage-starter-mongodb
  ├─ omni-agent-document-storage-starter-redis
  ├─ omni-agent-document-storage-starter-s3
  ├─ omni-agent-document-storage-starter-minio
  └─ omni-agent-document-storage-starter-elasticsearch
```

---

## 🔤 接口命名规范

### Persistence 接口

**格式**: `{Domain}Persistence`

**示例**:
```java
// ✅ 推荐
public interface QuestionClassifierPersistence { }
public interface SystemConfigurationPersistence { }
public interface WorkflowRulePersistence { }

// ❌ 不推荐
public interface QuestionClassifierStorage { }  // 混淆！应该用 Persistence
public interface ConfigService { }              // 不明确
```

### Storage 接口

**格式**: `{Domain}StorageService` 或 `{Domain}Storage`

**示例**:
```java
// ✅ 推荐
public interface DocumentStorageService { }
public interface ImageStorageService { }
public interface ContentStorageService { }

// ❌ 不推荐
public interface DocumentPersistence { }  // 混淆！应该用 Storage
public interface DocService { }           // 缩写不清晰
```

---

## 📁 包命名规范

### Persistence 包

```
top.yumbo.ai.persistence.api
  ├─ QuestionClassifierPersistence.java
  ├─ model/
  │   └─ QuestionTypeConfig.java
  └─ config/
      └─ PersistenceCompositeProperties.java
```

### Storage 包

```
top.yumbo.ai.storage.api
  ├─ DocumentStorageService.java
  ├─ model/
  │   ├─ Chunk.java
  │   ├─ Image.java
  │   └─ OptimizationData.java
  └─ config/
      └─ StorageProperties.java
```

---

## 🏷️ 方法命名规范

### Persistence 方法

**特点**: 强调配置和元数据管理

```java
public interface QuestionClassifierPersistence {
    // ✅ 配置管理
    boolean saveQuestionType(QuestionTypeConfig config);
    Optional<QuestionTypeConfig> getQuestionType(String typeId);
    List<QuestionTypeConfig> getAllQuestionTypes();
    
    // ✅ 关键词管理
    boolean saveKeywords(String typeId, List<String> keywords);
    List<String> getKeywords(String typeId);
    
    // ✅ 版本和历史
    String getVersion();
    List<ChangeRecord> getChangeHistory(int limit);
}
```

### Storage 方法

**特点**: 强调内容和数据存储

```java
public interface DocumentStorageService {
    // ✅ 文档存储
    String saveDocument(String documentId, String filename, byte[] fileData);
    Optional<byte[]> getDocument(String documentId);
    void deleteDocument(String documentId);
    
    // ✅ 内容存储
    String saveExtractedText(String documentId, String text);
    Optional<String> getExtractedText(String documentId);
    
    // ✅ 分块存储
    String saveChunk(String documentId, Chunk chunk);
    List<Chunk> getChunksByDocument(String documentId);
}
```

---

## 🎨 实现类命名规范

### Persistence 实现类

**格式**: `{Backend}{Domain}Persistence`

```java
// ✅ 推荐
public class SQLiteQuestionClassifierPersistence implements QuestionClassifierPersistence { }
public class RedisQuestionClassifierPersistence implements QuestionClassifierPersistence { }
public class MongoDBQuestionClassifierPersistence implements QuestionClassifierPersistence { }

// ❌ 不推荐
public class SQLitePersistenceImpl { }  // 不明确
public class QuestionPersistence { }    // 太简短
```

### Storage 实现类

**格式**: `{Backend}{Domain}Storage`

```java
// ✅ 推荐
public class FileDocumentStorage implements DocumentStorageService { }
public class S3DocumentStorage implements DocumentStorageService { }
public class MongoDBDocumentStorage implements DocumentStorageService { }

// ❌ 不推荐
public class FileStorageImpl { }       // 不明确
public class DocumentStorage { }       // 缺少后端信息
```

---

## 📝 注释规范

### Persistence 接口注释模板

```java
/**
 * {领域}持久化接口
 * ({Domain} Persistence Interface)
 *
 * <h3>职责范围</h3>
 * <p>本接口用于持久化<strong>系统配置和元数据</strong>，管理{具体领域}的配置信息</p>
 * 
 * <h3>适用场景</h3>
 * <ul>
 *   <li>✅ 存储{领域}配置（结构化小数据）</li>
 *   <li>✅ 管理规则和元数据</li>
 *   <li>✅ 数据量小（KB级别），访问频繁</li>
 * </ul>
 * 
 * <h3>不适用场景</h3>
 * <ul>
 *   <li>❌ 大文件存储（请使用 DocumentStorageService）</li>
 *   <li>❌ 二进制内容（请使用 DocumentStorageService）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @see top.yumbo.ai.omni.storage.api.DocumentStorageService 文档和内容存储服务
 */
public interface {Domain}Persistence {
    // ...
}
```

### Storage 接口注释模板

```java
/**
 * {领域}存储服务接口
 * ({Domain} Storage Service Interface)
 *
 * <h3>职责范围</h3>
 * <p>本接口用于存储<strong>业务数据和内容</strong>，管理{具体内容}等大文件和非结构化数据</p>
 * 
 * <h3>适用场景</h3>
 * <ul>
 *   <li>✅ 存储{具体内容}（可能很大）</li>
 *   <li>✅ 管理{具体数据}</li>
 *   <li>✅ 数据量大（MB-GB级别），简单CRUD</li>
 * </ul>
 * 
 * <h3>不适用场景</h3>
 * <ul>
 *   <li>❌ 系统配置管理（请使用 Persistence API）</li>
 *   <li>❌ 规则和元数据（请使用 Persistence API）</li>
 * </ul>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 * @see top.yumbo.ai.omni.persistence.api.QuestionClassifierPersistence 配置和元数据持久化服务
 */
public interface {Domain}StorageService {
    // ...
}
```

---

## 🎯 使用场景示例

### 场景1: 添加新的配置管理

**需求**: 添加工作流配置管理

**命名**:
```java
// ✅ 正确 - 使用 Persistence
public interface WorkflowConfigurationPersistence {
    boolean saveWorkflowConfig(WorkflowConfig config);
    Optional<WorkflowConfig> getWorkflowConfig(String workflowId);
    List<WorkflowConfig> getAllWorkflows();
}
```

**原因**: 工作流配置是**系统配置**，属于元数据

---

### 场景2: 添加新的内容存储

**需求**: 添加视频文件存储

**命名**:
```java
// ✅ 正确 - 使用 Storage
public interface VideoStorageService {
    String saveVideo(String videoId, String filename, byte[] videoData);
    Optional<byte[]> getVideo(String videoId);
    void deleteVideo(String videoId);
}
```

**原因**: 视频文件是**业务数据**，属于大文件内容

---

### 场景3: 混合场景

**需求**: 文档提取结果管理

**分析**:
```java
// 提取结果包含：
// 1. 提取的文本（可能很大） → Storage
// 2. 元数据（状态、耗时等） → 可以跟随内容一起存储

// ✅ 推荐：统一使用 Storage（当前方案）
public class DocumentExtractionResultServiceImpl {
    private final DocumentStorageService storageService;
    
    public void save(DocumentExtractionResult result) {
        // 将整个对象（包括文本和元数据）存储在 Storage
        storageService.saveDocument(path, filename, jsonBytes);
    }
}
```

**原因**: 
- 主要内容是提取的文本（可能很大）
- 元数据是附属信息
- 统一存储更简单

---

## 📊 对照表

### 快速决策指南

| 问题 | 答案 | 使用 |
|------|------|------|
| 是系统配置吗？ | 是 | **Persistence** |
| 是规则或元数据吗？ | 是 | **Persistence** |
| 数据量小（KB级）吗？ | 是 | **Persistence** |
| 需要复杂查询吗？ | 是 | **Persistence** |
| 是用户上传的内容吗？ | 是 | **Storage** |
| 是大文件或二进制吗？ | 是 | **Storage** |
| 数据量大（MB-GB）吗？ | 是 | **Storage** |
| 主要是简单CRUD吗？ | 是 | **Storage** |

---

## ✅ 检查清单

在添加新功能时，使用此清单确保命名符合规范：

### Persistence 清单
- [ ] 接口名包含 `Persistence`
- [ ] 用于存储配置或元数据
- [ ] 数据量小（KB级）
- [ ] 包含查询和管理方法
- [ ] 注释清晰说明职责范围
- [ ] 包含 `@see` 指向 Storage（说明区别）

### Storage 清单
- [ ] 接口名包含 `Storage` 或 `StorageService`
- [ ] 用于存储内容或大数据
- [ ] 数据量大（MB-GB级）
- [ ] 主要是简单CRUD
- [ ] 注释清晰说明职责范围
- [ ] 包含 `@see` 指向 Persistence（说明区别）

---

## 🎓 培训建议

### 新开发者入门

1. **阅读本文档** - 理解命名规范和职责划分
2. **查看示例代码** - 对比 Persistence 和 Storage 的实现
3. **参考注释** - 接口上的注释清晰说明了使用场景

### 代码审查重点

1. 检查命名是否符合规范
2. 验证职责划分是否正确
3. 确保注释完整清晰

---

## 📝 总结

### 核心规则

1. **Persistence** = 配置、元数据、规则（系统的"大脑"）
2. **Storage** = 内容、数据、文件（系统的"仓库"）
3. **命名要清晰** - 让人一眼看出职责
4. **注释要完整** - 说明适用和不适用场景

### 记忆口诀

```
Persistence 管配置，小而精
Storage 存内容，大而广
名字要清晰，职责要分明
```

---

生成时间: 2025-12-24
作者: AI Assistant
状态: ✅ 命名规范已制定
适用范围: 所有 Persistence 和 Storage 相关模块

