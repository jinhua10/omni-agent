# RAG 文档模型统一方案

> 日期：2025-12-27  
> 决策：统一使用 Document 模型，淘汰 RagDocument

---

## 🎯 决策结论

**选择：统一使用功能完整的 `Document` 模型**

将 `top.yumbo.ai.rag.api.model.Document` 移动到正确的包路径：
```
top.yumbo.ai.omni.rag.model.Document
```

**淘汰：** `RagDocument` - 功能过于简单，不适合生产使用

---

## 📊 两个模型对比

### Document（完整版）✅ 推荐

**包路径：** `top.yumbo.ai.omni.rag.model.Document`

**字段列表：**
```java
- id: String                    // 文档ID
- title: String                 // 标题
- content: String               // 内容
- summary: String               // 摘要
- embedding: float[]            // 向量 ⭐ 关键
- metadata: Map<String, Object> // 元数据
- source: String                // 来源
- type: String                  // 类型
- author: String                // 作者
- tags: List<String>            // 标签
- createdAt: Long               // 创建时间
- updatedAt: Long               // 更新时间
- indexedAt: Long               // 索引时间
- score: Double                 // 相关性得分 ⭐ 新增
```

**优势：**
- ✅ 功能完整，支持向量存储
- ✅ 时间戳字段完善
- ✅ 元数据丰富
- ✅ 支持相关性评分

### RagDocument（简化版）❌ 淘汰

**包路径：** `top.yumbo.ai.omni.rag.model.RagDocument`

**字段列表：**
```java
- id: String
- content: String
- title: String
- summary: String
- score: Double
- metadata: Map<String, Object>
```

**问题：**
- ❌ 缺少 embedding 字段（无法存储向量）
- ❌ 缺少时间戳字段
- ❌ 缺少来源、类型等信息
- ❌ 功能过于简单

---

## 🔧 实施方案

### 1. 创建统一的 Document 模型

**位置：** `omni-agent-rag-api/src/main/java/top/yumbo/ai/omni/rag/model/Document.java`

**变更：**
- 复制原 `top.yumbo.ai.rag.api.model.Document` 的所有字段
- 添加 `score` 字段（来自 RagDocument）
- 移动到正确的包路径

### 2. 更新 RagService 接口

**修改：** `omni-agent-rag-api/src/main/java/top/yumbo/ai/omni/rag/RagService.java`

```java
// 修改前
import top.yumbo.ai.omni.rag.model.RagDocument;
List<RagDocument> semanticSearch(String query, int maxResults);
void batchIndex(List<RagDocument> documents);

// 修改后
import top.yumbo.ai.omni.rag.model.Document;
List<Document> semanticSearch(String query, int maxResults);
void batchIndex(List<Document> documents);
```

### 3. 更新 MockRagService

**修改：** `omni-agent-rag-starter-adapter/src/main/java/top/yumbo/ai/omni/rag/adapter/MockRagService.java`

- 所有 `RagDocument` 引用改为 `Document`
- 修复 `batchIndex` 方法的参数遍历

### 4. 更新 KnowledgeStorageService

**修改：** `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/service/knowledge/KnowledgeStorageService.java`

- 导入改为 `top.yumbo.ai.omni.rag.model.Document`
- `convertToRAGDocument` 返回类型改为 `Document`
- 添加 `createdAt` 时间戳

---

## ✅ 修改文件清单

### 新建文件（1个）
1. ✅ `omni-agent-rag-api/src/main/java/top/yumbo/ai/omni/rag/model/Document.java`

### 修改文件（3个）
2. ✅ `omni-agent-rag-api/src/main/java/top/yumbo/ai/omni/rag/RagService.java`
3. ✅ `omni-agent-rag-starter-adapter/src/main/java/top/yumbo/ai/omni/rag/adapter/MockRagService.java`
4. ✅ `omni-agent-core/src/main/java/top/yumbo/ai/omni/core/service/knowledge/KnowledgeStorageService.java`

### 待淘汰文件（1个）
5. ❌ `omni-agent-rag-api/src/main/java/top/yumbo/ai/omni/rag/model/RagDocument.java`（可删除）

---

## 📈 迁移影响

### ✅ 已更新模块
- `omni-agent-core` - KnowledgeStorageService
- `omni-agent-rag-api` - RagService 接口
- `omni-agent-rag-starter-adapter` - MockRagService

### ⏳ 待更新模块
- `omni-agent-web` - 仍在使用 `top.yumbo.ai.rag.api.model.Document`
- `omni-agent-rag-starter-*` - 各个实现仍在使用旧包路径
- `omni-agent-example-basic` - 示例代码

**下一步：** 需要将这些模块统一迁移到 `top.yumbo.ai.omni.rag.model.Document`

---

## 🎓 统一后的优势

### 1. 包路径规范
```
✅ top.yumbo.ai.omni.rag.model.Document  （正确）
❌ top.yumbo.ai.rag.api.model.Document    （不规范）
```

### 2. 功能完整性

统一的 Document 模型支持：
- 向量存储（embedding）
- 时间追踪（createdAt, updatedAt, indexedAt）
- 相关性评分（score）
- 完整元数据（metadata）
- 文档分类（type, source, tags）

### 3. 代码一致性

所有模块使用同一个 Document 模型，避免转换混乱。

---

## 🚀 后续清理任务

### 短期（1-2天）
1. ⏳ 更新 Web 模块使用新的 Document
2. ⏳ 更新所有 RAG starter 实现
3. ⏳ 更新示例代码
4. ⏳ 删除旧的 `top.yumbo.ai.rag.api.model.Document`
5. ⏳ 删除 `RagDocument.java`

### 中期（3-5天）
1. ⏳ 编写迁移指南
2. ⏳ 更新 API 文档
3. ⏳ 添加弃用警告（@Deprecated）
4. ⏳ 完整的集成测试

---

## 📝 总结

### 最终方案

**只使用一个模型：`top.yumbo.ai.omni.rag.model.Document`**

- ✅ 包路径规范
- ✅ 功能完整
- ✅ 统一一致

**淘汰两个模型：**
- ❌ `top.yumbo.ai.rag.api.model.Document` - 包路径不规范
- ❌ `top.yumbo.ai.omni.rag.model.RagDocument` - 功能不完整

---

**创建时间：** 2025-12-27  
**状态：** ✅ 核心模块已统一  
**下一步：** 清理其他模块的旧引用


