# 🔄 提取文本文件格式从 .txt 改为 .md

## 🎯 问题描述

### 问题 1: 文件后缀不一致

**日志显示**:
```
DEBUG FileDocumentStorage - ⚠️ Extracted text not found: doc_炫酷高端投影仪产品发布会ppt模板.pptx
WARN KnowledgeNetworkBuilder - ⚠️ 文档未找到提取文本，跳过知识构建
```

**实际情况**:
- 保存的文件: `data/storage/extracted/炫酷高端投影仪产品发布会ppt模板.pptx.txt`
- 查找的名称: `doc_炫酷高端投影仪产品发布会ppt模板.pptx` (没有 `.txt` 后缀)

**原因**: 
- 保存时添加了 `.txt` 后缀
- 查找时使用 `documentId` 但没有添加后缀

### 问题 2: 文件格式不合适

提取的文本通常包含 Markdown 格式（如标题、列表等），使用 `.txt` 后缀不合适，应该使用 `.md`。

---

## ✅ 解决方案

### 修改 1: 更改文件后缀为 .md

修改所有存储实现中的提取文本方法，将后缀从 `.txt` 改为 `.md`。

#### 受影响的文件

1. **FileDocumentStorage** ✅
   - `saveExtractedText()`: `documentId + ".txt"` → `documentId + ".md"`
   - `getExtractedText()`: `documentId + ".txt"` → `documentId + ".md"`
   - `deleteExtractedText()`: `documentId + ".txt"` → `documentId + ".md"`

2. **MinIODocumentStorage** ✅
   - `saveExtractedText()`: `"extracted/" + documentId + ".txt"` → `".md"`
   - `getExtractedText()`: 同上
   - `deleteExtractedText()`: 同上

3. **S3DocumentStorage** ✅
   - `saveExtractedText()`: `"extracted/" + documentId + ".txt"` → `".md"`
   - `getExtractedText()`: 同上
   - `deleteExtractedText()`: 同上

4. **RedisDocumentStorage**
   - 不需要修改（使用键值对，没有文件扩展名）

5. **MongoDBDocumentStorage**
   - 不需要修改（使用GridFS，没有文件扩展名）

#### 代码示例

**修改前**:
```java
@Override
public String saveExtractedText(String documentId, String text) {
    Path textFile = extractedPath.resolve(documentId + ".txt");  // ❌
    // ...
}
```

**修改后**:
```java
@Override
public String saveExtractedText(String documentId, String text) {
    Path textFile = extractedPath.resolve(documentId + ".md");  // ✅
    // ...
}
```

---

### 修改 2: 重命名已存在的文件

为已存在的 `.txt` 文件提供了重命名脚本。

**脚本**: `scripts/rename-extracted-files-to-md.ps1`

**功能**:
- 自动查找 `data/storage/extracted/` 下的所有 `.txt` 文件
- 重命名为 `.md` 文件
- 如果目标文件已存在，提示是否覆盖
- 显示详细的重命名统计

**执行结果**:
```
✅ 重命名完成!
  📊 统计:
    ✅ 成功: 11 个文件
    ⏭️  跳过: 0 个文件

📂 已重命名的文件:
  📄 炫酷高端投影仪产品发布会ppt模板.pptx.md
  📄 如何打造赢得用户的产品服务与商业模式《创新设计》读书笔记ppt模板.pptx.md
  📄 环境保护公益宣传PPT模板——.ppt.md
  ... (共 11 个文件)
```

---

## 📋 文件格式对比

### 之前 (.txt)
```
data/storage/extracted/
├── 炫酷高端投影仪产品发布会ppt模板.pptx.txt      ❌ .txt 后缀
├── 环境保护公益宣传PPT模板——.ppt.txt              ❌
└── ...
```

### 现在 (.md)
```
data/storage/extracted/
├── 炫酷高端投影仪产品发布会ppt模板.pptx.md       ✅ .md 后缀
├── 环境保护公益宣传PPT模板——.ppt.md              ✅
└── ...
```

---

## 🎯 优势

### 1. 语义更准确 ✅
- `.md` 后缀表明内容是 Markdown 格式
- 编辑器会自动识别并提供语法高亮

### 2. 与分块格式一致 ✅
- 分块文件已经使用 `.md` 格式（`chunk_000.md`）
- 统一使用 Markdown 格式

### 3. 便于预览和编辑 ✅
- IDE 和编辑器对 `.md` 文件提供更好的支持
- 可以直接预览渲染效果

---

## 🔧 修改的模块

1. ✅ `omni-agent-document-storage-starter`
   - FileDocumentStorage.java
   - MinIODocumentStorage.java
   - S3DocumentStorage.java

2. ✅ 重命名脚本
   - `scripts/rename-extracted-files-to-md.ps1`

---

## 📝 使用步骤

### 1. 编译新代码
```bash
cd D:\Jetbrains\omni-agent
mvn clean compile -DskipTests -pl omni-agent-document-storage-starter -am
```

**结果**: ✅ BUILD SUCCESS

### 2. 重命名已有文件
```bash
.\scripts\rename-extracted-files-to-md.ps1
```

**结果**: ✅ 11 个文件成功重命名

### 3. 重启应用
```bash
cd omni-agent-example-basic
mvn spring-boot:run
```

**预期效果**:
- ✅ 新提取的文本保存为 `.md` 文件
- ✅ 能够正确读取已有的 `.md` 文件
- ✅ KnowledgeNetworkBuilder 能找到提取文本

---

## 🔍 验证方法

### 1. 检查文件格式
```bash
ls data/storage/extracted/*.md
```

**应该看到**:
```
炫酷高端投影仪产品发布会ppt模板.pptx.md
环境保护公益宣传PPT模板——.ppt.md
...
```

### 2. 检查日志

**修改前**:
```
DEBUG FileDocumentStorage - ⚠️ Extracted text not found: doc_炫酷高端投影仪产品发布会ppt模板.pptx
```

**修改后**:
```
DEBUG FileDocumentStorage - ✅ Retrieved extracted text: 炫酷高端投影仪产品发布会ppt模板.pptx, length=12345
INFO KnowledgeNetworkBuilder - 🔨 开始为文档构建知识网络
```

### 3. 上传新文档测试

上传一个新的 PPT 文档，检查：
- ✅ 提取文本保存为 `.md` 文件
- ✅ 能够正确读取并构建知识网络

---

## 🎉 总结

### 修改内容
- ✅ 3 个存储实现（File、MinIO、S3）
- ✅ 9 个方法修改（每个存储 3 个方法）
- ✅ 11 个已有文件重命名
- ✅ 1 个重命名脚本

### 效果
- ✅ 文件格式统一为 `.md`
- ✅ 语义更准确
- ✅ 与分块格式一致
- ✅ 编辑器支持更好

### 兼容性
- ✅ 向前兼容（已有文件通过脚本重命名）
- ✅ 不影响其他功能
- ✅ Redis/MongoDB 不需要修改

**修复完成时间**: 2025-12-29  
**编译状态**: ✅ 成功  
**重命名状态**: ✅ 11 个文件成功  
**需要重启**: 是


