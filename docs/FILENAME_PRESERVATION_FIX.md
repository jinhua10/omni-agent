# 📄 原始文件名保留修复报告

**修复时间**: 2025-12-19  
**问题**: 上传的文件保存时使用 documentId 作为文件名，导致文件名不可读  
**状态**: ✅ 已修复

---

## 📋 问题描述

用户反馈：上传 `"倡导节约用水PPT作品下载——.pptx"` 后，文件被保存为：
```
doc_1766142807149_______PPT______.pptx.pptx
```

**问题**:
- ❌ 文件名不可读
- ❌ 中文被替换为下划线
- ❌ 无法通过文件系统直接识别文件内容

---

## 🔍 根本原因

### 旧的实现

```java
// ❌ 旧方法：使用 documentId + 扩展名
String extension = filename.substring(filename.lastIndexOf('.'));
Path documentFile = documentsPath.resolve(documentId + extension);
Files.write(documentFile, fileData);
```

**问题**:
1. `documentId` 包含了经过处理的文件名片段
2. 中文字符被 `replaceAll("[^a-zA-Z0-9._-]", "_")` 替换
3. 最终文件名变成：`doc_1766142807149_______PPT______.pptx`

---

## ✅ 修复方案

### 新的目录结构

使用 **documentId 子目录** + **原始文件名**：

```
./data/documents/
├── doc_1766142807149/
│   └── 倡导节约用水PPT作品下载——.pptx  ⭐ 保留原始文件名
├── doc_1766142807150/
│   └── 技术文档.pdf                    ⭐ 保留原始文件名
└── doc_1766142807151/
    └── README.md                        ⭐ 保留原始文件名
```

**优势**:
- ✅ 保留原始文件名（包括中文、特殊字符）
- ✅ 通过 documentId 快速定位
- ✅ 避免文件名冲突
- ✅ 文件系统中可读性好

---

## 📝 代码修改

### 1. saveDocument() 方法

**修改前**:
```java
@Override
public String saveDocument(String documentId, String filename, byte[] fileData) {
    // ❌ 保存在根目录，使用 documentId + 扩展名
    Path documentFile = documentsPath.resolve(documentId + extension);
    Files.write(documentFile, fileData);
    return documentId;
}
```

**修改后**:
```java
@Override
public String saveDocument(String documentId, String filename, byte[] fileData) {
    // ✅ 创建 documentId 子目录
    Path docDir = documentsPath.resolve(documentId);
    Files.createDirectories(docDir);

    // ✅ 在子目录中保存原始文件名
    Path documentFile = docDir.resolve(filename);
    Files.write(documentFile, fileData);

    log.debug("Saved document: {} -> {}", documentId, filename);
    return documentId;
}
```

### 2. getDocument() 方法

**修改前**:
```java
@Override
public Optional<byte[]> getDocument(String documentId) {
    // ❌ 在根目录中搜索以 documentId 开头的文件
    Path[] matchingFiles = Files.list(documentsPath)
            .filter(p -> p.getFileName().toString().startsWith(documentId))
            .toArray(Path[]::new);
    // ...
}
```

**修改后**:
```java
@Override
public Optional<byte[]> getDocument(String documentId) {
    // ✅ 在 documentId 子目录中查找文件
    Path docDir = documentsPath.resolve(documentId);
    if (!Files.exists(docDir) || !Files.isDirectory(docDir)) {
        return Optional.empty();
    }

    // 读取目录中的第一个文件（原始文件）
    Path[] files = Files.list(docDir)
            .filter(Files::isRegularFile)
            .toArray(Path[]::new);

    if (files.length > 0) {
        byte[] data = Files.readAllBytes(files[0]);
        return Optional.of(data);
    }
    return Optional.empty();
}
```

### 3. deleteDocument() 方法

**修改前**:
```java
@Override
public void deleteDocument(String documentId) {
    // ❌ 在根目录中搜索并删除匹配的文件
    Files.list(documentsPath)
            .filter(p -> p.getFileName().toString().startsWith(documentId))
            .forEach(p -> Files.delete(p));
}
```

**修改后**:
```java
@Override
public void deleteDocument(String documentId) {
    // ✅ 删除整个 documentId 子目录
    Path docDir = documentsPath.resolve(documentId);
    if (Files.exists(docDir)) {
        Files.walk(docDir)
                .sorted(Comparator.reverseOrder())
                .forEach(p -> Files.delete(p));
        log.debug("Deleted document directory: {}", documentId);
    }
}
```

---

## 📊 效果对比

### 修复前

```
./data/documents/
├── doc_1766142807149_______PPT______.pptx.pptx  ❌ 不可读
├── doc_1766142807150____.pdf                    ❌ 不可读
└── doc_1766142807151_README.md                  ❌ 不可读
```

### 修复后

```
./data/documents/
├── doc_1766142807149/
│   └── 倡导节约用水PPT作品下载——.pptx  ✅ 完全可读！
├── doc_1766142807150/
│   └── 技术文档.pdf                    ✅ 完全可读！
└── doc_1766142807151/
    └── README.md                        ✅ 完全可读！
```

---

## 🔍 文件名处理对比

### 示例 1: 中文文件名

**原始文件名**: `倡导节约用水PPT作品下载——.pptx`

| 方案 | 保存的文件名 | 可读性 |
|------|------------|--------|
| **修复前** | `doc_1766142807149_______PPT______.pptx.pptx` | ❌ 不可读 |
| **修复后** | `doc_1766142807149/倡导节约用水PPT作品下载——.pptx` | ✅ 完全可读 |

### 示例 2: 特殊字符

**原始文件名**: `项目计划 (2024-Q4) [最终版].docx`

| 方案 | 保存的文件名 | 可读性 |
|------|------------|--------|
| **修复前** | `doc_xxx____2024_Q4_______.docx` | ❌ 不可读 |
| **修复后** | `doc_xxx/项目计划 (2024-Q4) [最终版].docx` | ✅ 完全可读 |

### 示例 3: 英文文件名

**原始文件名**: `Technical-Documentation_v2.0.pdf`

| 方案 | 保存的文件名 | 可读性 |
|------|------------|--------|
| **修复前** | `doc_xxx_Technical-Documentation_v2.0.pdf` | △ 基本可读 |
| **修复后** | `doc_xxx/Technical-Documentation_v2.0.pdf` | ✅ 完全可读 |

---

## 🎯 优势分析

### 1. 可读性

**修复前**: 在文件管理器中看到文件名无法识别内容
```
doc_1766142807149_______PPT______.pptx.pptx  ← 这是什么？
```

**修复后**: 一目了然
```
doc_1766142807149/
  └── 倡导节约用水PPT作品下载——.pptx  ← 清楚知道是什么！
```

### 2. 兼容性

✅ **支持所有字符**:
- 中文字符：`倡导节约用水.pptx`
- 日文字符：`プレゼンテーション.pptx`
- 特殊符号：`项目 (2024) [v2].docx`
- Emoji：`📄文档.pdf` （如果操作系统支持）

### 3. 可维护性

**修复前**: 需要查数据库才知道原始文件名
```bash
# 看到文件名无法知道内容
ls ./data/documents/
doc_1766142807149_______PPT______.pptx.pptx
```

**修复后**: 直接就能看到
```bash
# 立即知道每个文档是什么
ls ./data/documents/doc_1766142807149/
倡导节约用水PPT作品下载——.pptx
```

### 4. 唯一性

通过 **documentId 子目录** 保证唯一性：
```
./data/documents/
├── doc_1766142807149/  ← documentId 保证唯一
│   └── report.pdf      ← 即使同名也不会冲突
└── doc_1766142807150/  ← 不同的 documentId
    └── report.pdf      ← 可以共存
```

---

## 📂 完整目录结构

```
./data/
├── documents/                  # 原始文档（按 documentId 子目录组织）⭐
│   ├── doc_123/
│   │   └── 倡导节约用水.pptx   # 保留原始文件名
│   ├── doc_456/
│   │   └── 技术文档.pdf
│   └── doc_789/
│       └── README.md
│
├── chunks/                     # 分块（按 documentId 子目录组织）
│   ├── doc_123/
│   │   ├── chunk_0.chunk
│   │   └── chunk_1.chunk
│   └── doc_456/
│       └── chunk_0.chunk
│
├── images/                     # 图片（按 documentId 子目录组织）
│   └── doc_123/
│       ├── img_xxx.img
│       └── img_yyy.img
│
└── ppl/                        # PPL数据（按 documentId 子目录组织）
    └── doc_123/
        └── ppl.data
```

**统一的组织结构！**

---

## ✅ 验证方法

### 1. 上传文档

```bash
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@倡导节约用水PPT作品下载——.pptx" \
  -F "autoIndex=true"
```

**返回**:
```json
{
  "success": true,
  "documentId": "doc_1766142807149",
  "fileName": "倡导节约用水PPT作品下载——.pptx"
}
```

### 2. 检查文件系统

```bash
# Windows PowerShell
ls .\data\documents\doc_1766142807149\
```

**应该看到**:
```
倡导节约用水PPT作品下载——.pptx  ✅ 原始文件名！
```

### 3. 下载文档

```bash
curl -X GET "http://localhost:8080/api/documents/download?fileName=倡导节约用水PPT作品下载——.pptx" \
  -o downloaded.pptx
```

**文件名正确！**

---

## 🔄 迁移指南

### 对于已存在的文档

**旧格式**:
```
./data/documents/doc_xxx_filename.ext
```

**新格式**:
```
./data/documents/doc_xxx/filename.ext
```

**迁移脚本**（PowerShell）:
```powershell
# 列出所有旧格式的文件
Get-ChildItem ./data/documents -File | ForEach-Object {
    $filename = $_.Name
    if ($filename -match '^(doc_\d+)(.+)$') {
        $docId = $matches[1]
        $originalName = # 从数据库获取原始文件名
        
        # 创建子目录
        New-Item -ItemType Directory -Path "./data/documents/$docId" -Force
        
        # 移动文件
        Move-Item $_.FullName "./data/documents/$docId/$originalName"
    }
}
```

---

## 🎉 总结

### 核心改进

1. ✅ **保留原始文件名**: 支持中文、特殊字符
2. ✅ **子目录组织**: 通过 documentId 隔离
3. ✅ **可读性增强**: 文件系统中一目了然
4. ✅ **唯一性保证**: documentId 避免冲突

### 用户价值

- 📂 **易于管理**: 直接在文件系统中识别文档
- 🔍 **便于查找**: 保留原始文件名便于搜索
- 🌍 **国际化支持**: 支持任意语言的文件名
- 🛡️ **数据安全**: 子目录隔离，不会冲突

---

**修复完成时间**: 2025-12-19  
**状态**: ✅ 已修复并验证  
**编译状态**: ✅ BUILD SUCCESS

🎉 **文件名保留问题已解决！现在上传的文档会保留原始文件名！** 📄✨

