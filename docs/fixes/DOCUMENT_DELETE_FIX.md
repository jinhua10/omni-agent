# 🐛 文档删除功能修复

**问题**: 页面删除文档后，`data/documents/` 目录下的物理文件仍然存在

**原因**: 删除逻辑只删除了数据库数据，未删除物理文件

---

## 🔍 问题分析

### 旧的删除逻辑

```java
// 只删除了数据库数据
storageService.deleteChunksByDocument(documentId);  // 删除分块
storageService.deleteImagesByDocument(documentId);   // 删除图片
ragService.deleteDocument(documentId);               // 删除RAG索引

// ❌ 缺少：删除 data/documents/ 下的原始文件
```

### 文件存储结构

```
data/
├── documents/                           ← 原始文件（.pptx, .pdf, .txt等）
│   └── doc_1765910250684_______.pptx   ← 这个文件未被删除！
├── chunks/                              ← 分块数据（已删除✅）
├── images/                              ← 图片数据（已删除✅）
└── rag-index/                          ← RAG索引（已删除✅）
```

---

## ✅ 修复方案

### 1. 新增方法：`deleteFileByDocumentId`

在 `FileStorageUtil` 中添加：

```java
/**
 * 通过文档ID删除文件（搜索以documentId开头的文件）
 */
public static boolean deleteFileByDocumentId(String documentId) {
    // 搜索 data/documents/ 目录
    // 查找以 {documentId}_ 开头的文件
    // 例如: doc_1765910250684_xxx.pptx
    
    List<Path> matchingFiles = Files.list(uploadDir)
        .filter(path -> path.getFileName().toString().startsWith(documentId + "_"))
        .collect(Collectors.toList());
    
    // 删除所有匹配的文件
    for (Path filePath : matchingFiles) {
        Files.delete(filePath);
        log.info("✅ 物理文件删除成功: {}", filePath);
    }
}
```

### 2. 更新删除逻辑

```java
// 单个文档删除
@DeleteMapping("/{documentId}")
public Map<String, Object> deleteDocument(@PathVariable String documentId) {
    // 1. 删除物理文件（新增）✅
    boolean fileDeleted = FileStorageUtil.deleteFileByDocumentId(actualDocumentId);
    
    // 2. 删除分块
    storageService.deleteChunksByDocument(actualDocumentId);
    
    // 3. 删除图片
    storageService.deleteImagesByDocument(actualDocumentId);
    
    // 4. 删除RAG索引
    ragService.deleteDocument(actualDocumentId);
}

// 批量删除
@PostMapping("/delete/batch")
public Map<String, Object> deleteDocuments(@RequestBody BatchDeleteRequest request) {
    for (String documentId : request.getDocumentIds()) {
        // 同样添加物理文件删除 ✅
        FileStorageUtil.deleteFileByDocumentId(documentId);
        storageService.deleteChunksByDocument(documentId);
        storageService.deleteImagesByDocument(documentId);
        ragService.deleteDocument(documentId);
    }
}
```

---

## 🎯 修复效果

### 删除前

```
data/documents/
├── doc_1765910250684_xxx.pptx     ← 存在
├── doc_1765920123456_yyy.pdf      ← 存在
└── doc_1765930987654_zzz.txt      ← 存在
```

### 删除 doc_1765910250684

```bash
DELETE /api/documents/doc_1765910250684
```

**日志输出**:
```
🗑️ 删除文档请求: doc_1765910250684
✅ 物理文件删除成功: data/documents/doc_1765910250684_xxx.pptx
✅ 分块删除成功
✅ 图片删除成功
✅ RAG索引删除成功
✅ 文档删除成功
```

### 删除后

```
data/documents/
├── doc_1765920123456_yyy.pdf      ← 存在
└── doc_1765930987654_zzz.txt      ← 存在
                                    ← doc_1765910250684_xxx.pptx 已删除！✅
```

---

## 🧪 测试方法

1. **上传文档**
   ```bash
   POST /api/documents/upload
   文件: test.pptx
   ```

2. **验证文件存在**
   ```bash
   ls data/documents/
   # 应该看到: doc_xxxxx_test.pptx
   ```

3. **删除文档**
   ```bash
   DELETE /api/documents/{documentId}
   ```

4. **验证文件已删除**
   ```bash
   ls data/documents/
   # 文件应该消失
   ```

---

## 📝 相关文件

- `DocumentManagementController.java` - 添加物理文件删除调用
- `FileStorageUtil.java` - 新增 `deleteFileByDocumentId()` 方法

---

## ✅ 验证清单

- [x] 单个文档删除 - 物理文件被删除
- [x] 批量文档删除 - 物理文件被删除
- [x] 分块数据删除 - 正常工作
- [x] 图片数据删除 - 正常工作
- [x] RAG索引删除 - 正常工作
- [x] 日志记录完整 - 可以看到删除成功的日志

---

**修复版本**: v3.0.1  
**修复日期**: 2025-12-17  
**问题严重程度**: 🔴 高（导致磁盘空间浪费）  
**修复状态**: ✅ 已完成

