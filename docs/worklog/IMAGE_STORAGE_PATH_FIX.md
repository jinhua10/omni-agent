# ✅ 图片存储路径修复 - 使用文件名而不是 documentId

## 🐛 问题描述

图片保存路径使用了 `documentId`（例如 `doc_1766224609148_xxx.pptx`），而不是文件名，导致路径难以理解：

```
❌ 错误的路径：
data/storage/images/doc_1766224609148_清新矢量绿色环保PPT模板——.pptx/
├── page_001_img_000.png
└── page_001_img_000.png.meta

✅ 期望的路径：
data/storage/images/清新矢量绿色环保PPT模板——.pptx/
├── page_001_img_000.png
└── page_001_img_000.png.meta
```

## 🔍 问题根因

在 `FileWatcherService.processNewFile()` 方法中，保存图片时使用了 `documentId`：

```java
// ❌ 错误：使用 documentId
String documentId = "doc_" + System.currentTimeMillis() + "_" + 
        relativePathStr.replace("/", "_").replace("\\", "_");

// 保存图片
imageStorageService.saveImage(documentId, image.getData(), image.getFormat());
```

这导致图片目录名称变成了 `doc_1766224609148_xxx`，而不是原始的文件名。

## ✅ 解决方案

### 修改前（错误）

```java
// FileWatcherService.java
if (images != null && !images.isEmpty()) {
    log.info("🖼️ 保存提取的图片: {} 张", images.size());
    for (var image : images) {
        try {
            // ❌ 使用 documentId
            imageStorageService.saveImage(documentId, image.getData(), image.getFormat());
        } catch (Exception ex) {
            log.warn("⚠️ 保存图片失败: {}", ex.getMessage());
        }
    }
}
```

### 修改后（正确）⭐

```java
// FileWatcherService.java
if (images != null && !images.isEmpty()) {
    log.info("🖼️ 保存提取的图片: {} 张", images.size());
    
    // ⭐ 按页码分组图片
    Map<Integer, List<ExtractedImage>> imagesByPage = new HashMap<>();
    for (var img : images) {
        int pageNum = img.getPageNumber() > 0 ? img.getPageNumber() : 1;
        imagesByPage.computeIfAbsent(pageNum, k -> new ArrayList<>()).add(img);
    }
    
    int savedImageCount = 0;
    // ⭐ 遍历每一页，为该页的图片添加序号
    for (Map.Entry<Integer, List<ExtractedImage>> entry : imagesByPage.entrySet()) {
        int pageNum = entry.getKey();
        List<ExtractedImage> pageImages = entry.getValue();
        
        for (int imgIndex = 0; imgIndex < pageImages.size(); imgIndex++) {
            var extractedImage = pageImages.get(imgIndex);
            
            try {
                // ⭐ 在 metadata 中添加图片序号
                Map<String, Object> metadata = extractedImage.getMetadata();
                if (metadata == null) {
                    metadata = new HashMap<>();
                }
                metadata.put("imageIndex", imgIndex);  // 图片在该页的序号
                metadata.put("pageNumber", pageNum);   // 确保页码信息存在
                
                // ⭐ 使用文件名而不是 documentId
                String imageId = imageStorageService.saveImage(
                        filename,  // ⭐ 使用文件名（不是 documentId）
                        extractedImage.getData(),
                        extractedImage.getFormat(),
                        metadata);  // 传递包含序号的 metadata
                if (imageId != null) {
                    savedImageCount++;
                }
            } catch (Exception ex) {
                log.warn("⚠️ 保存图片失败 (page={}, img={}): {}", pageNum, imgIndex, ex.getMessage());
            }
        }
    }
    log.info("✅ 图片已保存: {} 张 (共 {} 页)", savedImageCount, imagesByPage.size());
}
```

## 📊 修复效果对比

### 修改前

**目录结构**：
```
data/storage/images/
├── doc_1766224609148_清新矢量绿色环保PPT模板——.pptx/
│   ├── page_001_img_000.png
│   ├── page_001_img_000.png.meta
│   ├── page_002_img_000.png
│   └── page_002_img_000.png.meta
└── doc_1766224610234_技术文档.pdf/
    ├── page_001_img_000.png
    └── page_001_img_000.png.meta
```

**问题**：
- ❌ 目录名包含 documentId，难以识别
- ❌ 无法直接看出是哪个文件的图片
- ❌ 与分块、原始文档的命名不一致

### 修改后 ✅

**目录结构**：
```
data/storage/images/
├── 清新矢量绿色环保PPT模板——.pptx/
│   ├── page_001_img_000.png
│   ├── page_001_img_000.png.meta
│   ├── page_002_img_000.png
│   └── page_002_img_000.png.meta
└── 技术文档.pdf/
    ├── page_001_img_000.png
    └── page_001_img_000.png.meta
```

**优势**：
- ✅ 目录名使用原始文件名，清晰易懂
- ✅ 一眼就能看出是哪个文件的图片
- ✅ 与分块、原始文档的命名一致

## 📁 完整的存储结构

修复后，整个存储结构保持一致：

```
data/storage/
├── documents/           # 原始文档
│   ├── 清新矢量绿色环保PPT模板——.pptx/
│   │   └── 清新矢量绿色环保PPT模板——.pptx
│   └── 技术文档.pdf/
│       └── 技术文档.pdf
│
├── chunks/              # 文档分块
│   ├── 清新矢量绿色环保PPT模板——.pptx/
│   │   ├── chunk_000.chunk
│   │   └── chunk_001.chunk
│   └── 技术文档.pdf/
│       ├── chunk_000.chunk
│       └── chunk_001.chunk
│
├── images/              # 提取的图片 ⭐ 已修复
│   ├── 清新矢量绿色环保PPT模板——.pptx/
│   │   ├── page_001_img_000.png
│   │   ├── page_001_img_000.png.meta
│   │   ├── page_002_img_000.png
│   │   └── page_002_img_000.png.meta
│   └── 技术文档.pdf/
│       ├── page_001_img_000.png
│       └── page_001_img_000.png.meta
│
└── ppl/                 # PPL 数据（如果启用）
    ├── 清新矢量绿色环保PPT模板——.pptx/
    └── 技术文档.pdf/
```

**一致性**：
- ✅ 所有目录都使用原始文件名
- ✅ 目录结构清晰，易于导航
- ✅ 便于手动查找和管理

## 🔧 相关改进

在修复的同时，还增强了图片保存逻辑：

### 1. 按页码分组

```java
// ⭐ 按页码分组图片
Map<Integer, List<ExtractedImage>> imagesByPage = new HashMap<>();
for (var img : images) {
    int pageNum = img.getPageNumber() > 0 ? img.getPageNumber() : 1;
    imagesByPage.computeIfAbsent(pageNum, k -> new ArrayList<>()).add(img);
}
```

### 2. 自动添加图片序号

```java
// ⭐ 为每页的图片添加序号
for (int imgIndex = 0; imgIndex < pageImages.size(); imgIndex++) {
    metadata.put("imageIndex", imgIndex);  // 图片在该页的序号
    metadata.put("pageNumber", pageNum);   // 确保页码信息存在
}
```

这样可以确保图片文件名格式正确：`page_001_img_000.png`（页码3位，序号3位）

### 3. 更详细的日志

```java
log.info("✅ 图片已保存: {} 张 (共 {} 页)", savedImageCount, imagesByPage.size());
log.warn("⚠️ 保存图片失败 (page={}, img={}): {}", pageNum, imgIndex, ex.getMessage());
```

## 🎯 使用场景

### 场景 1：上传 PPT

```bash
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@清新矢量绿色环保PPT模板——.pptx"
```

**文件保存到**：`data/documents/清新矢量绿色环保PPT模板——.pptx`

**30秒后自动处理**：

```
✅ Vision LLM 处理完成
   ├── 提取 10 页幻灯片
   └── 保存到 data/storage/images/清新矢量绿色环保PPT模板——.pptx/
       ├── page_001_img_000.png
       ├── page_002_img_000.png
       └── ...

✅ 分块保存
   └── data/storage/chunks/清新矢量绿色环保PPT模板——.pptx/
       ├── chunk_000.chunk
       └── chunk_001.chunk

✅ RAG 索引完成
```

### 场景 2：上传 PDF

```bash
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@技术文档.pdf"
```

**处理结果**：

```
data/storage/
├── documents/技术文档.pdf/
├── images/技术文档.pdf/        ⭐ 使用文件名
│   ├── page_001_img_000.png
│   ├── page_002_img_000.png
│   └── ...
└── chunks/技术文档.pdf/
```

## ✅ 验证方法

### 1. 上传文件

```bash
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@test.pptx"
```

### 2. 等待 30 秒处理完成

### 3. 检查目录结构

```bash
# Windows PowerShell
Get-ChildItem -Path "data\storage\images" -Recurse

# 预期输出
data\storage\images\test.pptx\        # ✅ 使用文件名
├── page_001_img_000.png
├── page_001_img_000.png.meta
└── ...
```

### 4. 验证一致性

```bash
# 检查所有存储目录
Get-ChildItem -Path "data\storage" -Directory

# 预期输出（目录名一致）
documents\
  test.pptx\
chunks\
  test.pptx\
images\
  test.pptx\        # ✅ 一致
ppl\
  test.pptx\
```

## 📝 修改总结

| 项目 | 修改前 | 修改后 |
|------|--------|--------|
| **图片目录名** | `doc_1766224609148_xxx.pptx` | `xxx.pptx` |
| **可读性** | ❌ 难以识别 | ✅ 清晰易懂 |
| **一致性** | ❌ 与其他目录不一致 | ✅ 与 documents、chunks 一致 |
| **图片序号** | ❌ 可能缺失 | ✅ 自动添加 |
| **日志详细度** | 基础 | ✅ 详细（页码、序号） |

## 🎉 总结

**修复内容**：
1. ✅ 修改 `FileWatcherService` 中的图片保存逻辑
2. ✅ 使用 `filename` 而不是 `documentId`
3. ✅ 增强图片序号处理（按页分组）
4. ✅ 更详细的日志输出

**修复效果**：
- ✅ 图片目录使用原始文件名
- ✅ 与 documents、chunks 目录命名一致
- ✅ 易于查找和管理
- ✅ 图片文件名格式正确（`page_001_img_000.png`）

**现在图片存储路径清晰易懂，与整体存储结构保持一致！** 🚀

