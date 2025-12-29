# ✅ 图片命名优化完成

## 🎯 优化内容

图片文件名从随机 UUID 改为**有意义的页码+序号格式**，使用 **3位补零**，在文件系统中排序清晰。

## 📝 命名格式

### 新格式

```
page_001_img_000.png
page_001_img_001.png
page_002_img_000.png
page_002_img_001.png
page_002_img_002.png
```

**格式说明**：
- `page_001`: 页码，3位补零（001-999）
- `img_000`: 该页的图片序号，3位补零（000-999）
- `.png`: 图片格式

### 旧格式（已废弃）

```
image_0e466bb7.png   ❌ 随机 UUID，无法识别页码
```

## 📊 示例对比

### 优化前

```
data/storage/images/倡导节约用水PPT.pptx/
├── image_0e466bb7.png
├── image_0e466bb7.png.meta
├── image_a3d2f91c.png
├── image_a3d2f91c.png.meta
├── image_7b8e4a2f.png
└── image_7b8e4a2f.png.meta
```

**问题**：
- ❌ 无法识别是哪一页的图片
- ❌ 文件名乱序，难以浏览
- ❌ 无法快速定位某一页的图片

### 优化后

```
data/storage/images/倡导节约用水PPT.pptx/
├── page_001_img_000.png
├── page_001_img_000.png.meta
├── page_002_img_000.png
├── page_002_img_000.png.meta
├── page_002_img_001.png
├── page_002_img_001.png.meta
├── page_003_img_000.png
└── page_003_img_000.png.meta
```

**优点**：
- ✅ 一眼看出是哪一页的图片
- ✅ 自动按页码排序
- ✅ 同一页的图片聚在一起
- ✅ 快速定位：想看第5页？直接找 `page_005_*`

## 🔧 技术实现

### 1. **按页面分组图片**

```java
// 按页码分组图片
Map<Integer, List<ExtractedImage>> imagesByPage = new HashMap<>();
for (ExtractedImage img : images) {
    int pageNum = img.getPageNumber() > 0 ? img.getPageNumber() : 1;
    imagesByPage.computeIfAbsent(pageNum, k -> new ArrayList<>()).add(img);
}
```

### 2. **为每页的图片添加序号**

```java
// 遍历每一页
for (Map.Entry<Integer, List<ExtractedImage>> entry : imagesByPage.entrySet()) {
    int pageNum = entry.getKey();
    List<ExtractedImage> pageImages = entry.getValue();
    
    // 为该页的图片添加序号
    for (int imgIndex = 0; imgIndex < pageImages.size(); imgIndex++) {
        metadata.put("imageIndex", imgIndex);  // 图片序号
        metadata.put("pageNumber", pageNum);   // 页码
    }
}
```

### 3. **生成文件名**

```java
if (pageNum != null && pageNum > 0 && imageIndex != null) {
    // ⭐ 格式：page_001_img_000.png（页码3位，图片序号3位）
    imageFilename = String.format("page_%03d_img_%03d.%s", pageNum, imageIndex, format);
} else if (pageNum != null && pageNum > 0) {
    // 如果只有页码，没有图片序号：page_001_img.png
    imageFilename = String.format("page_%03d_img.%s", pageNum, format);
} else {
    // 降级：使用 image_xxx.png 格式
    imageFilename = String.format("image_%s.%s", imageId.substring(0, 8), format);
}
```

## 📁 完整目录结构示例

```
data/storage/
└── images/
    └── 倡导节约用水PPT.pptx/
        ├── page_001_img_000.png         # 第1页，第1张图片
        ├── page_001_img_000.png.meta    # 元数据（包含 Vision 分析）
        ├── page_002_img_000.png         # 第2页，第1张图片
        ├── page_002_img_000.png.meta
        ├── page_002_img_001.png         # 第2页，第2张图片
        ├── page_002_img_001.png.meta
        ├── page_003_img_000.png         # 第3页，第1张图片
        ├── page_003_img_000.png.meta
        ├── ...
        ├── page_010_img_000.png         # 第10页，第1张图片
        └── page_010_img_000.png.meta
```

## 🎯 使用场景

### 1. **浏览图片**

在文件管理器中打开 `data/storage/images/你的文档/`，可以清晰地看到：
- 哪些页有图片
- 每页有几张图片
- 按页码自动排序

### 2. **定位图片**

想看第5页的图片？直接搜索 `page_005_`

### 3. **调试和检查**

- 快速检查某一页的图片是否提取成功
- 查看某页的 Vision LLM 分析结果（`.meta` 文件）

### 4. **批量处理**

```bash
# 查看所有第1页的图片
ls data/storage/images/*/page_001_*

# 统计每个文档第1页有几张图片
ls data/storage/images/*/page_001_* | wc -l
```

## 🔍 元数据示例

`page_001_img_000.png.meta`:

```json
{
  "id": "img_xyz789",
  "documentId": "倡导节约用水PPT.pptx",
  "filename": "page_001_img_000.png",
  "format": "png",
  "width": 1920,
  "height": 1080,
  "pageNumber": 1,
  "size": 245678,
  "metadata": {
    "imageIndex": 0,
    "pageNumber": 1,
    "visionAnalysis": "这是PPT的标题页，展示了节约用水的主题...",
    "slideText": "节约用水 从我做起",
    "fileName": "倡导节约用水PPT.pptx",
    "processor": "VisionLLM",
    "model": "qwen-vl-plus",
    "analyzedAt": 1734615694000
  },
  "createdAt": 1734615694000
}
```

## ✨ 优势总结

| 方面 | 优化前 | 优化后 |
|------|--------|--------|
| **可读性** | ❌ UUID 无意义 | ✅ 页码+序号清晰 |
| **排序** | ❌ 随机乱序 | ✅ 自动按页码排序 |
| **定位** | ❌ 无法快速定位 | ✅ 一眼找到目标页 |
| **浏览** | ❌ 难以理解结构 | ✅ 文件夹清晰有序 |
| **调试** | ❌ 不知道哪张出错 | ✅ 明确知道页码 |

## 🚀 测试

### 上传 PPT 文档

```bash
cd omni-agent-p2p-basic
mvn spring-boot:run
```

访问 http://localhost:8080，上传一个 PPT 文档。

### 查看结果

```bash
# 查看图片目录
ls data/storage/images/你的文档名.pptx/

# 预期看到
page_001_img_000.png
page_001_img_000.png.meta
page_002_img_000.png
page_002_img_000.png.meta
...
```

**完美！图片命名现在一目了然！** ✅

