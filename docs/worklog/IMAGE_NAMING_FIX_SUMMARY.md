# ✅ 图片命名优化 - 完成总结

## 🎯 优化目标

将图片文件名从随机 UUID 改为**有意义的页码+序号格式**，使用 **3位补零**。

## ✨ 最终效果

### 命名格式

```
page_001_img_000.png     # 第1页，第1张图片
page_001_img_001.png     # 第1页，第2张图片  
page_002_img_000.png     # 第2页，第1张图片
page_010_img_005.png     # 第10页，第6张图片
```

**格式说明**：
- `page_XXX`: 页码，3位补零（001-999）
- `img_XXX`: 该页的图片序号，3位补零（000-999）
- 文件在文件系统中自动按页码排序

### 目录结构

```
data/storage/images/倡导节约用水PPT.pptx/
├── page_001_img_000.png          ✅ 清晰的页码和序号
├── page_001_img_000.png.meta
├── page_002_img_000.png
├── page_002_img_000.png.meta
├── page_002_img_001.png          ✅ 同一页的多张图片
├── page_002_img_001.png.meta
├── page_003_img_000.png
└── page_003_img_000.png.meta
```

## 🔧 代码修改

### 1. DocumentManagementController.java

**修改点**：`saveExtractedImages()` 方法

```java
// ⭐ 按页码分组图片
Map<Integer, List<ExtractedImage>> imagesByPage = new HashMap<>();

// ⭐ 为每页的图片添加序号
for (int imgIndex = 0; imgIndex < pageImages.size(); imgIndex++) {
    metadata.put("imageIndex", imgIndex);  // 图片序号
    metadata.put("pageNumber", pageNum);   // 页码
}
```

### 2. FileDocumentStorage.java

**修改点**：`saveImage()` 方法

```java
// ⭐ 从 metadata 中获取页码和图片序号
Integer pageNum = image.getPageNumber();
Integer imageIndex = (Integer) image.getMetadata().get("imageIndex");

// ⭐ 格式：page_001_img_000.png（页码3位，图片序号3位）
if (pageNum != null && pageNum > 0 && imageIndex != null) {
    imageFilename = String.format("page_%03d_img_%03d.%s", 
                                  pageNum, imageIndex, format);
}
```

## 📊 优势对比

| 方面 | 优化前 | 优化后 |
|------|--------|--------|
| **文件名** | `image_0e466bb7.png` | `page_001_img_000.png` |
| **可读性** | ❌ UUID 无意义 | ✅ 一眼看出页码和序号 |
| **排序** | ❌ 随机乱序 | ✅ 自动按页码排序 |
| **定位** | ❌ 无法定位 | ✅ 快速找到目标页 |
| **浏览** | ❌ 混乱 | ✅ 清晰有序 |

## 🚀 使用示例

### 场景 1：快速定位某一页的图片

```bash
# 查看第5页的所有图片
ls data/storage/images/你的文档/page_005_*

# 输出
page_005_img_000.png
page_005_img_000.png.meta
page_005_img_001.png
page_005_img_001.png.meta
```

### 场景 2：查看同一页的图片分析

```bash
# 查看第1页第1张图片的 Vision 分析
cat data/storage/images/你的文档/page_001_img_000.png.meta | jq .metadata.visionAnalysis
```

### 场景 3：统计每页图片数量

```bash
# 统计每页有几张图片
for i in {001..010}; do
  count=$(ls data/storage/images/你的文档/page_${i}_img_*.png 2>/dev/null | wc -l)
  echo "第 $i 页: $count 张图片"
done
```

## 📝 元数据示例

`page_001_img_000.png.meta`:

```json
{
  "id": "img_xyz789",
  "documentId": "倡导节约用水PPT.pptx",
  "filename": "page_001_img_000.png",
  "format": "png",
  "pageNumber": 1,
  "metadata": {
    "imageIndex": 0,               // ⭐ 图片序号
    "pageNumber": 1,                // ⭐ 页码
    "visionAnalysis": "节约用水主题页...",
    "slideText": "节约用水 从我做起",
    "fileName": "倡导节约用水PPT.pptx"
  },
  "createdAt": 1734615694000
}
```

## ✅ 测试验证

### 启动应用

```bash
cd D:\Jetbrains\omni-agent\omni-agent-example-basic
mvn spring-boot:run
```

### 上传 PPT 测试

1. 访问 http://localhost:8080
2. 上传一个 PPT 文档
3. 检查生成的图片文件

### 预期结果

```bash
ls data/storage/images/你的文档名.pptx/

# 应该看到
page_001_img_000.png
page_001_img_000.png.meta
page_002_img_000.png
page_002_img_000.png.meta
...
```

## 🎉 总结

✅ **图片命名已优化**
- 使用 `page_XXX_img_XXX.png` 格式
- 页码和序号都是 3位补零
- 文件系统中自动排序
- 一眼识别页码和序号
- 方便浏览、定位和调试

**现在图片文件名清晰有序，在文件系统中排序一目了然！** 🎊

