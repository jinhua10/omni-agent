# ✅ Excel 图片提取和 Vision LLM 支持

## 🎯 功能说明

为 Excel 文档（新旧版本）添加了完整的图片提取和 Vision LLM 支持，将图片分析结果作为文本插入到表格末尾。

## 📋 支持的格式

| 格式 | 图片提取 | Vision LLM | 结果插入位置 |
|------|---------|-----------|-------------|
| `.xlsx` (新版) | ✅ | ✅ | 表格末尾 |
| `.xls` (旧版) | ✅ | ✅ | 表格末尾 |

## 🔧 技术实现

### 1. 图片提取

#### 新版 Excel (.xlsx)

```java
// 获取绘图容器
XSSFDrawing drawing = ((XSSFSheet) sheet).getDrawingPatriarch();

if (drawing != null) {
    for (XSSFShape shape : drawing.getShapes()) {
        if (shape instanceof XSSFPicture) {
            XSSFPicture picture = (XSSFPicture) shape;
            
            // 获取图片数据
            XSSFPictureData pictureData = picture.getPictureData();
            byte[] imageBytes = pictureData.getData();
            String extension = pictureData.suggestFileExtension();
            
            // 获取图片位置（锚点）
            XSSFClientAnchor anchor = picture.getClientAnchor();
            int row = anchor.getRow1() + 1;
            int col = anchor.getCol1() + 1;
        }
    }
}
```

#### 旧版 Excel (.xls)

```java
// 获取绘图容器
HSSFPatriarch patriarch = ((HSSFSheet) sheet).getDrawingPatriarch();

if (patriarch != null) {
    for (HSSFShape shape : patriarch.getChildren()) {
        if (shape instanceof HSSFPicture) {
            HSSFPicture picture = (HSSFPicture) shape;
            
            // 获取图片数据
            HSSFPictureData pictureData = picture.getPictureData();
            byte[] imageBytes = pictureData.getData();
            String extension = pictureData.suggestFileExtension();
            
            // 获取图片位置（锚点）
            HSSFClientAnchor anchor = picture.getClientAnchor();
            int row = anchor.getRow1() + 1;
            int col = anchor.getCol1() + 1;
        }
    }
}
```

### 2. Vision LLM 分析

图片提取后，使用 `SmartImageExtractor` 进行内容提取（调用 Vision LLM）：

```java
ByteArrayInputStream imageStream = new ByteArrayInputStream(imageBytes);
String imageContent = imageExtractor.extractContent(imageStream, imageName);
```

### 3. 结果插入

图片分析结果插入到工作表数据的末尾：

```java
// 提取表格数据
for (Row row : sheet) {
    // 提取单元格内容...
}

// ⭐ 插入图片分析结果
if (extractImages && imageExtractor != null) {
    List<String> imageDescriptions = extractExcelImages(workbook, sheet, i, filename);
    if (!imageDescriptions.isEmpty()) {
        content.append("\n--- 图片内容 ---\n");
        for (String desc : imageDescriptions) {
            content.append(desc).append("\n");
        }
    }
}
```

## 📊 输出格式

### 没有图片的工作表

```
=== 工作表: Sheet1 ===
姓名	年龄	城市
张三	25	北京
李四	30	上海
王五	28	广州
```

### 有图片的工作表

```
=== 工作表: Sheet1 ===
姓名	年龄	城市
张三	25	北京
李四	30	上海
王五	28	广州

--- 图片内容 ---
[图片 1] 位置: 第2行, 第4列
这是一张产品图片，展示了...
（Vision LLM 分析结果）

[图片 2] 位置: 第5行, 第4列
这是一张统计图表，显示...
（Vision LLM 分析结果）
```

## 🔍 图片位置信息

提取图片时会记录其在工作表中的位置（基于锚点）：

- **行号**：图片左上角所在的行（1-based）
- **列号**：图片左上角所在的列（1-based）

这样用户可以知道图片原本在表格的哪个位置。

## 📝 完整代码示例

### SimpleDocumentParser.java

```java
/**
 * 解析 Excel Workbook（通用方法，支持新旧版本）
 * ⭐ 支持图片提取，将图片描述添加到表格末尾
 */
private String parseExcelWorkbook(Workbook workbook, String filename) {
    StringBuilder content = new StringBuilder();
    int sheetCount = workbook.getNumberOfSheets();

    for (int i = 0; i < sheetCount; i++) {
        Sheet sheet = workbook.getSheetAt(i);
        content.append("=== 工作表: ").append(sheet.getSheetName()).append(" ===\n");

        // 提取表格数据
        for (Row row : sheet) {
            // 提取单元格内容...
        }

        // ⭐ 提取图片（如果启用）
        if (extractImages && imageExtractor != null) {
            List<String> imageDescriptions = extractExcelImages(workbook, sheet, i, filename);
            if (!imageDescriptions.isEmpty()) {
                content.append("\n--- 图片内容 ---\n");
                for (String desc : imageDescriptions) {
                    content.append(desc).append("\n");
                }
            }
        }

        content.append("\n");
    }

    return content.toString().trim();
}

/**
 * 提取 Excel 工作表中的图片
 * ⭐ 支持新旧版本 Excel
 */
private List<String> extractExcelImages(Workbook workbook, Sheet sheet, int sheetIndex, String filename) {
    List<String> imageDescriptions = new ArrayList<>();
    int imageCount = 0;

    try {
        // 新版 Excel (.xlsx)
        if (workbook instanceof XSSFWorkbook) {
            XSSFDrawing drawing = ((XSSFSheet) sheet).getDrawingPatriarch();
            if (drawing != null) {
                for (XSSFShape shape : drawing.getShapes()) {
                    if (shape instanceof XSSFPicture) {
                        // 提取图片...
                    }
                }
            }
        }
        // 旧版 Excel (.xls)
        else if (workbook instanceof HSSFWorkbook) {
            HSSFPatriarch patriarch = ((HSSFSheet) sheet).getDrawingPatriarch();
            if (patriarch != null) {
                for (HSSFShape shape : patriarch.getChildren()) {
                    if (shape instanceof HSSFPicture) {
                        // 提取图片...
                    }
                }
            }
        }
    } catch (Exception e) {
        log.error("提取 Excel 工作表图片失败", e);
    }

    return imageDescriptions;
}
```

## 💡 使用场景

### 1. 产品目录表格

```
Excel 表格：
产品名称 | 价格 | 库存 | [产品图片]
手机    | 3999 | 100  | (图片)
电脑    | 5999 | 50   | (图片)

↓ 处理后

产品名称 | 价格 | 库存
手机    | 3999 | 100
电脑    | 5999 | 50

--- 图片内容 ---
[图片 1] 位置: 第2行, 第4列
这是一款智能手机，黑色外壳...

[图片 2] 位置: 第3行, 第4列
这是一台笔记本电脑，银色外壳...
```

### 2. 数据分析报表

```
Excel 表格：
月份 | 销售额 | [趋势图]
1月  | 10万  | (图片：折线图)
2月  | 12万  | ...

↓ 处理后

月份 | 销售额
1月  | 10万
2月  | 12万

--- 图片内容 ---
[图片 1] 位置: 第2行, 第3列
这是一张销售趋势折线图，显示了1-6月的销售增长...
横轴是月份，纵轴是销售额...
```

### 3. 工程图纸表格

```
Excel 表格：
零件编号 | 名称 | [技术图纸]
P001   | 螺栓 | (图片)

↓ 处理后

零件编号 | 名称
P001   | 螺栓

--- 图片内容 ---
[图片 1] 位置: 第2行, 第3列
这是一张机械零件的技术图纸，显示了螺栓的尺寸标注...
```

## ✅ 优势

1. **✅ 完整信息提取**
   - 表格数据 + 图片内容 = 完整信息
   - 不会遗漏 Excel 中的图片信息

2. **✅ 位置标注**
   - 记录图片在表格中的原始位置
   - 方便定位和理解上下文

3. **✅ 结构清晰**
   - 表格数据在前
   - 图片描述在后（专门的"图片内容"区域）
   - 便于阅读和检索

4. **✅ 支持新旧格式**
   - `.xlsx` 和 `.xls` 统一处理
   - 代码复用性高

5. **✅ Vision LLM 分析**
   - 自动理解图片内容
   - 提取图表中的数据和趋势
   - 识别产品图片特征

## 🧪 测试

### 上传包含图片的 Excel

```bash
# 启动应用
cd D:\Jetbrains\omni-agent\omni-agent-example-basic
mvn spring-boot:run
```

访问 http://localhost:8080，上传包含图片的 Excel 文件。

### 预期结果

1. **文本提取**：所有单元格内容
2. **图片提取**：所有嵌入图片
3. **图片分析**：Vision LLM 分析每张图片
4. **结果格式**：
```
=== 工作表: Sheet1 ===
单元格数据...

--- 图片内容 ---
[图片 1] 位置: ...
Vision 分析结果...

[图片 2] 位置: ...
Vision 分析结果...
```

## 📦 修改的文件

1. ✅ `SimpleDocumentParser.java`
   - `parseExcelWorkbook()` - 添加图片提取调用
   - `extractExcelImages()` - 新增方法，提取 Excel 图片

2. ✅ `OLD_OFFICE_FORMAT_SUPPORT.md` - 更新支持矩阵

## 🎉 总结

**Excel 现在完全支持图片提取和 Vision LLM！**

| 格式 | 文本 | 图片 | Vision LLM | 插入位置 |
|------|------|------|-----------|---------|
| `.xlsx` | ✅ | ✅ | ✅ | 表格末尾 |
| `.xls` | ✅ | ✅ | ✅ | 表格末尾 |

无论新旧格式，都能：
- ✅ 提取完整的表格数据
- ✅ 提取所有嵌入图片
- ✅ 记录图片所在位置
- ✅ 使用 Vision LLM 分析图片
- ✅ 将图片描述插入表格末尾
- ✅ 支持 RAG 索引和检索

**完美！所有 Office 格式都支持图片提取和 Vision LLM了！** 🚀

