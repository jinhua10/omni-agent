# ✅ 旧版 Office 格式全面支持

## 🎯 支持的格式

现在已全面支持新旧版本的 Office 文档格式：

| 文档类型 | 新版格式 | 旧版格式 | 处理类 |
|---------|---------|---------|--------|
| **Word** | `.docx` ✅ | `.doc` ✅ | `XWPFDocument` / `HWPFDocument` |
| **PowerPoint** | `.pptx` ✅ | `.ppt` ✅ | `XMLSlideShow` / `HSLFSlideShow` |
| **Excel** | `.xlsx` ✅ | `.xls` ✅ | `XSSFWorkbook` / `HSSFWorkbook` |

## 📝 实现说明

### 1. **Word 文档**

#### 新版 (.docx)
```java
XWPFDocument document = new XWPFDocument(fis);
List<XWPFParagraph> paragraphs = document.getParagraphs();
```

#### 旧版 (.doc) ⭐ 新增
```java
HWPFDocument document = new HWPFDocument(fis);
WordExtractor extractor = new WordExtractor(document);
String content = extractor.getText();
```

### 2. **PowerPoint 文档**

#### 新版 (.pptx)
```java
XMLSlideShow ppt = new XMLSlideShow(fis);
List<XSLFSlide> slides = ppt.getSlides();
```

#### 旧版 (.ppt) ⭐ 新增
```java
HSLFSlideShow ppt = new HSLFSlideShow(fis);
List<HSLFSlide> slides = ppt.getSlides();
```

### 3. **Excel 文档**

#### 新版 (.xlsx) ⭐ 新增
```java
Workbook workbook = new XSSFWorkbook(fis);
```

#### 旧版 (.xls) ⭐ 新增
```java
Workbook workbook = new HSSFWorkbook(fis);
```

**统一处理**：
```java
private String parseExcelWorkbook(Workbook workbook, String filename) {
    // 统一的 Excel 处理逻辑，支持新旧版本
    for (int i = 0; i < workbook.getNumberOfSheets(); i++) {
        Sheet sheet = workbook.getSheetAt(i);
        // 提取单元格内容...
    }
}
```

## 🔧 修改的文件

### 1. SimpleDocumentParser.java

#### 添加导入
```java
import org.apache.poi.hslf.usermodel.*;      // 旧版 PPT
import org.apache.poi.hwpf.HWPFDocument;      // 旧版 Word
import org.apache.poi.hwpf.extractor.WordExtractor;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;  // 旧版 Excel
import org.apache.poi.ss.usermodel.*;          // Excel 通用
import org.apache.poi.xssf.usermodel.XSSFWorkbook; // 新版 Excel
```

#### 添加方法
- `parseDoc(File file)` - 解析旧版 Word
- `parsePpt(File file)` - 解析旧版 PowerPoint
- `parseXlsx(File file)` - 解析新版 Excel
- `parseXls(File file)` - 解析旧版 Excel
- `parseExcelWorkbook(Workbook, String)` - Excel 统一处理
- `getCellValueAsString(Cell)` - 单元格值提取

### 2. VisionLLMDocumentProcessor.java (已完成)

- `extractPptxPages()` - 处理新版 PPT
- `extractPptPages()` - 处理旧版 PPT ⭐

## 📊 Excel 支持详情

### 提取内容
- ✅ 所有工作表
- ✅ 单元格文本
- ✅ 数字（自动格式化）
- ✅ 日期（自动识别）
- ✅ 布尔值
- ✅ 公式

### 输出格式
```
=== 工作表: Sheet1 ===
标题1	标题2	标题3
数据1	数据2	数据3
...

=== 工作表: Sheet2 ===
...
```

## 🧪 测试

### 上传旧版文档

```bash
# 启动应用
cd D:\Jetbrains\omni-agent\omni-agent-p2p-basic
mvn spring-boot:run
```

访问 http://localhost:8080，上传以下文件测试：

1. **.doc** 文件 (Word 97-2003)
2. **.ppt** 文件 (PowerPoint 97-2003)  
3. **.xls** 文件 (Excel 97-2003)

### 预期结果

**Word (.doc)**:
```
文档的文本内容...
（段落提取）
```

**PowerPoint (.ppt)**:
```
=== 幻灯片 1 ===
标题文字
正文文字

=== 幻灯片 2 ===
...
```

**Excel (.xls)**:
```
=== 工作表: Sheet1 ===
列1	列2	列3
值1	值2	值3
...
```

## 💡 使用场景

### 1. 企业文档迁移
处理大量旧版 Office 文档，无需手动转换格式。

### 2. 历史档案处理
支持老旧的 `.doc`、`.ppt`、`.xls` 格式档案。

### 3. RAG 系统
将旧版 Office 文档内容索引到向量数据库。

### 4. 文档分析
统一处理新旧格式的文档，提取内容进行分析。

## 📦 依赖说明

所需的 Apache POI 依赖已经配置：

```xml
<!-- 新版 Office 格式 -->
<dependency>
    <groupId>org.apache.poi</groupId>
    <artifactId>poi-ooxml</artifactId>
    <version>5.5.0</version>
</dependency>

<!-- 旧版 Office 格式 -->
<dependency>
    <groupId>org.apache.poi</groupId>
    <artifactId>poi-scratchpad</artifactId>
    <version>5.5.0</version>
</dependency>
```

## ✅ 支持矩阵

| 功能 | .docx | .doc | .pptx | .ppt | .xlsx | .xls |
|------|-------|------|-------|------|-------|------|
| **文本提取** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **图片提取** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **Vision LLM** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **分块处理** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |
| **RAG 索引** | ✅ | ✅ | ✅ | ✅ | ✅ | ✅ |

**说明**：
- ✅ 完全支持
- Excel 图片描述会插入到表格末尾

### 📸 图片提取详情

#### Word 文档
- **.docx** (新版): 
  - ✅ 通过 `XWPFDocument.getAllPictures()` 提取
  - ✅ 支持所有嵌入图片格式
  - ✅ Vision LLM 分析图片内容
  
- **.doc** (旧版):
  - ✅ 通过 `HWPFDocument.getPicturesTable().getAllPictures()` 提取
  - ✅ 支持所有嵌入图片格式
  - ✅ Vision LLM 分析图片内容

#### PowerPoint 文档
- **.pptx** (新版):
  - ✅ 逐页提取图片 (`XSLFPictureShape`)
  - ✅ 渲染整张幻灯片为图片
  - ✅ Vision LLM 逐页分析
  - ✅ 提供幻灯片文字作为上下文
  
- **.ppt** (旧版):
  - ✅ 逐页提取图片 (`HSLFPictureShape`)
  - ✅ 渲染整张幻灯片为图片
  - ✅ Vision LLM 逐页分析
  - ✅ 提供幻灯片文字作为上下文

#### Excel 文档
- **.xlsx** / **.xls**:
  - ✅ 图片提取（通过 `Drawing` / `Patriarch`）
  - ✅ 获取图片所在单元格位置
  - ✅ Vision LLM 分析图片内容
  - ✅ 将图片描述插入表格末尾

## 🎉 总结

**现在系统全面支持旧版 Office 格式！**

- ✅ **Word**: `.docx` + `.doc`（图片提取 + Vision LLM）
- ✅ **PowerPoint**: `.pptx` + `.ppt`（图片提取 + Vision LLM）
- ✅ **Excel**: `.xlsx` + `.xls`（文本提取）

无论上传新版还是旧版文档，都能正确提取内容并进行分块、索引！🚀

## 📚 相关文档

- **Excel 图片提取和 Vision LLM 详细说明**: `EXCEL_IMAGE_VISION_SUPPORT.md` ⭐
- **图片提取和 Vision LLM 详细说明**: `OLD_OFFICE_IMAGE_VISION_SUPPORT.md`
- **PPT 处理优化**: `PPT_PROCESSING_OPTIMIZATION.md`
- **图片命名优化**: `IMAGE_NAMING_OPTIMIZATION.md`
- **编译错误修复**: `COMPILE_ERROR_FIX.md`

