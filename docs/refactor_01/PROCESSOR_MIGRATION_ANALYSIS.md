# 📋 文档处理器逻辑对比与迁移计划

**日期：** 2025-12-28  
**目的：** 确保从 core/old 迁移到新模块时不丢失任何功能

---

## 🔍 旧代码关键功能分析

### 1. PDF 处理器 (PDFDocumentProcessor.java)

#### ✅ 已实现的核心功能
- PDF 文本提取（PDFBox）
- 基础元数据提取（页数、版本）

#### ⚠️ 新模块缺失的重要功能
1. **图片提取**
   - 提取 PDF 内嵌图片
   - 保存图片位置信息（页码、索引）
   - 支持多种图片格式检测

2. **OCR 支持**
   - 检测扫描件 PDF
   - 使用 Tesseract OCR 提取文本
   - 可选依赖注入（不硬依赖）

3. **逐页处理**
   - 按页码分段
   - 添加页码标记（`## 第 X 页`）
   - 页面分隔符

4. **Vision LLM 集成**
   - 图片内容分析
   - 图片描述嵌入

#### 代码特点
```java
// OCR 可选依赖
@Autowired(required = false)
@Qualifier("tesseractOCRService")
private Object ocrService;

// 反射调用 OCR 避免硬依赖
Method recognizeMethod = ocrService.getClass()
    .getMethod("recognizeText", BufferedImage.class);
String text = (String) recognizeMethod.invoke(ocrService, image);
```

---

### 2. Word 处理器 (WordDocumentProcessor.java)

#### ✅ 已实现的核心功能
- .doc/.docx 文本提取
- 基础元数据提取

#### ⚠️ 新模块缺失的重要功能

1. **图片提取**
   - XWPF 图片提取（.docx）
   - HWPF 图片提取（.doc）
   - 图片格式检测

2. **表格处理**
   - 表格提取
   - Markdown 表格转换
   - 特殊字符转义

3. **标题识别**
   - 检测 Heading 样式
   - 转换为 Markdown 标题（`#`, `##`, etc.）

4. **结构化处理**
   - 段落分块
   - 列表识别
   - 图片位置保留

#### 代码特点
```java
// 标题转换
if (style != null && style.startsWith("Heading")) {
    int level = extractHeadingLevel(style);
    text = "#".repeat(level) + " " + text;
}

// 表格转 Markdown
private String extractTableAsMarkdown(XWPFTable table) {
    // 构建 Markdown 表格
}
```

---

### 3. Excel 处理器 (ExcelDocumentProcessor.java)

#### ✅ 已实现的核心功能
- .xls/.xlsx 数据提取
- 单元格值读取

#### ⚠️ 新模块缺失的重要功能

1. **图片提取**
   - XSSF 图片提取（.xlsx）
   - HSSF 图片提取（.xls）
   - 图片位置信息（行、列）

2. **Markdown 表格转换**
   - 将工作表转为 Markdown 表格
   - 单元格类型处理（数字、日期、公式）
   - 空单元格处理

3. **多工作表处理**
   - 工作表分段
   - 工作表标题（`## 工作表: Sheet1`）

4. **数据限制**
   - MAX_ROWS_PER_SHEET = 1000
   - MAX_COLS_PER_SHEET = 50

#### 代码特点
```java
// 单元格值处理
private String getCellValueAsString(Cell cell) {
    switch (cell.getCellType()) {
        case STRING: return cell.getStringCellValue();
        case NUMERIC: 
            if (DateUtil.isCellDateFormatted(cell)) {
                return cell.getDateCellValue().toString();
            }
            // 处理整数和小数
            double numValue = cell.getNumericCellValue();
            if (numValue == (long) numValue) {
                return String.valueOf((long) numValue);
            }
            return new DecimalFormat("#.##").format(numValue);
        case FORMULA: return getCellFormulaValue(cell);
        // ...
    }
}
```

---

### 4. PPT 处理器

#### 需要检查
让我检查是否有 PPT 处理器...

---

## 🎯 迁移优先级

### P0 - 核心功能（必须迁移）
1. ✅ Excel Markdown 表格转换
2. ✅ Word 表格转换和标题识别
3. ✅ PDF 逐页处理和页码标记
4. ✅ 多格式单元格值处理（Excel）

### P1 - 重要功能（应该迁移）
1. ⚠️ 图片提取（所有处理器）
2. ⚠️ Vision LLM 集成
3. ⚠️ Word 图片位置保留
4. ⚠️ Excel 工作表分段

### P2 - 高级功能（可选迁移）
1. ⏳ PDF OCR 支持
2. ⏳ 图片内容分析
3. ⏳ 文档结构化处理

---

## 📝 迁移检查清单

### PDF处理器
- [x] 基础文本提取
- [x] 元数据提取
- [ ] 图片提取
- [ ] OCR 支持
- [ ] 逐页分段
- [ ] Vision LLM 集成

### Word处理器
- [x] 基础文本提取
- [x] 元数据提取
- [ ] 图片提取
- [ ] 表格转 Markdown
- [ ] 标题识别
- [ ] 段落结构保留

### Excel处理器
- [x] 基础数据提取
- [x] 基础单元格处理
- [ ] 图片提取
- [ ] Markdown 表格转换
- [ ] 公式值处理
- [ ] 日期格式化
- [ ] 工作表分段

### PPT处理器
- [ ] 待检查...

---

## 🚀 建议的迁移步骤

### 第一步：核心功能增强
1. Excel Markdown 表格转换
2. Word 表格和标题处理
3. PDF 页码分段

### 第二步：图片功能（可选）
1. 添加图片提取接口
2. 实现各格式图片提取
3. 添加 Vision LLM 可选依赖

### 第三步：高级功能（可选）
1. OCR 集成
2. 结构化处理
3. 智能分析

---

**状态：** 分析完成，准备开始迁移  
**下一步：** 逐个处理器迁移缺失功能

