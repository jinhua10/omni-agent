# Excel 文档处理完整流程说明

## 概述

OmniAgent 完整支持 Excel 文档（XLS 和 XLSX）的智能处理，包括：
- ✅ **多个工作表（Sheet）** - 遍历所有工作表
- ✅ **嵌入图片提取** - 提取每个工作表中的所有图片
- ✅ **图片位置信息** - 记录图片所在的行列位置
- ✅ **Vision LLM 分析** - 使用 Vision 模型理解图片内容
- ✅ **智能批处理** - 动态批次处理，提高效率
- ✅ **并行处理** - 多批次并行，大幅提升速度

## 支持的 Excel 格式

| 格式 | 扩展名 | 处理方法 | POI 库 | 说明 |
|------|--------|---------|---------|------|
| **新版 Excel** | `.xlsx` | `extractXlsxPages()` | XSSFWorkbook | Office 2007+ |
| **旧版 Excel** | `.xls` | `extractXlsPages()` | HSSFWorkbook | Office 97-2003 |

## Excel 处理流程

### 整体流程图

```
Excel 文档 (.xlsx/.xls)
    ↓
VisionLLMDocumentProcessor.extractPages()
    ↓
根据扩展名选择处理方法
    ├─ .xlsx → extractXlsxPages()
    └─ .xls  → extractXlsPages()
    ↓
遍历所有工作表（Sheet）
    ↓
提取每个工作表的所有图片
    ├─ 图片数据
    ├─ 图片位置（行、列）
    ├─ 工作表名称
    └─ 元数据信息
    ↓
每张图片作为一个 DocumentPage
    ↓
智能分批（SmartBatching）
    ↓
并行处理多个批次
    ↓
Vision LLM 分析每批图片
    ↓
合并结果
```

## 详细实现

### 1. XLSX 格式处理（新版 Excel）

```java
private List<DocumentPage> extractXlsxPages(ProcessingContext context) throws Exception {
    // 1. 打开 Excel 工作簿
    try (XSSFWorkbook workbook = new XSSFWorkbook(inputStream)) {
        List<DocumentPage> pages = new ArrayList<>();
        int pageNumber = 1;

        // 2. 遍历所有工作表
        for (int sheetIdx = 0; sheetIdx < workbook.getNumberOfSheets(); sheetIdx++) {
            XSSFSheet sheet = workbook.getSheetAt(sheetIdx);
            XSSFDrawing drawing = sheet.getDrawingPatriarch();

            // 3. 提取工作表中的所有图片
            if (drawing != null) {
                for (XSSFShape shape : drawing.getShapes()) {
                    if (shape instanceof XSSFPicture) {
                        XSSFPicture picture = (XSSFPicture) shape;
                        
                        // 4. 获取图片数据
                        XSSFPictureData pictureData = picture.getPictureData();
                        byte[] imageData = pictureData.getData();
                        
                        // 5. 获取图片位置（锚点）
                        XSSFClientAnchor anchor = picture.getClientAnchor();
                        String location = String.format(
                            "工作表[%s] 第%d行, 第%d列",
                            sheet.getSheetName(), 
                            anchor.getRow1() + 1,  // 行号（从1开始）
                            anchor.getCol1() + 1   // 列号（从1开始）
                        );
                        
                        // 6. 创建元数据
                        Map<String, Object> metadata = new HashMap<>();
                        metadata.put("fileName", context.getOriginalFileName());
                        metadata.put("sheetName", sheet.getSheetName());
                        metadata.put("sheetIndex", sheetIdx);
                        metadata.put("location", location);
                        metadata.put("documentType", "Excel");
                        
                        // 7. 创建 ExtractedImage
                        ExtractedImage image = ExtractedImage.builder()
                            .data(imageData)
                            .format(pictureData.suggestFileExtension())
                            .pageNumber(pageNumber)
                            .position(new ImagePosition(
                                anchor.getCol1(),  // X 坐标（列）
                                anchor.getRow1(),  // Y 坐标（行）
                                0, 0
                            ))
                            .metadata(metadata)
                            .build();
                        
                        // 8. 每张图片作为一页
                        DocumentPage page = new DocumentPage(pageNumber);
                        page.addImage(image);
                        pages.add(page);
                        pageNumber++;
                    }
                }
            }
        }
        
        return pages;
    }
}
```

### 2. XLS 格式处理（旧版 Excel）

```java
private List<DocumentPage> extractXlsPages(ProcessingContext context) throws Exception {
    // 1. 打开旧版 Excel 工作簿
    try (HSSFWorkbook workbook = new HSSFWorkbook(inputStream)) {
        List<DocumentPage> pages = new ArrayList<>();
        int pageNumber = 1;

        // 2. 遍历所有工作表
        for (int sheetIdx = 0; sheetIdx < workbook.getNumberOfSheets(); sheetIdx++) {
            HSSFSheet sheet = workbook.getSheetAt(sheetIdx);
            HSSFPatriarch patriarch = sheet.getDrawingPatriarch();

            // 3. 提取工作表中的所有图片
            if (patriarch != null) {
                for (HSSFShape shape : patriarch.getChildren()) {
                    if (shape instanceof HSSFPicture) {
                        HSSFPicture picture = (HSSFPicture) shape;
                        
                        // 4. 获取图片数据
                        HSSFPictureData pictureData = picture.getPictureData();
                        byte[] imageData = pictureData.getData();
                        
                        // 5. 获取图片位置
                        HSSFClientAnchor anchor = picture.getClientAnchor();
                        String location = String.format(
                            "工作表[%s] 第%d行, 第%d列",
                            sheet.getSheetName(),
                            anchor.getRow1() + 1,
                            anchor.getCol1() + 1
                        );
                        
                        // 6-8. 创建元数据、ExtractedImage 和 DocumentPage
                        // （与 XLSX 处理相同）
                        ...
                    }
                }
            }
        }
        
        return pages;
    }
}
```

## 提取的信息

### 1. 图片数据
- **原始字节数组**: `byte[] imageData`
- **图片格式**: `png`, `jpg`, `jpeg`, `bmp` 等

### 2. 位置信息
```java
ImagePosition {
    int x;        // X 坐标（列号）
    int y;        // Y 坐标（行号）
    int width;    // 宽度（Excel 中通常为 0）
    int height;   // 高度（Excel 中通常为 0）
}
```

### 3. 元数据信息
```java
Map<String, Object> metadata {
    "fileName": "财务报表.xlsx",           // 文件名
    "sheetName": "第一季度",               // 工作表名称
    "sheetIndex": 0,                      // 工作表索引（从0开始）
    "location": "工作表[第一季度] 第5行, 第3列",  // 位置描述
    "documentType": "Excel"                // 文档类型
}
```

## Vision LLM 提示词构建

系统会为每张图片构建包含上下文的提示词：

```java
private String buildVisionPrompt(DocumentPage page, String basePrompt) {
    StringBuilder prompt = new StringBuilder();
    
    // 1. 任务说明
    prompt.append("# 任务说明\n");
    prompt.append("请分析这张 Excel 工作表中的图片。\n\n");
    
    // 2. 文档信息
    Map<String, Object> metadata = page.getImages().get(0).getMetadata();
    String fileName = (String) metadata.get("fileName");
    String sheetName = (String) metadata.get("sheetName");
    String location = (String) metadata.get("location");
    
    prompt.append("## 文档信息\n");
    prompt.append("- 文件名：").append(fileName).append("\n");
    prompt.append("- 工作表：").append(sheetName).append("\n");
    prompt.append("- 位置：").append(location).append("\n\n");
    
    // 3. 分析要求
    prompt.append("## 分析要求\n");
    prompt.append("- 识别图片类型（图表、表格、流程图等）\n");
    prompt.append("- 提取图片中的文字内容\n");
    prompt.append("- 理解图表的数据含义\n");
    prompt.append("- 描述关键信息和趋势\n\n");
    
    return prompt.toString();
}
```

## 使用示例

### 示例 1: 财务报表分析

```
Excel 文件: 财务报表.xlsx
包含工作表:
  - 第一季度（包含 2 张图片：营收图表、费用饼图）
  - 第二季度（包含 1 张图片：利润趋势图）
  - 第三季度（包含 3 张图片）
  - 第四季度（包含 1 张图片）

处理结果:
  - 总共 7 个 DocumentPage（每张图片一页）
  - 智能分批：2-3 页一批
  - 并行处理：4 个批次同时进行
  - Vision LLM 分析每张图表的含义
  - 合并所有分析结果
```

### 示例 2: 项目报告

```
Excel 文件: 项目进度.xlsx
包含工作表:
  - 总览（包含甘特图）
  - 里程碑（包含时间线图）
  - 资源分配（包含资源分配表）
  - 风险评估（包含风险矩阵图）

每个工作表的图片都会被提取并分析，保留工作表名称和位置信息
```

## 智能批处理策略

### 1. 动态批次大小

```yaml
omni-agent:
  vision-llm:
    batch-processing:
      enabled: true
      max-context-tokens: 8000       # 最大上下文 token
      estimated-tokens-per-slide: 1500  # 每张图片预估 token
      min-batch-size: 1              # 最小批次
      max-batch-size: 5              # 最大批次
```

### 2. 批次计算逻辑

```java
int maxSlidesPerBatch = (maxContextTokens - reservedTokens) / estimatedTokensPerSlide;
// 例如：(8000 - 2000) / 1500 = 4 张图片/批次
```

### 3. 并行处理

```java
// 多个批次并行处理
List<CompletableFuture<BatchResult>> futures = new ArrayList<>();
for (List<DocumentPage> batch : batches) {
    CompletableFuture<BatchResult> future = CompletableFuture.supplyAsync(() -> {
        return processPageBatch(batch);
    }, visionLlmExecutor);
    futures.add(future);
}

// 等待所有批次完成
CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).get();
```

## 性能优化建议

### 1. Excel 文档优化

- **大型工作簿**: 建议拆分成多个小文件
- **图片数量**: 单个工作表图片 < 20 张最佳
- **图片大小**: 压缩图片以减少处理时间

### 2. 批处理配置

```yaml
# 针对图片多的 Excel 文档
omni-agent:
  vision-llm:
    batch-processing:
      max-batch-size: 3  # 降低批次大小
      min-batch-size: 1
```

### 3. 并发控制

```yaml
omni-agent:
  thread-pool:
    vision-llm:
      core-pool-size: 2  # 核心线程数
      max-pool-size: 4   # 最大线程数
```

## 常见场景

### 场景 1: 财务报表
```
包含: 营收图表、费用分析、利润趋势图
Vision LLM 会:
  - 识别图表类型（柱状图、饼图、折线图）
  - 提取数据值
  - 分析趋势变化
  - 总结关键指标
```

### 场景 2: 项目管理
```
包含: 甘特图、里程碑图、资源分配表
Vision LLM 会:
  - 识别项目阶段
  - 提取时间节点
  - 理解资源分配
  - 识别关键路径
```

### 场景 3: 数据分析
```
包含: 散点图、热力图、相关性矩阵
Vision LLM 会:
  - 识别数据模式
  - 发现异常值
  - 理解相关性
  - 提取统计信息
```

## 错误处理

### 1. 工作表无图片

```java
if (drawing == null) {
    log.debug("工作表 {} 没有图片", sheet.getSheetName());
    continue;  // 跳过该工作表
}
```

### 2. 图片提取失败

```java
try {
    // 提取图片
    byte[] imageData = pictureData.getData();
} catch (Exception e) {
    log.warn("提取 Excel 工作表 {} 中的图片失败", sheet.getSheetName(), e);
    continue;  // 继续处理下一张图片
}
```

### 3. 空 Excel 文件

```java
if (pages.isEmpty()) {
    log.warn("Excel 文档没有图片");
    return List.of();  // 返回空列表
}
```

## API 调用示例

```bash
# 处理 Excel 文档
curl -X POST "http://localhost:3000/api/documents/processing/财务报表.xlsx/extract" \
  -H "Content-Type: application/json" \
  -d '{
    "model": "vision-llm",
    "streaming": true
  }'

# 返回结果
{
  "type": "content",
  "content": "工作表[第一季度] 第5行, 第3列: \n营收柱状图显示...\n\n工作表[第一季度] 第12行, 第7列:\n费用饼图显示...\n\n..."
}
```

## 总结

### ✅ 已实现的功能

1. **多工作表支持** - 遍历所有 Sheet
2. **图片提取** - 提取所有嵌入图片
3. **位置记录** - 记录行列位置
4. **元数据保存** - 工作表名称、索引等
5. **Vision 分析** - AI 理解图片内容
6. **智能批处理** - 动态批次优化
7. **并行处理** - 多批次并行执行
8. **格式兼容** - 支持 XLS 和 XLSX

### 🎯 适用场景

- ✅ 财务报表分析
- ✅ 项目进度跟踪
- ✅ 数据可视化报告
- ✅ 业务分析文档
- ✅ 统计报告
- ✅ 科研数据图表

### 🚀 性能特点

- **快速处理**: 智能批处理 + 并行执行
- **准确识别**: Vision LLM 理解复杂图表
- **完整信息**: 保留工作表和位置上下文
- **鲁棒性强**: 错误处理完善，单张图片失败不影响整体

