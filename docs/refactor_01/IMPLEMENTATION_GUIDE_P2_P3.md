# 📘 Phase 2 & 3 详细实施指南

**版本：** 1.0  
**创建日期：** 2025-12-28  
**目标：** 完整实现所有 P1 和 P2 功能

---

## ✅ 已完成工作

### 1. 图片提取 API（部分完成）
- ✅ `ExtractedImage` 数据模型
- ✅ `ImagePosition` 位置信息
- ✅ `ProcessedDocument` 添加图片列表

---

## 📋 剩余任务详细指南

### Phase 2 - 任务 1：完成图片提取（剩余4-5小时）

#### 1.1 PDF 图片提取

**文件位置：** `PDFProcessor.java`

**实现步骤：**
```java
// 1. 在 process() 方法中添加图片提取
List<ExtractedImage> allImages = new ArrayList<>();

for (int pageIndex = 0; pageIndex < pageCount; pageIndex++) {
    int pageNumber = pageIndex + 1;
    // ...existing text extraction...
    
    // 提取该页图片
    List<ExtractedImage> pageImages = extractImagesFromPage(
        document.getPage(pageIndex), pageNumber
    );
    allImages.addAll(pageImages);
}

// 2. 添加到返回结果
return ProcessedDocument.builder()
    // ...existing fields...
    .images(allImages)
    .build();

// 3. 实现图片提取方法
private List<ExtractedImage> extractImagesFromPage(PDPage page, int pageNumber) {
    List<ExtractedImage> images = new ArrayList<>();
    
    try {
        if (page.getResources() != null) {
            for (COSName name : page.getResources().getXObjectNames()) {
                PDXObject xObject = page.getResources().getXObject(name);
                
                if (xObject instanceof PDImageXObject) {
                    PDImageXObject image = (PDImageXObject) xObject;
                    ExtractedImage extracted = convertPDFImage(image, pageNumber);
                    if (extracted != null) {
                        images.add(extracted);
                    }
                }
            }
        }
    } catch (Exception e) {
        log.warn("提取 PDF 第 {} 页图片失败", pageNumber, e);
    }
    
    return images;
}

// 4. 图片转换方法
private ExtractedImage convertPDFImage(PDImageXObject image, int pageNumber) {
    try {
        BufferedImage bufferedImage = image.getImage();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        ImageIO.write(bufferedImage, "png", baos);
        
        return ExtractedImage.builder()
                .imageId(UUID.randomUUID().toString())
                .data(baos.toByteArray())
                .format("png")
                .pageNumber(pageNumber)
                .width(bufferedImage.getWidth())
                .height(bufferedImage.getHeight())
                .createdAt(System.currentTimeMillis())
                .build();
    } catch (Exception e) {
        log.warn("转换 PDF 图片失败", e);
        return null;
    }
}
```

**测试要点：**
- 测试包含图片的 PDF
- 测试多页 PDF
- 测试不同图片格式（JPG、PNG）

---

#### 1.2 Word 图片提取

**文件位置：** `WordProcessor.java`

**实现步骤：**
```java
// 1. 在 processDocx() 方法中
List<ExtractedImage> allImages = new ArrayList<>();
int imageIndex = 0;

for (IBodyElement element : document.getBodyElements()) {
    if (element instanceof XWPFParagraph) {
        XWPFParagraph paragraph = (XWPFParagraph) element;
        
        // 提取段落中的图片
        for (XWPFRun run : paragraph.getRuns()) {
            List<XWPFPicture> pictures = run.getEmbeddedPictures();
            for (XWPFPicture picture : pictures) {
                ExtractedImage image = convertWordImage(picture, imageIndex++);
                if (image != null) {
                    allImages.add(image);
                }
            }
        }
    }
}

// 2. 图片转换方法
private ExtractedImage convertWordImage(XWPFPicture picture, int imageIndex) {
    try {
        XWPFPictureData pictureData = picture.getPictureData();
        
        return ExtractedImage.builder()
                .imageId(UUID.randomUUID().toString())
                .data(pictureData.getData())
                .format(extractFormat(pictureData.getFileName()))
                .pageNumber(imageIndex + 1)
                .width(null) // Word 不提供尺寸
                .height(null)
                .position(ExtractedImage.ImagePosition.builder()
                        .paragraphIndex(imageIndex)
                        .description("段落内嵌图片")
                        .build())
                .createdAt(System.currentTimeMillis())
                .build();
    } catch (Exception e) {
        log.warn("转换 Word 图片失败", e);
        return null;
    }
}

private String extractFormat(String fileName) {
    if (fileName == null || !fileName.contains(".")) {
        return "png";
    }
    return fileName.substring(fileName.lastIndexOf(".") + 1).toLowerCase();
}
```

---

#### 1.3 Excel 图片提取

**文件位置：** `ExcelProcessor.java`

**实现步骤：**
```java
// 1. 在 process() 方法中
List<ExtractedImage> allImages = new ArrayList<>();

for (int i = 0; i < workbook.getNumberOfSheets(); i++) {
    Sheet sheet = workbook.getSheetAt(i);
    
    // 提取工作表图片
    List<ExtractedImage> sheetImages = extractImagesFromSheet(sheet, i + 1);
    allImages.addAll(sheetImages);
}

// 2. 提取方法（XLSX）
private List<ExtractedImage> extractImagesFromSheet(Sheet sheet, int sheetIndex) {
    List<ExtractedImage> images = new ArrayList<>();
    
    try {
        if (sheet instanceof XSSFSheet) {
            XSSFDrawing drawing = ((XSSFSheet) sheet).getDrawingPatriarch();
            if (drawing != null) {
                for (XSSFShape shape : drawing.getShapes()) {
                    if (shape instanceof XSSFPicture) {
                        XSSFPicture picture = (XSSFPicture) shape;
                        ExtractedImage image = convertExcelImage(picture, sheet, sheetIndex);
                        if (image != null) {
                            images.add(image);
                        }
                    }
                }
            }
        }
        // TODO: 添加 HSSF (XLS) 支持
    } catch (Exception e) {
        log.warn("提取 Excel 图片失败", e);
    }
    
    return images;
}

// 3. 转换方法
private ExtractedImage convertExcelImage(XSSFPicture picture, Sheet sheet, int sheetIndex) {
    try {
        XSSFPictureData pictureData = picture.getPictureData();
        XSSFClientAnchor anchor = picture.getClientAnchor();
        
        return ExtractedImage.builder()
                .imageId(UUID.randomUUID().toString())
                .data(pictureData.getData())
                .format(pictureData.suggestFileExtension())
                .pageNumber(sheetIndex)
                .position(ExtractedImage.ImagePosition.builder()
                        .row(anchor.getRow1())
                        .column(anchor.getCol1())
                        .description(String.format("第%d行, 第%d列", 
                                anchor.getRow1() + 1, anchor.getCol1() + 1))
                        .build())
                .createdAt(System.currentTimeMillis())
                .build();
    } catch (Exception e) {
        log.warn("转换 Excel 图片失败", e);
        return null;
    }
}
```

---

### Phase 2 - 任务 2：Vision LLM 集成（3-4小时）

#### 2.1 创建 Vision 服务接口

**新文件：** `VisionService.java`

```java
package top.yumbo.ai.omni.vision;

import top.yumbo.ai.omni.document.processor.ExtractedImage;

/**
 * Vision AI 服务接口
 */
public interface VisionService {
    
    /**
     * 分析图片内容
     */
    String analyzeImage(ExtractedImage image);
    
    /**
     * 生成图片描述
     */
    String describeImage(byte[] imageData, String format);
    
    /**
     * 检测图片类型（图表、表格、照片等）
     */
    String detectImageType(ExtractedImage image);
}
```

#### 2.2 实现 Vision 服务

**新文件：** `OpenAIVisionService.java`

```java
@Service
@ConditionalOnProperty(prefix = "omni-agent.vision", name = "provider", havingValue = "openai")
public class OpenAIVisionService implements VisionService {
    
    @Value("${omni-agent.vision.openai.api-key}")
    private String apiKey;
    
    @Override
    public String analyzeImage(ExtractedImage image) {
        // 调用 OpenAI GPT-4 Vision API
        String base64Image = Base64.getEncoder().encodeToString(image.getData());
        
        // 构建请求
        Map<String, Object> request = Map.of(
            "model", "gpt-4-vision-preview",
            "messages", List.of(Map.of(
                "role", "user",
                "content", List.of(
                    Map.of("type", "text", "text", "请详细描述这张图片的内容"),
                    Map.of("type", "image_url", "image_url", Map.of(
                        "url", "data:image/" + image.getFormat() + ";base64," + base64Image
                    ))
                )
            ))
        );
        
        // 发送请求并返回结果
        // TODO: 实现 HTTP 请求逻辑
        return "图片描述...";
    }
}
```

---

### Phase 2 - 任务 3：单元测试（4-5小时）

#### 3.1 分块策略测试

**新文件：** `FixedLengthStrategyTest.java`

```java
@SpringBootTest
class FixedLengthStrategyTest {
    
    @Autowired
    private ChunkingService chunkingService;
    
    @Test
    void testFixedLengthChunking() {
        String content = "这是测试内容。".repeat(100);
        
        ChunkingConfig config = ChunkingConfig.builder()
                .strategy(ChunkingStrategy.FIXED_LENGTH)
                .fixedLengthSize(512)
                .overlap(50)
                .build();
        
        List<Chunk> chunks = chunkingService.chunk("test-doc", content, config);
        
        assertThat(chunks).isNotEmpty();
        assertThat(chunks.get(0).getLength()).isLessThanOrEqualTo(512);
    }
}
```

#### 3.2 文档处理器测试

**新文件：** `PDFProcessorTest.java`

```java
@SpringBootTest
class PDFProcessorTest {
    
    @Autowired
    private DocumentProcessor pdfProcessor;
    
    @Test
    void testPDFProcessing() throws Exception {
        InputStream input = getClass().getResourceAsStream("/test.pdf");
        
        ProcessedDocument doc = pdfProcessor.process("test.pdf", input);
        
        assertThat(doc.isSuccess()).isTrue();
        assertThat(doc.getText()).isNotBlank();
        assertThat(doc.getPageCount()).isGreaterThan(0);
    }
}
```

---

### Phase 3 - 任务 1：PDF OCR 支持（3-4小时）

#### 1.1 创建 OCR 服务接口

**新文件：** `OCRService.java`

```java
public interface OCRService {
    /**
     * 识别图片中的文字
     */
    String recognizeText(BufferedImage image);
    
    /**
     * 识别图片中的文字（支持多语言）
     */
    String recognizeText(BufferedImage image, String language);
}
```

#### 1.2 Tesseract 实现

```java
@Service
@ConditionalOnProperty(prefix = "omni-agent.ocr", name = "enabled", havingValue = "true")
public class TesseractOCRService implements OCRService {
    
    private final Tesseract tesseract;
    
    public TesseractOCRService() {
        tesseract = new Tesseract();
        tesseract.setDatapath("/usr/share/tessdata");
        tesseract.setLanguage("chi_sim+eng"); // 中英文
    }
    
    @Override
    public String recognizeText(BufferedImage image) {
        try {
            return tesseract.doOCR(image);
        } catch (TesseractException e) {
            log.error("OCR 识别失败", e);
            return "";
        }
    }
}
```

---

### Phase 3 - 任务 2：性能优化（3-4小时）

#### 2.1 大文件流式处理

```java
// PDF 流式处理
public ProcessedDocument processLargePDF(InputStream input) {
    try (PDDocument document = PDDocument.load(input, 
            MemoryUsageSetting.setupMixed(100 * 1024 * 1024))) {
        
        // 逐页处理，避免全部加载到内存
        for (int i = 0; i < document.getNumberOfPages(); i++) {
            // 处理一页后立即释放
            processPageAndRelease(document, i);
        }
    }
}
```

#### 2.2 结果缓存

```java
@Cacheable(value = "document-cache", key = "#documentId")
public ProcessedDocument process(String documentId, InputStream input) {
    // 处理逻辑
}
```

#### 2.3 并行处理

```java
@Service
public class ParallelDocumentProcessor {
    
    private final ExecutorService executorService = 
            Executors.newFixedThreadPool(10);
    
    public List<ProcessedDocument> processBatch(List<InputStream> inputs) {
        List<CompletableFuture<ProcessedDocument>> futures = inputs.stream()
                .map(input -> CompletableFuture.supplyAsync(
                        () -> processor.process("doc", input), 
                        executorService))
                .toList();
        
        return futures.stream()
                .map(CompletableFuture::join)
                .toList();
    }
}
```

---

### Phase 3 - 任务 3：批量处理（2-3小时）

#### 3.1 批量处理 API

```java
@RestController
@RequestMapping("/api/documents/batch")
public class BatchProcessingController {
    
    @PostMapping("/process")
    public BatchProcessingResult processBatch(@RequestBody List<String> documentIds) {
        BatchProcessingJob job = batchService.createJob(documentIds);
        
        // 异步处理
        CompletableFuture.runAsync(() -> {
            for (String docId : documentIds) {
                try {
                    processor.process(docId, getInputStream(docId));
                    job.incrementProgress();
                } catch (Exception e) {
                    job.addError(docId, e.getMessage());
                }
            }
            job.complete();
        });
        
        return BatchProcessingResult.builder()
                .jobId(job.getId())
                .totalCount(documentIds.size())
                .build();
    }
    
    @GetMapping("/status/{jobId}")
    public BatchJobStatus getStatus(@PathVariable String jobId) {
        return batchService.getJobStatus(jobId);
    }
}
```

---

## 🎯 实施优先级建议

### 立即实施（关键路径）
1. ✅ 完成 PDF 图片提取
2. ✅ 完成 Word 图片提取  
3. ✅ 完成 Excel 图片提取
4. ✅ Vision LLM 基础集成
5. ✅ 核心功能单元测试

### 后续实施（增强功能）
6. ⏳ PPT 图片提取
7. ⏳ 图片内容分析
8. ⏳ PDF OCR 支持
9. ⏳ 性能优化
10. ⏳ 批量处理

---

## 📊 预计工作量

| 任务 | 优先级 | 预计时间 | 难度 |
|------|--------|---------|------|
| PDF 图片提取 | P0 | 2h | ⭐⭐ |
| Word 图片提取 | P0 | 2h | ⭐⭐ |
| Excel 图片提取 | P0 | 2h | ⭐⭐⭐ |
| Vision LLM 集成 | P1 | 4h | ⭐⭐⭐⭐ |
| 单元测试 | P1 | 5h | ⭐⭐⭐ |
| OCR 支持 | P2 | 4h | ⭐⭐⭐⭐ |
| 性能优化 | P2 | 4h | ⭐⭐⭐⭐⭐ |
| 批量处理 | P2 | 3h | ⭐⭐⭐ |

---

## ✅ 下一步行动

1. **立即开始**：实现 PDF 图片提取（参考上述代码）
2. **并行进行**：Word 和 Excel 图片提取
3. **集成测试**：使用真实文档验证
4. **逐步完善**：Vision LLM、OCR、性能优化

---

**文档版本：** 1.0  
**最后更新：** 2025-12-28  
**状态：** 📖 实施指南就绪，可开始执行

