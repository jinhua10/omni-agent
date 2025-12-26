# OmniAgent OCR Starter - Tesseract

基于 Tesseract OCR 引擎的 Spring Boot Starter。

## 📋 功能特性

- ✅ 自动配置 Tesseract OCR 服务
- ✅ 支持多语言识别（中文、英文等）
- ✅ 灵活的配置选项
- ✅ Spring Boot 自动装配

---

## 📦 Maven 依赖

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-ocr-starter-tesseract</artifactId>
    <version>1.0.0-SNAPSHOT</version>
</dependency>
```

---

## ⚙️ 前置条件

### 1. 安装 Tesseract OCR

#### Windows

下载并安装 [Tesseract OCR](https://github.com/UB-Mannheim/tesseract/wiki)

```powershell
# 使用 Chocolatey 安装
choco install tesseract

# 或下载安装包
# https://github.com/UB-Mannheim/tesseract/wiki
```

#### Linux (Ubuntu/Debian)

```bash
sudo apt-get update
sudo apt-get install tesseract-ocr
sudo apt-get install tesseract-ocr-chi-sim  # 简体中文语言包
sudo apt-get install tesseract-ocr-chi-tra  # 繁体中文语言包
```

#### macOS

```bash
brew install tesseract
brew install tesseract-lang  # 所有语言包
```

### 2. 下载语言数据文件

语言数据文件（.traineddata）通常在以下位置：
- **Windows**: `C:\Program Files\Tesseract-OCR\tessdata\`
- **Linux**: `/usr/share/tesseract-ocr/4.00/tessdata/`
- **macOS**: `/usr/local/share/tessdata/`

或从官方下载：
- [Tesseract 语言数据](https://github.com/tesseract-ocr/tessdata)
- [快速版本（推荐）](https://github.com/tesseract-ocr/tessdata_fast)
- [最佳版本](https://github.com/tesseract-ocr/tessdata_best)

---

## 🔧 配置

### 基础配置

```yaml
omni-agent:
  ocr:
    tesseract:
      enabled: true                    # 启用 OCR
      language: chi_sim+eng            # 简体中文 + 英文
      dpi: 300                         # DPI 设置
```

### 完整配置

```yaml
omni-agent:
  ocr:
    tesseract:
      # 是否启用
      enabled: true
      
      # 识别语言
      # eng: 英文
      # chi_sim: 简体中文
      # chi_tra: 繁体中文
      # chi_sim+eng: 简体中文 + 英文
      language: chi_sim+eng
      
      # Tesseract 数据文件路径（可选，不设置则使用系统默认）
      data-path: /usr/share/tesseract-ocr/4.00/tessdata
      
      # DPI 设置（默认：300）
      dpi: 300
      
      # 页面分割模式（默认：3）
      # 3 = 全自动页面分割
      # 6 = 单个文本块
      # 7 = 单行文本
      page-segmentation-mode: 3
      
      # OCR 引擎模式（默认：3）
      # 1 = LSTM 引擎
      # 3 = 默认（基于可用引擎）
      ocr-engine-mode: 3
      
      # 最小置信度（0-100，默认：0）
      min-confidence: 0
      
      # 超时时间（秒，默认：30）
      timeout: 30
```

---

## 💻 使用示例

### 1. 注入服务

```java
@Service
public class MyService {
    
    @Autowired(required = false)
    private TesseractOCRService ocrService;
    
    public void processImage(BufferedImage image) {
        if (ocrService != null && ocrService.isAvailable()) {
            String text = ocrService.recognizeText(image);
            System.out.println("识别结果: " + text);
        }
    }
}
```

### 2. PDF 中使用 OCR

```java
@Component
public class PDFProcessor {
    
    @Autowired(required = false)
    private TesseractOCRService ocrService;
    
    public String extractTextFromPDF(File pdfFile) {
        // 加载 PDF
        PDDocument document = PDDocument.load(pdfFile);
        PDFRenderer renderer = new PDFRenderer(document);
        
        StringBuilder allText = new StringBuilder();
        
        for (int pageIndex = 0; pageIndex < document.getNumberOfPages(); pageIndex++) {
            // 渲染为图片
            BufferedImage image = renderer.renderImageWithDPI(pageIndex, 300);
            
            // OCR 识别
            if (ocrService != null) {
                String pageText = ocrService.recognizeText(image);
                allText.append(pageText).append("\n\n");
            }
        }
        
        return allText.toString();
    }
}
```

### 3. 检查是否包含文字

```java
if (ocrService.hasText(image)) {
    System.out.println("图片包含文字");
}
```

---

## 📊 性能建议

### 1. DPI 设置

- **低质量图片**：150-200 DPI
- **一般质量**：300 DPI（推荐）
- **高质量/小字**：400-600 DPI

### 2. 页面分割模式

- **单列文档**：4
- **单个文本块**：6
- **单行文本**：7
- **混合布局**：3（默认）

### 3. 处理时间

| DPI | 图片大小 | 处理时间（估算） |
|-----|---------|----------------|
| 150 | 1000x1500 | ~1-2s |
| 300 | 2000x3000 | ~2-4s |
| 600 | 4000x6000 | ~5-10s |

---

## 🐛 常见问题

### 1. Tesseract not found

**问题**：启动时报错 "Tesseract not found"

**解决**：
1. 确认已安装 Tesseract
2. 检查环境变量 PATH 是否包含 Tesseract 安装路径
3. 或在配置中指定 `data-path`

### 2. 语言包未找到

**问题**：识别时报错 "Language 'chi_sim' not found"

**解决**：
1. 下载对应语言包
2. 放到 tessdata 目录
3. 或在配置中指定正确的 `data-path`

### 3. 识别率低

**解决**：
1. 提高 DPI 设置
2. 调整页面分割模式
3. 使用预处理（二值化、去噪等）

---

## 📝 扩展阅读

- [Tesseract 官方文档](https://github.com/tesseract-ocr/tesseract)
- [Tess4J GitHub](https://github.com/nguyenq/tess4j)
- [页面分割模式说明](https://tesseract-ocr.github.io/tessdoc/ImproveQuality.html)

---

## ✅ 总结

- ✅ 开箱即用的 OCR 功能
- ✅ 支持多语言识别
- ✅ 灵活的配置选项
- ✅ Spring Boot 自动装配
- ✅ 适用于发票、扫描件等场景

