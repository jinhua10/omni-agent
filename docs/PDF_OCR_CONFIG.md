# PDF OCR 配置示例

## 📋 使用场景

PDF OCR 功能适用于：
- ✅ 发票扫描件
- ✅ 合同扫描件
- ✅ 图片类 PDF
- ✅ 无法直接提取文本的 PDF

---

## 🔧 配置步骤

### 1. 添加依赖

在 `pom.xml` 中添加 OCR starter：

```xml
<dependency>
    <groupId>top.yumbo.ai</groupId>
    <artifactId>omni-agent-ocr-starter-tesseract</artifactId>
    <version>1.0.0-SNAPSHOT</version>
</dependency>
```

### 2. 安装 Tesseract

参考 `omni-agent-ocr-starter-tesseract/README.md` 中的安装说明。

### 3. 配置 application.yml

```yaml
omni-agent:
  # PDF 处理器配置
  pdf:
    enabled: true
    enable-ocr: true  # 启用 OCR（当普通文本提取失败时使用）
  
  # OCR 配置
  ocr:
    tesseract:
      enabled: true
      language: chi_sim+eng  # 简体中文 + 英文
      dpi: 300
      data-path: /usr/share/tesseract-ocr/4.00/tessdata  # 可选
```

---

## 💡 工作流程

```
PDF 文件
    ↓
PDFDocumentProcessor 处理
    ↓
逐页处理：
    ├─ 1. 尝试提取文本（PDFTextStripper）
    ├─ 2. 如果文本为空 且 enable-ocr=true
    │     └─ 渲染页面为图片（300 DPI）
    │     └─ 调用 Tesseract OCR 识别
    └─ 3. 返回提取的文本
```

---

## 🎯 配置选项说明

### PDF 配置

| 配置项 | 说明 | 默认值 |
|-------|------|--------|
| `omni-agent.pdf.enabled` | 是否启用 PDF 处理器 | `true` |
| `omni-agent.pdf.enable-ocr` | 是否启用 OCR | `false` |

### OCR 配置

| 配置项 | 说明 | 默认值 |
|-------|------|--------|
| `omni-agent.ocr.tesseract.enabled` | 是否启用 Tesseract OCR | `false` |
| `omni-agent.ocr.tesseract.language` | 识别语言 | `chi_sim+eng` |
| `omni-agent.ocr.tesseract.dpi` | DPI 设置 | `300` |
| `omni-agent.ocr.tesseract.data-path` | 数据文件路径 | （系统默认） |
| `omni-agent.ocr.tesseract.page-segmentation-mode` | 页面分割模式 | `3` |

---

## 📊 性能影响

### 普通 PDF（可直接提取文本）

```
处理时间：~2-5 秒/页
OCR：不使用
```

### 扫描件 PDF（需要 OCR）

```
处理时间：~5-10 秒/页
OCR：每页都使用
```

---

## 🚀 优化建议

### 1. 仅在需要时启用 OCR

```yaml
omni-agent:
  pdf:
    enable-ocr: true  # 仅处理扫描件时启用
```

### 2. 调整 DPI

```yaml
omni-agent:
  ocr:
    tesseract:
      dpi: 200  # 降低 DPI 提高速度（牺牲准确度）
      # dpi: 400  # 提高 DPI 提高准确度（牺牲速度）
```

### 3. 选择合适的语言包

```yaml
omni-agent:
  ocr:
    tesseract:
      language: eng  # 仅英文（更快）
      # language: chi_sim  # 仅简体中文
      # language: chi_sim+eng  # 中英混合（推荐）
```

---

## 📝 示例场景

### 场景 1：处理发票扫描件

```yaml
omni-agent:
  pdf:
    enable-ocr: true
  ocr:
    tesseract:
      enabled: true
      language: chi_sim+eng
      dpi: 300
      page-segmentation-mode: 6  # 单个文本块（发票通常是规整的）
```

### 场景 2：处理合同扫描件

```yaml
omni-agent:
  pdf:
    enable-ocr: true
  ocr:
    tesseract:
      enabled: true
      language: chi_sim
      dpi: 400  # 提高准确度
      page-segmentation-mode: 3  # 自动分割
```

### 场景 3：只处理正常 PDF

```yaml
omni-agent:
  pdf:
    enable-ocr: false  # 不启用 OCR，提高速度
```

---

## ✅ 总结

- ✅ **可选功能**：默认不启用，需要时才配置
- ✅ **自动降级**：普通 PDF 不使用 OCR，扫描件才使用
- ✅ **灵活配置**：可根据场景调整参数
- ✅ **独立模块**：通过 starter 方式引入，依赖清晰

