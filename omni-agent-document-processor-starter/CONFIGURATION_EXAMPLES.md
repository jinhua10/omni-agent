# OmniAgent 文档处理器配置示例

## 基础配置

```yaml
omni-agent:
  # 文档处理器总开关（默认启用）
  document-processor:
    enabled: true
```

## 各处理器配置

```yaml
omni-agent:
  # Excel 处理器（默认启用）
  excel:
    enabled: true
    
  # Word 处理器（默认启用）
  word:
    enabled: true
    
  # PDF 处理器（默认启用）
  pdf:
    enabled: true
    enable-ocr: false  # 是否启用 OCR（需要 tesseract-ocr-starter）
    
  # PowerPoint 处理器（默认启用）
  ppt:
    enabled: true
    render-scale: 2.0  # 渲染缩放倍数（默认 2.0）
    
  # 文本处理器（默认启用）
  text:
    enabled: true
    
  # 媒体文件处理器（默认不启用）
  media:
    enabled: false
    
  # Vision LLM 处理器（默认不启用）
  vision-llm:
    enabled: false
    model: "qwen-vl-plus"
    system-prompt: "请分析这张图片并提取其中的关键信息。"
```

## Vision LLM 完整配置

```yaml
omni-agent:
  vision-llm:
    enabled: true
    model: "qwen-vl-plus"  # 支持的模型：qwen-vl-plus, gpt-4o 等
    system-prompt: "请分析这张图片并提取其中的关键信息。"
    
    # 批处理配置（重要！性能优化）
    batch-processing:
      enabled: true              # 启用批处理
      max-batch-size: 5          # 每批最多处理 5 张图片/页面
      max-context-tokens: 8000   # 最大上下文 token 数
      
  # 线程池配置（用于并行处理）
  executor:
    vision-llm:
      core-pool-size: 3          # 核心线程数
      max-pool-size: 6           # 最大线程数
      queue-capacity: 100        # 队列容量
      thread-name-prefix: "vision-llm-"
      keep-alive-seconds: 60
```

## 完整配置示例

```yaml
spring:
  application:
    name: omni-agent-document-service

omni-agent:
  # ========== 文档处理器配置 ==========
  document-processor:
    enabled: true
    
  # Excel 处理器
  excel:
    enabled: true
    
  # Word 处理器
  word:
    enabled: true
    
  # PDF 处理器
  pdf:
    enabled: true
    enable-ocr: false
    
  # PowerPoint 处理器
  ppt:
    enabled: true
    render-scale: 2.0
    
  # 文本处理器
  text:
    enabled: true
    
  # 媒体文件处理器（按需启用）
  media:
    enabled: false
    
  # Vision LLM 处理器（推荐启用，性能提升显著）
  vision-llm:
    enabled: true
    model: "qwen-vl-plus"
    system-prompt: "请分析这张图片并提取其中的关键信息。"
    
    # 批处理配置
    batch-processing:
      enabled: true
      max-batch-size: 5
      max-context-tokens: 8000
      
  # ========== AI 服务配置 ==========
  ai:
    # 通用 AI 服务
    provider: "openai"  # 或 "qwen", "deepseek" 等
    api-key: "${AI_API_KEY}"
    base-url: "https://api.openai.com/v1"
    model: "gpt-4"
    
    # Vision AI 服务（用于图片分析）
    vision:
      provider: "qwen"
      api-key: "${QWEN_API_KEY}"
      model: "qwen-vl-plus"
      
  # ========== 线程池配置 ==========
  executor:
    # Vision LLM 线程池
    vision-llm:
      core-pool-size: 3
      max-pool-size: 6
      queue-capacity: 100
      thread-name-prefix: "vision-llm-"
      keep-alive-seconds: 60
      
  # ========== OCR 配置（可选）==========
  ocr:
    tesseract:
      enabled: false
      data-path: "/usr/share/tesseract-ocr/4.00/tessdata"
      language: "chi_sim+eng"  # 中文简体 + 英文

# ========== 日志配置 ==========
logging:
  level:
    top.yumbo.ai.omni.document.processor: INFO
    top.yumbo.ai.omni.ai: INFO
```

## 按场景配置

### 场景 1：仅处理 Office 文档（不使用 Vision LLM）

```yaml
omni-agent:
  excel:
    enabled: true
  word:
    enabled: true
  ppt:
    enabled: false  # 不处理 PPT（需要 Vision LLM）
  pdf:
    enabled: true
  text:
    enabled: true
  vision-llm:
    enabled: false
```

### 场景 2：高性能 PPT 处理（推荐）

```yaml
omni-agent:
  ppt:
    enabled: true
  vision-llm:
    enabled: true
    model: "qwen-vl-plus"
    batch-processing:
      enabled: true
      max-batch-size: 10  # 增大批次
  executor:
    vision-llm:
      core-pool-size: 5   # 增加线程数
      max-pool-size: 10
```

### 场景 3：PDF OCR 识别

```yaml
omni-agent:
  pdf:
    enabled: true
    enable-ocr: true
  ocr:
    tesseract:
      enabled: true
      language: "chi_sim+eng"
```

### 场景 4：纯 Vision LLM 模式（所有文档转图片）

```yaml
omni-agent:
  excel:
    enabled: false
  word:
    enabled: false
  pdf:
    enabled: false
  ppt:
    enabled: false
  text:
    enabled: true  # 文本还是保留
  vision-llm:
    enabled: true
    model: "qwen-vl-plus"
    batch-processing:
      enabled: true
```

## 配置优先级

1. 各处理器的 `enabled` 配置优先级最高
2. `document-processor.enabled` 是总开关
3. 如果总开关关闭，所有处理器都不生效

## 性能调优建议

### 1. 批处理大小

```yaml
omni-agent:
  vision-llm:
    batch-processing:
      max-batch-size: 5  # 根据内存和网络情况调整
```

**建议**：
- 内存充足：5-10
- 内存有限：3-5
- 网络较慢：2-3

### 2. 线程池大小

```yaml
omni-agent:
  executor:
    vision-llm:
      core-pool-size: 3   # 根据 CPU 核心数调整
      max-pool-size: 6
```

**建议**：
- 4 核 CPU：core=2, max=4
- 8 核 CPU：core=4, max=8
- 16 核 CPU：core=6, max=12

### 3. 图片分辨率

```yaml
omni-agent:
  ppt:
    render-scale: 2.0  # 根据清晰度需求调整
```

**建议**：
- 高清晰度需求：3.0-4.0
- 平衡模式：2.0（默认）
- 快速处理：1.0-1.5

## 环境变量配置

可以通过环境变量覆盖配置：

```bash
export OMNI_AGENT_VISION_LLM_ENABLED=true
export OMNI_AGENT_VISION_LLM_MODEL=qwen-vl-plus
export OMNI_AGENT_VISION_LLM_BATCH_PROCESSING_MAX_BATCH_SIZE=10
export AI_API_KEY=your_api_key_here
```

---

**提示**：
- 建议在生产环境启用 Vision LLM 和批处理，性能提升显著
- 根据实际场景调整配置参数
- 关注日志输出，及时发现问题

