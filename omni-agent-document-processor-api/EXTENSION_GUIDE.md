# 文档处理器扩展机制使用指南

## 📚 概述

文档处理器扩展机制允许用户通过实现特定接口来定制化文档处理流程，类似于 Spring 的扩展机制。

## 🎯 扩展接口

### 1. PreProcessor（前置处理器）

在文档处理前执行，可用于：
- 文档验证和预处理
- 文件格式转换
- 参数补充和修改
- 权限检查
- 日志记录

**示例代码：**

```java
@Component
@Order(1)  // 执行顺序（数字越小优先级越高）
public class MyPreProcessor implements PreProcessor {
    
    @Override
    public String getName() {
        return "MyPreProcessor";
    }
    
    @Override
    public int getOrder() {
        return 1;
    }
    
    @Override
    public ProcessingContext preProcess(ProcessingContext context) throws Exception {
        // 在这里添加你的前置处理逻辑
        
        // 例如：验证文件大小
        if (context.getFileSize() > MAX_SIZE) {
            throw new IllegalArgumentException("文件过大");
        }
        
        // 返回修改后的上下文
        return context;
    }
    
    @Override
    public boolean supports(String processorName) {
        // 指定支持哪些处理器（返回 true 表示支持所有）
        return processorName.contains("PDF") || processorName.contains("Word");
    }
}
```

### 2. PostProcessor（后置处理器）

在文档处理后执行，可用于：
- 结果验证和清理
- 内容格式化和美化
- 敏感信息过滤
- 统计信息收集
- 结果持久化

**示例代码：**

```java
@Component
@Order(10)
public class MyPostProcessor implements PostProcessor {
    
    @Override
    public String getName() {
        return "MyPostProcessor";
    }
    
    @Override
    public ProcessingResult postProcess(ProcessingContext context, ProcessingResult result) throws Exception {
        // 在这里添加你的后置处理逻辑
        
        // 例如：过滤敏感信息
        String filteredContent = filterSensitiveInfo(result.getContent());
        result.setContent(filteredContent);
        
        return result;
    }
    
    private String filterSensitiveInfo(String content) {
        // 过滤逻辑
        return content.replaceAll("敏感词", "***");
    }
}
```

### 3. ContentEnhancer（内容增强器）

对提取的内容进行增强处理，可用于：
- 内容格式转换（Markdown、HTML）
- 文本摘要生成
- 关键词提取
- 语义分析
- 翻译
- 内容分类

**示例代码：**

```java
@Component
@Order(20)
public class MyContentEnhancer implements ContentEnhancer {
    
    @Override
    public String getName() {
        return "MyContentEnhancer";
    }
    
    @Override
    public EnhancedContent enhance(ProcessingContext context, String originalContent) throws Exception {
        // 在这里添加你的内容增强逻辑
        
        // 例如：提取关键词
        List<String> keywords = extractKeywords(originalContent);
        
        // 生成摘要
        String summary = generateSummary(originalContent);
        
        return EnhancedContent.builder()
                .content(originalContent)  // 可以修改原内容
                .keywords(keywords)
                .summary(summary)
                .build();
    }
}
```

### 4. ImageHandler（图片处理器）

对提取的图片进行自定义处理，可用于：
- 图片压缩和优化
- 格式转换
- 水印添加
- OCR 文字识别
- 图片分类
- 对象检测

**示例代码：**

```java
@Component
@Order(5)
public class MyImageHandler implements ImageHandler {
    
    @Override
    public String getName() {
        return "MyImageHandler";
    }
    
    @Override
    public ProcessedImage handle(ProcessingContext context, ExtractedImage image) throws Exception {
        // 在这里添加你的图片处理逻辑
        
        // 例如：压缩图片
        byte[] compressedData = compressImage(image.getData());
        
        // OCR 识别
        String ocrText = performOCR(image.getData());
        
        return ProcessedImage.builder()
                .data(compressedData)
                .format(image.getFormat())
                .ocrText(ocrText)
                .build();
    }
}
```

### 5. MetadataExtractor（元数据提取器）

提取文档的元数据信息，可用于：
- 文档属性提取（作者、标题、创建时间等）
- 版本信息
- 安全标签
- 自定义属性

**示例代码：**

```java
@Component
@Order(1)
public class MyMetadataExtractor implements MetadataExtractor {
    
    @Override
    public String getName() {
        return "MyMetadataExtractor";
    }
    
    @Override
    public ExtractedMetadata extract(ProcessingContext context) throws Exception {
        // 在这里添加你的元数据提取逻辑
        
        return ExtractedMetadata.builder()
                .author("John Doe")
                .title("Sample Document")
                .createdDate("2024-01-01")
                .build();
    }
}
```

## 🔧 使用方法

### 1. 创建扩展类

在你的项目中创建一个类，实现相应的扩展接口：

```java
package com.example.myapp.extensions;

import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.document.processor.extension.PreProcessor;

@Component  // ⭐ 必须添加此注解，让 Spring 自动扫描
@Order(10)  // 可选：指定执行顺序
public class MyCustomPreProcessor implements PreProcessor {
    // 实现接口方法...
}
```

### 2. 启用自动扫描

确保你的 Spring Boot 应用能够扫描到扩展类：

```java
@SpringBootApplication
@ComponentScan(basePackages = {
    "top.yumbo.ai.omni",      // OmniAgent 包
    "com.p2p.myapp"       // 你的应用包
})
public class MyApplication {
    public static void main(String[] args) {
        SpringApplication.run(MyApplication.class, args);
    }
}
```

### 3. 配置（可选）

如果你的扩展需要配置，可以在 `application.yml` 中添加：

```yaml
# 自定义配置
myapp:
  document:
    max-file-size: 10485760  # 10MB
    enable-ocr: true
```

然后在扩展类中注入配置：

```java
@Component
public class MyPreProcessor implements PreProcessor {
    
    @Value("${myapp.document.max-file-size:10485760}")
    private long maxFileSize;
    
    // ...
}
```

## 📋 执行顺序

扩展的执行顺序由 `getOrder()` 方法决定，数字越小优先级越高：

1. **PreProcessor**：order 1 → 2 → 3 → ...
2. **MetadataExtractor**：order 1 → 2 → 3 → ...
3. **ImageHandler**：order 1 → 2 → 3 → ...
4. **ContentEnhancer**：order 1 → 2 → 3 → ...
5. **PostProcessor**：order 1 → 2 → 3 → ...

## 🎨 高级用法

### 1. 条件启用

你可以通过 `@ConditionalOnProperty` 来条件启用扩展：

```java
@Component
@ConditionalOnProperty(
    prefix = "myapp.document",
    name = "enable-validation",
    havingValue = "true"
)
public class MyPreProcessor implements PreProcessor {
    // ...
}
```

### 2. 指定支持的处理器

通过 `supports()` 方法指定扩展支持哪些处理器：

```java
@Override
public boolean supports(String processorName) {
    // 仅支持 PDF 和 Word 处理器
    return processorName.contains("PDF") || processorName.contains("Word");
}
```

### 3. 动态启用/禁用

通过 `isEnabled()` 方法动态控制扩展是否启用：

```java
@Override
public boolean isEnabled() {
    // 根据某些条件决定是否启用
    return someCondition;
}
```

## 📦 示例项目

参考 `examples` 包中的示例实现：

- `FileSizeValidationPreProcessor`：文件大小验证
- `SensitiveInfoFilterPostProcessor`：敏感信息过滤
- `ImageCompressionHandler`：图片压缩
- `KeywordExtractionContentEnhancer`：关键词提取

## 🚀 最佳实践

1. **单一职责**：每个扩展只做一件事
2. **异常处理**：妥善处理异常，避免影响主流程
3. **性能优化**：注意处理性能，避免阻塞
4. **日志记录**：记录关键操作，方便调试
5. **配置化**：将可变参数配置化，提高灵活性

## ❓ 常见问题

### Q1: 扩展没有生效？

**A**: 检查以下几点：
1. 是否添加了 `@Component` 注解
2. 是否在 Spring 的扫描路径中
3. `isEnabled()` 是否返回 `true`
4. `supports()` 是否支持当前处理器

### Q2: 如何调试扩展？

**A**: 添加日志输出，查看执行情况：

```java
@Slf4j
@Component
public class MyPreProcessor implements PreProcessor {
    @Override
    public ProcessingContext preProcess(ProcessingContext context) {
        log.info("MyPreProcessor 开始执行");
        // ...
        log.info("MyPreProcessor 执行完成");
        return context;
    }
}
```

### Q3: 多个扩展之间如何通信？

**A**: 可以通过上下文的 `options` 或元数据传递数据：

```java
// 在 PreProcessor 中设置
context.getOptions().put("myData", someValue);

// 在 PostProcessor 中获取
Object myData = context.getOptions().get("myData");
```

## 📞 技术支持

如有问题，请联系 OmniAgent 团队或提交 Issue。

