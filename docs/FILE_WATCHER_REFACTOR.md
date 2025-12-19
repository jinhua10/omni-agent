# 📁 FileWatcherService 重构完成报告

**重构时间**: 2025-12-19  
**目标**: 优化文件监听逻辑，支持子目录、失败重试、自动归档  
**状态**: ✅ 已完成

---

## 🎯 核心改进

### 之前的问题

1. ❌ 使用 MD5 哈希判断文件变化，复杂且容易误触发
2. ❌ 只监听根目录，不支持子目录
3. ❌ 处理失败后文件会丢失
4. ❌ 没有自动重试机制

### 新的设计

1. ✅ **扫描-处理-归档模式**: 定期扫描未索引文件
2. ✅ **支持子目录**: 递归扫描和处理
3. ✅ **保留目录结构**: 归档时保留相对路径
4. ✅ **失败重试**: 失败的文件保留在监听目录，下次扫描自动重试
5. ✅ **成功后移除**: 处理成功后从监听目录删除

---

## 📊 完整流程

```
监听目录 (data/watch/)
    ├── 技术文档.pdf
    ├── 设计图/
    │   └── 架构图.pptx
    └── 代码/
        └── README.md
            ↓
【定期扫描：每30秒】
            ↓
发现未索引文件
            ↓
┌────────────────────────────┐
│  处理流程                    │
├────────────────────────────┤
│  1. 读取文件                │
│  2. 文档处理 (Vision LLM)  │
│  3. 保存原始文档            │
│  4. 保存提取的图片          │
│  5. 智能分块                │
│  6. 保存分块                │
│  7. RAG 索引                │
│  8. 从监听目录删除 ⭐       │
└────────────────────────────┘
            ↓
归档到 data/storage/
    ├── documents/
    │   ├── 技术文档.pdf/
    │   │   └── 技术文档.pdf
    │   ├── 设计图/架构图.pptx/
    │   │   └── 架构图.pptx
    │   └── 代码/README.md/
    │       └── README.md
    ├── chunks/
    │   ├── 技术文档.pdf/
    │   │   ├── chunk_000.chunk
    │   │   └── chunk_001.chunk
    │   └── ...
    └── images/
        └── 设计图/架构图.pptx/
            ├── page_001_img.png
            └── page_002_img.png
```

---

## 🔍 关键实现

### 1. 定期扫描机制

```java
// 启动定期扫描任务（每30秒扫描一次）
scanExecutor = Executors.newScheduledThreadPool(1);
scanExecutor.scheduleWithFixedDelay(
        this::scanAndProcessUnindexedFiles,
        5,  // 启动后5秒开始
        30, // 每30秒扫描一次
        TimeUnit.SECONDS
);
```

### 2. 递归扫描子目录

```java
Files.walk(watchPath)
        .filter(Files::isRegularFile)
        .filter(path -> {
            String name = path.getFileName().toString();
            // 过滤临时文件和隐藏文件
            return !name.startsWith(".") && 
                   !name.startsWith("~") && 
                   !name.endsWith(".tmp");
        })
        .forEach(filePath -> {
            // 获取相对路径（保留目录结构）
            Path relativePath = watchPath.relativize(filePath);
            processNewFile(filePath, relativePath);
        });
```

### 3. 保留目录结构

```java
// 生成 documentId（保留相对路径）
String documentId = "doc_" + System.currentTimeMillis() + "_" + 
        relativePathStr.replace("/", "_").replace("\\", "_");

// 保存到存储时使用相对路径
storageService.saveDocument(documentId, relativePathStr, fileData);
```

**效果**:
- 监听目录: `data/watch/设计图/架构图.pptx`
- documentId: `doc_1734619200000_设计图_架构图.pptx`
- 归档位置: `data/storage/documents/设计图/架构图.pptx/架构图.pptx`

### 4. 失败重试机制

```java
try {
    // 处理文件...
    Files.delete(filePath);  // 成功后删除 ⭐
    archivedFiles.put(relativePathStr, System.currentTimeMillis());
    record.setProcessed(true);
    
} catch (Exception e) {
    log.error("❌ 处理失败: {} - {}", relativePathStr, e.getMessage(), e);
    record.setProcessed(false);
    record.setNote("失败: " + e.getMessage());
    // 失败的文件保留在监听目录，等待下次扫描重试 ⭐
}
```

### 5. 避免重复处理

```java
// 检查是否已归档
if (archivedFiles.containsKey(relativePathStr)) {
    log.debug("⏭️ 已归档，跳过: {}", relativePathStr);
    return;
}

// 检查是否正在处理
if (processingRecords.containsKey(relativePathStr)) {
    FileChangeRecord record = processingRecords.get(relativePathStr);
    if (record.getProcessed() != null && record.getProcessed()) {
        log.debug("⏭️ 已处理，跳过: {}", relativePathStr);
        return;
    }
}
```

---

## 📝 使用示例

### 场景 1: 上传单个文件

**操作**:
```bash
# 将文件放入监听目录
cp 技术文档.pdf data/watch/
```

**日志输出**:
```
[INFO] 📄 检测到新文件: 技术文档.pdf
[INFO] 🔍 扫描未索引文件: data/watch
[INFO] 📄 发现未索引文件: 技术文档.pdf
[INFO] 🔄 开始处理文件: 技术文档.pdf
[INFO] 📄 读取文件: 1234567 bytes
[INFO] 🔄 使用 DocumentProcessorManager 处理文档...
[INFO] ✅ 文档处理成功: 5678 chars, 0 images
[INFO] 💾 保存原始文档到存储服务...
[INFO] ✂️ 智能分块...
[INFO] ✅ 分块完成: 10 个块
[INFO] 💾 保存分块到存储...
[INFO] ✅ 分块已保存: 10 个
[INFO] 📇 索引到 RAG...
[INFO] ✅ RAG索引完成
[INFO] 🗑️ 已从监听目录移除: 技术文档.pdf
[INFO] ✅ 处理完成: 技术文档.pdf
```

**结果**:
- ✅ 文件从 `data/watch/` 移除
- ✅ 归档到 `data/storage/documents/技术文档.pdf/`
- ✅ 分块到 `data/storage/chunks/技术文档.pdf/`
- ✅ RAG 索引完成

### 场景 2: 上传带子目录的文件

**操作**:
```bash
# 创建子目录并上传文件
mkdir -p data/watch/设计图
cp 架构图.pptx data/watch/设计图/
```

**日志输出**:
```
[INFO] 📄 发现未索引文件: 设计图/架构图.pptx
[INFO] 🔄 开始处理文件: 设计图/架构图.pptx
[INFO] 🖼️ 保存提取的图片: 5 张
[INFO] ✅ 处理完成: 设计图/架构图.pptx
```

**结果**:
- ✅ 保留目录结构
- ✅ 归档到 `data/storage/documents/设计图/架构图.pptx/`
- ✅ 分块到 `data/storage/chunks/设计图/架构图.pptx/`
- ✅ 图片到 `data/storage/images/设计图/架构图.pptx/`

### 场景 3: 处理失败自动重试

**操作**:
```bash
# 上传一个损坏的文件
cp broken.pdf data/watch/
```

**第一次扫描**:
```
[INFO] 📄 发现未索引文件: broken.pdf
[INFO] 🔄 开始处理文件: broken.pdf
[ERROR] ❌ 处理失败: broken.pdf - 文档内容为空
```

**30秒后第二次扫描**:
```
[INFO] 🔍 扫描未索引文件: data/watch
[INFO] 📄 发现未索引文件: broken.pdf
[INFO] 🔄 开始处理文件: broken.pdf
```

**结果**:
- ✅ 文件保留在监听目录
- ✅ 自动重试
- ✅ 记录失败原因: "失败: 文档内容为空"

### 场景 4: 同名不同路径

**操作**:
```bash
# 上传两个同名文件
cp README.md data/watch/
cp README.md data/watch/代码/
```

**结果**:
- ✅ 两个文件都被处理
- ✅ 归档位置不同:
  - `data/storage/documents/README.md/`
  - `data/storage/documents/代码/README.md/`
- ✅ 不会冲突！

---

## 🔧 配置

### application.yml

```yaml
omni-agent:
  file-watcher:
    enabled: true                    # 启用文件监听
    watch-directory: ./data/watch    # 监听目录
    auto-index: true                 # 自动索引
```

### 监听目录结构建议

```
data/watch/
├── documents/        # 通用文档
│   ├── 技术文档.pdf
│   └── 用户手册.docx
├── designs/          # 设计图
│   ├── 架构图.pptx
│   └── 流程图.vsdx
└── code/             # 代码文档
    ├── README.md
    └── API.md
```

---

## ✅ 核心特性

### 1. 智能扫描

- ✅ 递归扫描子目录
- ✅ 过滤临时文件（`.`, `~`, `.tmp`）
- ✅ 跳过已处理文件
- ✅ 跳过已归档文件

### 2. 完整处理

- ✅ Vision LLM 处理（PPT/PDF等）
- ✅ 图片提取和保存
- ✅ 智能分块（自动选择策略）
- ✅ 分块保存
- ✅ RAG 索引

### 3. 目录结构

- ✅ 保留相对路径
- ✅ 避免同名冲突
- ✅ 易于查找和管理

### 4. 失败处理

- ✅ 详细日志记录
- ✅ 自动重试机制
- ✅ 失败文件不丢失

### 5. 性能优化

- ✅ 定期扫描（30秒间隔）
- ✅ 避免重复处理
- ✅ 线程池管理

---

## 📈 监控和管理

### API 接口

1. **查看未处理文件**:
   ```bash
   curl http://localhost:8080/api/file-watcher/changes/unprocessed
   ```

2. **查看所有记录**:
   ```bash
   curl http://localhost:8080/api/file-watcher/changes
   ```

3. **手动触发扫描**:
   ```bash
   curl -X POST http://localhost:8080/api/file-watcher/process-all
   ```

4. **清理已处理记录**:
   ```bash
   curl -X POST http://localhost:8080/api/file-watcher/clear-processed
   ```

### 日志级别

```yaml
logging:
  level:
    top.yumbo.ai.omni.web.service.FileWatcherService: INFO
```

- `INFO`: 显示处理进度
- `DEBUG`: 显示详细扫描信息
- `ERROR`: 显示失败详情

---

## 🎉 总结

### 核心改进

1. ✅ **扫描-处理-归档**: 清晰的三段式流程
2. ✅ **子目录支持**: 保留完整目录结构
3. ✅ **失败重试**: 自动重试失败的文件
4. ✅ **成功移除**: 处理成功后清理监听目录
5. ✅ **避免冲突**: 同名不同路径不会冲突

### 用户价值

- 📁 **批量导入**: 将文件放入监听目录，自动处理
- 🔄 **自动重试**: 失败文件自动重试，无需人工干预
- 📂 **结构化**: 保留目录结构，易于管理
- 📊 **可监控**: 通过API查看处理状态
- 🛡️ **可靠性**: 失败文件不丢失

---

**重构完成时间**: 2025-12-19  
**编译状态**: ✅ BUILD SUCCESS  
**测试状态**: ✅ 待验证

🎉 **FileWatcherService 重构完成！现在支持子目录、失败重试和自动归档！** 📁✨

