# ✅ 文件自动索引问题修复

## 🐛 问题描述

上传文件到监听目录后，文件没有自动索引。

## 🔍 问题根因

`ConfigPersistenceService` 创建默认配置时，`autoIndex` 被硬编码为 `false`：

```java
// ❌ 错误的默认配置
private FileWatcherConfig createDefaultConfig() {
    FileWatcherConfig config = FileWatcherConfig.builder()
            .enabled(true)
            .autoIndex(false)  // ❌ 默认不自动索引
            .watchDirectory("./data/documents")
            .build();
    return config;
}
```

即使在 `application.yml` 中配置了 `auto-index: true`，也不会生效，因为：
1. `FileWatcherService` 从 `ConfigPersistenceService.loadFileWatcherConfig()` 加载配置
2. 如果配置文件不存在，使用硬编码的默认值（`autoIndex=false`）
3. `application.yml` 的配置被忽略了

## ✅ 解决方案

让 `ConfigPersistenceService` 从 `application.yml` 读取默认配置：

### 1. 添加 @Value 注解

```java
@Service
public class ConfigPersistenceService {
    // ⭐ 从 application.yml 读取配置
    @Value("${omni-agent.file-watcher.enabled:true}")
    private boolean fileWatcherEnabled;
    
    @Value("${omni-agent.file-watcher.auto-index:true}")
    private boolean fileWatcherAutoIndex;
    
    @Value("${omni-agent.file-watcher.watch-directory:./data/documents}")
    private String fileWatcherDirectory;
}
```

### 2. 使用动态配置值

```java
// ✅ 正确的默认配置
private FileWatcherConfig createDefaultConfig() {
    FileWatcherConfig config = FileWatcherConfig.builder()
            .enabled(fileWatcherEnabled)        // ⭐ 从 application.yml 读取
            .autoIndex(fileWatcherAutoIndex)    // ⭐ 从 application.yml 读取
            .watchDirectory(fileWatcherDirectory)  // ⭐ 从 application.yml 读取
            .lastUpdated(System.currentTimeMillis())
            .version("1.0")
            .build();

    log.info("🔧 创建默认配置: enabled={}, autoIndex={}, watchDirectory={}",
            config.getEnabled(), config.getAutoIndex(), config.getWatchDirectory());

    saveFileWatcherConfig(config);
    return config;
}
```

## 📊 配置优先级

现在的配置加载逻辑：

```
1. 尝试从 data/config/file-watcher-config.json 加载
   ↓
   如果文件存在 → 使用文件中的配置
   ↓
   如果文件不存在 → 创建默认配置
   ↓
2. 默认配置从 application.yml 读取
   ↓
3. 保存到 data/config/file-watcher-config.json
```

**优势**：
- ✅ 首次启动时使用 `application.yml` 的配置
- ✅ 后续可以通过 API 动态修改配置（保存到 JSON 文件）
- ✅ 配置持久化，重启后保留

## 🔧 application.yml 配置

```yaml
omni-agent:
  file-watcher:
    enabled: true                       # 启用文件监听
    watch-directory: ./data/documents   # 监听目录
    auto-index: true                    # ⭐ 自动索引（重要！）
    scan-interval: 30000                # 扫描间隔（毫秒）
```

## 📝 完整处理流程

### 1. 用户上传文件

```bash
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@test.pdf"
```

**立即返回**：
```json
{
  "success": true,
  "message": "文件上传成功，正在索引中...",
  "fileName": "test.pdf",
  "indexing": true
}
```

文件保存到：`./data/documents/test.pdf`

### 2. FileWatcherService 自动处理

#### 定期扫描（30秒一次）

```java
@Scheduled(fixedDelay = 30000)
private void scanAndProcessUnindexedFiles() {
    // 扫描 data/documents 目录
    // 发现未处理的文件
    // 调用 processNewFile()
}
```

#### 完整处理流程

```
检测到新文件: test.pdf
  ↓
① 读取文件（字节数组）
  ↓
② DocumentProcessorManager 处理
   - PDF → 渲染每页为图片（300 DPI）
   - 提取页面文字
   - Vision LLM 分析（智能批处理 + 并行）
  ↓
③ 保存原始文档到 storage
   → data/storage/documents/test.pdf/test.pdf
  ↓
④ 保存提取的图片
   → data/storage/images/test.pdf/page_001_img_000.png
  ↓
⑤ 智能分块（ChunkingStrategyManager）
   → 自动选择策略（PPL、固定大小等）
  ↓
⑥ 保存分块
   → data/storage/chunks/test.pdf/chunk_000.chunk
  ↓
⑦ RAG 索引
   → 每个分块索引到 Lucene/SQLite
  ↓
⑧ 删除监听目录中的文件
   → data/documents/test.pdf（已处理）
  ↓
✅ 完成，标记为"已索引"
```

### 3. 前端轮询检查状态

```javascript
// 每 5 秒检查一次
setInterval(async () => {
  const response = await fetch('/api/documents/list?keyword=test.pdf');
  const doc = response.documents.find(d => d.fileName === 'test.pdf');
  
  if (doc && doc.indexed) {
    // ✅ 索引完成
    updateUI(doc);
    stopPolling();
  }
}, 5000);
```

## 🧪 测试验证

### 1. 启动应用

```bash
cd omni-agent-example-basic
mvn spring-boot:run
```

**预期日志**：

```
🔧 创建默认配置: enabled=true, autoIndex=true, watchDirectory=./data/documents
✅ 加载文件监听配置: autoIndex=true, enabled=true
✅ 创建监听目录: D:\...\data\documents
🔍 开始监听文件变化...
⏰ 定期扫描任务已启动: 每 30 秒扫描一次
```

### 2. 上传文件

```bash
curl -X POST http://localhost:8080/api/documents/upload \
  -F "file=@test.pdf"
```

**立即响应**（秒级）：
```json
{
  "success": true,
  "message": "文件上传成功，正在索引中...",
  "fileName": "test.pdf",
  "fileSize": 123456,
  "documentId": null,
  "indexing": true
}
```

### 3. 观察后台处理

**30秒内应该看到**：

```
🔍 扫描未索引文件: ./data/documents
📄 发现未索引文件: test.pdf
🔄 开始处理文件: test.pdf
📄 读取文件: 123456 bytes
🔄 使用 DocumentProcessorManager 处理文档...
🔍 [VisionLLM] 开始处理文档: test.pdf
🔍 [VisionLLM] PDF 文档包含 10 页
✅ [VisionLLM] 成功渲染 PDF 页面 1 / 10
...
📦 [VisionLLM] 智能分批完成: 2 个批次
🚀 [Parallel Processing] 开始并行处理 2 个批次
✅ [Parallel Processing] 并行处理完成 - 耗时: 45234ms
✅ 文档处理成功: 2345 chars, 10 images
💾 保存原始文档到存储服务...
🖼️ 保存提取的图片: 10 张
✂️ 智能分块...
✅ 分块完成: 5 个块
💾 保存分块到存储...
✅ 分块已保存: 5 个
📇 索引到 RAG...
✅ RAG索引完成
🗑️ 已从监听目录移除: test.pdf
✅ 处理完成: test.pdf
```

### 4. 验证结果

```bash
# 检查文档列表
curl http://localhost:8080/api/documents/list

# 应该看到 test.pdf，indexed=true
```

**文件结构**：

```
data/
├── documents/          # 监听目录（处理后文件会被删除）
│   └── (空，文件已处理)
├── storage/
│   ├── documents/
│   │   └── test.pdf/
│   │       └── test.pdf
│   ├── chunks/
│   │   └── test.pdf/
│   │       ├── chunk_000.chunk
│   │       ├── chunk_001.chunk
│   │       └── ...
│   └── images/
│       └── test.pdf/
│           ├── page_001_img_000.png
│           ├── page_002_img_000.png
│           └── ...
└── config/
    └── file-watcher-config.json  # 持久化配置
```

## 📊 关键修改总结

| 文件 | 修改内容 | 说明 |
|------|---------|------|
| `ConfigPersistenceService.java` | ✅ 添加 @Value 注解<br>✅ 修改 createDefaultConfig() | 从 application.yml 读取配置 |
| `application.yml` | ✅ 添加 auto-index: true | 启用自动索引 |

## ✅ 验证清单

- [x] 修改 `ConfigPersistenceService` 读取 application.yml
- [x] 确保 `auto-index: true` 在配置文件中
- [x] 编译验证通过
- [x] FileWatcherService 逻辑完整（已有）
- [x] 文档更新

## 🎉 总结

**问题**：`autoIndex` 默认为 `false`，导致文件不自动索引。

**解决**：让 `ConfigPersistenceService` 从 `application.yml` 读取默认配置，确保 `auto-index: true` 生效。

**现在文件上传后会自动索引，完整的处理流程包括**：
1. ✅ Vision LLM 处理（PDF/PPT/Word/Excel）
2. ✅ 智能批处理 + 并行处理
3. ✅ 图片提取和保存
4. ✅ 智能分块（PPL/固定大小等）
5. ✅ RAG 索引
6. ✅ 自动归档到 storage

**启动应用即可测试！** 🚀

