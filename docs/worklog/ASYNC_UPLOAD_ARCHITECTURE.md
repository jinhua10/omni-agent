# ✅ 异步文档上传和索引架构

## 🎯 目标

实现异步文档上传和索引架构：
1. **上传时**：文件直接保存到监控目录 `data/documents`
2. **状态**：标记为"索引中"
3. **后台处理**：FileWatcherService 自动检测并处理
4. **完成后**：移动到 `data/storage/documents`，完成 RAG 索引

**优势**：避免批量上传时的阻塞，提供更好的用户体验

## 📊 架构流程

### 旧架构（同步，阻塞）

```
用户上传文件
    ↓
Controller 接收
    ↓
DocumentProcessor 处理（耗时！）
    ↓
文档分块
    ↓
RAG 索引
    ↓
保存到 storage
    ↓
返回响应（需要等待整个流程）
```

**问题**：
- ❌ 批量上传时需要等待所有文件处理完成
- ❌ 前端页面阻塞，用户体验差
- ❌ 处理失败时前端无响应

### 新架构（异步，非阻塞）⭐

```
用户上传文件
    ↓
Controller 接收
    ↓
保存到监控目录 (data/documents)  [快速！]
    ↓
立即返回"索引中"状态  [非阻塞！]
    ↓
[后台异步处理]
    ↓
FileWatcherService 扫描监控目录
    ↓
检测到新文件
    ↓
DocumentProcessor 处理
    ↓
文档分块
    ↓
RAG 索引
    ↓
移动到 storage (data/storage/documents)
    ↓
标记为"已索引"
```

**优势**：
- ✅ 上传立即返回，不阻塞
- ✅ 批量上传快速响应
- ✅ 前端可以显示"索引中"状态
- ✅ 处理失败时可重试

## 🔧 技术实现

### 1. DocumentManagementController 修改

#### 上传文档（单个）

```java
@PostMapping("/upload")
public UploadResponse uploadDocument(
        @RequestParam("file") MultipartFile file,
        @RequestParam(value = "autoIndex", defaultValue = "true") boolean autoIndex) {

    // ⭐ 直接保存到监听目录
    Path watchDir = Paths.get(fileWatcherConfig.getWatchDirectory());
    if (!Files.exists(watchDir)) {
        Files.createDirectories(watchDir);
    }

    Path targetFile = watchDir.resolve(filename);
    file.transferTo(targetFile);

    // ⭐ 立即返回"索引中"状态
    response.setIndexing(true);
    response.setMessage("文件上传成功，正在索引中...");
    response.setDocumentId(null);  // 索引完成后才有
    
    return response;
}
```

#### 批量上传

```java
@PostMapping("/upload-batch")
public BatchUploadResponse uploadBatch(
        @RequestParam("files") MultipartFile[] files) {

    // 批量保存到监听目录
    for (MultipartFile file : files) {
        Path targetFile = watchDir.resolve(file.getOriginalFilename());
        file.transferTo(targetFile);
        
        // ⭐ 标记为"索引中"
        uploadResult.setIndexing(true);
        uploadResult.setMessage("文件上传成功，正在索引中...");
    }
    
    // 立即返回
    return response;
}
```

### 2. FileWatcherService（已存在）

监听目录：`data/documents`（或配置的目录）

**处理流程**：
```java
// 1. 定期扫描（30秒）
@Scheduled(fixedDelay = 30000)
private void scanAndProcessUnindexedFiles() {
    // 扫描监听目录
    Files.walk(watchDir).forEach(file -> {
        // 检查是否已处理
        if (!isProcessed(file)) {
            // 处理文件
            processFile(file);
        }
    });
}

// 2. 处理文件
private void processFile(Path file) {
    // a. DocumentProcessor 处理
    // b. 保存原始文档
    // c. 保存图片
    // d. 文档分块
    // e. RAG 索引
    // f. 移动到 storage
    // g. 删除监听目录中的文件
}
```

### 3. 响应 DTO

```java
@Data
public static class UploadResponse {
    private boolean success;
    private String message;
    private String fileName;
    private long fileSize;
    private String documentId;       // null = 索引中
    private boolean autoIndexed;
    private boolean indexing;        // ⭐ 新增：索引中状态
}

@Data
public static class UploadResult {
    private boolean success;
    private String message;
    private String fileName;
    private String documentId;       // null = 索引中
    private long fileSize;
    private boolean indexing;        // ⭐ 新增：索引中状态
}
```

## 📱 前端UI处理

### 1. 上传后立即显示

```javascript
// 上传成功响应
{
  "success": true,
  "message": "文件上传成功，正在索引中...",
  "fileName": "技术文档.pdf",
  "fileSize": 1234567,
  "documentId": null,       // ⭐ 索引中时为 null
  "indexing": true          // ⭐ 显示"索引中"状态
}
```

### 2. UI 显示状态

```html
<div class="file-item">
  <span class="file-name">技术文档.pdf</span>
  
  <!-- ⭐ 根据 indexing 字段显示状态 -->
  <span v-if="file.indexing" class="status indexing">
    <i class="spinner"></i> 索引中...
  </span>
  <span v-else class="status indexed">
    <i class="check"></i> 已索引
  </span>
  
  <!-- 索引中时禁用某些操作 -->
  <button :disabled="file.indexing" @click="download(file)">下载</button>
  <button :disabled="file.indexing" @click="delete(file)">删除</button>
</div>
```

### 3. 轮询检查状态

```javascript
// 方案 1: 轮询文档列表
setInterval(() => {
  if (hasIndexingFiles()) {
    fetchDocumentList();  // 重新获取列表
  }
}, 5000);  // 每 5 秒检查一次

// 方案 2: 轮询单个文件状态
function checkFileStatus(fileName) {
  // 调用 /api/documents/list?keyword=fileName
  // 检查是否已完成索引（documentId 不为 null）
}
```

### 4. 完整示例

```vue
<template>
  <div class="document-upload">
    <input type="file" @change="uploadFiles" multiple />
    
    <div class="file-list">
      <div v-for="file in files" :key="file.fileName" class="file-item">
        <span class="file-name">{{ file.fileName }}</span>
        
        <!-- ⭐ 状态显示 -->
        <span v-if="file.indexing" class="status indexing">
          <i class="el-icon-loading"></i> 索引中...
        </span>
        <span v-else-if="file.documentId" class="status indexed">
          <i class="el-icon-success"></i> 已索引
        </span>
        <span v-else class="status error">
          <i class="el-icon-error"></i> 索引失败
        </span>
        
        <!-- 操作按钮 -->
        <button :disabled="file.indexing" @click="downloadFile(file)">
          下载
        </button>
        <button :disabled="file.indexing" @click="deleteFile(file)">
          删除
        </button>
      </div>
    </div>
  </div>
</template>

<script>
export default {
  data() {
    return {
      files: [],
      pollingTimer: null
    };
  },
  
  methods: {
    async uploadFiles(event) {
      const formData = new FormData();
      Array.from(event.target.files).forEach(file => {
        formData.append('files', file);
      });
      
      // 批量上传
      const response = await this.$axios.post('/api/documents/upload-batch', formData);
      
      // ⭐ 立即添加到列表，显示"索引中"
      response.results.forEach(result => {
        if (result.success) {
          this.files.push({
            fileName: result.fileName,
            fileSize: result.fileSize,
            documentId: result.documentId,  // null
            indexing: result.indexing        // true
          });
        }
      });
      
      // 开始轮询
      this.startPolling();
    },
    
    startPolling() {
      if (this.pollingTimer) return;
      
      this.pollingTimer = setInterval(async () => {
        // 检查是否有索引中的文件
        const indexingFiles = this.files.filter(f => f.indexing);
        if (indexingFiles.length === 0) {
          this.stopPolling();
          return;
        }
        
        // 重新获取文档列表
        const response = await this.$axios.get('/api/documents/list');
        
        // 更新索引状态
        indexingFiles.forEach(file => {
          const indexed = response.documents.find(d => 
            d.fileName === file.fileName && d.indexed
          );
          
          if (indexed) {
            // ⭐ 已索引完成
            file.indexing = false;
            file.documentId = indexed.documentId;
          }
        });
      }, 5000);  // 每 5 秒检查一次
    },
    
    stopPolling() {
      if (this.pollingTimer) {
        clearInterval(this.pollingTimer);
        this.pollingTimer = null;
      }
    }
  },
  
  beforeDestroy() {
    this.stopPolling();
  }
};
</script>
```

## 🎨 状态图标和样式

```css
.status {
  display: inline-flex;
  align-items: center;
  padding: 4px 8px;
  border-radius: 4px;
  font-size: 12px;
}

.status.indexing {
  background: #fff7e6;
  color: #fa8c16;
}

.status.indexing i {
  animation: spin 1s linear infinite;
}

.status.indexed {
  background: #f6ffed;
  color: #52c41a;
}

.status.error {
  background: #fff1f0;
  color: #ff4d4f;
}

@keyframes spin {
  from { transform: rotate(0deg); }
  to { transform: rotate(360deg); }
}
```

## 📊 完整流程示例

### 用户上传 3 个文件

```
用户选择 3 个文件并点击上传
    ↓
前端调用 /api/documents/upload-batch
    ↓
后端接收文件
    ↓
将 3 个文件保存到 data/documents/
    ↓
返回响应（立即，不阻塞）:
{
  "success": true,
  "message": "批量上传完成: 成功 3, 失败 0。文件正在后台索引中...",
  "successCount": 3,
  "results": [
    {
      "success": true,
      "fileName": "文件1.pdf",
      "indexing": true,
      "documentId": null
    },
    {
      "success": true,
      "fileName": "文件2.docx",
      "indexing": true,
      "documentId": null
    },
    {
      "success": true,
      "fileName": "文件3.pptx",
      "indexing": true,
      "documentId": null
    }
  ]
}
    ↓
前端立即显示 3 个文件，状态为"索引中"
    ↓
前端开始轮询（每 5 秒）
    ↓
[后台处理]
    ↓
FileWatcherService 扫描到 3 个新文件
    ↓
并行处理 3 个文件（如果配置了线程池）
    ↓
文件1 处理完成 (30s)
    ↓
文件2 处理完成 (35s)
    ↓
文件3 处理完成 (40s)
    ↓
移动到 data/storage/documents/
    ↓
前端轮询检测到已索引
    ↓
更新 UI 状态为"已索引"
    ↓
停止轮询
```

## ⚙️ 配置

### application.yml

```yaml
omni-agent:
  file-watcher:
    enabled: true
    watch-directory: ./data/documents
    auto-index: true
    scan-interval: 30000  # 30 秒扫描一次
  
  thread-pool:
    file-watcher:
      core-pool-size: 1
      max-pool-size: 2
      queue-capacity: 50
```

## 🎉 优势总结

| 项目 | 旧架构（同步） | 新架构（异步） | 提升 |
|------|--------------|--------------|------|
| **上传响应** | 需要等待处理完成 | 立即返回 | ✅ 快速响应 |
| **批量上传** | 阻塞，需等待所有文件 | 非阻塞 | ✅ 用户体验佳 |
| **失败处理** | 前端超时/无响应 | 后台重试 | ✅ 更可靠 |
| **资源利用** | 占用请求线程 | 后台异步 | ✅ 更高效 |
| **状态可见** | 无 | 索引中/已索引 | ✅ 透明 |

## ✅ 完成状态

- ✅ 修改 `DocumentManagementController`
- ✅ 添加 `indexing` 字段到响应 DTO
- ✅ 文件直接保存到监听目录
- ✅ 移除旧的同步处理方法
- ✅ 编译验证通过

**现在系统支持异步文档上传和索引，提供更好的用户体验！** 🚀

