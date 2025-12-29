# ✅ 文档管理 - Pending 区域功能完成

## 🎯 功能说明

在文档管理页面添加"待处理"（Pending）区域，显示 `data/documents` 目录下等待索引的文件，并允许取消索引。

## 📊 功能特性

| 功能 | 说明 | 状态 |
|------|------|------|
| **显示待处理文件** | 列出 data/documents 下的所有文件 | ✅ |
| **显示处理状态** | 标记文件是否正在处理 | ✅ |
| **取消索引** | 删除未开始处理的文件 | ✅ |
| **防止误删** | 正在处理的文件不允许取消 | ✅ |
| **实时更新** | 支持轮询刷新状态 | ✅ |

## 🔌 后端 API

### 1. 获取待处理文件列表

**请求**：
```http
GET /api/documents/pending
```

**响应**：
```json
{
  "success": true,
  "message": null,
  "files": [
    {
      "fileName": "技术文档.pdf",
      "relativePath": "技术文档.pdf",
      "fileSize": 1234567,
      "uploadTime": 1734691234000,
      "processing": false,
      "cancelable": true
    },
    {
      "fileName": "设计图.pptx",
      "relativePath": "design/设计图.pptx",
      "fileSize": 2345678,
      "uploadTime": 1734691235000,
      "processing": true,
      "cancelable": false
    }
  ],
  "count": 2
}
```

**字段说明**：
- `fileName`: 文件名
- `relativePath`: 相对路径（支持子目录）
- `fileSize`: 文件大小（字节）
- `uploadTime`: 上传时间（时间戳）
- `processing`: 是否正在处理（true = 正在处理，false = 等待处理）
- `cancelable`: 是否可以取消（只有未开始处理的才能取消）

### 2. 取消文件索引

**请求**：
```http
DELETE /api/documents/pending/{fileName}
```

**示例**：
```http
DELETE /api/documents/pending/技术文档.pdf
```

**响应（成功）**：
```json
{
  "success": true,
  "message": "文件已删除"
}
```

**响应（正在处理）**：
```json
{
  "success": false,
  "message": "文件正在处理中，无法取消"
}
```

**响应（文件不存在）**：
```json
{
  "success": false,
  "message": "文件不存在"
}
```

## 🎨 前端实现示例

### Vue 3 + Element Plus

```vue
<template>
  <div class="document-management">
    <!-- 待处理区域 ⭐ -->
    <el-card class="pending-section" v-if="pendingFiles.length > 0">
      <template #header>
        <div class="card-header">
          <span>⏳ 待处理文件 ({{ pendingFiles.length }})</span>
          <el-button text @click="refreshPending">
            <el-icon><Refresh /></el-icon>
            刷新
          </el-button>
        </div>
      </template>

      <el-table :data="pendingFiles" style="width: 100%">
        <el-table-column prop="fileName" label="文件名" />
        
        <el-table-column prop="fileSize" label="大小" width="120">
          <template #default="{ row }">
            {{ formatFileSize(row.fileSize) }}
          </template>
        </el-table-column>
        
        <el-table-column prop="uploadTime" label="上传时间" width="180">
          <template #default="{ row }">
            {{ formatDate(row.uploadTime) }}
          </template>
        </el-table-column>
        
        <el-table-column label="状态" width="150">
          <template #default="{ row }">
            <el-tag v-if="row.processing" type="warning">
              <el-icon class="is-loading"><Loading /></el-icon>
              正在处理
            </el-tag>
            <el-tag v-else type="info">
              <el-icon><Clock /></el-icon>
              等待处理
            </el-tag>
          </template>
        </el-table-column>
        
        <el-table-column label="操作" width="120">
          <template #default="{ row }">
            <el-popconfirm
              title="确定要取消索引吗？"
              @confirm="cancelFile(row)"
              :disabled="!row.cancelable"
            >
              <template #reference>
                <el-button
                  type="danger"
                  text
                  :disabled="!row.cancelable"
                  :icon="Delete"
                >
                  取消
                </el-button>
              </template>
            </el-popconfirm>
          </template>
        </el-table-column>
      </el-table>
    </el-card>

    <!-- 已索引文档区域 -->
    <el-card class="indexed-section">
      <template #header>
        <span>📚 已索引文档 ({{ indexedDocuments.length }})</span>
      </template>
      
      <!-- 已索引文档列表 -->
      <el-table :data="indexedDocuments">
        <!-- ... -->
      </el-table>
    </el-card>
  </div>
</template>

<script setup>
import { ref, onMounted, onUnmounted } from 'vue';
import { ElMessage } from 'element-plus';
import { Refresh, Loading, Clock, Delete } from '@element-plus/icons-vue';
import axios from 'axios';

const pendingFiles = ref([]);
const indexedDocuments = ref([]);
let pollingTimer = null;

// 获取待处理文件
async function fetchPendingFiles() {
  try {
    const response = await axios.get('/api/documents/pending');
    if (response.data.success) {
      pendingFiles.value = response.data.files;
    }
  } catch (error) {
    console.error('获取待处理文件失败:', error);
  }
}

// 刷新待处理文件
async function refreshPending() {
  await fetchPendingFiles();
  ElMessage.success('已刷新');
}

// 取消文件索引
async function cancelFile(file) {
  try {
    const response = await axios.delete(
      `/api/documents/pending/${encodeURIComponent(file.fileName)}`
    );
    
    if (response.data.success) {
      ElMessage.success('已取消索引');
      await fetchPendingFiles();
    } else {
      ElMessage.error(response.data.message);
    }
  } catch (error) {
    ElMessage.error('取消失败: ' + error.message);
  }
}

// 格式化文件大小
function formatFileSize(bytes) {
  if (bytes < 1024) return bytes + ' B';
  if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(2) + ' KB';
  if (bytes < 1024 * 1024 * 1024) return (bytes / (1024 * 1024)).toFixed(2) + ' MB';
  return (bytes / (1024 * 1024 * 1024)).toFixed(2) + ' GB';
}

// 格式化日期
function formatDate(timestamp) {
  return new Date(timestamp).toLocaleString('zh-CN');
}

// 获取已索引文档
async function fetchIndexedDocuments() {
  try {
    const response = await axios.get('/api/documents/list');
    if (response.data.success) {
      indexedDocuments.value = response.data.documents;
    }
  } catch (error) {
    console.error('获取已索引文档失败:', error);
  }
}

// 开始轮询
function startPolling() {
  // 每 5 秒刷新一次
  pollingTimer = setInterval(async () => {
    await fetchPendingFiles();
    await fetchIndexedDocuments();
  }, 5000);
}

// 停止轮询
function stopPolling() {
  if (pollingTimer) {
    clearInterval(pollingTimer);
    pollingTimer = null;
  }
}

// 初始化
onMounted(async () => {
  await fetchPendingFiles();
  await fetchIndexedDocuments();
  startPolling();
});

// 清理
onUnmounted(() => {
  stopPolling();
});
</script>

<style scoped>
.document-management {
  padding: 20px;
}

.pending-section {
  margin-bottom: 20px;
  border: 2px solid #e6a23c;
}

.card-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
}

.is-loading {
  animation: rotating 2s linear infinite;
}

@keyframes rotating {
  from { transform: rotate(0deg); }
  to { transform: rotate(360deg); }
}
</style>
```

### React + Ant Design

```jsx
import React, { useState, useEffect } from 'react';
import { Card, Table, Tag, Button, Popconfirm, message, Space } from 'antd';
import { ClockCircleOutlined, LoadingOutlined, DeleteOutlined, ReloadOutlined } from '@ant-design/icons';
import axios from 'axios';

export default function DocumentManagement() {
  const [pendingFiles, setPendingFiles] = useState([]);
  const [indexedDocuments, setIndexedDocuments] = useState([]);

  // 获取待处理文件
  const fetchPendingFiles = async () => {
    try {
      const response = await axios.get('/api/documents/pending');
      if (response.data.success) {
        setPendingFiles(response.data.files);
      }
    } catch (error) {
      console.error('获取待处理文件失败:', error);
    }
  };

  // 取消文件索引
  const cancelFile = async (file) => {
    try {
      const response = await axios.delete(
        `/api/documents/pending/${encodeURIComponent(file.fileName)}`
      );
      
      if (response.data.success) {
        message.success('已取消索引');
        await fetchPendingFiles();
      } else {
        message.error(response.data.message);
      }
    } catch (error) {
      message.error('取消失败: ' + error.message);
    }
  };

  // 格式化文件大小
  const formatFileSize = (bytes) => {
    if (bytes < 1024) return bytes + ' B';
    if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(2) + ' KB';
    if (bytes < 1024 * 1024 * 1024) return (bytes / (1024 * 1024)).toFixed(2) + ' MB';
    return (bytes / (1024 * 1024 * 1024)).toFixed(2) + ' GB';
  };

  // 待处理文件列表列配置
  const pendingColumns = [
    {
      title: '文件名',
      dataIndex: 'fileName',
      key: 'fileName',
    },
    {
      title: '大小',
      dataIndex: 'fileSize',
      key: 'fileSize',
      width: 120,
      render: (size) => formatFileSize(size),
    },
    {
      title: '上传时间',
      dataIndex: 'uploadTime',
      key: 'uploadTime',
      width: 180,
      render: (time) => new Date(time).toLocaleString('zh-CN'),
    },
    {
      title: '状态',
      key: 'status',
      width: 150,
      render: (_, record) => (
        record.processing ? (
          <Tag icon={<LoadingOutlined spin />} color="warning">
            正在处理
          </Tag>
        ) : (
          <Tag icon={<ClockCircleOutlined />} color="default">
            等待处理
          </Tag>
        )
      ),
    },
    {
      title: '操作',
      key: 'action',
      width: 120,
      render: (_, record) => (
        <Popconfirm
          title="确定要取消索引吗？"
          onConfirm={() => cancelFile(record)}
          disabled={!record.cancelable}
        >
          <Button
            type="link"
            danger
            icon={<DeleteOutlined />}
            disabled={!record.cancelable}
          >
            取消
          </Button>
        </Popconfirm>
      ),
    },
  ];

  // 初始化和轮询
  useEffect(() => {
    fetchPendingFiles();
    
    // 每 5 秒刷新一次
    const timer = setInterval(() => {
      fetchPendingFiles();
    }, 5000);

    return () => clearInterval(timer);
  }, []);

  return (
    <div style={{ padding: 20 }}>
      {/* 待处理区域 */}
      {pendingFiles.length > 0 && (
        <Card
          title={
            <Space>
              <span>⏳ 待处理文件 ({pendingFiles.length})</span>
            </Space>
          }
          extra={
            <Button
              icon={<ReloadOutlined />}
              onClick={fetchPendingFiles}
            >
              刷新
            </Button>
          }
          style={{ marginBottom: 20, borderColor: '#faad14' }}
        >
          <Table
            dataSource={pendingFiles}
            columns={pendingColumns}
            rowKey="fileName"
            pagination={false}
          />
        </Card>
      )}

      {/* 已索引文档区域 */}
      <Card title="📚 已索引文档">
        {/* ... */}
      </Card>
    </div>
  );
}
```

## 🎯 使用流程

### 1. 用户上传文件

```bash
curl -X POST http://localhost:8080/api/documents/upload -F "file=@test.pdf"
```

文件立即保存到 `data/documents/test.pdf`，返回"索引中"状态。

### 2. 前端显示待处理文件

```javascript
// 获取待处理文件列表
GET /api/documents/pending

// 响应
{
  "files": [
    {
      "fileName": "test.pdf",
      "processing": false,  // 等待处理
      "cancelable": true    // 可以取消
    }
  ]
}
```

前端在 **Pending 区域** 显示该文件，状态为"等待处理"。

### 3. FileWatcherService 开始处理

30秒内，FileWatcherService 检测到新文件并开始处理：

```javascript
// 再次获取列表
GET /api/documents/pending

// 响应
{
  "files": [
    {
      "fileName": "test.pdf",
      "processing": true,   // 正在处理 ⭐
      "cancelable": false   // 不可取消 ⭐
    }
  ]
}
```

前端更新状态为"正在处理"，取消按钮变为禁用。

### 4. 处理完成

处理完成后，文件从 `data/documents` 删除（移到 `data/storage`）：

```javascript
// 获取列表
GET /api/documents/pending

// 响应
{
  "files": []  // 文件已处理完成，从待处理列表消失
}
```

前端从 Pending 区域移除该文件。

同时，已索引文档列表会增加该文件。

### 5. 取消索引（可选）

如果文件还在"等待处理"状态，用户可以取消：

```javascript
// 取消索引
DELETE /api/documents/pending/test.pdf

// 响应
{
  "success": true,
  "message": "文件已删除"
}
```

文件从 `data/documents` 删除，不会被索引。

## 🔧 后端关键实现

### 1. 检查文件是否正在处理

```java
// FileWatcherService.java
public boolean isFileProcessing(String relativePathOrFileName) {
    FileChangeRecord record = processingRecords.get(relativePathOrFileName);
    return record != null && !Boolean.TRUE.equals(record.getProcessed());
}
```

### 2. 获取待处理文件

```java
// DocumentManagementController.java
@GetMapping("/pending")
public PendingFilesResponse getPendingFiles() {
    // 扫描 data/documents 目录
    Files.walk(watchDir)
        .filter(Files::isRegularFile)
        .forEach(filePath -> {
            // 检查是否正在处理
            boolean isProcessing = fileWatcherService.isFileProcessing(fileName);
            
            fileInfo.setProcessing(isProcessing);
            fileInfo.setCancelable(!isProcessing);  // 未处理的可以取消
        });
}
```

### 3. 取消索引

```java
// DocumentManagementController.java
@DeleteMapping("/pending/{fileName:.+}")
public Map<String, Object> cancelPendingFile(@PathVariable String fileName) {
    // 检查是否正在处理
    if (fileWatcherService.isFileProcessing(fileName)) {
        return error("文件正在处理中，无法取消");
    }
    
    // 删除文件
    Files.delete(filePath);
    return success("文件已删除");
}
```

## ✅ 功能验证

### 测试步骤

1. **启动应用**
   ```bash
   cd omni-agent-p2p-basic
   mvn spring-boot:run
   ```

2. **上传文件**
   ```bash
   curl -X POST http://localhost:8080/api/documents/upload -F "file=@test.pdf"
   ```

3. **立即查看待处理文件**
   ```bash
   curl http://localhost:8080/api/documents/pending
   ```

4. **等待 5 秒后再次查看**（应该显示 processing=true）

5. **尝试取消**
   ```bash
   curl -X DELETE http://localhost:8080/api/documents/pending/test.pdf
   ```

6. **如果文件还未开始处理** → 删除成功
   **如果文件正在处理** → 返回"无法取消"

## 🎉 总结

**新增功能**：
1. ✅ `/api/documents/pending` - 获取待处理文件列表
2. ✅ `/api/documents/pending/{fileName}` - 取消文件索引
3. ✅ `FileWatcherService.isFileProcessing()` - 检查处理状态

**用户体验**：
- ✅ 上传后立即看到文件在 Pending 区域
- ✅ 实时看到处理状态（等待/处理中）
- ✅ 可以取消未开始处理的文件
- ✅ 正在处理的文件不允许取消（防止误删）
- ✅ 处理完成后自动从 Pending 区域消失

**前端实现简单**：只需轮询 `/api/documents/pending` API 即可实时更新状态！🚀

