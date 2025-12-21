# 📁 文件监听和自动索引功能

**版本**: v3.0.0  
**日期**: 2025-12-17

---

## 🎯 功能概述

自动监听 `data/documents/` 目录的文件变化，记录变化并支持手动/自动重新索引。

### 核心特性

1. ✅ **实时监听** - 监听文件的新增、修改、删除
2. ✅ **持久化配置** - 配置保存在 `data/config/file-watcher-config.json`
3. ✅ **自动/手动模式** - 可选择自动索引或手动确认
4. ✅ **变化记录** - 记录所有文件变化，供后续处理
5. ✅ **REST API** - 完整的 API 管理接口
6. ✅ **启动时加载** - 重启后自动恢复配置

---

## 📋 配置文件

配置文件位置: `data/config/file-watcher-config.json`

```json
{
  "enabled": true,           // 是否启用文件监听
  "auto_index": false,       // 是否自动重新索引（默认false，需手动确认）
  "watch_directory": "./data/documents",
  "last_updated": 1734441600000,
  "version": "1.0"
}
```

### 配置说明

| 字段 | 类型 | 说明 | 默认值 |
|------|------|------|--------|
| `enabled` | Boolean | 是否启用文件监听 | `true` |
| `auto_index` | Boolean | 文件变化时是否自动重新索引 | `false` |
| `watch_directory` | String | 监听的目录路径 | `./data/documents` |
| `last_updated` | Long | 配置最后更新时间戳 | 当前时间 |
| `version` | String | 配置版本号 | `1.0` |

---

## 🚀 使用流程

### 方案1：手动模式（推荐）⭐

```
1. 文件变化 → 2. 记录变化 → 3. 用户在UI查看 → 4. 用户选择重新索引
```

**优点**: 用户可以确认后再索引，避免误操作

### 方案2：自动模式

```
1. 文件变化 → 2. 自动重新索引
```

**优点**: 无需手动干预，自动保持同步

---

## 🔌 API 接口

### 1. 获取当前配置

```http
GET /api/file-watcher/config
```

**响应示例**:
```json
{
  "success": true,
  "config": {
    "enabled": true,
    "auto_index": false,
    "watch_directory": "./data/documents",
    "last_updated": 1734441600000,
    "version": "1.0"
  }
}
```

### 2. 更新配置

```http
PUT /api/file-watcher/config
Content-Type: application/json

{
  "enabled": true,
  "autoIndex": false
}
```

**响应示例**:
```json
{
  "success": true,
  "message": "配置更新成功",
  "config": {
    "enabled": true,
    "auto_index": false,
    ...
  }
}
```

### 3. 获取未处理的文件变化

```http
GET /api/file-watcher/changes/unprocessed
```

**响应示例**:
```json
{
  "success": true,
  "total": 3,
  "changes": [
    {
      "id": "uuid-1",
      "filePath": "./data/documents/doc_123_test.pdf",
      "fileName": "doc_123_test.pdf",
      "documentId": "doc_123",
      "changeType": "MODIFY",
      "fileSize": 1024000,
      "changedAt": 1734441600000,
      "processed": false
    },
    {
      "id": "uuid-2",
      "fileName": "doc_456_report.docx",
      "changeType": "CREATE",
      "changedAt": 1734441500000,
      "processed": false
    }
  ]
}
```

### 4. 获取所有文件变化

```http
GET /api/file-watcher/changes
```

返回所有变化记录（包括已处理和未处理）。

### 5. 手动处理单个文件变化

```http
POST /api/file-watcher/changes/{recordId}/process
```

**响应示例**:
```json
{
  "success": true,
  "message": "处理成功",
  "recordId": "uuid-1"
}
```

### 6. 批量处理所有未处理的变化

```http
POST /api/file-watcher/changes/process-all
```

**响应示例**:
```json
{
  "success": true,
  "message": "批量处理完成: 成功 3 个",
  "successCount": 3,
  "totalCount": 3
}
```

### 7. 清除已处理的记录

```http
DELETE /api/file-watcher/changes/processed
```

**响应示例**:
```json
{
  "success": true,
  "message": "已清除 5 条记录",
  "clearedCount": 5
}
```

---

## 🎨 UI 集成建议

### 文件监听管理页面

```
┌──────────────────────────────────────┐
│  📁 文件监听管理                      │
├──────────────────────────────────────┤
│                                       │
│  ⚙️ 配置                              │
│  ┌────────────────────────────────┐ │
│  │ [✓] 启用文件监听               │ │
│  │ [ ] 自动重新索引（需手动确认） │ │
│  │ 监听目录: ./data/documents     │ │
│  │ [保存配置]                     │ │
│  └────────────────────────────────┘ │
│                                       │
│  📋 未处理的文件变化 (3)             │
│  ┌────────────────────────────────┐ │
│  │ ✏️ test.pdf (修改)              │ │
│  │    2025-12-17 10:30            │ │
│  │    大小: 1.0 MB                │ │
│  │    [重新索引] [忽略]           │ │
│  ├────────────────────────────────┤ │
│  │ 📄 report.docx (新增)          │ │
│  │    2025-12-17 10:25            │ │
│  │    大小: 500 KB                │ │
│  │    [索引] [忽略]               │ │
│  ├────────────────────────────────┤ │
│  │ 🗑️ old.txt (删除)              │ │
│  │    2025-12-17 10:20            │ │
│  │    [删除索引] [忽略]           │ │
│  └────────────────────────────────┘ │
│                                       │
│  [批量处理所有] [清除已处理记录]     │
└──────────────────────────────────────┘
```

---

## 🔄 工作流程

### 启动流程

```
1. 应用启动
   ↓
2. 加载配置文件 (data/config/file-watcher-config.json)
   ↓
3. 如果 enabled=true，启动文件监听
   ↓
4. 开始监听 data/documents/ 目录
```

### 文件变化处理流程

#### 自动模式 (auto_index=true)

```
1. 检测到文件变化
   ↓
2. 记录变化
   ↓
3. 自动重新索引
   ↓
4. 标记为已处理
```

#### 手动模式 (auto_index=false) ⭐

```
1. 检测到文件变化
   ↓
2. 记录变化
   ↓
3. 用户在UI查看未处理的变化
   ↓
4. 用户点击"重新索引"按钮
   ↓
5. 调用 API 处理
   ↓
6. 标记为已处理
```

---

## 📝 使用示例

### 示例1：启用自动索引

```bash
# 更新配置
curl -X PUT http://localhost:8080/api/file-watcher/config \
  -H "Content-Type: application/json" \
  -d '{"autoIndex": true}'

# 之后文件变化会自动重新索引
```

### 示例2：查看未处理的变化

```bash
# 获取未处理的变化
curl http://localhost:8080/api/file-watcher/changes/unprocessed

# 响应
{
  "success": true,
  "total": 2,
  "changes": [...]
}
```

### 示例3：手动处理文件变化

```bash
# 处理单个变化
curl -X POST http://localhost:8080/api/file-watcher/changes/uuid-123/process

# 或批量处理所有
curl -X POST http://localhost:8080/api/file-watcher/changes/process-all
```

---

## 🔐 文件变化检测机制

### 基于 MD5 哈希的内容检测 ⭐

系统使用 **MD5 哈希值**来准确判断文件内容是否真正改变，避免虚假的 MODIFY 事件。

#### 工作原理

```
1. 启动时扫描 data/documents/ 目录
   ↓
2. 计算每个文件的 MD5 哈希值并缓存
   ↓
3. 监听到 MODIFY 事件时
   ↓
4. 重新计算文件的 MD5
   ↓
5. 与缓存的旧哈希值对比
   ↓
6. 如果哈希值相同 → 忽略（内容未变）
   如果哈希值不同 → 记录变化（内容真正改变）
```

#### 优势

- ✅ **准确检测** - 只有内容真正改变才会触发
- ✅ **过滤虚假事件** - 忽略文件打开、保存临时文件等操作
- ✅ **MD5 快速** - 对大文件也能快速��算
- ✅ **哈希缓存** - 避免重复计算

#### 数据结构

每条文件变化记录包含：

```json
{
  "fileName": "test.pdf",
  "fileHash": "5d41402abc4b2a76b9719d911017c592",      // 新的 MD5
  "oldFileHash": "098f6bcd4621d373cade4e832627b4f6",   // 旧的 MD5
  "fileSize": 1024000,
  "fileModifiedTime": 1734441600000,
  "changeType": "MODIFY"
}
```

### 为什么选择 MD5？

| 算法 | 速度 | 安全性 | 碰撞概率 | 适用场景 |
|------|------|--------|----------|----------|
| **MD5** ⭐ | ⚡ 极快 | ⚠️ 中等 | 极低 | **文件变化检测**（我们的场景） |
| SHA256 | 🐢 较慢 | 🔒 很高 | 几乎为0 | 密码存储、数字签名 |

**结论**: MD5 在文件变化检测场景下是最佳选择，因为：
- 我们不需要密码学级别的安全性
- MD5 速度快，适合频繁计算
- 碰撞概率对文件监听来说足够低

---

## ⚠️ 注意事项

1. **临时文件过滤**: 自动忽略 `.`, `~`, `.tmp` 开头/结尾的文件
2. **内存存储**: 当前变化记录存储在内存中，重启后清空（可改为持久化）
3. **文件名格式**: 需要遵循 `{documentId}_{originalName}` 格式才能自动关联
4. **性能考虑**: 大量文件变化时，建议使用手动模式
5. **并发安全**: 使用 `ConcurrentHashMap` 保证线程安全
6. **哈希缓存**: 启动时会扫描现有文件并计算哈希值

---

## 🎯 推荐配置

### 开发环境

```json
{
  "enabled": true,
  "auto_index": true   // 开发时自动索引
}
```

### 生产环境

```json
{
  "enabled": true,
  "auto_index": false  // 生产环境手动确认
}
```

---

## 🔮 未来增强

- [ ] 变化记录持久化到数据库
- [ ] 支持文件内容变化检测（MD5/SHA256）
- [ ] 支持批量操作的撤销功能
- [ ] 支持文件变化的通知推送
- [ ] 支持更细粒度的文件过滤规则

---

**文档版本**: v1.0  
**最后更新**: 2025-12-17  
**维护团队**: OmniAgent Team

