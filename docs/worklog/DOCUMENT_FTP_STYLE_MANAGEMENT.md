# 📁 文档库FTP风格管理实现

**实现时间**: 2025-12-19  
**核心理念**: data/storage/documents 就是文档库，像FTP一样管理  
**状态**: ✅ 已完成

---

## 🎯 核心改进

### 问题1: 文档存储路径重复

**之前**:
```
data/storage/documents/
└── 倡导节约用水PPT作品下载——.pptx/
    └── 倡导节约用水PPT作品下载——.pptx  ❌ 重复！
```

**现在**:
```
data/storage/documents/
└── 倡导节约用水PPT作品下载——.pptx  ✅ 直接存储！
```

### 问题2: 文档管理应该像FTP

**之前**: 用户看到技术细节（分块、索引等）  
**现在**: 用户只看到文档库，像FTP一样管理文件和文件夹

---

## 📂 最终目录结构

```
data/storage/
├── documents/                          ⭐ 用户可见的文档库（FTP风格）
│   ├── 倡导节约用水.pptx              ✅ 直接存储
│   ├── 技术文档.pdf
│   ├── 设计图/
│   │   ├── 架构图.pptx
│   │   └── 流程图.vsdx
│   └── 代码/
│       └── README.md
│
├── chunks/                             🔒 系统内部（用户不可见）
│   ├── 倡导节约用水.pptx/
│   │   ├── chunk_000.chunk
│   │   └── chunk_001.chunk
│   └── ...
│
├── images/                             🔒 系统内部（用户不可见）
│   └── 倡导节约用水.pptx/
│       ├── page_001_img.png
│       └── page_002_img.png
│
└── ppl/                                🔒 系统内部（用户不可见）
    └── 倡导节约用水.pptx/
        └── ppl.data
```

---

## 🔑 核心实现

### 1. 文档直接存储（不创建子目录）

```java
@Override
public String saveDocument(String documentId, String filename, byte[] fileData) {
    // 使用原文件名直接保存（保留相对路径中的目录结构）
    // 例如: filename = "设计图/架构图.pptx"
    //      保存为: documents/设计图/架构图.pptx
    Path documentFile = documentsPath.resolve(filename);
    
    // 确保父目录存在
    Path parentDir = documentFile.getParent();
    if (parentDir != null) {
        Files.createDirectories(parentDir);
    }

    Files.write(documentFile, fileData);
    return documentId;
}
```

### 2. RAG索引中记录文件路径

```java
// 在 metadata 中记录 storagePath
.metadata(Map.of(
        "fileName", filename,
        "storagePath", relativePathStr,            // ⭐ 存储路径（用于下载）
        "documentId", documentId,
        "chunkIndex", chunk.getSequence()
))
```

### 3. FTP风格的文档浏览API

**新增接口**: `DocumentBrowseController`

#### 列出文件和文件夹
```bash
GET /api/documents/browse/list?path=设计图
```

**返回**:
```json
{
  "success": true,
  "path": "设计图",
  "items": [
    {
      "name": "架构图.pptx",
      "type": "file",
      "path": "设计图/架构图.pptx",
      "size": 1234567,
      "modified": 1734619200000
    },
    {
      "name": "流程图",
      "type": "directory",
      "path": "设计图/流程图"
    }
  ]
}
```

#### 下载文件
```bash
GET /api/documents/browse/download?path=设计图/架构图.pptx
```

#### 删除文件或文件夹
```bash
DELETE /api/documents/browse/delete?path=设计图/架构图.pptx
```

#### 创建文件夹
```bash
POST /api/documents/browse/mkdir?path=设计图/新文件夹
```

#### 统计信息
```bash
GET /api/documents/browse/stats
```

**返回**:
```json
{
  "success": true,
  "totalFiles": 42,
  "totalFolders": 8,
  "totalSize": 1234567890,
  "totalSizeFormatted": "1.15 GB"
}
```

---

## 🎨 前端集成示例

### Vue.js 组件示例

```vue
<template>
  <div class="document-browser">
    <!-- 面包屑导航 -->
    <div class="breadcrumb">
      <span @click="navigateTo('')">根目录</span>
      <span v-for="(part, idx) in pathParts" :key="idx">
        / <span @click="navigateTo(getPathUntil(idx))">{{ part }}</span>
      </span>
    </div>

    <!-- 文件列表 -->
    <table class="file-list">
      <thead>
        <tr>
          <th>名称</th>
          <th>类型</th>
          <th>大小</th>
          <th>修改时间</th>
          <th>操作</th>
        </tr>
      </thead>
      <tbody>
        <tr v-for="item in items" :key="item.path">
          <td>
            <i :class="item.type === 'directory' ? 'icon-folder' : 'icon-file'"></i>
            <span @click="handleClick(item)">{{ item.name }}</span>
          </td>
          <td>{{ item.type === 'directory' ? '文件夹' : '文件' }}</td>
          <td>{{ formatSize(item.size) }}</td>
          <td>{{ formatDate(item.modified) }}</td>
          <td>
            <button @click="download(item)" v-if="item.type === 'file'">下载</button>
            <button @click="deleteItem(item)">删除</button>
          </td>
        </tr>
      </tbody>
    </table>
  </div>
</template>

<script>
export default {
  data() {
    return {
      currentPath: '',
      items: []
    };
  },
  computed: {
    pathParts() {
      return this.currentPath ? this.currentPath.split('/') : [];
    }
  },
  methods: {
    async loadFiles(path = '') {
      const res = await fetch(`/api/documents/browse/list?path=${encodeURIComponent(path)}`);
      const data = await res.json();
      this.items = data.items;
      this.currentPath = path;
    },
    handleClick(item) {
      if (item.type === 'directory') {
        this.loadFiles(item.path);
      } else {
        this.download(item);
      }
    },
    download(item) {
      window.open(`/api/documents/browse/download?path=${encodeURIComponent(item.path)}`);
    },
    async deleteItem(item) {
      if (confirm(`确定删除 ${item.name}？`)) {
        await fetch(`/api/documents/browse/delete?path=${encodeURIComponent(item.path)}`, {
          method: 'DELETE'
        });
        this.loadFiles(this.currentPath);
      }
    },
    navigateTo(path) {
      this.loadFiles(path);
    },
    getPathUntil(idx) {
      return this.pathParts.slice(0, idx + 1).join('/');
    },
    formatSize(size) {
      if (!size) return '-';
      if (size < 1024) return size + ' B';
      if (size < 1024 * 1024) return (size / 1024).toFixed(2) + ' KB';
      if (size < 1024 * 1024 * 1024) return (size / (1024 * 1024)).toFixed(2) + ' MB';
      return (size / (1024 * 1024 * 1024)).toFixed(2) + ' GB';
    },
    formatDate(timestamp) {
      if (!timestamp) return '-';
      return new Date(timestamp).toLocaleString();
    }
  },
  mounted() {
    this.loadFiles();
  }
};
</script>
```

---

## 📊 使用场景

### 场景1: 用户浏览文档库

1. 访问文档管理页面
2. 看到文档库的文件夹和文件（像FTP一样）
3. 点击文件夹进入子目录
4. 点击文件下载

### 场景2: 用户删除文档

1. 在文档库中找到要删除的文件
2. 点击删除按钮
3. 系统：
   - 删除 `documents/` 中的原文件
   - 自动清理对应的 `chunks/`、`images/`、`ppl/`
   - 从 RAG 索引中移除

### 场景3: 用户组织文档

1. 创建文件夹 "2024年度报告"
2. 将相关文档移动到该文件夹
3. 系统自动更新存储路径
4. RAG 索引中的 `storagePath` 自动更新

### 场景4: 用户上传带目录的文档

**监听目录**:
```
data/watch/
└── 项目文档/
    ├── 需求文档.docx
    └── 设计文档.pdf
```

**归档后**:
```
data/storage/documents/
└── 项目文档/
    ├── 需求文档.docx  ✅ 保留目录结构
    └── 设计文档.pdf
```

**用户看到**: 在文档库中看到 "项目文档" 文件夹

---

## 🔒 安全性

### 路径遍历保护

```java
Path fullPath = Paths.get(DOCUMENT_ROOT, path).normalize();

// 安全检查：防止路径遍历攻击
if (!fullPath.startsWith(Paths.get(DOCUMENT_ROOT).normalize())) {
    return ResponseEntity.badRequest().body(Map.of(
            "success", false,
            "message", "非法路径"
    ));
}
```

**防止攻击**:
- `path=../../etc/passwd` ❌ 被阻止
- `path=设计图/架构图.pptx` ✅ 允许

---

## 🎉 核心优势

### 1. 用户友好

- 📁 **像FTP一样**: 用户看到的就是文件和文件夹
- 🔍 **直观管理**: 创建、删除、下载文件夹和文件
- 📂 **保留结构**: 支持多级目录

### 2. 系统透明

- 🔒 **隐藏技术细节**: chunks、images、ppl 对用户不可见
- 🔗 **自动关联**: 通过 storagePath 自动关联
- 🗑️ **级联删除**: 删除文档时自动清理相关数据

### 3. 扩展性好

- 📋 **标准API**: RESTful 接口，易于集成
- 🔌 **前后端分离**: 可以使用任何前端框架
- 📊 **统计信息**: 支持查询文档库统计

---

## 📝 API 完整列表

| 接口 | 方法 | 功能 |
|------|------|------|
| `/api/documents/browse/list` | GET | 列出文件和文件夹 |
| `/api/documents/browse/download` | GET | 下载文件 |
| `/api/documents/browse/delete` | DELETE | 删除文件或文件夹 |
| `/api/documents/browse/mkdir` | POST | 创建文件夹 |
| `/api/documents/browse/stats` | GET | 获取统计信息 |

---

## ✅ 最终效果

### 用户视角

```
文档管理
├── 📁 2024年度报告
│   ├── 📄 Q1报告.pdf
│   └── 📄 Q2报告.pdf
├── 📁 设计图
│   ├── 📄 架构图.pptx
│   └── 📄 流程图.vsdx
└── 📄 README.md
```

**操作**:
- ✅ 点击文件夹进入
- ✅ 点击文件下载
- ✅ 删除文件或文件夹
- ✅ 创建新文件夹

### 系统视角

```
data/storage/
├── documents/          ← 用户可见（FTP风格）
│   ├── 2024年度报告/
│   ├── 设计图/
│   └── README.md
├── chunks/             ← 系统内部
├── images/             ← 系统内部
└── ppl/                ← 系统内部
```

**RAG索引包含**:
```json
{
  "id": "chunk_001",
  "content": "...",
  "metadata": {
    "fileName": "架构图.pptx",
    "storagePath": "设计图/架构图.pptx",  // ⭐ 用于下载
    "documentId": "doc_xxx",
    "chunkIndex": 0
  }
}
```

---

**完成时间**: 2025-12-19  
**编译状态**: ✅ BUILD SUCCESS  
**核心理念**: 文档库就是 FTP，用户不需要关心技术细节

🎉 **FTP风格文档管理实现完成！用户可以像管理FTP一样管理文档库！** 📁✨

