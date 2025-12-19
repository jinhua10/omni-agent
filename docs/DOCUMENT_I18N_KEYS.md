# 📋 文档管理国际化键清单

**更新时间**: 2025-12-19  
**模块**: document  
**状态**: ✅ 完整

---

## 📊 完整键列表

### 1. 基础信息

| 键 | 中文 | 英文 |
|----|------|------|
| `document.title` | 文档管理 | Document Management |

### 2. 视图模式（viewMode）

| 键 | 中文 | 英文 |
|----|------|------|
| `document.viewMode.browser` | 浏览器视图 | Browser View |
| `document.viewMode.list` | 列表视图 | List View |
| `document.viewMode.card` | 卡片视图 | Card View |

### 3. FTP浏览器（browse）

#### 3.1 基础标签

| 键 | 中文 | 英文 |
|----|------|------|
| `document.browse.root` | 根目录 | Root |
| `document.browse.name` | 名称 | Name |
| `document.browse.type` | 类型 | Type |
| `document.browse.size` | 大小 | Size |
| `document.browse.modified` | 修改时间 | Modified |
| `document.browse.actions` | 操作 | Actions |

#### 3.2 类型标识

| 键 | 中文 | 英文 |
|----|------|------|
| `document.browse.folder` | 文件夹 | Folder |
| `document.browse.file` | 文件 | File |
| `document.browse.files` | 文件 | Files |
| `document.browse.folders` | 文件夹 | Folders |
| `document.browse.totalSize` | 总大小 | Total Size |

#### 3.3 操作按钮

| 键 | 中文 | 英文 |
|----|------|------|
| `document.browse.upload` | 上传文件 | Upload File |
| `document.browse.uploadTitle` | 上传文档 | Upload Document |
| `document.browse.createFolder` | 新建文件夹 | New Folder |
| `document.browse.createFolderTitle` | 创建文件夹 | Create Folder |
| `document.browse.download` | 下载 | Download |
| `document.browse.delete` | 删除 | Delete |
| `document.browse.viewDetail` | 查看详情 | View Details |
| `document.browse.aiChat` | AI交互 | AI Interaction |

#### 3.4 提示消息

| 键 | 中文 | 英文 |
|----|------|------|
| `document.browse.createFolderSuccess` | 文件夹创建成功 | Folder created successfully |
| `document.browse.createFolderFailed` | 文件夹创建失败 | Failed to create folder |
| `document.browse.folderNameRequired` | 请输入文件夹名称 | Please enter folder name |
| `document.browse.folderNamePlaceholder` | 请输入文件夹名称 | Enter folder name |
| `document.browse.downloadStarted` | 开始下载 | Download started |
| `document.browse.downloadFailed` | 下载失败 | Download failed |
| `document.browse.confirmDelete` | 确认删除 | Confirm Delete |
| `document.browse.deleteWarning` | 此操作不可恢复，确定要删除 | This action cannot be undone. Are you sure to delete |
| `document.browse.deleteSuccess` | 删除成功 | Deleted successfully |
| `document.browse.deleteFailed` | 删除失败 | Failed to delete |
| `document.browse.detailTitle` | 文档详情 | Document Details |
| `document.browse.loadFailed` | 加载失败 | Failed to load |
| `document.browse.emptyFolder` | 文件夹为空 | Folder is empty |

### 4. 文档操作

| 键 | 中文 | 英文 |
|----|------|------|
| `document.upload` | 上传文档 | Upload Document |
| `document.view` | 查看 | View |
| `document.delete` | 删除 | Delete |
| `document.download` | 下载 | Download |
| `document.preview` | 预览 | Preview |
| `document.detail` | 详情 | Detail |

### 5. 搜索相关

| 键 | 中文 | 英文 |
|----|------|------|
| `document.search` | 搜索 | Search |
| `document.keyword` | 关键词 | Keyword |
| `document.simpleSearch` | 简单搜索 | Simple Search |
| `document.advancedSearch` | 高级搜索 | Advanced Search |
| `document.searchPlaceholder` | 搜索文档名称、标签... | Search documents by name, tags... |

### 6. 状态消息

| 键 | 中文 | 英文 |
|----|------|------|
| `document.uploadSuccess` | 上传成功 | Upload successful |
| `document.uploadFailed` | 上传失败 | Upload failed |
| `document.deleteSuccess` | 删除成功 | Delete successful |
| `document.deleteFailed` | 删除失败 | Delete failed |
| `document.downloadSuccess` | 下载成功 | Download successful |
| `document.downloadFailed` | 下载失败 | Download failed |
| `document.loading` | 加载中... | Loading... |
| `document.loadFailed` | 加载失败 | Failed to load |
| `document.noDocuments` | 暂无文档 | No documents yet |

---

## 🎯 使用示例

### DocumentBrowser 组件

```javascript
// 面包屑
<HomeOutlined /> {t('document.browse.root')}

// 工具栏
<Button>{t('document.browse.upload')}</Button>
<Button>{t('document.browse.createFolder')}</Button>

// 统计信息
<Tag>{t('document.browse.files')}: {stats.totalFiles}</Tag>
<Tag>{t('document.browse.folders')}: {stats.totalFolders}</Tag>
<Tag>{t('document.browse.totalSize')}: {stats.totalSizeFormatted}</Tag>

// 表格列
<Table columns={[
  { title: t('document.browse.name') },
  { title: t('document.browse.type') },
  { title: t('document.browse.size') },
  { title: t('document.browse.modified') },
  { title: t('document.browse.actions') }
]} />

// 操作按钮
<Tooltip title={t('document.browse.download')}>
  <Button icon={<DownloadOutlined />} />
</Tooltip>

// 确认对话框
Modal.confirm({
  title: t('document.browse.confirmDelete'),
  content: `${t('document.browse.deleteWarning')}: ${item.name}`
})

// 成功提示
antdMessage.success(t('document.browse.deleteSuccess'))
```

### DocumentManagement 组件

```javascript
// 标题
<h2>{t('document.title')}</h2>

// 视图切换
<Segmented options={[
  { label: t('document.viewMode.browser'), value: 'browser' },
  { label: t('document.viewMode.list'), value: 'list' }
]} />
```

---

## ✅ 检查清单

### 完整性检查

- [x] 视图模式翻译（3个键）
- [x] FTP浏览器基础标签（6个键）
- [x] FTP浏览器类型标识（5个键）
- [x] FTP浏览器操作按钮（8个键）
- [x] FTP浏览器提示消息（13个键）
- [x] 文档操作（6个键）
- [x] 搜索相关（5个键）
- [x] 状态消息（9个键）

**总计**: 55个翻译键 ✅

### 一致性检查

- [x] 所有键在中英文文件中都存在
- [x] 键名命名规范统一
- [x] 翻译内容准确对应
- [x] 无重复或冲突的键

### 功能覆盖

- [x] 工具栏所有按钮
- [x] 面包屑导航
- [x] 表格所有列标题
- [x] 操作按钮所有 Tooltip
- [x] 确认对话框
- [x] 成功/失败提示
- [x] 空状态提示

---

## 🔍 验证方法

### 1. 静态检查

```bash
# 检查中文翻译
grep -A 40 "browse: {" UI/src/lang/zh.js

# 检查英文翻译
grep -A 40 "browse: {" UI/src/lang/en.js

# 检查键数量是否一致
```

### 2. 运行时检查

```javascript
// 在浏览器控制台测试
const testKeys = [
  'document.browse.root',
  'document.browse.folders',
  'document.browse.upload',
  'document.browse.createFolder',
  'document.browse.deleteSuccess'
];

testKeys.forEach(key => {
  console.log(`${key}: ${t(key)}`);
});
```

### 3. UI 检查

启动应用，检查以下界面元素：

- [ ] 页面标题显示正确
- [ ] 视图切换按钮显示正确
- [ ] 工具栏按钮文本显示正确
- [ ] 统计信息标签显示正确
- [ ] 面包屑导航显示正确
- [ ] 表格列标题显示正确
- [ ] 操作按钮 Tooltip 显示正确
- [ ] 对话框标题和内容显示正确
- [ ] 提示消息显示正确
- [ ] 切换语言后所有文本正确切换

---

## 📈 扩展建议

### 未来可能需要的键

```javascript
browse: {
  // 批量操作
  selectAll: '全选',
  deselectAll: '取消全选',
  selectedItems: '已选择 {count} 项',
  batchDelete: '批量删除',
  batchDownload: '批量下载',
  
  // 排序
  sortByName: '按名称排序',
  sortBySize: '按大小排序',
  sortByDate: '按日期排序',
  sortAscending: '升序',
  sortDescending: '降序',
  
  // 过滤
  filterByType: '按类型筛选',
  showOnlyFiles: '仅显示文件',
  showOnlyFolders: '仅显示文件夹',
  
  // 预览
  preview: '预览',
  previewNotSupported: '不支持预览此文件类型',
  
  // 拖拽上传
  dropFilesHere: '将文件拖到这里',
  uploading: '上传中...',
  uploadProgress: '上传进度: {percent}%',
}
```

---

**更新时间**: 2025-12-19  
**键总数**: 55个  
**完整性**: ✅ 100%

🎉 **文档管理模块国际化翻译完整！** 🌍✨

