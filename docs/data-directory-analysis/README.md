# data 目录结构说明

## 📋 目录概述

`data/` 目录是 OmniAgent 系统的**数据根目录**，存储了系统运行时产生的所有数据，包括文档、索引、配置、数据库等。

**重要性**: ⭐⭐⭐⭐⭐ 核心数据目录，请务必定期备份！

---

## 📂 目录结构总览

```
data/
├── config/              ← 系统配置文件
├── documents/           ← 文档中转站（临时存储）
├── omni-agent.db        ← SQLite 主数据库
├── rag-index/           ← RAG 全文检索索引（Lucene）
├── storage/             ← 业务数据存储（文档、图片、分块等）
└── workflows/           ← 工作流定义和数据
```

---

## 📁 目录详解

### 1. config/ - 系统配置目录

```
data/config/
```

**用途**: 存储系统的运行时配置文件

**包含内容**:
- 系统配置参数
- RAG 配置
- 分块策略配置
- 提取模型配置

**数据类型**: JSON/YAML 配置文件

**大小**: 通常 < 1MB

**备份建议**: ⭐⭐⭐⭐⭐ 必须备份

**示例**:
```
config/
├── system-rag-config.json     ← RAG 系统配置
├── chunking-strategies.json   ← 分块策略配置
└── extraction-models.json     ← 文本提取模型配置
```

**何时生成**:
- 首次启动应用时自动创建
- 用户修改系统配置时更新

**可以删除吗**: ❌ 不建议，删除后会使用默认配置

---

### 2. documents/ - 文档中转站

```
data/documents/
```

**用途**: 临时存储用户上传的原始文档

**工作流程**:
```
用户上传文档
  ↓
保存到 documents/ (中转站)
  ↓
处理后移动到 storage/documents/
  ↓
可选择性清理中转站
```

**包含内容**:
- 用户刚上传的文档
- 待处理的文档队列
- 临时文件

**数据类型**: PDF, PPT, Word, Excel, TXT 等各种文档

**大小**: 取决于上传的文档数量和大小

**备份建议**: ⭐⭐ 可选备份（临时数据）

**何时生成**: 用户上传文档时

**可以删除吗**: ⚠️ 可以，但请确保文档已处理完成并移动到 storage

**清理建议**:
```bash
# 删除7天前的临时文件
find data/documents/ -mtime +7 -delete
```

---

### 3. omni-agent.db - SQLite 主数据库

```
data/omni-agent.db
```

**用途**: 系统的主数据库，存储结构化数据

**包含内容**:
- 问题分类配置
- 用户数据
- 系统元数据
- 工作流状态
- 文档元信息

**数据库类型**: SQLite（嵌入式关系型数据库）

**文件大小**: 通常 1MB - 100MB

**备份建议**: ⭐⭐⭐⭐⭐ 必须定期备份！

**表结构示例**:
```sql
-- 问题类型配置表
CREATE TABLE question_types (
    type_id TEXT PRIMARY KEY,
    name TEXT,
    description TEXT,
    created_at INTEGER
);

-- 关键词映射表
CREATE TABLE keywords (
    id INTEGER PRIMARY KEY,
    type_id TEXT,
    keyword TEXT
);

-- 文档元数据表
CREATE TABLE document_metadata (
    doc_id TEXT PRIMARY KEY,
    filename TEXT,
    upload_time INTEGER,
    file_size INTEGER,
    status TEXT
);
```

**何时生成**: 首次启动应用时自动创建

**可以删除吗**: ❌ 绝对不可以！删除会丢失所有配置和元数据

**备份方法**:
```bash
# 手动备份
cp data/omni-agent.db data/omni-agent.db.backup

# 或使用 SQLite 命令
sqlite3 data/omni-agent.db ".backup data/omni-agent.db.backup"
```

**查看内容**:
```bash
# 使用 SQLite 命令行
sqlite3 data/omni-agent.db

# 查看所有表
.tables

# 查看表结构
.schema question_types

# 查询数据
SELECT * FROM question_types;
```

---

### 4. rag-index/ - RAG 全文检索索引

```
data/rag-index/
├── segments_2y               ← 段元数据（最重要！）
├── write.lock                ← 写锁文件
├── _33_Lucene99_0.doc        ← 倒排索引（词→文档映射）
├── _33_Lucene99_0.pos        ← 词项位置信息
├── _33_Lucene99_0.tim        ← 词项索引树
├── _33_Lucene99_0.tip        ← 词项索引指针
├── _33.fdt                   ← 存储的文档内容
├── _33.fdx                   ← 字段索引
├── _33.fnm                   ← 字段名称映射
├── _33.si                    ← 段信息
├── _34.cfe                   ← 复合文件条目
├── _34.cfs                   ← 复合文件存储
└── ...                       ← 更多段文件
```

**用途**: Apache Lucene 全文检索索引，用于 RAG 系统的文档检索

**核心功能**:
```
RAG 检索流程:
用户提问
  ↓
在 rag-index 中搜索相关文档  ← 这里！
  ↓
返回 TOP-K 最相关的文档
  ↓
作为上下文传给 LLM
  ↓
LLM 生成答案
```

**工作原理**: 倒排索引
```
原始文档: "机器学习是人工智能的重要分支"
    ↓ 分词
["机器", "学习", "是", "人工智能", "重要", "分支"]
    ↓ 建立倒排索引
词项 "机器" → 文档ID列表: [doc-001, doc-005, ...]
词项 "学习" → 文档ID列表: [doc-001, doc-003, ...]
    ↓ 存储到文件
_33_Lucene99_0.doc (倒排索引文件)
_33.fdt (文档内容存储)
```

**文件大小**: 
- 典型: 1000个文档 ≈ 15MB
- 分布: 倒排索引(54%) + 存储文件(33%) + 元数据(13%)

**备份建议**: ⭐⭐⭐⭐ 推荐备份（可重建，但耗时）

**何时生成**: 
- 应用启动时创建索引目录
- 索引文档时生成段文件
- 后台自动合并段

**可以删除吗**: ⚠️ 可以删除，但会丢失所有检索索引，需要重建
```bash
# 删除索引
rm -rf data/rag-index/*

# 重启应用后调用重建接口
curl -X POST http://localhost:3000/api/rag/rebuild-index
```

**性能指标**:
```
索引单个文档:  ~5ms
文本搜索:      ~2ms (返回TOP-10)
段合并:        后台异步（不影响查询）
```

**详细说明**: 参见 [RAG_INDEX_DIRECTORY_EXPLAINED.md](./RAG_INDEX_DIRECTORY_EXPLAINED.md)

---

### 5. storage/ - 业务数据存储目录

```
data/storage/
├── chunks/              ← 文档分块数据
├── documents/           ← 原始文档永久存储
├── extracted/           ← 提取的文本内容
├── images/              ← 文档中的图片
├── optimization/        ← RAG 优化数据
└── ppl/                 ← PPL 分析数据（已废弃）
```

**用途**: 存储所有业务相关的数据和内容

**虚拟目录机制**: 使用 DocumentStorageService 统一管理，支持多种后端：
- File（文件系统）← 当前使用
- MongoDB
- Redis
- S3/MinIO
- Elasticsearch

---

#### 5.1 storage/chunks/ - 文档分块

```
storage/chunks/
├── 倡导节约用水PPT作品下载——.pptx/
│   ├── chunk_001.json    ← 分块1的数据
│   ├── chunk_002.json    ← 分块2的数据
│   └── ...
├── 海洋环境保护宣传PPT模板——.pptx/
│   └── ...
└── ...
```

**用途**: 存储文档的智能分块结果

**分块策略**:
- 按段落分块
- 按语义分块
- 按固定大小分块
- 滑动窗口分块

**JSON 格式示例**:
```json
{
  "chunkId": "chunk_001",
  "documentId": "doc_123",
  "content": "这是第一个分块的内容...",
  "startIndex": 0,
  "endIndex": 512,
  "metadata": {
    "pageNumber": 1,
    "chunkStrategy": "semantic"
  },
  "embedding": [0.1, 0.2, 0.3, ...]
}
```

**大小**: 每个分块通常 1-5KB

**备份建议**: ⭐⭐⭐ 建议备份（可重新分块，但耗时）

**何时生成**: 文档智能分块时

**可以删除吗**: ⚠️ 可以，但会丢失分块结果，需要重新分块

---

#### 5.2 storage/documents/ - 原始文档

```
storage/documents/
├── 倡导节约用水PPT作品下载——.pptx      (16MB)
├── 海洋环境保护宣传PPT模板——.pptx      (2.2MB)
├── 环境保护公益宣传PPT模板——.ppt       (2.7MB)
├── 简约简洁竞聘求职简历自我介绍.pptx    (26MB)
└── ...
```

**用途**: 永久存储用户上传的原始文档

**文件类型**:
- Office 文档: .pptx, .ppt, .docx, .doc, .xlsx, .xls
- PDF 文档: .pdf
- 文本文档: .txt, .md
- 其他: 根据配置支持

**大小**: 取决于文档数量，单个文档 1MB-100MB

**备份建议**: ⭐⭐⭐⭐⭐ 必须备份！（用户的原始数据）

**何时生成**: 用户上传并处理完成后移动到此处

**可以删除吗**: ❌ 除非确定不再需要，否则不要删除

**与 documents/ 的区别**:
```
documents/     ← 临时中转站（处理中）
storage/documents/  ← 永久存储（已处理完成）
```

---

#### 5.3 storage/extracted/ - 提取的文本

```
storage/extracted/
└── (提取结果的JSON文件)
```

**用途**: 存储文档的文本提取结果

**包含内容**:
- 提取的纯文本
- 提取元数据（状态、耗时等）
- 提取使用的模型信息

**JSON 格式示例**:
```json
{
  "documentId": "doc_123",
  "extractedText": "完整的提取文本...",
  "extractionModel": "vision-llm",
  "status": "COMPLETED",
  "completedTime": 1703404800000,
  "duration": 25000
}
```

**大小**: 每个文档的提取结果 10KB-10MB

**备份建议**: ⭐⭐⭐ 建议备份（可重新提取，但耗时）

**何时生成**: 文档文本提取时

**可以删除吗**: ⚠️ 可以，但会丢失提取结果，需要重新提取

---

#### 5.4 storage/images/ - 文档图片

```
storage/images/
├── 倡导节约用水PPT作品下载——.pptx/
│   ├── image_001.png
│   ├── image_002.jpg
│   └── ...
├── 海洋环境保护宣传PPT模板——.pptx/
│   └── ...
└── doc_1766224609148_清新矢量绿色环保PPT模板——.pptx/
    └── ...
```

**用途**: 存储从文档中提取的图片

**图片来源**:
- PPT 幻灯片中的图片
- Word 文档中的图片
- PDF 中的图片
- Excel 中的图表

**格式**: PNG, JPG, GIF 等

**元数据**: 包含图片位置、页码、尺寸等信息

**大小**: 单张图片 10KB-5MB

**备份建议**: ⭐⭐⭐ 建议备份（重新提取会耗时）

**何时生成**: 文档处理时提取图片

**可以删除吗**: ⚠️ 可以，但会丢失图片数据

**Vision LLM 使用**: 这些图片会被 Vision LLM 分析以提取更丰富的信息

---

#### 5.5 storage/optimization/ - RAG 优化数据

```
storage/optimization/
└── (RAG 优化分析结果)
```

**用途**: 存储 RAG 优化算法的分析结果

**包含内容**:
- PPL (Perplexity) 分析
- HyDE (Hypothetical Document Embeddings)
- Rerank 结果
- Query Expansion 数据

**JSON 格式示例**:
```json
{
  "documentId": "doc_123",
  "optimizationType": "ppl",
  "analysisResult": {
    "perplexity": 2.5,
    "confidence": 0.85
  },
  "createdAt": 1703404800000
}
```

**大小**: 每个分析结果 1-10KB

**备份建议**: ⭐⭐ 可选备份（可重新计算）

**何时生成**: RAG 优化分析时

**可以删除吗**: ✅ 可以安全删除，会自动重新计算

---

#### 5.6 storage/ppl/ - PPL 数据（已废弃）

```
storage/ppl/
```

**用途**: 旧版 PPL 分析数据存储（已废弃）

**状态**: ⚠️ 已废弃，推荐使用 `storage/optimization/`

**可以删除吗**: ✅ 可以安全删除

**迁移建议**:
```bash
# 清理旧的 PPL 数据
rm -rf data/storage/ppl/*
```

---

### 6. workflows/ - 工作流目录

```
data/workflows/
├── definitions/          ← 工作流定义文件
│   └── example/
└── workflows.db          ← 工作流数据库
```

**用途**: 存储工作流引擎的定义和运行时数据

---

#### 6.1 workflows/definitions/ - 工作流定义

```
workflows/definitions/
└── example/
    ├── workflow.json     ← 工作流定义
    └── config.json       ← 工作流配置
```

**用途**: 存储用户自定义的工作流定义

**工作流定义示例**:
```json
{
  "workflowId": "doc-processing",
  "name": "文档处理工作流",
  "steps": [
    {
      "id": "extract",
      "type": "text-extraction",
      "config": {
        "model": "vision-llm"
      }
    },
    {
      "id": "chunk",
      "type": "chunking",
      "config": {
        "strategy": "semantic"
      }
    },
    {
      "id": "index",
      "type": "indexing"
    }
  ]
}
```

**大小**: 每个工作流定义 1-10KB

**备份建议**: ⭐⭐⭐⭐ 推荐备份（用户自定义逻辑）

**何时生成**: 用户创建工作流时

**可以删除吗**: ⚠️ 删除会丢失工作流定义

---

#### 6.2 workflows/workflows.db - 工作流数据库

```
data/workflows/workflows.db
```

**用途**: 存储工作流的运行时状态和历史

**包含内容**:
- 工作流实例状态
- 执行历史
- 任务队列
- 错误日志

**文件大小**: 通常 10KB-10MB

**备份建议**: ⭐⭐⭐ 建议备份

**何时生成**: 工作流引擎启动时

**可以删除吗**: ⚠️ 删除会丢失工作流运行历史

---

## 📊 目录大小估算

### 典型场景

**场景1: 小型部署（10个文档）**
```
data/
├── config/              ~100KB
├── documents/           ~0MB (已处理完成)
├── omni-agent.db        ~1MB
├── rag-index/           ~5MB
├── storage/
│   ├── chunks/          ~500KB
│   ├── documents/       ~50MB
│   ├── extracted/       ~1MB
│   ├── images/          ~10MB
│   └── optimization/    ~100KB
└── workflows/           ~500KB

总计: ~68MB
```

**场景2: 中型部署（100个文档）**
```
data/
├── config/              ~500KB
├── documents/           ~0MB
├── omni-agent.db        ~10MB
├── rag-index/           ~50MB
├── storage/
│   ├── chunks/          ~5MB
│   ├── documents/       ~500MB
│   ├── extracted/       ~10MB
│   ├── images/          ~100MB
│   └── optimization/    ~1MB
└── workflows/           ~5MB

总计: ~682MB
```

**场景3: 大型部署（1000个文档）**
```
data/
├── config/              ~1MB
├── documents/           ~0MB
├── omni-agent.db        ~100MB
├── rag-index/           ~500MB
├── storage/
│   ├── chunks/          ~50MB
│   ├── documents/       ~5GB
│   ├── extracted/       ~100MB
│   ├── images/          ~1GB
│   └── optimization/    ~10MB
└── workflows/           ~50MB

总计: ~6.8GB
```

---

## 🔧 维护建议

### 1. 备份策略

**必须备份** ⭐⭐⭐⭐⭐:
- `omni-agent.db` - 主数据库
- `storage/documents/` - 原始文档
- `config/` - 系统配置

**推荐备份** ⭐⭐⭐⭐:
- `rag-index/` - 全文索引（可重建，但耗时）
- `storage/chunks/` - 分块数据（可重新分块）
- `workflows/definitions/` - 工作流定义

**可选备份** ⭐⭐:
- `storage/extracted/` - 提取结果（可重新提取）
- `storage/images/` - 图片（可重新提取）
- `storage/optimization/` - 优化数据（可重新计算）

**无需备份**:
- `documents/` - 临时中转站
- `storage/ppl/` - 已废弃

### 2. 备份脚本

```bash
#!/bin/bash
# backup-data.sh

BACKUP_DIR="./backups/$(date +%Y%m%d_%H%M%S)"
mkdir -p "$BACKUP_DIR"

# 备份数据库
cp data/omni-agent.db "$BACKUP_DIR/"

# 备份配置
cp -r data/config "$BACKUP_DIR/"

# 备份原始文档
cp -r data/storage/documents "$BACKUP_DIR/"

# 可选：备份索引（较大）
# tar -czf "$BACKUP_DIR/rag-index.tar.gz" data/rag-index/

echo "备份完成: $BACKUP_DIR"
```

### 3. 清理策略

**定期清理**:
```bash
# 清理临时文档（7天前）
find data/documents/ -mtime +7 -delete

# 清理旧的 PPL 数据（已废弃）
rm -rf data/storage/ppl/*

# 优化 SQLite 数据库
sqlite3 data/omni-agent.db "VACUUM;"

# 优化 Lucene 索引
curl -X POST http://localhost:3000/api/rag/optimize-index
```

### 4. 磁盘空间监控

```bash
# 检查各目录大小
du -sh data/*

# 找出最大的文件
find data/ -type f -size +10M -ls

# 磁盘使用情况
df -h data/
```

---

## 🚨 故障排查

### 问题1: 索引损坏

**症状**:
```
org.apache.lucene.index.CorruptIndexException: checksum failed
```

**解决**:
```bash
# 1. 备份现有索引（可选）
mv data/rag-index data/rag-index.backup

# 2. 删除损坏的索引
rm -rf data/rag-index/*

# 3. 重启应用，自动创建新索引

# 4. 重建索引
curl -X POST http://localhost:3000/api/rag/rebuild-index
```

---

### 问题2: 数据库锁定

**症状**:
```
database is locked
```

**解决**:
```bash
# 1. 停止应用

# 2. 检查是否有其他进程占用
lsof data/omni-agent.db

# 3. 重启应用
```

---

### 问题3: 磁盘空间不足

**症状**:
```
No space left on device
```

**解决**:
```bash
# 1. 检查空间使用
du -sh data/*

# 2. 清理临时文件
rm -rf data/documents/*

# 3. 清理旧的备份
rm -rf backups/*

# 4. 考虑归档旧文档
```

---

### 问题4: 锁文件残留

**症状**:
```
Lock held by another program: write.lock
```

**解决**:
```bash
# 删除 Lucene 锁文件
rm data/rag-index/write.lock

# 重启应用
```

---

## 📝 数据迁移

### 从文件存储迁移到 MongoDB

```bash
# 1. 备份当前数据
tar -czf data-backup.tar.gz data/

# 2. 修改配置
# application.yml:
# spring.profiles.include: storage-mongodb

# 3. 配置 MongoDB 连接
# spring.data.mongodb.uri: mongodb://localhost:27017/omni-agent

# 4. 启动应用（自动迁移）

# 5. 验证数据迁移成功

# 6. 清理旧数据（可选）
# rm -rf data/storage/
```

---

## 🔒 安全建议

### 1. 权限设置

```bash
# 设置目录权限（仅所有者可读写）
chmod 700 data/

# 设置数据库文件权限
chmod 600 data/omni-agent.db

# 设置配置文件权限
chmod 600 data/config/*
```

### 2. 加密建议

**敏感数据加密**:
- 使用磁盘加密（如 LUKS, BitLocker）
- 数据库加密（SQLite Encryption Extension）
- 文档存储加密（应用层加密）

### 3. 访问控制

```yaml
# 限制数据目录访问
omni-agent:
  security:
    data-directory-access: restricted
    allowed-ips:
      - 127.0.0.1
      - 192.168.1.0/24
```

---

## 📖 相关文档

- [RAG 索引详解](./RAG_INDEX_DIRECTORY_EXPLAINED.md)
- [RAG 索引流程图](./RAG_INDEX_FLOW_DIAGRAM.md)
- [文档提取结果持久化](./EXTRACTION_PERSISTENCE_IMPLEMENTATION.md)
- [虚拟目录方案](./EXTRACTION_VIRTUAL_DIRECTORY.md)

---

## ❓ 常见问题

### Q1: data 目录可以移动到其他位置吗？

**A**: 可以！通过配置参数指定：
```yaml
omni-agent:
  data-dir: /path/to/custom/data
```

---

### Q2: 如何查看数据库内容？

**A**: 使用 SQLite 命令行：
```bash
sqlite3 data/omni-agent.db
.tables
SELECT * FROM question_types;
```

---

### Q3: 索引文件可以手动编辑吗？

**A**: ❌ 不可以！Lucene 索引是二进制格式，手动编辑会损坏。请使用 API 操作。

---

### Q4: 如何估算需要多少磁盘空间？

**A**: 经验公式：
```
总空间 = 原始文档大小 × 1.5 + 文档数量 × 50KB
```
示例：
- 100个文档，平均5MB
- 空间需求 ≈ 500MB × 1.5 + 100 × 50KB ≈ 755MB

---

### Q5: 数据库会无限增长吗？

**A**: 不会。SQLite 数据库会定期 VACUUM 优化，释放未使用空间。

---

## 📊 监控指标

### 推荐监控的指标

1. **磁盘使用率**
   ```bash
   df -h data/
   ```

2. **索引文档数**
   ```bash
   curl http://localhost:3000/api/rag/statistics
   ```

3. **数据库大小**
   ```bash
   ls -lh data/omni-agent.db
   ```

4. **存储文档数**
   ```bash
   find data/storage/documents/ -type f | wc -l
   ```

---

## 🎓 最佳实践

1. **✅ 定期备份** - 至少每周备份一次关键数据
2. **✅ 监控磁盘空间** - 保持至少 20% 的空闲空间
3. **✅ 定期清理** - 清理临时文件和旧数据
4. **✅ 优化索引** - 每月优化一次 Lucene 索引
5. **✅ 验证备份** - 定期验证备份的完整性
6. **✅ 文档归档** - 将旧文档归档到冷存储
7. **✅ 权限控制** - 限制 data 目录的访问权限

---

生成时间: 2025-12-24  
版本: 1.0.0  
作者: OmniAgent Team  
状态: ✅ 完整文档

