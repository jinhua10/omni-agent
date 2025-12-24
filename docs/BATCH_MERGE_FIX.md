# 批次合并混乱问题修复

## 🐛 问题描述

**现象**：点击"合并批次"后，内容混乱：
```
"---  ## 📄 页面 1    ---  ## 📄 页面 5  ###### 幻 文字信息灯  片..."
```

**原因**：
1. 并行处理时，多个批次同时发送内容
2. 前端使用单个 `currentBatchIndex` 变量跟踪当前批次
3. 批次 1、3 并行时，`currentBatchIndex` 被覆盖
4. 导致批次 3 的内容被错误累加到批次 1

**示例**：
```
时间线：
T1: 批次 1 开始 → currentBatchIndex = 0
T2: 批次 3 开始 → currentBatchIndex = 2  （覆盖！）
T3: 批次 1 发送内容 → 累加到批次 2 ❌（错误！）
T4: 批次 3 发送内容 → 累加到批次 2 ✓
```

---

## ✅ 解决方案

### 核心思路
**在每条内容消息中包含批次索引，而不是依赖全局变量**

### 实现步骤

#### 1. 后端：使用 BATCH_CONTENT 协议

**格式**：`BATCH_CONTENT:批次索引:内容`

**示例**：
```
BATCH_CONTENT:0:## 📄 页面 1
BATCH_CONTENT:0:幻灯片内容...
BATCH_CONTENT:2:## 📄 页面 5
BATCH_CONTENT:2:另一个内容...
```

#### 2. Controller：解析 BATCH_CONTENT

```java
if (chunk.startsWith("BATCH_CONTENT:")) {
    int firstColon = chunk.indexOf(':', 14);
    String batchIndexStr = chunk.substring(14, firstColon);
    String content = chunk.substring(firstColon + 1);
    
    // 发送带批次索引的消息
    payload.put("type", "content");
    payload.put("batchIndex", Integer.parseInt(batchIndexStr));
    payload.put("content", content);
}
```

#### 3. VisionLLMDocumentProcessor：发送带索引的内容

**非流式模式**：
```java
callback.accept("BATCH_CONTENT:" + batchIndex + ":" + pageHeader);
callback.accept("BATCH_CONTENT:" + batchIndex + ":" + pageContent);
```

**流式模式**：
```java
// 在 context 中传递批次索引
context.getOptions().put("currentBatchIndex", batchIndex);

// 发送时使用 BATCH_CONTENT 格式
finalStreamCallback.accept("BATCH_CONTENT:" + batchIndex + ":" + token);
```

#### 4. 前端：使用消息中的 batchIndex

```javascript
else if (data.type === 'content') {
    // ⭐ 优先使用消息中的 batchIndex
    const batchIdx = typeof data.batchIndex === 'number' 
        ? data.batchIndex 
        : currentBatchIndex
    
    // 累加到对应批次
    if (batchIdx >= 0) {
        setBatches(prev => prev.map(b =>
            b.index === batchIdx 
                ? { ...b, content: b.content + newContent }
                : b
        ))
    }
}
```

---

## 📊 修复效果对比

### 修复前（错误）
```
批次 1: "---\n## 📄 页面 1\n内容1\n---\n## 📄 页面 5\n内容5"  ❌
批次 2: ""
批次 3: ""

合并后：混乱的内容
```

### 修复后（正确）
```
批次 1: "---\n## 📄 页面 1\n内容1\n---\n## 📄 页面 2\n内容2"  ✓
批次 2: "---\n## 📄 页面 3\n内容3\n---\n## 📄 页面 4\n内容4"  ✓
批次 3: "---\n## 📄 页面 5\n内容5"  ✓

合并后：按顺序合并，内容清晰
```

---

## 🔍 工作流程

### 并行处理 + 正确累加
```
T0: 收到 BATCH_INFO → 初始化 3 个批次

T1: BATCH_START:0 → 批次 1 = processing
T2: BATCH_START:2 → 批次 3 = processing
    ↓
T3: BATCH_CONTENT:0:内容A → 累加到批次 1 ✓
T4: BATCH_CONTENT:2:内容B → 累加到批次 3 ✓
T5: BATCH_CONTENT:0:内容C → 累加到批次 1 ✓
    ↓
T6: BATCH_END:0 → 批次 1 = completed
T7: BATCH_END:2 → 批次 3 = completed
    ↓
T8: BATCH_START:1 → 批次 2 = processing
T9: BATCH_CONTENT:1:内容D → 累加到批次 2 ✓
T10: BATCH_END:1 → 批次 2 = completed
    ↓
T11: 所有批次完成 → 显示"合并批次"按钮
T12: 点击合并 → 按顺序 1→2→3 合并 ✓
```

---

## 📝 改动文件清单

### 后端（2 个文件）

#### 1. VisionLLMDocumentProcessor.java
- ✅ 修改 `processPageBatch` 方法签名，添加 `batchIndex` 参数
- ✅ 在 context 的 options 中存储 `currentBatchIndex`
- ✅ 流式模式：使用 `BATCH_CONTENT:index:content` 格式
- ✅ 非流式模式：使用 `BATCH_CONTENT:index:content` 格式
- ✅ 并行处理调用：传递 `batchIndex`
- ✅ 串行处理调用：传递 `batchIndex`

#### 2. DocumentProcessingController.java
- ✅ 添加 `BATCH_CONTENT` 协议解析
- ✅ 提取批次索引和内容
- ✅ 发送带 `batchIndex` 的 JSON 消息

### 前端（1 个文件）

#### TextExtractionConfig.jsx
- ✅ 修改内容处理逻辑
- ✅ 优先使用 `data.batchIndex`
- ✅ 降级使用 `currentBatchIndex`（向后兼容）

---

## 🧪 测试验证

### 测试场景 1：并行处理
```
1. 上传 10 页 PPT
2. 点击"开始提取"
3. 观察：批次 1、3 同时处理
4. 观察：每个批次内容独立累加（不交错）
5. 等待完成
6. 点击"合并批次"
7. 验证：内容按顺序合并（1→2→3），无混乱
```

**预期结果**：
```markdown
---

## 📄 页面 1

内容...

---

## 📄 页面 2

内容...

---

## 📄 页面 3

内容...
```

### 测试场景 2：流式模式
```
1. 上传大文档
2. 开启流式模式
3. 观察：每个 token 正确累加到对应批次
4. 合并后内容完整
```

---

## 💡 关键改进

### 1. 协议升级
```
之前：content（无批次信息）
现在：BATCH_CONTENT:index:content（包含批次信息）
```

### 2. 无状态设计
```
之前：依赖全局 currentBatchIndex（并行时错误）
现在：每条消息自带 batchIndex（并行安全）
```

### 3. 向后兼容
```
if (typeof data.batchIndex === 'number') {
    // 新协议：使用消息中的索引
    batchIdx = data.batchIndex
} else {
    // 旧协议：降级使用全局变量
    batchIdx = currentBatchIndex
}
```

---

## ✅ 验证清单

- [x] 编译通过
- [x] 并行处理时内容不混乱
- [x] 批次内容正确累加
- [x] 合并后顺序正确
- [x] 流式模式正常工作
- [x] 非流式模式正常工作

---

**版本**：v4.1.1 - 批次合并修复版  
**修复时间**：2025-12-24  
**问题类型**：并行处理导致批次内容混乱  
**解决方案**：BATCH_CONTENT 协议 + 消息携带批次索引

