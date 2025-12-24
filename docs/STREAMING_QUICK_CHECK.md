# 流式输出问题快速检查清单

## 🔍 问题：后台有日志，前端不显示

### ✅ 第一步：验证 SSE 基础功能

```bash
# 在浏览器控制台运行
const evtSource = new EventSource('/api/documents/processing/test-streaming');
evtSource.onmessage = (event) => {
    console.log('📥 收到:', JSON.parse(event.data));
};
```

**预期结果**：每 0.5 秒看到一条消息
- ✅ 如果正常 → SSE 基础功能 OK，继续下一步
- ❌ 如果失败 → 检查网络/CORS/nginx 配置

---

### ✅ 第二步：检查前端是否收到数据

打开浏览器开发者工具：

1. **Network 标签页**
   - 找到 `/extract` 请求
   - Type 应该是 `eventsource` 或 `text/event-stream`
   - 点击查看 Response
   - ✅ 应该看到连续的 `event:message` 和 `data:{...}`

2. **Console 标签页**
   ```javascript
   // 应该看到
   📥 收到SSE事件: content {type: "content", content: "..."}
   📄 累加文本内容，长度: 123
   ```
   
   - ✅ 如果看到 → 前端收到了，但可能是显示问题
   - ❌ 如果没看到 → 前端未收到，检查后端

---

### ✅ 第三步：检查后端是否发送

查看后端日志（需要开启 DEBUG 级别）：

```properties
# application.yml 或 application.properties
logging.level.top.yumbo.ai.omni.web.controller=DEBUG
```

**关键日志**：
```
📤 发送流式内容: 123 字符      # ✅ 说明在发送
✅ 成功发送流式内容            # ✅ 发送成功
```

- ✅ 如果看到 → 后端正常发送
- ❌ 如果没看到 → 回调未触发，检查 VisionLLMDocumentProcessor

---

### ✅ 第四步：检查 JSON 格式

在后端日志中找到发送的内容，手动验证 JSON：

```javascript
// 复制日志中的 JSON 到浏览器控制台
JSON.parse('{"type":"content","content":"测试\\n内容"}')
```

- ✅ 如果能解析 → JSON 格式正确
- ❌ 如果报错 → JSON 转义有问题

---

### ✅ 第五步：检查前端状态更新

在 `TextExtractionConfig.jsx` 中添加调试：

```javascript
} else if (data.type === 'content') {
    console.log('🎯 收到内容:', data.content?.length, '字符');
    console.log('📝 当前总长度:', extractionResult.length);
    setExtractionResult(prev => {
        const newValue = prev + (data.content || '');
        console.log('🔄 更新后长度:', newValue.length);
        return newValue;
    });
}
```

**观察**：
- ✅ 如果长度在增长 → 状态更新正常
- ❌ 如果长度不变 → React 状态更新有问题

---

## 🚨 常见错误和解决方案

### 错误 1：SyntaxError: Unexpected token in JSON

**原因**：JSON 转义不完整
**解决**：
```diff
- String json = "{\"content\":\"" + chunk + "\"}";  // ❌
+ ObjectMapper mapper = new ObjectMapper();
+ String json = mapper.writeValueAsString(Map.of("content", chunk));  // ✅
```

---

### 错误 2：后端一直打印"发送流式内容"但前端不显示

**原因**：可能是 nginx 缓冲了 SSE
**解决**：nginx 配置
```nginx
location /api/ {
    proxy_pass http://backend;
    proxy_buffering off;  # ⭐ 关闭缓冲
    proxy_set_header Connection '';
    chunked_transfer_encoding off;
}
```

---

### 错误 3：前端收到数据但不显示

**排查**：
1. 检查 `extractionResult` 状态是否更新
2. 检查 TextArea 的 `value` 绑定
3. 检查是否有条件渲染隐藏了内容

**快速测试**：
```javascript
// 在浏览器控制台强制设置
window.testSetText = (text) => {
    // 找到 TextArea 并设置值
    const textarea = document.querySelector('textarea');
    if (textarea) textarea.value = text;
};

// 调用测试
testSetText('测试内容');
```

---

### 错误 4：流式模式很慢，卡顿

**原因**：并行批次 + 流式输出冲突
**解决**：强制串行处理

```java
// VisionLLMDocumentProcessor.java
if (streamingEnabled) {
    // 流式模式强制串行
    batchResults = processPageBatchesSequentially(batches);
} else {
    // 批量模式可以并行
    batchResults = visionLlmExecutor != null && batches.size() > 1
        ? processPageBatchesInParallel(batches)
        : processPageBatchesSequentially(batches);
}
```

---

## 🎯 5 分钟快速诊断

| 步骤 | 检查点 | 命令/操作 | 预期结果 |
|------|--------|----------|---------|
| 1 | SSE 基础 | 访问 `/test-streaming` | 看到 10 条消息 |
| 2 | 前端接收 | Network → EventStream | 看到连续数据 |
| 3 | 后端发送 | 查看日志 | 看到 "📤 发送" |
| 4 | JSON 格式 | 手动 parse | 不报错 |
| 5 | 状态更新 | Console 日志 | 长度增长 |

**每一步都通过** → 🎉 功能正常
**某一步失败** → 🔍 重点排查该步骤

---

## 💊 万能解决方案

如果以上都不行，尝试以下步骤：

### 1. 清空缓存
```bash
# 前端
Ctrl + Shift + Delete → 清空缓存

# 后端
mvn clean && mvn package
```

### 2. 使用最简单的测试
```javascript
// 前端手动测试
fetch('/api/documents/processing/test-streaming')
    .then(response => response.body.getReader())
    .then(reader => {
        const decoder = new TextDecoder();
        function read() {
            reader.read().then(({done, value}) => {
                if (done) return;
                console.log(decoder.decode(value));
                read();
            });
        }
        read();
    });
```

### 3. 降级到非流式模式
- 先确保批量模式能正常工作
- 再逐步排查流式模式的问题

---

## 📞 获取帮助

提供以下信息：
1. ✅ 完成了哪些检查步骤
2. ❌ 哪一步失败了
3. 📋 完整的错误日志
4. 🖼️ Network 标签页截图
5. 📝 Console 控制台输出

---

**更新时间**：2025-12-24  
**预计诊断时间**：5-10 分钟

