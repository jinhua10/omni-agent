# 流式输出问题排查和解决方案

## 🐛 问题描述

**症状**：后台通过 LLM 解析内容时有日志输出，但前端页面没有动态展示已经提取的内容。

## 🔍 根本原因分析

### 1. JSON 转义问题
**问题**：手动拼接 JSON 字符串时，特殊字符（如换行符、引号、反斜杠）转义不完整。

```java
// ❌ 错误的做法
String safe = chunk.replace("\\", "\\\\").replace("\"", "\\\"");
String json = "{\"type\":\"content\",\"content\":\"" + safe + "\"}";
```

**后果**：
- 换行符 `\n` 未转义导致 JSON 解析失败
- 特殊字符破坏 JSON 格式
- 前端 `JSON.parse()` 抛出异常

### 2. 回调未触发
**问题**：流式回调逻辑中，空字符串或 null 直接返回，导致有效内容未发送。

### 3. 日志级别问题
**问题**：使用 `log.debug()` 导致生产环境看不到流式发送日志，无法排查问题。

## ✅ 解决方案

### 1. 使用 Jackson 进行 JSON 序列化

```java
// ✅ 正确的做法
com.fasterxml.jackson.databind.ObjectMapper mapper = new ObjectMapper();
Map<String, Object> payload = new HashMap<>();
payload.put("type", "content");
payload.put("content", chunk);  // Jackson 自动处理所有转义

String jsonPayload = mapper.writeValueAsString(payload);
emitter.send(SseEmitter.event().name("message").data(jsonPayload));
```

**优点**：
- ✅ 自动处理所有特殊字符转义
- ✅ 支持复杂对象
- ✅ 类型安全
- ✅ 代码更简洁

### 2. 添加空值检查和日志

```java
if (chunk == null || chunk.isEmpty()) {
    return;  // 跳过空内容
}

log.debug("📤 发送流式内容: {} 字符", chunk.length());
// ... 发送逻辑
log.debug("✅ 成功发送流式内容");
```

### 3. 添加测试端点

创建独立的测试端点验证 SSE 流式输出：

```java
@GetMapping("/test-streaming")
public SseEmitter testStreaming() {
    // 每 500ms 发送一条测试消息，共 10 条
    // 用于验证 SSE 是否正常工作
}
```

**测试方法**：
```bash
# 使用 curl 测试
curl http://localhost:3000/api/documents/processing/test-streaming

# 或在浏览器中打开
# 应该看到每 0.5 秒输出一行消息
```

## 🔧 修改清单

### 后端改动

#### 1. DocumentProcessingController.java

**改动点 1：优化流式回调逻辑**
```java
// 位置：extractText 方法中的流式回调
chunk -> {
    if (chunk == null || chunk.isEmpty()) {
        return;  // ⭐ 跳过空内容
    }
    
    log.debug("📤 发送流式内容: {} 字符", chunk.length());
    
    // ⭐ 使用 Jackson 序列化
    ObjectMapper mapper = new ObjectMapper();
    Map<String, Object> payload = new HashMap<>();
    payload.put("type", "content");
    payload.put("content", chunk);
    
    String jsonPayload = mapper.writeValueAsString(payload);
    emitter.send(SseEmitter.event().name("message").data(jsonPayload));
    
    log.debug("✅ 成功发送流式内容");
}
```

**改动点 2：添加测试端点**
```java
@GetMapping(value = "/test-streaming", produces = "text/event-stream;charset=UTF-8")
public SseEmitter testStreaming() {
    // 发送 10 条测试消息
}
```

## 🧪 测试步骤

### 步骤 1：测试 SSE 基础功能

1. 启动应用
2. 打开浏览器控制台
3. 访问测试端点：
   ```javascript
   const evtSource = new EventSource('/api/documents/processing/test-streaming');
   evtSource.onmessage = (event) => {
       console.log('收到消息:', event.data);
   };
   ```
4. **预期结果**：每 0.5 秒看到一条消息，共 10 条

### 步骤 2：测试文档提取流式输出

1. 上传一个测试文档（如 PPT）
2. 进入"文本提取"页面
3. 选择"流式"模式
4. 点击"开始提取"
5. **观察**：
   - 后端日志应该看到 `📤 发送流式内容: X 字符`
   - 前端应该看到内容逐步增长
   - 浏览器控制台应该看到 `📥 收到SSE事件: content`

### 步骤 3：对比流式 vs 批量

| 模式 | 预期行为 |
|------|---------|
| 流式 | 实时看到内容逐字增长，每个 token 都实时显示 |
| 批量 | 每页完成后显示该页完整内容 |

## 📊 调试方法

### 1. 查看后端日志

```bash
# 开启 DEBUG 日志
logging.level.top.yumbo.ai.omni.web.controller=DEBUG

# 关键日志
📤 发送流式内容: 123 字符  # 每次发送都会打印
✅ 成功发送流式内容        # 发送成功确认
❌ 发送流式内容失败: xxx   # 发送失败会打印异常
```

### 2. 查看前端控制台

```javascript
// 前端应该看到
📥 收到SSE事件: content {type: "content", content: "..."}
📄 累加文本内容，长度: 123 模式: 流式

// 如果没看到，检查
1. Network 标签页，SSE 连接是否建立
2. Console 有无 JSON 解析错误
3. 检查 response 的 content-type 是否为 text/event-stream
```

### 3. 使用 curl 原始测试

```bash
# 测试 SSE 端点
curl -N http://localhost:3000/api/documents/processing/test-streaming

# 应该看到
event:message
data:{"type":"content","content":"第 1 条测试消息\n"}

event:message
data:{"type":"content","content":"第 2 条测试消息\n"}
...
```

## 🚨 常见问题

### Q1: 后端有日志，但前端没显示
**排查**：
1. 检查浏览器控制台是否有 JSON 解析错误
2. 检查 Network → EventStream 是否有数据
3. 检查 `setExtractionResult` 是否被调用

### Q2: SSE 连接建立失败
**排查**：
1. 检查 CORS 配置
2. 检查 nginx/反向代理配置（可能缓冲了 SSE）
3. 检查防火墙/安全组规则

### Q3: 内容显示乱码
**原因**：JSON 转义问题
**解决**：确保使用 Jackson 序列化，不要手动拼接 JSON

### Q4: 流式很慢，卡顿
**可能原因**：
1. Vision LLM 推理速度慢（这是正常的）
2. 网络延迟
3. 并行批次导致资源竞争

**优化**：
- 调整批次大小（`max-slides-per-batch`）
- 流式模式下使用串行处理
- 检查网络带宽

## 💡 最佳实践

### 1. JSON 序列化
✅ **使用**：Jackson ObjectMapper
❌ **不要**：手动拼接 JSON 字符串

### 2. 日志级别
✅ **使用**：`log.info()` 记录关键事件
✅ **使用**：`log.debug()` 记录详细信息
❌ **不要**：只用 debug，生产环境看不到

### 3. 错误处理
✅ **捕获并记录所有异常**
✅ **向前端发送友好的错误信息**
❌ **不要**：静默失败

### 4. 测试
✅ **先测试简单场景**（test-streaming 端点）
✅ **再测试复杂场景**（实际文档提取）
✅ **对比流式 vs 批量模式**

## 📝 改动摘要

| 文件 | 改动 | 影响 |
|------|------|------|
| DocumentProcessingController.java | 使用 Jackson 序列化 JSON | 修复 JSON 转义问题 |
| DocumentProcessingController.java | 添加空值检查 | 避免发送空内容 |
| DocumentProcessingController.java | 添加 debug 日志 | 便于排查问题 |
| DocumentProcessingController.java | 新增测试端点 | 独立测试 SSE |

## 🎯 验证清单

- [ ] 编译通过
- [ ] test-streaming 端点正常工作
- [ ] 流式模式实时显示内容
- [ ] 批量模式按页显示内容
- [ ] 后端日志正常输出
- [ ] 前端控制台无错误
- [ ] 长文档（10+ 页）测试通过
- [ ] 特殊字符（换行、引号）正常显示

## 📞 需要进一步帮助？

如果问题仍然存在：
1. 提供后端完整日志
2. 提供前端控制台截图
3. 提供 Network 标签页的 SSE 请求详情
4. 说明测试文档类型和大小

---

**更新时间**：2025-12-24
**作者**：OmniAgent Team

