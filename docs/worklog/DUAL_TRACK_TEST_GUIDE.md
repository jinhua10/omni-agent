# 🧪 双轨流式问答测试指南

## 📋 测试前准备

### 1. 启动应用

```bash
cd D:\Jetbrains\omni-agent
.\test-startup.ps1
```

或者手动启动：

```bash
cd omni-agent-p2p-basic
mvn spring-boot:run
```

### 2. 准备测试数据

确保系统中有一些索引的文档，可以通过以下方式：

1. 访问文档管理页面上传文档
2. 使用 API 批量索引文档
3. 使用测试数据脚本

---

## 🎯 测试场景

### 场景 1: 单轨模式（不使用RAG）

**目的**：验证纯LLM模式

**请求：**
```bash
curl -N "http://localhost:8080/api/qa/stream/dual-track?question=什么是人工智能&knowledgeMode=none"
```

**预期结果：**
- ✅ 只有一个轨道输出（event: llm）
- ✅ 流式返回答案
- ✅ 最后发送 complete 事件

**UI测试：**
1. 打开问答页面
2. 知识库模式选择："不使用RAG"
3. 输入问题："什么是人工智能"
4. 点击提问
5. 观察只有一个面板显示答案

---

### 场景 2: 双轨RAG模式

**目的**：验证左轨RAG+LLM，右轨HOPE智能系统

**请求：**
```bash
curl -N "http://localhost:8080/api/qa/stream/dual-track?question=如何使用Spring Boot&knowledgeMode=rag"
```

**预期结果：**
- ✅ 先发送参考文档（如果有）
- ✅ 左轨流式输出（event: left）
- ✅ 右轨流式输出（event: right）
- ✅ 最后发送 complete 事件

**UI测试：**
1. 打开问答页面
2. 知识库模式选择："使用RAG"
3. 输入问题："如何使用Spring Boot"
4. 点击提问
5. 观察两个面板同时显示答案
   - 左面板：🤖 RAG + LLM 回答
   - 右面板：🧠 HOPE智能系统

---

### 场景 3: 双轨角色模式

**目的**：验证左轨RAG+LLM，右轨角色专业回答

**前提**：系统中已注册角色（如：java-expert）

**请求：**
```bash
curl -N "http://localhost:8080/api/qa/stream/dual-track?question=如何优化Java性能&knowledgeMode=role&roleName=java-expert"
```

**预期结果：**
- ✅ 先发送参考文档（如果有）
- ✅ 左轨流式输出（event: left）
- ✅ 右轨流式输出（event: right），以角色身份回答
- ✅ 最后发送 complete 事件

**UI测试：**
1. 打开问答页面
2. 知识库模式选择："角色知识库"
3. 角色选择："Java专家"
4. 输入问题："如何优化Java性能"
5. 点击提问
6. 观察两个面板：
   - 左面板：基于文档的标准答案
   - 右面板：Java专家的专业见解

---

### 场景 4: 无检索结果提示

**目的**：验证当知识库中没有相关文档时的友好提示

**请求：**
```bash
curl -N "http://localhost:8080/api/qa/stream/dual-track?question=量子计算机的工作原理&knowledgeMode=rag"
```

**预期结果：**
- ✅ 发送友好提示："未检索到相关文档，将基于通用知识和系统学习回答"
- ✅ 左轨仍然生成答案（基于LLM通用知识）
- ✅ 右轨使用HOPE系统回答

**UI测试：**
1. 输入一个知识库中不存在的问题
2. 观察是否显示友好提示
3. 观察两个轨道是否仍然正常工作

---

### 场景 5: 错误处理

**目的**：验证各种错误情况的处理

#### 5.1 左轨失败

模拟左轨生成失败的情况：
- ✅ 发送警告消息
- ✅ 继续执行右轨
- ✅ 不影响整体流程

#### 5.2 右轨失败

模拟右轨生成失败的情况：
- ✅ 发送警告消息
- ✅ 显示友好错误提示
- ✅ 左轨结果仍然可用

#### 5.3 连接超时

- ✅ 5分钟后自动关闭连接
- ✅ 清理资源

---

## 📊 监控日志

启动应用后，观察控制台日志：

### 正常流程日志示例

```
2025-12-19 07:50:29 [Thread-2] INFO  t.y.a.o.w.controller.DemoController - 🚂 双轨流式问答: question=如何使用Spring Boot, mode=rag, role=null
2025-12-19 07:50:29 [Thread-2] INFO  t.y.a.o.w.controller.DemoController - 📚 检索到 3 个参考文档
2025-12-19 07:50:29 [Thread-2] INFO  t.y.a.o.w.controller.DemoController - 🚂 双轨模式：RAG + HOPE智能系统
2025-12-19 07:50:29 [Thread-2] INFO  t.y.a.o.w.controller.DemoController - ⬅️ 启动左轨：传统RAG+LLM
2025-12-19 07:50:30 [Thread-2] INFO  t.y.a.o.w.controller.DemoController - ✅ 左轨完成
2025-12-19 07:50:30 [Thread-2] INFO  t.y.a.o.w.controller.DemoController - ➡️ 启动右轨：HOPE智能系统
2025-12-19 07:50:32 [Thread-2] INFO  t.y.a.o.w.controller.DemoController - ✅ 右轨完成
2025-12-19 07:50:32 [Thread-2] INFO  t.y.a.o.w.controller.DemoController - ✅ 双轨流式问答完成
2025-12-19 07:50:32 [Thread-2] INFO  t.y.a.o.w.controller.DemoController - ✅ SSE连接关闭
```

### 关键日志标识

| 符号 | 含义 |
|------|------|
| 🚂 | 双轨模式启动 |
| 📚 | 检索文档 |
| ⬅️ | 左轨处理 |
| ➡️ | 右轨处理 |
| 📤 | 发送token |
| ✅ | 成功完成 |
| ❌ | 错误失败 |
| ⚠️ | 警告信息 |
| 💡 | 友好提示 |

---

## 🐛 常见问题

### Q1: 左轨或右轨没有输出

**可能原因：**
- AI服务未正确配置
- 网络连接问题
- LLM API配额超限

**解决方法：**
1. 检查 `application.yml` 中的 AI 配置
2. 查看控制台日志确认错误信息
3. 验证 LLM API 可用性

### Q2: 检索不到文档

**可能原因：**
- 文档未索引
- 问题与文档内容不相关
- RAG服务未启动

**解决方法：**
1. 检查文档管理页面，确认有索引的文档
2. 尝试更相关的问题
3. 查看RAG服务日志

### Q3: HOPE系统未生效

**可能原因：**
- HOPE服务未初始化
- 问题分类器未正常工作

**解决方法：**
1. 检查 `HOPEKnowledgeManager` 是否正确注入
2. 查看 `QuestionClassifier` 日志
3. 验证三层知识服务状态

### Q4: 角色未找到

**可能原因：**
- 角色未注册
- 角色ID错误

**解决方法：**
1. 通过角色管理页面查看已注册角色
2. 验证角色ID拼写
3. 使用默认角色测试

---

## ✅ 测试检查清单

- [ ] 单轨模式正常工作
- [ ] 双轨RAG模式正常工作
- [ ] 双轨角色模式正常工作
- [ ] 无检索结果时显示友好提示
- [ ] 左轨流式输出流畅
- [ ] 右轨流式输出流畅
- [ ] 参考文档正确显示
- [ ] 完成标记正常发送
- [ ] 错误处理正确
- [ ] UI双面板正常显示
- [ ] 语言切换正常（中/英文）
- [ ] 日志输出清晰

---

## 🎓 测试技巧

1. **使用浏览器开发者工具**
   - Network标签查看SSE连接
   - Console查看前端日志
   - 观察事件流

2. **使用curl测试**
   - `-N` 参数禁用缓冲，实时看到SSE事件
   - 可以快速验证后端逻辑

3. **对比两个轨道**
   - 观察回答的差异
   - 理解HOPE系统的优化效果
   - 体验角色专业性

4. **测试边界情况**
   - 超长问题
   - 特殊字符
   - 多语言问题

---

## 📚 相关文档

- [双轨架构设计](DUAL_TRACK_ARCHITECTURE.md)
- [HOPE知识系统](../refactor/phase-2/PHASE2_HOPE_COMPLETE_MILESTONE.md)
- [算法市场指南](ALGORITHM_MARKET_GUIDE.md)
- [角色系统指南](./ROLE_SYSTEM_GUIDE.md)（如果存在）

---

**祝测试顺利！** 🚀

