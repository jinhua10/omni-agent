# 🔄 AI Starter 模块合并方案

## 📋 目标

将以下三个独立的 AI Starter 模块合并为一个统一的 `omni-agent-ai-starter` 模块：
- `omni-agent-ai-starter-ollama`
- `omni-agent-ai-starter-online-api`  
- `omni-agent-ai-starter-onnx`

## 🎯 设计模式

参考以下模块的设计模式：
- `omni-agent-chunking-starter` - 统一的分块服务，支持多种策略
- `omni-agent-document-storage-starter` - 统一的存储服务，支持多种后端

## 📁 新模块结构

```
omni-agent-ai-starter/
├── pom.xml
└── src/main/java/top/yumbo/ai/omni/ai/starter/
    ├── config/
    │   ├── AIAutoConfiguration.java          ← 主自动配置类
    │   ├── AIProperties.java                 ← 统一配置属性
    │   └── AIServiceManager.java             ← AI服务管理器
    │
    ├── impl/
    │   ├── OllamaAIService.java              ← Ollama实现（移动过来）
    │   ├── OnlineAPIAIService.java           ← Online API实现（移动过来）
    │   └── OnnxAIService.java                ← ONNX实现（移动过来）
    │
    ├── properties/
    │   ├── OllamaProperties.java             ← Ollama配置
    │   ├── OnlineAPIProperties.java          ← Online API配置
    │   └── OnnxProperties.java               ← ONNX配置
    │
    └── archive/                               ← AI调用归档功能（移动到这里）
        ├── AICallArchive.java
        ├── AICallArchiveService.java
        ├── aspect/
        │   └── AICallArchiveAspect.java
        ├── config/
        │   ├── AIArchiveAutoConfiguration.java
        │   └── AIArchiveProperties.java
        └── impl/
            └── FileAICallArchiveService.java
```

## 🔧 配置示例

### 新的配置方式

```yaml
omni-agent:
  ai:
    # 选择AI服务类型
    type: online-api  # ollama | online-api | onnx
    
    # ========== Ollama 配置 ==========
    ollama:
      base-url: http://localhost:11434
      model: qwen2.5:latest
      temperature: 0.7
      max-tokens: 2000
      timeout: 30000
    
    # ========== Online API 配置 ==========
    online:
      provider: deepseek  # qianwen | openai | claude | zhipu | deepseek
      endpoint: https://api.deepseek.com/v1/chat/completions
      api-key: ${AI_API_KEY}
      default-model: deepseek-chat
      temperature: 0.7
      max-tokens: 2048
      stream-enabled: true
    
    # ========== ONNX 配置 ==========
    onnx:
      model-path: ./models/qwen/model.onnx
      tokenizer-path: ./models/qwen
      max-sequence-length: 2048
      device: cpu
    
    # ========== AI调用归档配置 ==========
    archive:
      enabled: true
      storage-path: data/ai-archives
      enable-memory-index: true
      max-memory-index: 1000
      auto-clean-days: 30
      thread-pool:
        core-size: 2
        max-size: 5
        queue-capacity: 1000
```

## 📝 实施步骤

### Phase 1: 创建新模块 ✅
1. 创建 `omni-agent-ai-starter` 模块
2. 设置 pom.xml 依赖

### Phase 2: 迁移实现类
1. 将三个服务实现类移动到 `impl/` 包
2. 调整包名和导入

### Phase 3: 创建统一配置
1. 创建 `AIProperties` 统一配置
2. 创建 `AIAutoConfiguration` 自动配置
3. 创建 `AIServiceManager` 管理器

### Phase 4: 迁移归档功能
1. 将归档相关代码从 `ai-api` 移动到 `ai-starter`
2. 修复依赖问题（Jackson、AspectJ等）

### Phase 5: 更新依赖
1. 更新 `omni-agent-web` 的依赖
2. 更新 `omni-agent-example-basic` 的依赖
3. 更新 `omni-agent-example-production` 的依赖

### Phase 6: 测试验证
1. 编译所有模块
2. 测试三种AI服务
3. 测试归档功能

### Phase 7: 清理旧模块
1. 标记旧模块为 deprecated
2. 更新文档

## 🎯 优势

### 1. 统一管理 ✅
- 所有AI服务实现在一个模块中
- 统一的配置方式
- 统一的依赖管理

### 2. 易于扩展 ✅
- 添加新的AI服务实现更简单
- 配置更加清晰
- 减少模块数量

### 3. 减少依赖复杂度 ✅
- 用户只需依赖一个 starter 模块
- 自动根据配置选择实现
- 减少 pom.xml 的复杂度

### 4. 归档功能集成 ✅
- AI调用归档功能直接集成
- 不需要额外的 API 模块依赖
- AspectJ 依赖统一管理

## 📊 对比

### 之前（3个独立模块）
```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-ai-starter-ollama</artifactId>
</dependency>
<!-- 或 -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-ai-starter-online-api</artifactId>
</dependency>
<!-- 或 -->
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-ai-starter-onnx</artifactId>
</dependency>
```

### 现在（1个统一模块）
```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-ai-starter</artifactId>
</dependency>
```

## 🔄 迁移指南

### 对于现有项目

**步骤1**: 更新依赖
```xml
<!-- 删除旧的依赖 -->
- omni-agent-ai-starter-ollama
- omni-agent-ai-starter-online-api
- omni-agent-ai-starter-onnx

<!-- 添加新的依赖 -->
+ omni-agent-ai-starter
```

**步骤2**: 更新配置
```yaml
# 之前的配置方式保持兼容
omni-agent:
  ai:
    type: online-api  # 只需要这一个type字段

    ollama:
      # ...existing config...
    
    online:
      # ...existing config...
```

**步骤3**: 重新编译和测试

## 📋 待办事项

- [ ] 创建 `omni-agent-ai-starter` 模块
- [ ] 迁移三个服务实现类
- [ ] 创建统一的自动配置
- [ ] 迁移AI调用归档功能
- [ ] 修复编译错误
- [ ] 更新所有依赖项目
- [ ] 更新文档
- [ ] 测试验证

## 🎉 完成标准

1. ✅ 新模块编译成功
2. ✅ 三种AI服务都能正常工作
3. ✅ AI调用归档功能正常
4. ✅ 所有示例项目编译成功
5. ✅ 配置向后兼容
6. ✅ 文档更新完成

---

**创建日期**: 2025-12-29  
**状态**: 规划中  
**预计工作量**: 2-3小时


