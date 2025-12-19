# 🚀 PPL ONNX 模式启动指南

## ✅ 配置状态

**所有配置已完成！可以直接启动使用。**

- ✅ PPL 分块策略已配置为默认策略
- ✅ ONNX 模式已启用
- ✅ BGE-base-zh 模型文件已就位
- ✅ 依赖已编译完成

## 📋 快速启动

### 方式 1：使用 Maven 启动

```powershell
cd D:\Jetbrains\omni-agent\omni-agent-example-basic
mvn spring-boot:run
```

### 方式 2：使用 JAR 启动

```powershell
cd D:\Jetbrains\omni-agent\omni-agent-example-basic
java -jar target/omni-agent-example-basic-1.0.0.jar
```

## 🔍 验证启动成功

启动后查看控制台输出，应该看到以下关键日志：

### 1. **ONNX PPL 服务初始化**

```
🚀 初始化 ONNX PPL 服务
✅ ONNX Environment 创建成功
✅ ONNX 模型加载成功: ./models/bge-base-zh/model.onnx
✅ Tokenizer 加载成功: ./models/bge-base-zh
✅ PPL 缓存初始化: size=1000, ttl=3600s
🎉 ONNX PPL 服务初始化完成
```

### 2. **分块策略注册**

```
ChunkingStrategyManager initialized with 4 strategies
Registered chunking strategy: fixed_size - 固定大小分块策略
Registered chunking strategy: semantic - 语义分块策略
Registered chunking strategy: ppl - PPL困惑度分块策略
Registered chunking strategy: paragraph - 段落分块策略
```

### 3. **应用启动完成**

```
Started BasicExampleApplication in X.XXX seconds
Tomcat started on port 8080
```

## 🧪 测试 PPL ONNX 分块

### 1. 打开浏览器

访问：http://localhost:8080

### 2. 上传测试文档

准备一个测试文档（PDF、TXT、DOCX 等），通过 Web 界面上传。

### 3. 查看分块结果

上传后，查看生成的分块文件：

```powershell
# 列出分块文件
Get-ChildItem data\storage\chunks -Recurse -Filter "*.md"

# 查看某个分块的元数据
Get-Content data\storage\chunks\你的文档名\chunk_000.md.meta | ConvertFrom-Json | Format-List
```

**预期看到的元数据**：

```json
{
  "id": "chunk_xxx",
  "documentId": "你的文档名",
  "filename": "chunk_000.md",
  "sequence": 0,
  "size": 500,
  "metadata": {
    "strategy": "ppl",
    "avgPerplexity": 2.45,    // ⭐ ONNX 计算的真实困惑度
    "sentences": 8
  },
  "createdAt": 1734615694000
}
```

### 4. 测试 RAG 检索

```powershell
# 使用 curl 测试检索
curl "http://localhost:8080/api/chat?question=你的问题"
```

## 📊 性能监控

### 查看 PPL 计算性能

在上传文档时观察日志：

```
🔍 [VisionLLM] 调用 Vision API 分析页面 1, 图片数: 1
📦 使用 ChunkingStrategyManager 进行分块...
PPL chunking: 5 chunks created with avg perplexity boundaries
✅ 分块完成: 共 5 个块, 策略: ppl
```

### 缓存命中情况

重复上传相同内容的文档，第二次应该更快（缓存生效）。

## ⚙️ 配置调优

### 如果性能较慢

编辑 `application.yml`：

```yaml
ppl:
  onnx:
    use-cache: true
    cache-size: 2000      # 增大缓存
    cache-ttl: 7200       # 延长缓存时间
```

### 如果分块太细

```yaml
omni-agent:
  chunking:
    ppl:
      min-chunk-size: 300      # 增大最小分块
      threshold: 0.4           # 提高阈值
```

### 如果分块太粗

```yaml
omni-agent:
  chunking:
    ppl:
      max-chunk-size: 600      # 减小最大分块
      threshold: 0.2           # 降低阈值
```

## 🔧 故障排查

### 问题 1: ���型加载失败

**错误信息**：
```
❌ ONNX PPL 服务初始化失败
Failed to load model: ./models/bge-base-zh/model.onnx
```

**解决方案**：
```powershell
# 检查模型文件是否存在
Test-Path models\bge-base-zh\model.onnx

# 检查文件大小
(Get-Item models\bge-base-zh\model.onnx).Length / 1MB
```

如果文件不存在或损坏，重新下载 BGE-base-zh 模型。

### 问题 2: Tokenizer 加载失败

**错误信息**：
```
Failed to load tokenizer from: ./models/bge-base-zh
```

**解决方案**：
```powershell
# 检查 tokenizer 文件
Test-Path models\bge-base-zh\tokenizer.json
Test-Path models\bge-base-zh\vocab.txt
```

### 问题 3: 内存不足

**错误信息**：
```
OutOfMemoryError
```

**解决方案**：增加 JVM 内存

```powershell
# 启动时指定内存
java -Xmx4G -jar target/omni-agent-example-basic-1.0.0.jar
```

或修改 `pom.xml` 的 spring-boot-maven-plugin 配置。

### 问题 4: PPL 分块未生效

**检查步骤**：

1. 确认配置：
```powershell
Get-Content omni-agent-example-basic\src\main\resources\application.yml | Select-String "default-strategy"
```

应该看到：`default-strategy: ppl`

2. 查看启动日志，确认 PPL 策略已注册

3. 检查分块元数据中的 `strategy` 字段

## 📚 更多资源

- **详细配置说明**：`docs/PPL_ONNX_SETUP_GUIDE.md`
- **分块策略验证报告**：`docs/CHUNKING_STRATEGIES_VERIFICATION.md`
- **PPL 实现文档**：`docs/PPL_CONFIG_DRIVEN_IMPLEMENTATION.md`

## 🎉 成功标志

如果看到以下所有内容，说明 PPL ONNX 模式已成功运行：

- ✅ 启动日志中显示 "ONNX PPL 服务初始化完成"
- ✅ 上传文档后能看到分块文件
- ✅ 分块元数据中 `strategy` 为 "ppl"
- ✅ 分块元数据中有 `avgPerplexity` 字段
- ✅ RAG 检索返回准确结果

**祝使用愉快！** 🚀

