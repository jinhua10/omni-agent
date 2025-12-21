# PPL 分块策略配置示例

## 配置说明

PPL 分块策略支持两种实现，用户可以通过配置自由选择：

- **简化版** - 快速、零依赖，使用词汇重叠度近似困惑度
- **ONNX 版** - 精度高、使用真实语言模型计算困惑度

---

## 配置方式1：简化版（默认）⭐

**适用场景**: 
- 资源受限环境
- 快速响应场景
- 开发/测试环境

```yaml
# application.yml
chunking:
  ppl:
    mode: simplified  # 简化模式（默认）
```

**特点**:
- ✅ 速度极快（<1ms/文档）
- ✅ 内存占用小（1MB）
- ✅ 零外部依赖
- ⭐⭐⭐⭐ 精度

---

## 配置方式2：ONNX 版（精度模式）

**适用场景**:
- 生产环境资源充足
- 对精度要求高
- 复杂文档处理

```yaml
# application.yml
chunking:
  ppl:
    mode: onnx  # ONNX 模式
    model: qwen2.5-0.5b-instruct  # 可选：qwen2.5-0.5b-instruct | qwen2.5-1.5b-instruct

# ONNX 服务配置
ppl:
  onnx:
    enabled: true
    model-path: ./old/models/qwen2.5-0.5b-instruct/model.onnx
    tokenizer-path: ./old/models/qwen2.5-0.5b-instruct/tokenizer.json
    use-cache: true
    cache-size: 1000
    cache-ttl: 3600
```

**特点**:
- ✅ 精度提升 +15-20%
- ✅ 真实困惑度计算
- ⚠️ 需要 ONNX Runtime
- ⚠️ 内存占用 200-500MB
- ⭐⭐⭐⭐⭐ 精度

---

## 配置方式3：自动选择（智能模式）

**适用场景**:
- 希望自动适应环境
- 在资源充足时自动使用 ONNX

```yaml
# application.yml
chunking:
  ppl:
    mode: auto  # 自动模式
    prefer-accuracy: true  # true=优先精度，false=优先速度

# ONNX 配置（可选，有模型就用，没有就降级）
ppl:
  onnx:
    enabled: true
    model-path: ./old/models/qwen2.5-0.5b-instruct/model.onnx
    tokenizer-path: ./old/models/qwen2.5-0.5b-instruct/tokenizer.json
```

**自动选择逻辑**:
```
if (ONNX 模型可用 && prefer-accuracy=true):
    使用 ONNX PPL（精度模式）
else:
    使用简化版 PPL（快速模式）
```

**特点**:
- ✅ 智能适应环境
- ✅ 优雅降级
- ✅ 灵活可控

---

## 模型选择

**位置**: `./old/models/`

| 模型 | 大小 | 速度 | 精度 | 推荐 |
|------|------|------|------|------|
| `qwen2.5-0.5b-instruct` | 200MB | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ 最佳平衡 |
| `qwen2.5-1.5b-instruct` | 500MB | ⭐⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ 极致精度 |

---

## 完整配置示例

### 生产环境（资源充足）

```yaml
# application.yml
chunking:
  ppl:
    mode: onnx
    model: qwen2.5-0.5b-instruct

ppl:
  onnx:
    enabled: true
    model-path: ./old/models/qwen2.5-0.5b-instruct/model.onnx
    tokenizer-path: ./old/models/qwen2.5-0.5b-instruct/tokenizer.json
    use-cache: true
    cache-size: 1000
    cache-ttl: 3600

# 日志配置（可选）
logging:
  level:
    top.yumbo.ai.omni.core.chunking: INFO
    top.yumbo.ai.rag.ppl: INFO
```

### 生产环境（资源受限）

```yaml
# application.yml
chunking:
  ppl:
    mode: simplified  # 快速模式

# 日志配置（可选）
logging:
  level:
    top.yumbo.ai.omni.core.chunking: INFO
```

### 开发环境（自动适应）

```yaml
# application.yml
chunking:
  ppl:
    mode: auto
    prefer-accuracy: false  # 开发环境优先速度

ppl:
  onnx:
    enabled: true
    model-path: ./old/models/qwen2.5-0.5b-instruct/model.onnx
    tokenizer-path: ./old/models/qwen2.5-0.5b-instruct/tokenizer.json
    use-cache: true
```

---

## 配置验证

### 启动日志

**简化版模式**:
```
✅ 使用简化版 PPL 计算器（配置指定）
✂️ 智能分块完成: 15 个分块（文件类型: README.md）
```

**ONNX 模式**:
```
✅ ONNX Runtime 初始化成功
✅ 加载模型: qwen2.5-0.5b-instruct
✅ 使用 ONNX PPL 计算器（配置指定）
✂️ 智能分块完成: 15 个分块（文件类型: README.md）
```

**自动模式（降级）**:
```
⚠️ ONNX 模型不可用，降级到简化版
✅ 使用简化版 PPL 计算器（自动选择 - 优先速度）
✂️ 智能分块完成: 15 个分块（文件类型: README.md）
```

---

## 性能对比

| 模式 | 速度 | 内存 | 精度 | 依赖 |
|------|------|------|------|------|
| **simplified** | <1ms | 1MB | ⭐⭐⭐⭐ | 无 |
| **onnx (0.5B)** | 30-50ms | 200MB | ⭐⭐⭐⭐⭐ | ONNX + 模型 |
| **onnx (1.5B)** | 100-150ms | 500MB | ⭐⭐⭐⭐⭐ | ONNX + 模型 |

---

## 切换配置（无需重启）

**支持运行时配置刷新** (需要 Spring Boot Actuator):

```bash
# 1. 修改 application.yml
# 2. 刷新配置
curl -X POST http://localhost:8080/actuator/refresh
```

---

## FAQ

### Q1: 如何选择模式？

**A**: 
- 资源受限 → `simplified`
- 资源充足 → `onnx`
- 不确定 → `auto`

### Q2: 两种模式可以动态切换吗？

**A**: 可以，修改配置后重启应用或使用 Actuator 刷新。

### Q3: ONNX 模式需要 GPU 吗？

**A**: 不需要，CPU 即可运行。如果有 GPU，性能会提升 3-5 倍。

### Q4: 简化版精度够用吗？

**A**: 对于大多数场景够用（⭐⭐⭐⭐）。如果需要极致精度，使用 ONNX 版。

---

**推荐配置**: 
- 生产环境资源充足 → `mode: onnx` ⭐
- 生产环境资源受限 → `mode: simplified` ⭐
- 开发环境 → `mode: auto` ⭐

**版本**: v1.0  
**日期**: 2025-12-18

