# ONNX依赖编译错误修复

## 🐛 问题描述

**错误信息**:
```
D:\Jetbrains\omni-agent\omni-agent-rag-starter-adapter\src\main\java\top\yumbo\ai\omni\rag\adapter\embedding\OnnxEmbeddingServiceFactory.java:34:49 
java: 无法访问ai.onnxruntime.OrtException
找不到ai.onnxruntime.OrtException的类文件
```

## 🔍 根本原因

在 `omni-agent-rag-starter-adapter/pom.xml` 中，ONNX Runtime依赖被标记为 `<optional>true</optional>`：

```xml
<dependency>
    <groupId>com.microsoft.onnxruntime</groupId>
    <artifactId>onnxruntime</artifactId>
    <version>1.16.0</version>
    <optional>true</optional>  ❌ 问题所在
</dependency>
```

### Maven Optional依赖的含义

- `<optional>true</optional>` 表示该依赖是可选的
- 当其他模块依赖此模块时，**不会传递**这个依赖
- 但在**编译时**仍然需要这个类，导致编译失败

### 为什么会有这个问题

1. `OnnxEmbeddingServiceFactory` 直接使用了 `OnnxEmbeddingService`
2. `OnnxEmbeddingService` 的构造函数抛出 `OrtException`
3. 编译器需要 `OrtException` 的类定义
4. 但因为 `optional=true`，依赖在编译时可能被忽略

## ✅ 解决方案

### 修复的文件

**文件**: `omni-agent-rag-starter-adapter/pom.xml`

**修改前**:
```xml
<dependency>
    <groupId>com.microsoft.onnxruntime</groupId>
    <artifactId>onnxruntime</artifactId>
    <version>1.16.0</version>
    <optional>true</optional>  ❌
</dependency>
```

**修改后**:
```xml
<dependency>
    <groupId>com.microsoft.onnxruntime</groupId>
    <artifactId>onnxruntime</artifactId>
    <version>1.16.0</version>
</dependency>
```

### 为什么这样修复

1. **ONNX是核心功能**: 项目中多处使用了ONNX嵌入服务
   - `OnnxEmbeddingServiceFactory`
   - `SemanticStrategy` (语义分块)
   - `OnnxEmbeddingAutoConfiguration`

2. **不应该是可选的**: ONNX Runtime是向量化的核心依赖，RAG系统必须要有

3. **依赖传递正确**: 其他依赖此模块的项目也会自动获得ONNX依赖

## 📊 影响范围

### 受影响的模块

| 模块 | 用途 | ONNX用途 |
|------|------|----------|
| `omni-agent-rag-starter-adapter` | RAG适配器 | ONNX向量化 ✅ |
| `omni-agent-chunking-starter` | 分块策略 | 语义分块 ✅ |
| `omni-agent-ai-starter` | AI服务 | 嵌入服务 ✅ |

### 依赖关系

```
omni-agent-rag-starter-adapter
  └─ onnxruntime (1.16.0) ✅ 现在是必需依赖
  
omni-agent-chunking-starter
  └─ omni-agent-ai-starter
      └─ onnxruntime (1.16.0) ✅ 通过传递依赖
```

## ✅ 验证结果

```bash
mvn clean compile -pl omni-agent-rag-starter-adapter -am
```

**输出**:
```
[INFO] BUILD SUCCESS
```

✅ 编译成功，问题已解决！

## 🎯 其他optional依赖检查

让我们检查是否有其他类似的问题。项目中其他optional依赖：

### 合理的optional依赖

这些依赖确实应该是optional的（特定场景才需要）：

```xml
<!-- Lucene - 只在使用File RAG时需要 -->
<dependency>
    <groupId>org.apache.lucene</groupId>
    <artifactId>lucene-core</artifactId>
    <optional>true</optional>  ✅ 合理
</dependency>

<!-- MongoDB - 只在使用MongoDB RAG时需要 -->
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-mongodb</artifactId>
    <optional>true</optional>  ✅ 合理
</dependency>

<!-- Redis - 只在使用Redis RAG时需要 -->
<dependency>
    <groupId>org.springframework.boot</groupId>
    <artifactId>spring-boot-starter-data-redis</artifactId>
    <optional>true</optional>  ✅ 合理
</dependency>
```

### 不合理的optional依赖（已修复）

```xml
<!-- ONNX - RAG核心功能，不应该是optional -->
<dependency>
    <groupId>com.microsoft.onnxruntime</groupId>
    <artifactId>onnxruntime</artifactId>
    <optional>true</optional>  ❌ 已修复为必需依赖
</dependency>
```

## 📝 最佳实践

### 何时使用 optional=true

1. **插件式功能** - 可选的扩展功能
2. **多选一场景** - 例如多种数据库只需要一种
3. **特定环境** - 某些环境才需要的依赖

### 何时不使用 optional=true

1. **核心功能** - 系统必需的依赖
2. **直接使用** - 代码中直接import的类
3. **编译时需要** - 编译器需要的类定义

## 🚀 后续建议

1. **全项目扫描**: 检查所有pom.xml中的optional依赖
2. **依赖分析**: 使用 `mvn dependency:tree` 分析依赖关系
3. **编译验证**: 定期执行 `mvn clean compile` 确保编译正常

## 📌 命令参考

```bash
# 编译单个模块
mvn clean compile -pl omni-agent-rag-starter-adapter

# 编译模块及其依赖
mvn clean compile -pl omni-agent-rag-starter-adapter -am

# 编译整个项目
mvn clean compile

# 查看依赖树
mvn dependency:tree -pl omni-agent-rag-starter-adapter

# 分析依赖
mvn dependency:analyze
```

---

**修复时间**: 2025-12-30  
**问题类型**: Maven依赖配置错误  
**严重程度**: 高（阻止编译）  
**状态**: ✅ 已解决  
**影响**: 无（仅修复了配置错误）

