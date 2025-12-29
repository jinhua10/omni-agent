# POM依赖统一管理重构报告

**重构日期**: 2025-12-29  
**重构目标**: 将所有子模块的依赖版本统一到父pom.xml管理

---

## 🎯 重构内容

### 1. 父pom.xml更新

#### 1.1 新增Properties版本管理

在 `<properties>` 中添加了以下版本号统一管理：

```xml
<!-- HTTP客户端 -->
<okhttp.version>4.12.0</okhttp.version>

<!-- 数据库相关 -->
<mongodb.version>5.2.1</mongodb.version>
<jedis.version>5.2.0</jedis.version>
<h2.version>2.3.232</h2.version>
<sqlite.version>3.47.1.0</sqlite.version>
<elasticsearch.version>8.17.0</elasticsearch.version>

<!-- AI模型相关 -->
<onnxruntime.version>1.20.1</onnxruntime.version>

<!-- OCR相关 -->
<tesseract.version>5.15.0</tesseract.version>

<!-- 云存储 -->
<minio.version>8.5.14</minio.version>
<aws-s3.version>1.12.778</aws-s3.version>

<!-- 工具类 -->
<commons-io.version>2.18.0</commons-io.version>
<commons-lang3.version>3.17.0</commons-lang3.version>
<commons-compress.version>1.27.1</commons-compress.version>
<guava.version>33.3.1-jre</guava.version>
```

#### 1.2 新增DependencyManagement

在 `<dependencyManagement>` 中添加了：

**第三方依赖**：
- OkHttp3
- MongoDB Driver
- Redis Jedis
- H2 Database
- SQLite JDBC
- Elasticsearch Java Client
- ONNX Runtime
- Tesseract OCR
- MinIO
- AWS S3 SDK
- Apache Commons IO
- Apache Commons Lang3
- Google Guava

**项目内部模块**（共16个）：
- omni-agent-common
- omni-agent-core
- omni-agent-document-storage-api
- omni-agent-document-storage-starter
- omni-agent-document-processor-api
- omni-agent-document-processor-starter
- omni-agent-chunking-api
- omni-agent-chunking-starter
- omni-agent-rag-api
- omni-agent-rag-starter-adapter
- omni-agent-ai-api
- omni-agent-ai-starter
- omni-agent-knowledge-registry-api
- omni-agent-knowledge-registry-starter
- omni-agent-p2p-api
- omni-agent-p2p-starter
- omni-agent-workflow
- omni-agent-marketplace
- omni-agent-ocr-starter-tesseract

---

### 2. 子模块pom.xml更新

已修改以下子模块，移除了硬编码版本号：

#### 2.1 omni-agent-common
- ✅ 移除 `okhttp` 的版本号 `4.12.0`

#### 2.2 omni-agent-web
- ✅ 移除 `okhttp` 的版本号 `4.12.0`
- ✅ 移除 `omni-agent-document-processor-starter` 的版本号 `1.0.0`
- ✅ 移除 `omni-agent-chunking-starter` 的版本号 `1.0.0`

#### 2.3 omni-agent-marketplace
- ✅ 移除 `okhttp` 的版本号 `4.12.0`

#### 2.4 omni-agent-example-basic
- ✅ 移除 `okhttp` 的版本号 `4.12.0`

---

## ✨ 优势

### 1. **集中管理**
所有依赖版本在父pom.xml的一个地方管理，便于维护和升级。

### 2. **版本一致性**
确保所有子模块使用相同版本的依赖，避免版本冲突。

### 3. **简化子模块**
子模块pom.xml更加简洁，只需要声明依赖的groupId和artifactId，不需要关心版本号。

### 4. **易于升级**
当需要升级某个依赖时，只需在父pom.xml修改一处即可，所有子模块自动生效。

### 5. **安全性**
统一管理安全补丁版本，如Logback从1.4.x升级到1.5.19修复CVE-2025-11226。

---

## 📋 使用方法

### 子模块引用依赖的标准格式

**之前（硬编码版本）**：
```xml
<dependency>
    <groupId>com.squareup.okhttp3</groupId>
    <artifactId>okhttp</artifactId>
    <version>4.12.0</version>
</dependency>
```

**现在（从父pom继承版本）**：
```xml
<dependency>
    <groupId>com.squareup.okhttp3</groupId>
    <artifactId>okhttp</artifactId>
</dependency>
```

### 项目内部模块引用

**之前（硬编码版本）**：
```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-common</artifactId>
    <version>1.0.0</version>
</dependency>
```

**现在（从父pom继承版本）**：
```xml
<dependency>
    <groupId>top.yumbo.ai.omni</groupId>
    <artifactId>omni-agent-common</artifactId>
</dependency>
```

---

## 🔍 待处理项

其他子模块中可能还存在硬编码版本号，建议按照以下步骤继续清理：

### 1. 查找硬编码版本
```bash
# 在各个子模块pom.xml中查找版本标签
grep -r "<version>" */pom.xml | grep -v "project.version" | grep -v "parent"
```

### 2. 常见需要清理的依赖
- Jackson系列（jackson-databind, jackson-core等）
- 数据库驱动（mongodb-driver-sync, jedis, h2, sqlite-jdbc）
- AI相关（onnxruntime）
- 文档处理（poi-ooxml, pdfbox, tika-core）

### 3. 建议清理顺序
1. API模块（最简单，依赖最少）
2. Starter模块（中等复杂度）
3. 应用模块（最复杂，依赖最多）

---

## ✅ 验证

### 编译验证
```bash
mvn clean compile
```

### 依赖树查看
```bash
mvn dependency:tree
```

### 版本冲突检查
```bash
mvn dependency:analyze
```

---

## 📝 备注

1. **Optional依赖**：某些依赖标记为 `<optional>true</optional>`，表示该依赖是可选的，不会传递给依赖此模块的其他模块。

2. **Scope设置**：
   - `compile`：默认范围，编译和运行时都需要
   - `test`：仅测试时需要
   - `provided`：编译时需要，运行时由容器提供（如Lombok）

3. **版本号占位符**：使用 `${project.version}` 表示当前项目版本，确保所有内部模块版本一致。

---

**重构完成状态**: 🟡 部分完成（已完成4个关键模块，建议继续清理其他模块）

**下一步建议**: 逐个检查其他子模块，移除所有硬编码的版本号，统一由父pom管理。

