# OmniAgent 打包部署指南

## 🎯 打包可执行JAR

### 方式1：打包单个模块（推荐）

```bash
# 进入项目根目录
cd D:\Jetbrains\omni-agent

# 打包 basic 示例模块（包含所有依赖）
mvn clean package -pl omni-agent-example-basic -am -DskipTests
```

**说明**:
- `-pl omni-agent-example-basic`: 指定要打包的模块
- `-am`: also-make，同时构建该模块依赖的其他模块
- `-DskipTests`: 跳过测试，加快构建速度

### 方式2：打包整个项目

```bash
# 打包所有模块
mvn clean package -DskipTests
```

## 📦 打包产物

### 位置

```
omni-agent-example-basic/target/
├── omni-agent-example-basic-1.0.0.jar           # ⭐ 可执行JAR（包含所有依赖）
├── omni-agent-example-basic-1.0.0.jar.original  # 原始JAR（不含依赖）
└── classes/                                      # 编译后的class文件
```

### 文件说明

| 文件 | 大小 | 说明 |
|------|------|------|
| `omni-agent-example-basic-1.0.0.jar` | ~80MB | **可执行JAR**，包含所有依赖，可直接运行 |
| `omni-agent-example-basic-1.0.0.jar.original` | ~50KB | 原始JAR，仅包含本模块代码 |

## 🚀 运行JAR包

### 本地运行

```bash
# 进入target目录
cd omni-agent-example-basic/target

# 运行JAR包
java -jar omni-agent-example-basic-1.0.0.jar

# 指定端口运行
java -jar omni-agent-example-basic-1.0.0.jar --server.port=8080

# 指定配置文件
java -jar omni-agent-example-basic-1.0.0.jar --spring.config.location=application.yml
```

### 生产环境运行

```bash
# 后台运行，日志输出到文件
nohup java -jar omni-agent-example-basic-1.0.0.jar --server.port=8080 > app.log 2>&1 &

# 查看日志
tail -f app.log

# 查看进程
ps aux | grep omni-agent

# 停止进程
kill <PID>
```

### 使用systemd管理（推荐）

创建 `/etc/systemd/system/omni-agent.service`:

```ini
[Unit]
Description=OmniAgent Backend Service
After=network.target

[Service]
Type=simple
User=root
WorkingDirectory=/root/omni-agent
ExecStart=/usr/bin/java -jar omni-agent-example-basic-1.0.0.jar --server.port=8080
Restart=on-failure
RestartSec=10
StandardOutput=append:/root/omni-agent/app.log
StandardError=append:/root/omni-agent/app.log

[Install]
WantedBy=multi-user.target
```

管理命令:
```bash
# 启动服务
sudo systemctl start omni-agent

# 停止服务
sudo systemctl stop omni-agent

# 重启服务
sudo systemctl restart omni-agent

# 查看状态
sudo systemctl status omni-agent

# 开机自启
sudo systemctl enable omni-agent

# 查看日志
sudo journalctl -u omni-agent -f
```

## 📤 部署到服务器

### 上传JAR包

```bash
# 使用scp上传
scp omni-agent-example-basic/target/omni-agent-example-basic-1.0.0.jar root@yumbo.top:/root/omni-agent/

# 或使用rsync
rsync -avz omni-agent-example-basic/target/*.jar root@yumbo.top:/root/omni-agent/
```

### 服务器目录结构

```
/root/omni-agent/
├── omni-agent-example-basic-1.0.0.jar  # JAR包
├── application.yml                      # 配置文件（可选）
├── data/                                # 数据目录
│   ├── documents/                       # 文档存储
│   ├── rag-index/                       # RAG索引
│   └── workflows/                       # 工作流
└── logs/                                # 日志目录
    └── app.log
```

## ⚙️ Spring Boot Maven插件配置

`omni-agent-example-basic/pom.xml` 已配置：

```xml
<build>
    <finalName>${project.artifactId}-${project.version}</finalName>
    <plugins>
        <plugin>
            <groupId>org.springframework.boot</groupId>
            <artifactId>spring-boot-maven-plugin</artifactId>
            <configuration>
                <mainClass>top.yumbo.ai.omni.example.basic.BasicExampleApplication</mainClass>
                <!-- 包含所有依赖 -->
                <includeSystemScope>true</includeSystemScope>
                <!-- 生成可执行JAR -->
                <executable>true</executable>
                <!-- 排除devtools -->
                <excludes>
                    <exclude>
                        <groupId>org.springframework.boot</groupId>
                        <artifactId>spring-boot-devtools</artifactId>
                    </exclude>
                </excludes>
            </configuration>
            <executions>
                <execution>
                    <goals>
                        <!-- repackage目标将所有依赖打包进JAR -->
                        <goal>repackage</goal>
                    </goals>
                </execution>
            </executions>
        </plugin>
    </plugins>
</build>
```

### 关键配置说明

| 配置项 | 说明 |
|--------|------|
| `mainClass` | 指定主类 |
| `includeSystemScope` | 包含system scope的依赖 |
| `executable` | 生成可执行JAR（Linux可直接`./xxx.jar`运行） |
| `repackage` | 将所有依赖打包到一个JAR中 |

## 🔍 验证打包结果

### 1. 检查JAR包大小

```bash
cd omni-agent-example-basic/target
ls -lh *.jar
```

**预期**:
```
omni-agent-example-basic-1.0.0.jar          ~80MB
omni-agent-example-basic-1.0.0.jar.original ~50KB
```

### 2. 查看JAR包内容

```bash
# 列出JAR包内容
jar tf omni-agent-example-basic-1.0.0.jar | head -20

# 查看MANIFEST.MF
unzip -p omni-agent-example-basic-1.0.0.jar META-INF/MANIFEST.MF
```

**预期包含**:
```
BOOT-INF/classes/          # 应用代码
BOOT-INF/lib/              # 所有依赖JAR
org/springframework/boot/  # Spring Boot Loader
META-INF/MANIFEST.MF       # 清单文件
```

### 3. 测试运行

```bash
java -jar omni-agent-example-basic-1.0.0.jar --spring.profiles.active=test
```

**预期输出**:
```
  .   ____          _            __ _ _
 /\\ / ___'_ __ _ _(_)_ __  __ _ \ \ \ \
( ( )\___ | '_ | '_| | '_ \/ _` | \ \ \ \
 \\/  ___)| |_)| | | | | || (_| |  ) ) ) )
  '  |____| .__|_| |_|_| |_\__, | / / / /
 =========|_|==============|___/=/_/_/_/

Started BasicExampleApplication in X.XXX seconds
```

## 📊 包含的依赖模块

打包的JAR包含以下所有模块：

### 核心模块
- ✅ `omni-agent-core` - 核心功能
- ✅ `omni-agent-common` - 通用工具
- ✅ `omni-agent-web` - Web控制器

### API模块
- ✅ `omni-agent-document-storage-api` - 文档存储API
- ✅ `omni-agent-rag-api` - RAG检索API
- ✅ `omni-agent-ai-api` - AI服务API
- ✅ `omni-agent-chunking-api` - 分块API
- ✅ `omni-agent-document-processor-api` - 文档处理API
- ✅ `omni-agent-knowledge-registry-api` - 知识注册API

### Starter实现
- ✅ `omni-agent-document-storage-starter` - 文档存储实现
- ✅ `omni-agent-rag-starter-adapter` - RAG检索实现
- ✅ `omni-agent-ai-starter` - AI服务实现
- ✅ `omni-agent-chunking-starter` - 分块实现
- ✅ `omni-agent-document-processor-starter` - 文档处理实现
- ✅ `omni-agent-knowledge-registry-starter` - 知识注册实现

### 其他模块
- ✅ `omni-agent-workflow` - 工作流引擎
- ✅ `omni-agent-marketplace` - 算法市场

### 第三方依赖
- Spring Boot 3.4.1
- Ant Design (通过Web模块)
- Apache Lucene
- Apache POI
- Apache PDFBox
- OkHttp3
- Jackson
- Lombok
- 等...

## 🛠️ 常见问题

### 1. 打包失败：编译错误

**问题**: Maven编译失败

**解决**:
```bash
# 清理并重新编译
mvn clean compile

# 如果仍然失败，检查Java版本
java -version  # 需要Java 21
```

### 2. JAR包过大

**问题**: JAR包超过100MB

**原因**: 包含了所有依赖（正常现象）

**优化**:
- 使用`spring-boot-thin-layout`创建瘦JAR
- 或使用Docker镜像部署

### 3. 运行时找不到主类

**问题**: `no main manifest attribute`

**解决**: 确保使用了Spring Boot Maven插件的repackage目标

### 4. 依赖冲突

**问题**: 运行时类加载错误

**解决**:
```bash
# 查看依赖树
mvn dependency:tree

# 排除冲突依赖
# 在pom.xml中使用<exclusions>
```

## ✅ 打包检查清单

- [ ] Java 21已安装
- [ ] Maven 3.8+已安装
- [ ] 执行`mvn clean package -DskipTests`
- [ ] target目录生成JAR包
- [ ] JAR包大小正常（~80MB）
- [ ] 本地测试运行成功
- [ ] 上传到服务器
- [ ] 服务器运行成功
- [ ] API接口正常
- [ ] 日志无错误

## 📝 相关文档

- [生产部署指南](PRODUCTION_DEPLOYMENT_GUIDE.md)
- [Nginx配置](../nginx-production.conf)
- [Spring Boot文档](https://docs.spring.io/spring-boot/docs/current/reference/html/)

---

**更新时间**: 2025-12-29  
**JAR包名称**: `omni-agent-example-basic-1.0.0.jar`  
**包含依赖**: 是  
**可执行**: 是

