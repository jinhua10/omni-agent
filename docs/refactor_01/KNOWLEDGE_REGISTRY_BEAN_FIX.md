# ✅ 修复 KnowledgeRegistry Bean 缺失问题

## 问题描述

启动应用时出现错误：
```
Parameter 0 of constructor in top.yumbo.ai.omni.core.router.DomainRouter 
required a bean of type 'top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry' 
that could not be found.
```

## 问题原因

`DomainRouter` 需要注入 `KnowledgeRegistry` bean，但 Spring 容器中没有找到该 bean。

**根本原因：** `omni-agent-knowledge-registry-starter-file` 模块缺少 Spring Boot 自动配置注册文件。

虽然 `FileKnowledgeRegistryAutoConfiguration` 类存在，但 Spring Boot 无法自动发现它，因为缺少 `META-INF/spring.factories` 文件。

## 修复方案

### 创建 Spring Boot 自动配置注册文件

**文件位置：**
```
omni-agent-knowledge-registry-starter-file/
└── src/main/resources/META-INF/
    └── spring.factories
```

**文件内容：**
```properties
org.springframework.boot.autoconfigure.EnableAutoConfiguration=\
top.yumbo.ai.omni.knowledge.registry.file.FileKnowledgeRegistryAutoConfiguration
```

这样 Spring Boot 就能自动发现并加载 `FileKnowledgeRegistryAutoConfiguration`，从而创建 `KnowledgeRegistry` bean。

## 自动配置说明

`FileKnowledgeRegistryAutoConfiguration` 的配置逻辑：

```java
@Configuration
@ConditionalOnProperty(
        prefix = "omni-agent.knowledge-registry",
        name = "type",
        havingValue = "file",
        matchIfMissing = true  // 默认使用 file 类型
)
@EnableConfigurationProperties(FileKnowledgeRegistryProperties.class)
public class FileKnowledgeRegistryAutoConfiguration {

    @Bean
    @ConditionalOnMissingBean(KnowledgeRegistry.class)
    public KnowledgeRegistry knowledgeRegistry(FileKnowledgeRegistryProperties properties) {
        return new FileKnowledgeRegistry(
                properties.getBasePath(),
                properties.isPrettyPrint()
        );
    }
}
```

**配置说明：**
- `matchIfMissing = true` - 如果没有配置 `type`，默认使用 `file` 类型
- `@ConditionalOnMissingBean` - 如果用户没有自定义 `KnowledgeRegistry`，则使用默认实现
- 支持通过配置文件自定义存储路径和格式

## 配置选项

### 使用默认配置

不需要任何配置，自动使用文件存储：
```yaml
# application.yml
# 不需要配置，自动生效
```

### 自定义存储路径

```yaml
omni-agent:
  knowledge-registry:
    type: file
    base-path: ./data/knowledge-network/registry
    pretty-print: true
```

### 切换到其他实现

如果需要使用其他存储后端：

**MongoDB：**
```yaml
omni-agent:
  knowledge-registry:
    type: mongodb
    # MongoDB 配置...
```

**Redis：**
```yaml
omni-agent:
  knowledge-registry:
    type: redis
    # Redis 配置...
```

**H2/SQLite：**
```yaml
omni-agent:
  knowledge-registry:
    type: h2
    # H2 配置...
```

## 验证

修复后，应用启动时应该看到日志：
```
🚀 初始化文件知识注册表
   - 存储路径: ./data/knowledge-network/registry
   - 格式化输出: true
```

并且 `DomainRouter` 能正常注入 `KnowledgeRegistry` bean。

## 相关模块

已实现的 `KnowledgeRegistry` 实现：
1. ✅ `FileKnowledgeRegistry` - 基于文件（默认）
2. ✅ `MemoryKnowledgeRegistry` - 基于内存
3. ✅ `H2KnowledgeRegistry` - 基于 H2 数据库
4. ✅ `SQLiteKnowledgeRegistry` - 基于 SQLite
5. ✅ `MongoKnowledgeRegistry` - 基于 MongoDB
6. ✅ `RedisKnowledgeRegistry` - 基于 Redis
7. ✅ `ElasticsearchKnowledgeRegistry` - 基于 Elasticsearch

所有实现都需要在各自的 starter 模块中创建 `META-INF/spring.factories` 文件。

## 状态

✅ **已修复** - `FileKnowledgeRegistry` 现在可以正常注入
✅ **已编译通过**
✅ **应用可以正常启动**

---

**修复时间：** 2025-12-27  
**影响范围：** `omni-agent-knowledge-registry-starter-file` 模块  
**修复类型：** 添加 Spring Boot 自动配置注册文件

