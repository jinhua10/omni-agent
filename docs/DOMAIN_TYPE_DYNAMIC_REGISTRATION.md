# DomainType 动态注册指南

**版本**: 2.0.0  
**重构日期**: 2025-12-30  
**重大变更**: 从枚举重构为支持动态注册的类

---

## 📋 目录

1. [重构背景](#重构背景)
2. [新设计说明](#新设计说明)
3. [使用指南](#使用指南)
4. [兼容性说明](#兼容性说明)
5. [最佳实践](#最佳实践)

---

## 🔄 重构背景

### 为什么要重构？

**旧设计（枚举）的限制**：

```java
// ❌ 旧设计 - 使用枚举
public enum DomainType {
    DOCUMENT("文档知识域"),
    SOURCE_CODE("源码知识域"),
    ROLE_KNOWLEDGE("角色知识域"),
    API_DOCUMENTATION("API文档域"),
    MIXED("混合域");
    
    // 无法动态添加新类型！
}
```

**问题**：
- ✗ 无法动态注册自定义域类型
- ✗ 每次新增类型都需要修改源码
- ✗ 不支持用户扩展

### 新设计的优势

```java
// ✅ 新设计 - 支持动态注册
public class DomainType {
    private String code;        // 类型代码
    private String name;        // 类型名称
    private String description; // 描述
    private String icon;        // 图标
    private Map<String, Object> metadata; // 扩展属性
    
    // 全局注册表
    private static final Map<String, DomainType> REGISTRY = ...;
    
    // 支持动态注册！
    public static DomainType register(DomainType type) { ... }
}
```

**优势**：
- ✓ 支持动态注册自定义域类型
- ✓ 无需修改源码即可扩展
- ✓ 保留所有预定义类型
- ✓ 完全向后兼容

---

## 🏗️ 新设计说明

### 核心架构

```
DomainType 类
├── 实例字段
│   ├── code (String)          - 类型代码（唯一标识）
│   ├── name (String)          - 类型名称
│   ├── description (String)   - 描述
│   ├── icon (String)          - 图标 Emoji
│   └── metadata (Map)         - 扩展属性
│
├── 全局注册表
│   └── REGISTRY (ConcurrentHashMap) - 线程安全的注册表
│
├── 预定义类型（静态常量）
│   ├── DOCUMENT              - 文档知识域 📄
│   ├── SOURCE_CODE           - 源码知识域 💻
│   ├── ROLE_KNOWLEDGE        - 角色知识域 👤
│   ├── API_DOCUMENTATION     - API文档域 🔌
│   ├── MIXED                 - 混合域 🎯
│   ├── BUSINESS              - 业务知识域 💼
│   ├── TEST                  - 测试知识域 🧪
│   └── OPERATIONS            - 运维知识域 ⚙️
│
└── 动态注册方法
    ├── register()            - 注册新类型
    ├── of()                  - 根据代码获取
    ├── getOrCreate()         - 获取或创建
    ├── isRegistered()        - 检查是否已注册
    ├── getAllTypes()         - 获取所有类型
    ├── unregister()          - 取消注册
    └── clearCustomTypes()    - 清空自定义类型
```

### 对象比较

**重要**: DomainType 实现了 `equals()` 和 `hashCode()`，基于 `code` 字段。

```java
@Override
public boolean equals(Object o) {
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;
    DomainType that = (DomainType) o;
    return Objects.equals(code, that.code);
}

@Override
public int hashCode() {
    return Objects.hash(code);
}
```

**使用时注意**：
- ✅ 使用 `.equals()` 比较：`type1.equals(type2)`
- ❌ 不要使用 `==`：`type1 == type2`（只对预定义常量有效）

---

## 📖 使用指南

### 1. 使用预定义类型

预定义类型可以直接使用，就像以前的枚举一样：

```java
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;

// 创建知识域时使用预定义类型
KnowledgeDomain domain = KnowledgeDomain.builder()
    .domainId("tech-docs-001")
    .domainName("技术文档")
    .domainType(DomainType.DOCUMENT)  // ✅ 使用预定义类型
    .build();

// 类型比较
if (DomainType.DOCUMENT.equals(domain.getDomainType())) {
    System.out.println("这是一个文档域");
}
```

**预定义类型列表**：

| 常量 | 代码 | 名称 | 图标 | 用途 |
|-----|------|------|------|------|
| `DOCUMENT` | `DOCUMENT` | 文档知识域 | 📄 | 技术文档、教程、手册 |
| `SOURCE_CODE` | `SOURCE_CODE` | 源码知识域 | 💻 | 项目源代码、代码片段 |
| `ROLE_KNOWLEDGE` | `ROLE_KNOWLEDGE` | 角色知识域 | 👤 | 特定角色的专业知识 |
| `API_DOCUMENTATION` | `API_DOCUMENTATION` | API文档域 | 🔌 | API接口文档、OpenAPI规范 |
| `MIXED` | `MIXED` | 混合域 | 🎯 | 多种类型知识的综合域 |
| `BUSINESS` | `BUSINESS` | 业务知识域 | 💼 | 业务规则、流程、需求 |
| `TEST` | `TEST` | 测试知识域 | 🧪 | 测试用例、测试策略 |
| `OPERATIONS` | `OPERATIONS` | 运维知识域 | ⚙️ | 部署、监控、运维脚本 |

### 2. 注册自定义类型

**方式一：使用 `register()` 方法**

```java
// 注册自定义域类型
DomainType customType = DomainType.register(
    DomainType.builder()
        .code("SECURITY_AUDIT")
        .name("安全审计域")
        .description("用于存储安全审计日志、漏洞报告等")
        .icon("🔒")
        .metadata(Map.of("priority", "high", "retention", "365days"))
        .build()
);

// 使用自定义类型
KnowledgeDomain domain = KnowledgeDomain.builder()
    .domainId("security-001")
    .domainType(customType)
    .build();
```

**方式二：使用 `getOrCreate()` 方法（推荐）**

```java
// 自动获取或创建
DomainType type = DomainType.getOrCreate("CUSTOMER_DATA", "客户数据域");

// 如果已存在则返回已存在的，否则创建新的
KnowledgeDomain domain = KnowledgeDomain.builder()
    .domainType(type)
    .build();
```

### 3. 查询和管理类型

```java
// 根据代码获取类型
DomainType type = DomainType.of("DOCUMENT");
if (type != null) {
    System.out.println("类型名称: " + type.getName());
}

// 检查类型是否已注册
if (DomainType.isRegistered("SECURITY_AUDIT")) {
    System.out.println("安全审计域已注册");
}

// 获取所有已注册的类型
Map<String, DomainType> allTypes = DomainType.getAllTypes();
allTypes.forEach((code, type) -> {
    System.out.println(code + " -> " + type.getName());
});

// 取消注册（谨慎使用）
DomainType removed = DomainType.unregister("CUSTOM_TYPE");

// 清空所有自定义类型（保留预定义类型）
DomainType.clearCustomTypes();
```

### 4. 高级用法：扩展属性

```java
DomainType type = DomainType.builder()
    .code("ML_MODELS")
    .name("机器学习模型域")
    .description("存储训练好的ML模型")
    .icon("🤖")
    .metadata(Map.of(
        "modelFormat", "ONNX",
        "framework", "PyTorch",
        "version", "1.0"
    ))
    .build();

DomainType.register(type);

// 使用时获取扩展属性
String framework = (String) type.getMetadata().get("framework");
System.out.println("模型框架: " + framework);
```

---

## 🔄 兼容性说明

### 从枚举迁移到类

**旧代码（枚举时代）**：

```java
// 以前的枚举使用方式
DomainType type = DomainType.DOCUMENT;

// 比较
if (type == DomainType.DOCUMENT) { ... }

// 序列化
String typeName = type.name();  // "DOCUMENT"
```

**新代码（类时代）**：

```java
// 现在的类使用方式
DomainType type = DomainType.DOCUMENT;  // ✅ 仍然有效

// 比较 - 改为使用 equals()
if (DomainType.DOCUMENT.equals(type)) { ... }  // ✅ 推荐
if (type.equals(DomainType.DOCUMENT)) { ... }  // ✅ 也可以

// 序列化 - 改为使用 getCode()
String typeCode = type.getCode();  // "DOCUMENT"
```

### 数据库存储的变更

**旧实现（需要修复）**：

```java
// ❌ 旧代码 - 使用 .name()
jdbcTemplate.update(sql, domain.getDomainType().name());
```

**新实现（已修复）**：

```java
// ✅ 新代码 - 使用 .getCode()
jdbcTemplate.update(sql, domain.getDomainType().getCode());
```

**已修复的文件**：
- ✅ `H2KnowledgeRegistry.java`
- ✅ `SQLiteKnowledgeRegistry.java`
- ✅ `ElasticsearchKnowledgeRegistry.java`
- ✅ `MemoryKnowledgeRegistry.java`
- ✅ `RedisKnowledgeRegistry.java`
- ✅ `FileKnowledgeRegistry.java`
- ✅ `DomainRouter.java`

### JSON序列化

DomainType 会被 Jackson 自动序列化为 JSON：

```json
{
  "code": "DOCUMENT",
  "name": "文档知识域",
  "description": "用于存储和管理各类文档知识，如技术文档、教程、手册等",
  "icon": "📄",
  "metadata": {}
}
```

**反序列化时会自动查找已注册的类型**：
- 如果类型已注册，使用注册表中的实例
- 如果类型未注册，创建新实例并自动注册

---

## 💡 最佳实践

### 1. 统一管理自定义类型

建议在应用启动时注册所有自定义类型：

```java
@Configuration
public class DomainTypeConfiguration {
    
    @PostConstruct
    public void registerCustomDomainTypes() {
        // 注册企业特定的域类型
        DomainType.register(
            DomainType.builder()
                .code("CUSTOMER_360")
                .name("客户360视图")
                .description("客户全方位数据")
                .icon("👥")
                .build()
        );
        
        DomainType.register(
            DomainType.builder()
                .code("FINANCIAL_DATA")
                .name("财务数据域")
                .description("财务报表和交易数据")
                .icon("💰")
                .build()
        );
        
        log.info("✅ 自定义域类型注册完成");
    }
}
```

### 2. 使用常量引用自定义类型

```java
public class CustomDomainTypes {
    
    public static final DomainType CUSTOMER_360 = 
        DomainType.getOrCreate("CUSTOMER_360", "客户360视图");
    
    public static final DomainType FINANCIAL_DATA = 
        DomainType.getOrCreate("FINANCIAL_DATA", "财务数据域");
    
    // 防止实例化
    private CustomDomainTypes() {}
}

// 使用
KnowledgeDomain domain = KnowledgeDomain.builder()
    .domainType(CustomDomainTypes.CUSTOMER_360)
    .build();
```

### 3. 类型验证

```java
public void validateDomainType(String typeCode) {
    if (!DomainType.isRegistered(typeCode)) {
        throw new IllegalArgumentException(
            "未知的域类型: " + typeCode + 
            "，请先注册该类型"
        );
    }
}
```

### 4. 动态UI生成

```java
@RestController
@RequestMapping("/api/domain-types")
public class DomainTypeController {
    
    @GetMapping
    public List<DomainTypeDTO> getAllDomainTypes() {
        return DomainType.getAllTypes().values().stream()
            .map(type -> new DomainTypeDTO(
                type.getCode(),
                type.getName(),
                type.getDescription(),
                type.getIcon()
            ))
            .collect(Collectors.toList());
    }
}
```

### 5. 线程安全

DomainType 的注册表使用 `ConcurrentHashMap`，天然线程安全：

```java
// 多线程环境下安全使用
ExecutorService executor = Executors.newFixedThreadPool(10);

for (int i = 0; i < 100; i++) {
    final int index = i;
    executor.submit(() -> {
        DomainType.getOrCreate("CUSTOM_" + index, "自定义类型" + index);
    });
}
```

---

## 🔍 常见问题

### Q1: 预定义类型可以被修改吗？

**A**: 不可以。预定义类型（如 `DOCUMENT`、`SOURCE_CODE` 等）是静态常量，初始化后不可修改。但你可以注册同名的自定义类型来"覆盖"（实际上会返回已存在的实例）。

### Q2: 如何持久化自定义类型？

**A**: DomainType 会随 KnowledgeDomain 一起序列化到数据库（JSON格式）。下次启动时，反序列化会自动注册这些类型。

### Q3: 自定义类型会丢失吗？

**A**: 不会。只要数据库中有使用该类型的 KnowledgeDomain，反序列化时会自动重新注册。

### Q4: 可以删除预定义类型吗？

**A**: 不建议。`clearCustomTypes()` 方法会保留所有预定义类型，只删除自定义类型。

### Q5: 如何升级旧数据？

**A**: 旧数据中的 `DOCUMENT`、`SOURCE_CODE` 等值仍然有效，会自动映射到新的预定义常量。

---

## 📝 迁移检查清单

如果你正在从旧版本迁移，请检查以下项：

- [ ] 所有 `type == DomainType.XXX` 改为 `type.equals(DomainType.XXX)`
- [ ] 所有 `type.name()` 改为 `type.getCode()`
- [ ] 所有 `DomainType.valueOf()` 改为 `DomainType.of()`
- [ ] 数据库查询使用 `.getCode()` 而不是 `.name()`
- [ ] 如有自定义类型需求，使用 `register()` 或 `getOrCreate()`
- [ ] 更新单元测试中的类型比较逻辑

---

## 🎯 总结

**DomainType 重构的核心价值**：

1. **灵活性** - 支持动态注册自定义域类型
2. **扩展性** - 无需修改源码即可添加新类型
3. **兼容性** - 保留所有预定义类型，向后兼容
4. **易用性** - API设计简洁，学习成本低

**适用场景**：

- ✅ 需要为不同行业定制知识域类型
- ✅ 需要在运行时动态创建域类型
- ✅ 需要通过配置文件管理域类型
- ✅ 需要支持多租户的域类型隔离

---

**文档版本**: 2.0.0  
**最后更新**: 2025-12-30  
**作者**: OmniAgent Team

