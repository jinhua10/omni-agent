# OmniAgent 代码规范

**版本：** 1.0  
**更新时间：** 2025-12-31

---

## 🎯 核心原则

1. **简洁优先** - 代码简单明了，避免过度设计
2. **Spring规范** - 严格遵循Spring Boot最佳实践
3. **可测试性** - 所有业务逻辑必须可测试
4. **文档完整** - 公共API必须有JavaDoc

---

## 📝 命名规范

### 类名
- Controller: `XxxController`
- Service: `XxxService`
- Repository/DAO: `XxxRepository`
- Config: `XxxConfig`
- DTO: `XxxDTO` 或 `XxxRequest/Response`

### 方法名
- 查询: `get/find/query + 名词`
- 保存: `save/create/add + 名词`
- 更新: `update/modify + 名词`
- 删除: `delete/remove + 名词`
- 布尔: `is/has/can + 形容词/名词`

### 变量名
- 驼峰命名法，见名知义
- 集合: 复数形式 (`users`, `documents`)
- 布尔: `is/has/can` 前缀

---

## 🏗️ 代码结构

### 包结构
```
top.yumbo.ai.omni.模块名/
├── config/          # 配置类
├── controller/      # 控制器
├── service/         # 业务逻辑
├── repository/      # 数据访问
├── model/          # 数据模型
│   ├── entity/     # 实体类
│   ├── dto/        # 数据传输对象
│   └── vo/         # 视图对象
└── exception/      # 自定义异常
```

### 依赖注入
- ✅ 使用构造器注入（推荐）
- ✅ 使用 `@Autowired` 字段注入（接受）
- ❌ 禁止使用 `new` 创建Spring Bean

---

## 🔧 Spring规范

### Controller
```java
@Slf4j
@RestController
@RequestMapping("/api/xxx")
public class XxxController {
    
    @Autowired
    private XxxService xxxService;
    
    @Autowired
    private MessageService messageService;
    
    @GetMapping("/{id}")
    public ResponseEntity<?> getById(@PathVariable String id) {
        // 日志使用中文
        log.info(messageService.getForLog("log.xxx.query.start", id));
        
        // 业务逻辑
        
        // API响应自动国际化
        return ResponseEntity.ok(Map.of(
            "code", 200,
            "message", messageService.get("api.xxx.success"),
            "data", result
        ));
    }
}
```

### Service
```java
@Slf4j
@Service
public class XxxService {
    
    @Autowired
    private XxxRepository xxxRepository;
    
    public Xxx findById(String id) {
        // 业务逻辑
    }
}
```

---

## 🌍 国际化规范

### 使用方式
- **日志消息**: `messageService.getForLog("log.module.action", args...)`
- **API响应**: `messageService.get("api.module.action.status", args...)`

### 消息key规范
- 日志: `log.模块.操作.级别`
- API: `api.模块.操作.状态`

### 示例
```java
// 日志（统一中文）
log.info(messageService.getForLog("log.document.upload.start", filename));

// API响应（自动国际化）
messageService.get("api.document.upload.success")
```

---

## 🧪 测试规范

### 单元测试
- 测试类命名: `XxxTest`
- 测试方法命名: `test方法名_场景_预期结果`
- 覆盖率要求: **≥70%**

### 示例
```java
@Test
void testFindById_whenExists_returnsUser() {
    // given
    // when
    // then
}
```

---

## 📚 JavaDoc规范

### 必须添加JavaDoc
- ✅ 所有public类
- ✅ 所有public方法
- ✅ 重要的private方法

### 示例
```java
/**
 * 上传文档
 *
 * @param file 文档文件
 * @return 文档ID
 * @throws IllegalArgumentException 文件格式不支持
 */
public String uploadDocument(MultipartFile file) {
    // ...
}
```

---

## ⚠️ 禁止事项

- ❌ 禁止使用魔法数字（使用常量）
- ❌ 禁止捕获异常后不处理（至少打印日志）
- ❌ 禁止在循环中执行数据库操作
- ❌ 禁止使用 `System.out.println`（使用日志）
- ❌ 禁止硬编码配置（使用配置文件）
- ❌ 禁止硬编码国际化消息（使用MessageService）

---

## ✅ 最佳实践

### 异常处理
```java
try {
    // 业务逻辑
} catch (Exception e) {
    log.error(messageService.getForLog("log.xxx.failed", e.getMessage()), e);
    throw new BusinessException("xxx.failed", e);
}
```

### 返回值
```java
// ✅ 统一返回格式
return ResponseEntity.ok(Map.of(
    "code", 200,
    "message", messageService.get("api.success"),
    "data", result
));

// ❌ 避免直接返回实体
return user; // 不推荐
```

### 空值处理
```java
// ✅ 使用Optional
Optional<User> user = userRepository.findById(id);

// ✅ 提前判断
if (user == null) {
    throw new NotFoundException("user.notfound");
}
```

---

## 📊 代码质量

### 方法复杂度
- 单个方法行数: **≤50行**
- 参数个数: **≤5个**
- 嵌套层级: **≤3层**

### 类复杂度
- 单个类行数: **≤500行**
- 依赖注入字段: **≤10个**

---

## 🔄 版本记录

| 版本 | 日期 | 说明 |
|------|------|------|
| 1.0 | 2025-12-31 | 初始版本，基础规范 |

---

**注意：** 随着批次分析的进行，本规范会持续完善和更新。

