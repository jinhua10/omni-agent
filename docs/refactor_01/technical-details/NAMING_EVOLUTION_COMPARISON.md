# 命名演化对比

> 从 Persistence 到 KnowledgeRegistry 的语义化改进

---

## 📊 命名对比

### 阶段 1: 原始方案（❌ 语义不清）

```java
// 接口名称
QuestionClassifierPersistence

// 使用示例
@Service
public class KnowledgeDomainService {
    private final QuestionClassifierPersistence persistence;
    
    public void save(KnowledgeDomain domain) {
        // ❌ 语义混乱：用 QuestionClassifier 存储 KnowledgeDomain？
        persistence.saveQuestionType(config);
    }
}
```

**问题：**
- ❌ `QuestionClassifierPersistence` 看起来只用于问题分类
- ❌ 实际却用来存储知识域，语义不匹配
- ❌ 代码可读性差

---

### 阶段 2: 适配器方案（⚠️ 冗长但可用）

```java
// 适配器
KnowledgeDomainPersistenceAdapter

// 使用示例
@Service
public class KnowledgeDomainService {
    private final KnowledgeDomainPersistenceAdapter domainAdapter;
    
    public void save(KnowledgeDomain domain) {
        // ⚠️ 名称冗长，但语义稍好
        domainAdapter.save(domain);
    }
}
```

**问题：**
- ⚠️ 名称过长：`KnowledgeDomainPersistenceAdapter`
- ⚠️ 每种实体需要单独的适配器
- ⚠️ 扩展性差

---

### 阶段 3: 知识注册表方案（✅ 完美）

```java
// 接口名称
KnowledgeRegistry

// 使用示例
@Service
public class KnowledgeDomainService {
    private final KnowledgeRegistry knowledgeRegistry;
    
    public void save(KnowledgeDomain domain) {
        // ✅ 语义清晰：保存到知识注册表
        knowledgeRegistry.save("knowledge-domain", domain.getId(), domain);
    }
}
```

**优势：**
- ✅ 名称简洁清晰
- ✅ 语义完美契合
- ✅ 可扩展到多种实体类型

---

## 🎯 完整对比表

| 维度 | QuestionClassifierPersistence | KnowledgeDomainPersistenceAdapter | KnowledgeRegistry |
|------|------------------------------|----------------------------------|-------------------|
| **语义清晰度** | ❌ 差 | ⚠️ 中等 | ✅ 优秀 |
| **名称长度** | 中等 | ❌ 过长 | ✅ 简洁 |
| **扩展性** | ❌ 差 | ⚠️ 中等 | ✅ 优秀 |
| **可读性** | ❌ 差 | ⚠️ 中等 | ✅ 优秀 |
| **维护性** | ⚠️ 中等 | ⚠️ 中等 | ✅ 优秀 |

---

## 📈 使用场景对比

### 场景 1: 保存知识域

```java
// ❌ 阶段 1
persistence.saveQuestionType(config);
// "为什么用 QuestionType 保存域？"

// ⚠️ 阶段 2
domainAdapter.save(domain);
// "还行，但名称太长"

// ✅ 阶段 3
knowledgeRegistry.save("knowledge-domain", id, domain);
// "完美！一目了然"
```

### 场景 2: 保存多种实体

```java
// ❌ 阶段 1 - 无法扩展
persistence.saveQuestionType(config);

// ⚠️ 阶段 2 - 需要多个适配器
domainAdapter.save(domain);
roleAdapter.save(role);
projectAdapter.save(project);

// ✅ 阶段 3 - 统一接口
knowledgeRegistry.save("knowledge-domain", id, domain);
knowledgeRegistry.save("knowledge-role", id, role);
knowledgeRegistry.save("source-project", id, project);
```

### 场景 3: 新人理解代码

```java
// ❌ 阶段 1
QuestionClassifierPersistence persistence;
// 新人："这是干什么的？只用于问题分类吗？"

// ⚠️ 阶段 2
KnowledgeDomainPersistenceAdapter adapter;
// 新人："还行，但为什么叫 Adapter？"

// ✅ 阶段 3
KnowledgeRegistry registry;
// 新人："明白了！知识注册表，用来存储知识相关的东西"
```

---

## 🏗️ 架构层次对比

### 阶段 1（原始）

```
Service
  └── QuestionClassifierPersistence
      └── Persistence Starter (6 种)
      
问题：语义混乱，职责不清
```

### 阶段 2（适配器）

```
Service
  └── KnowledgeDomainPersistenceAdapter
      └── QuestionClassifierPersistence
          └── Persistence Starter (6 种)
          
问题：多一层适配，冗长
```

### 阶段 3（注册表）✅

```
Service
  └── KnowledgeRegistry (抽象层)
      └── PersistenceBasedKnowledgeRegistry (实现层)
          └── QuestionClassifierPersistence (复用层)
              └── Persistence Starter (6 种)
              
优势：层次清晰，语义完美
```

---

## 💡 命名哲学

### 好的命名应该：

1. **见名知意**
   ```java
   KnowledgeRegistry  ✅  // 一看就知道是知识注册表
   QuestionClassifierPersistence  ❌  // 看起来只管问题分类
   ```

2. **简洁明了**
   ```java
   KnowledgeRegistry  ✅  // 17 个字符
   KnowledgeDomainPersistenceAdapter  ❌  // 37 个字符
   ```

3. **扩展性强**
   ```java
   // ✅ 可以存储多种实体
   knowledgeRegistry.save("knowledge-domain", ...);
   knowledgeRegistry.save("knowledge-role", ...);
   
   // ❌ 只能存储问题类型
   persistence.saveQuestionType(...);
   ```

4. **符合领域模型**
   ```java
   // ✅ 知识网络 → 知识注册表
   KnowledgeRegistry
   
   // ❌ 知识网络 → 问题分类持久化？
   QuestionClassifierPersistence
   ```

---

## 📋 迁移建议

### 渐进式迁移（推荐）

**Week 1: 创建新接口**
```java
// 创建 KnowledgeRegistry 接口
// 实现 PersistenceBasedKnowledgeRegistry
// 编写测试
```

**Week 2: 逐步迁移**
```java
// 新代码使用 KnowledgeRegistry
// 旧代码继续使用 QuestionClassifierPersistence
// 逐步替换
```

**Week 3: 完全迁移**
```java
// 所有代码使用 KnowledgeRegistry
// 标记 QuestionClassifierPersistence 为 @Deprecated
```

**Week 4+: 清理（可选）**
```java
// 移除旧接口（如果不影响兼容性）
```

---

## 🎁 最终方案

### 推荐：KnowledgeRegistry ✅

```java
/**
 * 知识注册表接口
 * 用于存储和管理各种知识相关的元数据
 * 
 * 支持的实体类型：
 * - knowledge-domain: 知识域
 * - knowledge-role: 知识角色
 * - source-project: 源码项目
 * - question-classifier: 问题分类配置（向后兼容）
 */
public interface KnowledgeRegistry {
    <T> String save(String entityType, String entityId, T entity);
    <T> Optional<T> findById(String entityType, String entityId, Class<T> clazz);
    <T> List<T> findAll(String entityType, Class<T> clazz);
    <T> boolean update(String entityType, String entityId, T entity);
    boolean delete(String entityType, String entityId);
}
```

**理由：**
- ✅ 语义完美契合知识网络
- ✅ 名称简洁易懂
- ✅ 扩展性极强
- ✅ 可读性优秀
- ✅ 维护成本低

---

**更新时间：** 2025-12-27  
**决策：** 采用 KnowledgeRegistry 命名  
**作者：** OmniAgent 命名标准化组

