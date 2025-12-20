# ✅ 重排序失败问题修复

## 🐛 问题描述

重排序时出现类型转换错误：

```
ERROR t.y.a.o.m.EnhancedQueryService - ❌ 重排序失败: class java.util.HashMap cannot be cast to class java.util.List
```

## 🔍 问题根因

### 错误的实现（修复前）

`EnhancedQueryService.performRerank()` 方法传入的 `input` 参数格式不正确：

```java
// ❌ 错误：input 是一个 HashMap
Map<String, Object> input = new HashMap<>();
input.put("query", question);
input.put("documents", documents);  // List<String>

component.execute(input, params);  // input 是 Map
```

### 期望的格式

`AlgorithmMarketService.createRerankComponent()` 期望的 `input` 是一个 `List<Map<String, Object>>`（搜索结果列表）：

```java
// ✅ 正确：input 应该是 List<Map>
List<Map<String, Object>> searchResults = (List<Map<String, Object>>) input;
```

## ✅ 修复方案

### 修改 `performRerank` 方法

将 `SearchResult` 对象转换为 `Map` 列表，直接作为 `input` 传入：

```java
// ✅ 将 SearchResult 转换为 Map 列表
List<Map<String, Object>> searchResults = results.stream()
        .map(r -> {
            Map<String, Object> map = new HashMap<>();
            map.put("content", r.getDocument().getContent());
            map.put("score", r.getScore());
            map.put("documentId", r.getDocument().getId());
            if (r.getDocument().getTitle() != null) {
                map.put("title", r.getDocument().getTitle());
            }
            return map;
        })
        .collect(Collectors.toList());

// ✅ 查询文本通过 params 传递
Map<String, Object> params = new HashMap<>();
params.put("query", question);
params.put("topK", results.size());

// ✅ 直接传入 searchResults 列表
Object result = component.execute(searchResults, params);
```

## 📊 数据流对比

### 修复前（错误）

```
EnhancedQueryService.performRerank()
  ↓
  创建 input = Map {
    "query": "什么是世界地球日",
    "documents": ["文档1", "文档2", ...]
  }
  ↓
  component.execute(input, params)
  ↓
  AlgorithmMarketService.createRerankComponent()
  ↓
  ❌ List<Map> searchResults = (List<Map>) input;  // ClassCastException!
```

### 修复后（正确）

```
EnhancedQueryService.performRerank()
  ↓
  创建 input = List<Map> [
    {"content": "文档1", "score": 0.95, ...},
    {"content": "文档2", "score": 0.87, ...},
    ...
  ]
  ↓
  创建 params = Map {
    "query": "什么是世界地球日",
    "topK": 5
  }
  ↓
  component.execute(input, params)
  ↓
  AlgorithmMarketService.createRerankComponent()
  ↓
  ✅ List<Map> searchResults = (List<Map>) input;  // 成功！
  ✅ String query = (String) params.get("query");
```

## 🔧 其他改进

### 1. 添加 Debug 日志

```java
// 输入日志
log.debug("🔄 [Rerank] Input: {} results, query: '{}'", searchResults.size(), question);

// 输出日志
log.debug("🔄 [Rerank] Result type: {}", result != null ? result.getClass().getSimpleName() : "null");

// 重排序完成日志
log.debug("🔄 [Rerank] Reordered {} results using rerankedIndices", rerankedResults.size());
```

### 2. 增强异常处理

```java
catch (Exception e) {
    log.error("❌ 重排序失败: {}", e.getMessage(), e);  // 添加堆栈跟踪
    return results;  // 返回原始结果，不影响整体流程
}
```

## 📝 修改的文件

- ✅ `omni-agent-marketplace/src/main/java/top/yumbo/ai/omni/marketplace/EnhancedQueryService.java`

## 🧪 测试验证

### 测试场景

```
用户查询：什么是世界地球日
  ↓
1. 查询扩展（生成 4 个查询）
  ↓
2. 多查询检索（20 个结果）
  ↓
3. 结果融合（5 个结果）
  ↓
4. 重排序 ✅ 现在应该成功
  ↓
5. 返回最终结果
```

### 预期日志输出

```
INFO  t.y.a.o.m.EnhancedQueryService - 📈 查询扩展: 什么是世界地球日 -> 4 个查询
DEBUG t.yumbo.ai.rag.file.LuceneRAGService - 文本搜索完成，查询: 什么是世界地球日, 结果数: 5
INFO  t.y.a.o.m.EnhancedQueryService - 🔗 结果融合: 20 -> 5 个结果
DEBUG t.y.a.o.m.EnhancedQueryService - 🔄 [Rerank] Input: 5 results, query: '什么是世界地球日'
INFO  t.y.a.o.m.AlgorithmMarketService - 执行重排序: params={query=什么是世界地球日, topK=5}
DEBUG t.y.a.o.m.EnhancedQueryService - 🔄 [Rerank] Result type: HashMap
DEBUG t.y.a.o.m.EnhancedQueryService - 🔄 [Rerank] Reordered 5 results using rerankedIndices
INFO  t.y.a.o.m.EnhancedQueryService - 🎯 重排序完成: 5 个结果
INFO  t.y.a.o.m.EnhancedQueryService - ✅ 增强查询完成: 返回 5 个结果
```

## 🎯 关键点总结

| 项目 | 修复前 | 修复后 |
|------|--------|--------|
| **input 类型** | `Map<String, Object>` ❌ | `List<Map<String, Object>>` ✅ |
| **query 位置** | 在 `input` 中 | 在 `params` 中 |
| **文档格式** | `List<String>` | `List<Map>` (包含 content、score 等) |
| **异常处理** | 只打印消息 | 打印消息 + 堆栈跟踪 |
| **Debug 日志** | 无 | 有详细的输入输出日志 |

## 🚀 现在可以正常使用

重排序功能现在应该可以正常工作了！系统会：
1. ✅ 正确传递搜索结果给重排序组件
2. ✅ 计算语义相关度并重新排序
3. ✅ 返回优化后的结果顺序
4. ✅ 在 debug 模式下显示详细的处理过程

重启应用后测试即可看到效果！🎉

