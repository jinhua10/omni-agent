# 🏗️ OmniAgent 模块架构说明

**版本**: v3.0  
**日期**: 2025-12-17

---

## 📦 模块职责划分

### ✅ 正确的架构设计

```
omni-agent/
├── omni-agent-common/          ← 通用工具层
│   └── http/
│       ├── HttpClientAdapter   (接口)
│       ├── RestTemplateAdapter (实现)
│       └── OkHttp3Adapter      (实现)
│
├── omni-agent-core/            ← 核心业务逻辑
│   ├── optimization/           (RAG优化)
│   ├── ppl/                    (PPL算法)
│   └── service/                (业务服务)
│   ❌ 不依赖 Spring Web
│   ❌ 不依赖 common
│
├── omni-agent-web/             ← Web接口层
│   ├── controller/             (REST API)
│   └── config/                 (Web配置)
│   ✅ 依赖 common
│   ✅ 可使用 RestTemplate/OkHttp3
│
└── omni-agent-marketplace/     ← 算法市场（独立模块）
    ├── AlgorithmMarketService
    ├── security/
    │   ├── SecureScriptExecutor
    │   └── SecureRemoteExecutor
    └── config/
    ✅ 依赖 common
    ✅ 可使用 RestTemplate/OkHttp3
    ✅ 可选模块
```

---

## 🎯 为什么这样设计？

### 问题1: 算法市场放在 core 会有什么问题？

❌ **错误设计**:
```
omni-agent-core/
└── marketplace/
    └── SecureRemoteExecutor
        └── 使用 RestTemplate ❌  违反 core 职责
```

**问题**:
- core 定位是"纯业务逻辑"
- 不应该依赖 Spring Web
- 不应该依赖 common

### 解决方案: 独立模块

✅ **正确设计**:
```
omni-agent-marketplace/  (独立模块)
└── security/
    └── SecureRemoteExecutor
        └── 使用 HttpClientAdapter ✅  合理
```

**优点**:
- ✅ 职责清晰
- ✅ 可以依赖 common
- ✅ 可选模块
- ✅ 独立维护

---

## 📊 依赖关系图

```
┌─────────────────────────────────────────────┐
│  omni-agent-common (通用工具层)             │
│  - HttpClientAdapter                        │
│  - RestTemplateAdapter                      │
│  - OkHttp3Adapter                           │
└─────────────────────────────────────────────┘
         ↑                    ↑
         │                    │
    ┌────┴────┐         ┌─────┴──────┐
    │         │         │            │
┌───┴─────┐ ┌┴────────────┐ ┌────────┴───────┐
│  core   │ │    web      │ │  marketplace   │
│  (业务) │ │ (REST API)  │ │  (算法市场)    │
└─────────┘ └─────────────┘ └────────────────┘
    ↑             ↑                  ↑
    │             │                  │
    └─────────────┴──────────────────┘
              (其他模块可选依赖)
```

---

## 🎨 模块职责对照表

| 模块 | 职责 | 可依赖 | 不可依赖 |
|------|------|--------|----------|
| **common** | HTTP客户端、工具类 | Spring Web, OkHttp3 | - |
| **core** | 核心业务逻辑 | API接口 | Spring Web, common |
| **web** | REST API | common, core | - |
| **marketplace** | 算法市场 | common, core | - |

---

## ✅ 设计原则

### 1. 单一职责原则

每个模块只负责一件事：
- `common` → 通用工具
- `core` → 业务逻辑
- `web` → Web接口
- `marketplace` → 算法市场

### 2. 依赖倒置原则

依赖抽象而不是具体实现：
- `HttpClientAdapter` (接口)
- `RestTemplateAdapter` (实现)
- `OkHttp3Adapter` (实现)

### 3. 开闭原则

对扩展开放，对修改关闭：
- 用户可以选择 RestTemplate 或 OkHttp3
- 用户可以实现自定义 HttpClientAdapter

---

## 💡 使用示例

### 默认配置（RestTemplate）

```java
@SpringBootApplication
public class MyApp {
    public static void main(String[] args) {
        SpringApplication.run(MyApp.class, args);
    }
}
```

**自动使用 RestTemplate** - 无需配置

### 切换到 OkHttp3

```xml
<dependency>
    <groupId>com.squareup.okhttp3</groupId>
    <artifactId>okhttp</artifactId>
</dependency>
```

```java
@Configuration
public class HttpConfig {
    @Bean
    public HttpClientAdapter httpClientAdapter() {
        OkHttpClient client = new OkHttpClient.Builder()
            .connectionPool(new ConnectionPool(50, 5, TimeUnit.MINUTES))
            .build();
        return new OkHttp3Adapter(client);
    }
}
```

---

## 🎯 总结

### ✅ 正确的做法

1. **通用工具** → 放在 `omni-agent-common`
2. **业务逻辑** → 放在 `omni-agent-core`
3. **Web接口** → 放在 `omni-agent-web`
4. **独立功能** → 放在独立模块（如 `omni-agent-marketplace`）

### ❌ 错误的做法

1. ❌ 在 core 中使用 RestTemplate
2. ❌ 在 core 中依赖 common
3. ❌ 把所有功能都塞进 core

---

**架构原则**: 职责清晰、依赖合理、易于扩展

**版本**: v3.0  
**维护团队**: OmniAgent Team

