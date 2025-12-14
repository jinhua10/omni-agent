# 主题数据适配器架构说明

## 问题背景

- 系统有多套UI主题（气泡、现代、动漫等）
- 每个主题有独立的Shell组件负责展示
- 后期会增加更多主题
- 需要避免后端联调时修改多套代码

## 解决方案：统一数据适配器

### 架构图

```
Backend API
    ↓
PageDataAdapter.jsx (统一数据层)
    ↓
┌───────────────┬───────────────┬───────────────┐
│  Bubble Shell │ Modern Shell  │ Future Shells │
│  (气泡主题)    │  (现代主题)    │  (未来主题)    │
└───────────────┴───────────────┴───────────────┘
```

### 核心文件

#### 1. `UI/src/adapters/PageDataAdapter.jsx`
**唯一需要修改的数据层文件**

包含所有页面的数据获取Hook：
- `useQAPageData()` - QA页面数据
- `useHomePageData()` - 首页数据
- `useDocumentsPageData()` - 文档管理数据
- `useCollaborationPageData()` - 协作空间数据
- `useAnalyticsPageData()` - 数据分析数据
- `useSettingsPageData()` - 系统设置数据

#### 2. Shell组件使用方式

所有主题的Shell组件都使用相同的Hook：

```jsx
// Bubble主题的QA Shell
import { useQAPageData } from '../../../../adapters/PageDataAdapter';

function QAShell() {
  const { stats, recentQuestions, loading, error } = useQAPageData();
  // 使用数据渲染UI
}

// Modern主题的QA Shell（未来实现）
import { useQAPageData } from '../../../../adapters/PageDataAdapter';

function QAShell() {
  const { stats, recentQuestions, loading, error } = useQAPageData();
  // 使用相同的数据，不同的UI风格
}
```

## 后端联调步骤

### 当前状态（Mock数据）

```jsx
export function useQAPageData() {
  useEffect(() => {
    // 模拟数据
    setTimeout(() => {
      setData({
        stats: { totalQuestions: 1234, ... },
        loading: false
      });
    }, 500);
  }, []);
}
```

### 联调后（真实API）

**只需修改 `PageDataAdapter.jsx` 一个文件：**

```jsx
export function useQAPageData() {
  useEffect(() => {
    async function fetchData() {
      try {
        // 调用真实API
        const stats = await apiCall('/api/qa/stats');
        const questions = await apiCall('/api/qa/recent');
        
        setData({
          stats,
          recentQuestions: questions,
          loading: false,
          error: null
        });
      } catch (error) {
        setData(prev => ({ ...prev, loading: false, error: error.message }));
      }
    }
    
    fetchData();
  }, []);
}
```

## 优势

✅ **一次修改，全部生效** - 只需修改 `PageDataAdapter.jsx`，所有主题自动更新

✅ **主题独立** - 各主题Shell只负责UI展示，不关心数据来源

✅ **易于扩展** - 新增主题时，直接使用现有的数据Hook

✅ **统一配置** - API baseURL、headers、认证等统一配置

✅ **类型安全** - 数据结构在一个地方定义，减少错误

## 统一配置示例

### API基础配置

在 `PageDataAdapter.jsx` 中统一配置：

```jsx
export async function apiCall(endpoint, options = {}) {
  const baseURL = process.env.REACT_APP_API_BASE_URL || 'http://localhost:8080/api';
  
  const defaultOptions = {
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${getToken()}`, // 统一添加认证
    },
    ...options
  };

  const response = await fetch(`${baseURL}${endpoint}`, defaultOptions);
  return await response.json();
}
```

### 环境变量配置

`.env.development`:
```
REACT_APP_API_BASE_URL=http://localhost:8080/api
```

`.env.production`:
```
REACT_APP_API_BASE_URL=https://api.yourapp.com
```

## 数据流示例

### 1. QA页面数据流

```
用户访问QA页面
    ↓
ThemeRenderingEngine 加载对应主题的 QAShell
    ↓
QAShell 调用 useQAPageData()
    ↓
PageDataAdapter 调用后端API
    ↓
返回标准化数据结构
    ↓
QAShell 使用数据渲染UI（气泡风格）
```

### 2. 数据结构标准化

所有页面返回统一格式：

```typescript
{
  data: any,           // 页面特定数据
  loading: boolean,    // 加载状态
  error: string|null   // 错误信息
}
```

## 文件清单

### 需要修改的文件（后端联调时）

- ✏️ `UI/src/adapters/PageDataAdapter.jsx` - **仅此一个文件**

### 不需要修改的文件

- ✅ `UI/src/components/theme/shells/bubble/*` - 所有气泡主题Shell
- ✅ `UI/src/components/theme/shells/modern/*` - 所有现代主题Shell
- ✅ 未来所有新增主题的Shell组件

## 注意事项

1. **数据结构一致性** - 确保后端返回的数据结构与Adapter定义的一致
2. **错误处理** - Adapter已包含统一的错误处理
3. **Loading状态** - 所有Hook都提供loading状态，Shell组件应处理
4. **缓存策略** - 可以在Adapter层添加数据缓存逻辑
5. **实时更新** - 如需WebSocket等实时功能，在Adapter层统一实现

## 扩展示例

### 添加新页面

1. 在 `PageDataAdapter.jsx` 添加新的Hook：

```jsx
export function useNewPageData() {
  // 实现数据获取逻辑
}
```

2. 在各主题的Shell中使用：

```jsx
import { useNewPageData } from '../../../../adapters/PageDataAdapter';

function NewPageShell() {
  const data = useNewPageData();
  // 渲染UI
}
```

### 添加新主题

1. 创建新主题的Shell组件
2. 直接使用现有的数据Hook
3. 不需要修改任何数据层代码

## 总结

通过统一的 `PageDataAdapter.jsx`，实现了：
- **数据层与展示层完全分离**
- **一次修改，所有主题生效**
- **便于维护和扩展**
- **降低后端联调工作量**

后端联调时，**只需关注 `PageDataAdapter.jsx` 这一个文件**！
