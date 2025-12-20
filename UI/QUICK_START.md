# 🚀 工作流市场 - 快速开始

## ⚡ 3步集成

### 步骤 1: 导入组件

在 `UI/src/App.jsx` 文件顶部添加：

```jsx
import { WorkflowMarket } from './components/workflow'
```

### 步骤 2: 添加路由

在 `renderContent()` 函数中添加：

```jsx
case 'workflowMarket':
  return <WorkflowMarket />
```

### 步骤 3: 测试

启动应用，通过菜单访问工作流市场！

```bash
cd UI
npm run dev
```

---

## 📝 完整示例

```jsx
// UI/src/App.jsx

// 1. 在顶部导入
import { WorkflowMarket } from './components/workflow'

// 2. 在 renderContent 函数中
const renderContent = () => {
  switch (activeMenu) {
    case 'qa':
      return <QAPanel />
    case 'documents':
      return <DocumentManagement />
    case 'workflowMarket':       // 👈 添加这里
      return <WorkflowMarket />  // 👈 添加这里
    case 'profile':
      return <UserProfile />
    default:
      return <QAPanel />
  }
}
```

---

## ✅ 完成！

现在设置 `activeMenu = 'workflowMarket'` 就可以显示工作流市场了！

---

## 📚 更多信息

- [详细集成指南](./WORKFLOW_MARKET_INTEGRATION_GUIDE.md)
- [重构完成报告](./WORKFLOW_COMPONENT_REFACTOR_COMPLETE.md)

---

**就这么简单！** 🎉

