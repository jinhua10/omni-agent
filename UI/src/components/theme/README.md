# UI主题架构 / UI Theme Architecture

## 目录结构 / Directory Structure

```
UI/src/components/theme/
├── index.js                           # 主题组件统一导出
├── ThemeRenderingEngine.jsx           # 主题渲染引擎
├── ThemeEngineErrorBoundary.jsx       # 主题错误边界
├── UIThemeSwitcher.jsx                # UI主题切换器
├── ui-theme-switcher.css              # 主题切换器样式
└── shells/                            # 主题壳子目录
    ├── index.js                       # 壳子统一导出
    ├── modern/                        # 现代主题
    │   └── CollaborationShell.jsx     # 协作面板壳子
    └── bubble/                        # 气泡主题
        ├── CollaborationShell.jsx     # 协作面板壳子
        └── bubble-collaboration.css   # 气泡主题样式
```

## 架构说明 / Architecture Description

### 1. 主题引擎配置
主题配置在 `contexts/UIThemeEngineContext.jsx` 中定义，每个主题包含：
- 基本信息（名称、描述、作者等）
- 主题配置（布局、动画、密度）
- 壳子映射（shellMapping）：动态加载主题对应的UI壳子组件

### 2. 主题壳子 (Theme Shells)
位于 `components/theme/shells/` 目录下，按主题分类：
- **modern/**: 现代商务主题壳子
- **bubble/**: 梦幻气泡主题壳子
- 未来可扩展更多主题...

每个主题目录包含：
- 页面壳子组件（如 CollaborationShell.jsx）
- 主题特定样式（如 bubble-collaboration.css）

### 3. 动态加载机制
```javascript
// 在 UIThemeEngineContext.jsx 中
shellMapping: {
  collaboration: () => import('../components/theme/shells/modern/CollaborationShell'),
}
```

主题引擎通过动态 import 实现按需加载，提高性能。

### 4. 数据绑定
主题壳子通过 Adapter 模式获取数据和操作：
```javascript
const { state, actions } = useCollaborationBinding();
```
这确保了**功能与UI的完全解耦**，不同主题可以使用相同的数据源。

## 添加新主题 / Adding New Theme

### 步骤1: 创建主题目录
```bash
mkdir UI/src/components/theme/shells/your-theme
```

### 步骤2: 创建主题壳子
```jsx
// UI/src/components/theme/shells/your-theme/CollaborationShell.jsx
import React from 'react';
import { useCollaborationBinding } from '../../../../adapters/CollaborationAdapter';

function YourThemeCollaborationShell() {
  const { state, actions } = useCollaborationBinding();
  
  return (
    <div className="your-theme-collaboration">
      {/* 你的主题UI实现 */}
    </div>
  );
}

export default YourThemeCollaborationShell;
```

### 步骤3: 在主题引擎中注册
在 `contexts/UIThemeEngineContext.jsx` 的 `UI_THEMES` 中添加：
```javascript
yourTheme: {
  id: 'yourTheme',
  name: { zh: '你的主题', en: 'Your Theme' },
  description: { zh: '主题描述', en: 'Theme description' },
  shellMapping: {
    collaboration: () => import('../components/theme/shells/your-theme/CollaborationShell'),
  },
  status: 'active',
}
```

### 步骤4: 更新索引文件
在 `shells/index.js` 中添加导出：
```javascript
export { default as YourThemeCollaborationShell } from './your-theme/CollaborationShell';
```

## 迁移记录 / Migration Log

### 2025-12-12
- ✅ 从旧架构 `UI/src/themes/` 迁移到新架构 `UI/src/components/theme/shells/`
- ✅ 更新所有引用路径
- ✅ 移除旧的 themes 目录
- ✅ 创建统一的导出索引

### 架构优势
1. **更清晰的组织结构**：主题相关文件集中在 components/theme 下
2. **更好的模块化**：壳子按主题分类，便于维护
3. **统一的导出机制**：通过 index.js 统一管理导出
4. **更好的扩展性**：添加新主题更加简单直观

## 最佳实践 / Best Practices

1. **保持壳子纯净**：壳子只负责UI展示，不包含业务逻辑
2. **使用Adapter获取数据**：通过 useXxxBinding hooks 获取数据和操作
3. **主题独立样式**：每个主题的样式放在自己目录下
4. **动态加载**：使用动态 import 提高性能
5. **类型安全**：考虑使用 TypeScript 定义主题接口

## 参考文档 / References

- [UI主题引擎报告](../../../docs/refactor/20251212-True-UI-Theme-Engine-Report.md)
- [代码规范](../../../docs/refactor/20251209-23-00-00-CODE_STANDARDS.md)
