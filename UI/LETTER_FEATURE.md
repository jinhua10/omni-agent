# 欢迎信功能说明

## 📧 功能概述

在 omni-agent 首页添加了欢迎信功能，为不同类型的用户提供针对性的介绍信件。

## 🎯 功能特性

### 1. 首次进入自动弹出
- 用户首次进入应用时，自动弹出信件选择模态框
- 使用 localStorage 记录用户是否已查看过信件
- 包含庆祝动画效果（🎉 ✨ 🎊 💫 ⭐）

### 2. 三封信供用户选择
针对不同角色提供三封信：

| 信件 | 目标用户 | 颜色主题 | Emoji |
|------|---------|---------|-------|
| 致普通用户的一封信 | 非技术背景用户 | 蓝色 (#1890ff) | 👤 |
| 致开发者的一封信 | 技术开发者 | 绿色 (#52c41a) | 💻 |
| 致企业用户的一封信 | 企业决策者/CTO | 紫色 (#722ed1) | 💼 |

### 3. 精美的卡片动画
- ✨ Hover 时卡片上浮
- 🎈 Emoji 跳跃动画
- 🎨 颜色边框高亮
- 💫 平滑过渡效果

### 4. Markdown 渲染
- 📝 完整的 Markdown 支持
- 🎨 代码高亮显示（使用 react-syntax-highlighter）
- 📊 表格、列表、引用等格式化显示
- 🌓 暗色主题适配

### 5. 悬浮按钮
- 📍 位置：屏幕右下角
- 🔔 徽章提示：已查看过信件后显示徽章
- 💫 脉冲动画效果
- 🖱️ Hover 时显示提示文字"欢迎信"

## 📁 文件结构

```
UI/src/
├── components/common/
│   ├── LetterModal.jsx          # 信件模态框主组件
│   ├── LetterModal.css          # 信件模态框样式
│   ├── FloatingLetterButton.jsx # 悬浮按钮组件
│   ├── FloatingLetterButton.css # 悬浮按钮样式
│   └── index.js                 # 导出组件
├── assets/md/
│   ├── 致普通用户的一封信.md
│   ├── 致开发者的一封信.md
│   └── 致企业用户的一封信.md
└── App.jsx                      # 集成信件功能
```

## 🎨 组件说明

### LetterModal 组件

**Props:**
- `open` (boolean): 是否显示模态框
- `onClose` (function): 关闭回调函数

**子组件:**
- `LetterCard`: 信件选择卡片
- `LetterViewer`: 信件内容查看器（Markdown 渲染）

**功能:**
1. 显示三封信的选择界面
2. 点击卡片后打开对应信件的详细内容
3. 支持关闭和返回操作

### FloatingLetterButton 组件

**Props:**
- `onClick` (function): 点击回调函数
- `showBadge` (boolean): 是否显示徽章

**功能:**
1. 固定在屏幕右下角
2. 脉冲动画吸引注意
3. Hover 时显示提示文字
4. 支持徽章提示

## 🔄 工作流程

```
用户进入应用
    ↓
检查 localStorage (omni_agent_letter_seen)
    ↓
┌─────────────────────────────────┐
│ 首次进入                        │
├─────────────────────────────────┤
│ 1. 延迟 500ms 弹出选择框        │
│ 2. 显示庆祝动画                 │
│ 3. 用户选择一封信               │
│ 4. 查看 Markdown 内容           │
│ 5. 关闭后设置 localStorage      │
│ 6. 显示悬浮按钮（带徽章）       │
└─────────────────────────────────┘
    ↓
┌─────────────────────────────────┐
│ 再次进入                        │
├─────────────────────────────────┤
│ 1. 不自动弹出                   │
│ 2. 显示悬浮按钮（带徽章）       │
│ 3. 用户可手动点击查看           │
└─────────────────────────────────┘
```

## 🎬 动画效果

### 1. 庆祝动画（confetti）
```css
@keyframes fall {
  0% {
    top: -50px;
    transform: rotate(0deg) scale(1);
    opacity: 1;
  }
  100% {
    top: 100%;
    transform: rotate(360deg) scale(0.5);
    opacity: 0;
  }
}
```

### 2. 跳跃动画（bounce）
```css
@keyframes bounce {
  0%, 100% {
    transform: translateY(0);
  }
  50% {
    transform: translateY(-10px);
  }
}
```

### 3. 脉冲动画（pulse）
```css
@keyframes pulse {
  0%, 100% {
    box-shadow: 0 4px 12px rgba(24, 144, 255, 0.4);
  }
  50% {
    box-shadow: 0 4px 20px rgba(24, 144, 255, 0.6);
  }
}
```

### 4. 滑入动画（slideIn）
```css
@keyframes slideIn {
  from {
    opacity: 0;
    transform: translateX(10px);
  }
  to {
    opacity: 1;
    transform: translateX(0);
  }
}
```

## 📱 响应式设计

- **桌面端**: 卡片横向排列，悬浮按钮 56x56px
- **移动端**: 卡片纵向排列，悬浮按钮 48x48px，隐藏提示文字

## 🌓 主题支持

- 支持明亮主题
- 支持暗色主题（.theme-dark）
- 自动适配当前主题配置

## 🔧 技术栈

- **React 18**: 组件开发
- **Ant Design**: UI 组件库
- **react-markdown**: Markdown 渲染
- **react-syntax-highlighter**: 代码高亮
- **Vite**: 构建工具（支持 ?raw 导入）

## 📝 使用方法

### 在 App.jsx 中集成

```jsx
import { LetterModal, FloatingLetterButton } from './components/common'

function App() {
  const [letterModalOpen, setLetterModalOpen] = useState(false)
  const [showLetterBadge, setShowLetterBadge] = useState(false)

  // 检查是否首次进入
  useEffect(() => {
    const hasSeenLetter = localStorage.getItem('omni_agent_letter_seen')
    if (!hasSeenLetter) {
      setTimeout(() => setLetterModalOpen(true), 500)
    } else {
      setShowLetterBadge(true)
    }
  }, [])

  const handleCloseLetterModal = () => {
    setLetterModalOpen(false)
    localStorage.setItem('omni_agent_letter_seen', 'true')
    setShowLetterBadge(true)
  }

  const handleOpenLetterModal = () => {
    setLetterModalOpen(true)
    setShowLetterBadge(false)
  }

  return (
    <>
      {/* 主内容 */}
      <MainContent />

      {/* 悬浮信件按钮 */}
      <FloatingLetterButton 
        onClick={handleOpenLetterModal}
        showBadge={showLetterBadge}
      />

      {/* 信件模态框 */}
      <LetterModal 
        open={letterModalOpen}
        onClose={handleCloseLetterModal}
      />
    </>
  )
}
```

## 🎯 未来优化

- [ ] 添加信件阅读进度追踪
- [ ] 支持分享信件功能
- [ ] 添加信件反馈评分
- [ ] 支持多语言版本
- [ ] 添加信件打印功能
- [ ] 支持自定义信件内容

## 📄 许可证

Apache License 2.0

