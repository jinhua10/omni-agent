# Landing Page 路由修改说明

**修改日期**: 2025-12-29  
**修改内容**: 将Landing Page改为默认首页，通过路由控制显示

---

## 🔄 修改内容

### 1. 路由逻辑变更

#### 之前的逻辑（已废弃）
```javascript
// 使用localStorage判断是否首次访问
const [showLanding, setShowLanding] = useState(() => {
  const hasVisited = localStorage.getItem('omni_has_visited')
  return !hasVisited
})

// 进入应用后设置标记
const handleEnterApp = () => {
  setShowLanding(false)
  localStorage.setItem('omni_has_visited', 'true')
  window.location.hash = '#/qa'
}
```

**问题**: 
- ❌ 只能首次访问显示Landing Page
- ❌ 无法返回首页
- ❌ 不符合用户需求

#### 现在的逻辑（新设计）
```javascript
// 使用路由判断显示哪个页面
const [currentView, setCurrentView] = useState('landing')

React.useEffect(() => {
  const handleHashChange = () => {
    const hash = window.location.hash
    
    // 如果hash以 #/demo/ 开头，显示主应用
    if (hash.startsWith('#/demo/')) {
      setCurrentView('app')
      const path = hash.replace('#/demo/', '').split('?')[0]
      setActiveMenu(path || 'qa')
    } else {
      // 否则显示Landing Page
      setCurrentView('landing')
    }
  }
  
  handleHashChange()
  window.addEventListener('hashchange', handleHashChange)
  return () => window.removeEventListener('hashchange', handleHashChange)
}, [])

// 进入应用跳转到/demo/路由
const handleEnterApp = () => {
  window.location.hash = '#/demo/qa'
}
```

**优势**:
- ✅ Landing Page作为默认首页，长期显示
- ✅ 可以随时通过URL返回首页
- ✅ 路由清晰，易于维护

---

## 📍 URL路由设计

### Landing Page（默认首页）
```
访问URL: http://localhost:5173/
或: http://localhost:5173/#/
或: http://localhost:5173/#/home

显示: Landing Page
```

### 主应用（Demo后台）
```
访问URL: http://localhost:5173/#/demo/qa
显示: 问答面板

访问URL: http://localhost:5173/#/demo/documents
显示: 文档管理

访问URL: http://localhost:5173/#/demo/roles
显示: 角色管理

...等等
```

### 路由规则
- **规则**: Hash以`#/demo/`开头 → 显示主应用
- **规则**: 其他任何hash → 显示Landing Page
- **默认**: 无hash或空hash → 显示Landing Page

---

## 🎯 用户体验流程

### 流程图
```
用户访问首页
    ↓
显示 Landing Page
(http://localhost:5173/)
    ↓
点击"立即开始"或其他CTA按钮
    ↓
跳转到 #/demo/qa
    ↓
显示主应用（问答面板）
    ↓
用户可以:
  - 使用应用功能
  - 点击Logo或"返回首页"链接
  - 手动改URL为 / 或 #/
    ↓
返回 Landing Page
```

### 导航方式

#### 从Landing Page进入主应用
- 点击"立即开始"按钮
- 点击"体验在线Demo"按钮
- 手动访问: `http://localhost:5173/#/demo/qa`

#### 从主应用返回Landing Page
- 在浏览器地址栏改URL为: `http://localhost:5173/`
- 或: `http://localhost:5173/#/`
- 或在应用内添加"返回首页"按钮（可选）

---

## 🔧 技术实现

### 关键代码

#### App.jsx - 路由控制
```javascript
// 状态：'landing' 或 'app'
const [currentView, setCurrentView] = useState('landing')

// 监听hash变化
React.useEffect(() => {
  const handleHashChange = () => {
    const hash = window.location.hash
    if (hash.startsWith('#/demo/')) {
      setCurrentView('app')
      // 解析菜单
      const path = hash.replace('#/demo/', '').split('?')[0]
      setActiveMenu(path || 'qa')
    } else {
      setCurrentView('landing')
    }
  }
  
  handleHashChange()
  window.addEventListener('hashchange', handleHashChange)
  return () => window.removeEventListener('hashchange', handleHashChange)
}, [])

// 条件渲染
if (currentView === 'landing') {
  return <LandingPage onEnterApp={handleEnterApp} />
}

// 否则渲染主应用...
```

#### LandingPage.jsx - 按钮点击
```javascript
<Button 
  type="primary" 
  size="large" 
  icon={<RocketOutlined />}
  onClick={onEnterApp}  // 调用App.jsx传入的回调
>
  立即开始
</Button>
```

---

## ✅ 验证清单

测试路由功能是否正常：

### Landing Page显示
- [ ] 访问 `http://localhost:5173/` → 显示Landing Page
- [ ] 访问 `http://localhost:5173/#/` → 显示Landing Page
- [ ] 访问 `http://localhost:5173/#/home` → 显示Landing Page
- [ ] 刷新页面 → 仍显示Landing Page

### 进入主应用
- [ ] 点击"立即开始"按钮 → 跳转到 `#/demo/qa`
- [ ] 显示问答面板
- [ ] URL变为 `http://localhost:5173/#/demo/qa`

### 主应用导航
- [ ] 点击"文档管理"菜单 → URL变为 `#/demo/documents`
- [ ] 点击"角色管理"菜单 → URL变为 `#/demo/roles`
- [ ] 刷新页面 → 保持在当前菜单

### 返回首页
- [ ] 手动改URL为 `/` → 返回Landing Page
- [ ] 手动改URL为 `#/` → 返回Landing Page
- [ ] 浏览器后退 → 正确导航

---

## 🎨 可选增强功能

### 1. 添加"返回首页"按钮

在主应用的导航栏添加返回首页按钮：

```javascript
// 在ThemeRenderingEngine或主导航中添加
<Button 
  icon={<HomeOutlined />}
  onClick={() => window.location.hash = '#/'}
>
  返回首页
</Button>
```

### 2. 添加面包屑导航

```javascript
<Breadcrumb>
  <Breadcrumb.Item>
    <a href="#/">首页</a>
  </Breadcrumb.Item>
  <Breadcrumb.Item>
    Demo
  </Breadcrumb.Item>
  <Breadcrumb.Item>
    {activeMenu}
  </Breadcrumb.Item>
</Breadcrumb>
```

### 3. Logo点击返回首页

```javascript
<div 
  className="logo" 
  onClick={() => window.location.hash = '#/'}
  style={{ cursor: 'pointer' }}
>
  <ApiOutlined /> OmniAgent
</div>
```

---

## 📝 修改总结

### 修改文件
- ✅ `UI/src/App.jsx` - 修改路由逻辑

### 删除的代码
- ❌ `localStorage.getItem('omni_has_visited')` - 不再需要
- ❌ `localStorage.setItem('omni_has_visited', 'true')` - 不再需要
- ❌ 首次访问判断逻辑

### 新增的逻辑
- ✅ `currentView` 状态（'landing' 或 'app'）
- ✅ Hash变化监听，判断显示哪个视图
- ✅ `/demo/` 路由前缀
- ✅ 统一的路由控制

### 保持不变
- ✅ Landing Page组件
- ✅ 主应用功能
- ✅ 菜单导航逻辑

---

## 🚀 启动验证

```bash
cd UI
npm run dev
```

访问 `http://localhost:5173/` 验证：
1. 默认显示Landing Page ✅
2. 点击"立即开始"进入主应用 ✅
3. 主应用功能正常 ✅
4. 可以返回首页 ✅

---

**修改完成**: 2025-12-29  
**修改者**: GitHub Copilot  
**审核者**: Jinhua Yu  
**状态**: ✅ 已完成

