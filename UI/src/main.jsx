/**
 * 应用入口文件 (Application Entry Point)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import ReactDOM from 'react-dom/client'
import App from './App'

// 第三方样式 (Third-party styles)
import 'antd/dist/reset.css'

// 基础样式 (Base styles)
import './assets/css/reset.css'
import './assets/css/main.css'

// 主题样式 (Theme styles)
import './assets/css/theme-dark.css'

// 布局和响应式 (Layout and responsive)
import './assets/css/responsive.css'

// 动画效果 (Animations)
import './assets/css/animations.css'

// 工具类 (Utilities)
import './assets/css/utilities.css'

// 渲染应用 (Render application)
ReactDOM.createRoot(document.getElementById('root')).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
)
