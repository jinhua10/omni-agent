/**
 * AppContext 应用全局状态 (Application Global State Context)
 *
 * 管理应用级别的全局状态，如主题、侧边栏状态等
 * (Manages application-level global state such as theme, sidebar state, etc.)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { createContext, useContext, useState, useCallback } from 'react'
import PropTypes from 'prop-types'

// 创建 Context (Create context)
const AppContext = createContext()

/**
 * AppProvider 组件
 *
 * @param {Object} props - 组件属性
 * @param {ReactNode} props.children - 子组件
 */
export function AppProvider({ children }) {
  // 主题状态 (Theme state)
  const [theme, setTheme] = useState(() => {
    return localStorage.getItem('theme') || 'light'
  })

  // 侧边栏状态 (Sidebar state)
  const [sidebarCollapsed, setSidebarCollapsed] = useState(() => {
    const saved = localStorage.getItem('sidebarCollapsed')
    return saved ? JSON.parse(saved) : false
  })

  // 全局加载状态 (Global loading state)
  const [globalLoading, setGlobalLoading] = useState(false)

  // 页面标题 (Page title)
  const [pageTitle, setPageTitle] = useState('AI Reviewer')

  /**
   * 切换主题 (Toggle theme)
   */
  const toggleTheme = useCallback(() => {
    const newTheme = theme === 'light' ? 'dark' : 'light'
    setTheme(newTheme)
    localStorage.setItem('theme', newTheme)
    document.documentElement.setAttribute('data-theme', newTheme)
  }, [theme])

  /**
   * 切换侧边栏 (Toggle sidebar)
   */
  const toggleSidebar = useCallback(() => {
    const newState = !sidebarCollapsed
    setSidebarCollapsed(newState)
    localStorage.setItem('sidebarCollapsed', JSON.stringify(newState))
  }, [sidebarCollapsed])

  /**
   * 显示全局加载 (Show global loading)
   */
  const showLoading = useCallback(() => {
    setGlobalLoading(true)
  }, [])

  /**
   * 隐藏全局加载 (Hide global loading)
   */
  const hideLoading = useCallback(() => {
    setGlobalLoading(false)
  }, [])

  /**
   * 更新页面标题 (Update page title)
   */
  const updatePageTitle = useCallback((title) => {
    setPageTitle(title)
    document.title = `${title} - AI Reviewer`
  }, [])

  // Context 值 (Context value)
  const value = {
    // 状态 (States)
    theme,
    sidebarCollapsed,
    globalLoading,
    pageTitle,

    // 方法 (Methods)
    toggleTheme,
    toggleSidebar,
    showLoading,
    hideLoading,
    updatePageTitle,
  }

  return (
    <AppContext.Provider value={value}>
      {children}
    </AppContext.Provider>
  )
}

AppProvider.propTypes = {
  children: PropTypes.node.isRequired,
}

/**
 * useApp Hook
 *
 * @returns {Object} 应用状态和方法
 *
 * @example
 * const { theme, toggleTheme, showLoading } = useApp()
 */
export function useApp() {
  const context = useContext(AppContext)

  if (!context) {
    throw new Error('useApp must be used within AppProvider')
  }

  return context
}

export default AppContext

