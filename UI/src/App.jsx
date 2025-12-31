/**
 * 主应用组件 (Main Application Component)
 *
 * 管理整体应用状态、布局和路由
 * (Manages overall application state, layout, and routing)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { ConfigProvider, theme as antdTheme, App as AntdApp } from 'antd'
import { LanguageProvider, useLanguage } from './contexts/LanguageContext'
import { ThemeProvider, useTheme } from './contexts/ThemeContext'
import { UIThemeEngineProvider } from './contexts/UIThemeEngineContext'
import { QAProvider } from './contexts/QAContext'
import { ThemeRenderingEngine } from './components/theme'
import { ErrorBoundary, LetterModal, FloatingLetterButton } from './components/common'
import ThemeEngineErrorBoundary from './components/theme/ThemeEngineErrorBoundary'
import { FloatingAIButton, FloatingAIPanel, DOCK_POSITIONS } from './components/ai'
import ResizeSplitter from './components/ai/ResizeSplitter'
import { QAPanel } from './components/qa'
import { DocumentManagement } from './components/document'
import { RoleList } from './components/role'
import { FeedbackPanel } from './components/feedback'
import { CollaborationPanel } from './components/collaboration'
import { WishList } from './components/wish'
import { ServiceMarket } from './components/service'
import { UserProfile } from './components/profile'
import { AdminPanel } from './components/admin'
import { WorkflowMarket, WorkflowBuilder } from './components/workflow'
import LandingPage from './components/landing/LandingPage.jsx'
import { initializeUserId } from './utils/userManager'
import './assets/css/main.css'
import './assets/css/common/error-boundary.css'

/**
 * 应用内容组件 / App Content Component
 * 使用语言和主题上下文 / Uses language and theme context
 */
function AppContent() {
  const { t } = useLanguage()
  const { theme: currentTheme, themeName } = useTheme()
  const [activeMenu, setActiveMenu] = useState('qa')
  const [currentView, setCurrentView] = useState('landing') // 'landing' 或 'app'
  const [letterModalOpen, setLetterModalOpen] = useState(false)
  const [showLetterBadge, setShowLetterBadge] = useState(false)
  const [aiPanelConfig, setAIPanelConfig] = useState(() => {
    // 从localStorage读取AI面板配置
    try {
      const saved = localStorage.getItem('floating_ai_panel_config')
      if (saved) {
        const config = JSON.parse(saved)
        return config
      }
    } catch (e) {
      console.error('Failed to load AI panel config:', e)
    }
    return { dockPosition: DOCK_POSITIONS.NONE }
  })

  // 处理进入应用（从Landing Page跳转到主应用）
  const handleEnterApp = () => {
    // 跳转到问答页面（新路由格式）
    window.location.hash = '#/qa'
  }

  // 监听URL hash变化，判断显示Landing Page还是主应用
  React.useEffect(() => {
    const handleHashChange = () => {
      const hash = window.location.hash

      // 定义主应用的所有有效路由
      const appRoutes = [
        'qa', 'documents', 'roles', 'feedback', 'collaboration',
        'wish', 'aiService', 'workflowMarket', 'workflowBuilder',
        'profile', 'admin'
      ]

      // 如果hash以 #/demo/ 开头，显示主应用（兼容旧路由）
      if (hash.startsWith('#/demo/')) {
        setCurrentView('app')
        // 解析实际的菜单路径
        const path = hash.replace('#/demo/', '').split('?')[0]
        setActiveMenu(path || 'qa')
      }
      // 如果hash直接匹配主应用路由（如 #/documents）
      else if (hash.startsWith('#/')) {
        const path = hash.substring(2).split('?')[0] // 去掉 #/

        // 检查是否是有效的应用路由
        if (appRoutes.includes(path)) {
          setCurrentView('app')
          setActiveMenu(path)
        } else {
          // 不是有效路由，显示Landing Page
          setCurrentView('landing')
        }
      }
      // 空hash或仅 # 显示Landing Page
      else {
        setCurrentView('landing')
      }
    }

    // 初始化时检查
    handleHashChange()

    window.addEventListener('hashchange', handleHashChange)
    return () => window.removeEventListener('hashchange', handleHashChange)
  }, [])


  // 监听localStorage变化
  React.useEffect(() => {
    const handleStorageChange = () => {
      try {
        const saved = localStorage.getItem('floating_ai_panel_config')
        if (saved) {
          const config = JSON.parse(saved)
          setAIPanelConfig(config)
        }
      } catch (e) {
        console.error('Failed to update AI panel config:', e)
      }
    }

    window.addEventListener('storage', handleStorageChange)
    // 使用定时器轮询（因为同窗口localStorage变化不触发storage事件）
    const interval = setInterval(handleStorageChange, 500)

    return () => {
      window.removeEventListener('storage', handleStorageChange)
      clearInterval(interval)
    }
  }, [])

  // 初始化用户ID
  React.useEffect(() => {
    initializeUserId().then(userId => {
      console.log('✅ 用户ID已初始化:', userId)
    }).catch(error => {
      console.error('❌ 用户ID初始化失败:', error)
    })
  }, [])

  // 判断AI面板是否停靠（最大化时不算停靠）
  const isDocked = aiPanelConfig.dockPosition !== DOCK_POSITIONS.NONE && !aiPanelConfig.isMaximized
  const dockPosition = aiPanelConfig.dockPosition

  // 如果localStorage中有停靠状态但当前是浮动模式，重置配置
  React.useEffect(() => {
    if (!isDocked && aiPanelConfig.dockPosition && aiPanelConfig.dockPosition !== DOCK_POSITIONS.NONE) {
      const resetConfig = {
        ...aiPanelConfig,
        dockPosition: DOCK_POSITIONS.NONE,
      }
      setAIPanelConfig(resetConfig)
      localStorage.setItem('floating_ai_panel_config', JSON.stringify(resetConfig))
    }
  }, [isDocked, aiPanelConfig])

  // 处理分隔线拖拽调整大小 - 必须在条件返回之前定义
  const handleSplitterResize = React.useCallback((position) => {
    const minSize = 300 // 最小宽度/高度
    const maxSize = window.innerWidth * 0.8 // 最大80%

    let newDockSize

    if (dockPosition === DOCK_POSITIONS.LEFT) {
      // 左侧停靠，调整宽度
      newDockSize = Math.max(minSize, Math.min(position, maxSize))
    } else if (dockPosition === DOCK_POSITIONS.RIGHT) {
      // 右侧停靠，调整宽度（从右往左）
      newDockSize = Math.max(minSize, Math.min(window.innerWidth - position, maxSize))
    } else if (dockPosition === DOCK_POSITIONS.TOP) {
      // 顶部停靠，调整高度
      newDockSize = Math.max(minSize, Math.min(position, window.innerHeight * 0.8))
    } else if (dockPosition === DOCK_POSITIONS.BOTTOM) {
      // 底部停靠，调整高度（从下往上）
      newDockSize = Math.max(minSize, Math.min(window.innerHeight - position, window.innerHeight * 0.8))
    }

    // 更新配置
    const newConfig = {
      ...aiPanelConfig,
      dockSize: newDockSize,
    }
    setAIPanelConfig(newConfig)
    localStorage.setItem('floating_ai_panel_config', JSON.stringify(newConfig))
  }, [aiPanelConfig, dockPosition])

  // 菜单点击处理 / Menu click handler
  const handleMenuClick = (key) => {
    setActiveMenu(key)
    console.log('Navigate to:', key)
    // 使用新的路由格式（不带 /demo/ 前缀）
    if (typeof key === 'string' && key.length > 0) {
      const nextHash = `#/${key}`
      if (window.location.hash !== nextHash) {
        window.location.hash = nextHash
      }
    }
  }

  // 如果显示Landing Page，直接返回（所有hooks已经在上面调用完毕）
  if (currentView === 'landing') {
    return <LandingPage onEnterApp={handleEnterApp} />
  }

  /**
   * 渲染页面内容 / Render page content
   */
  const renderContent = () => {
    switch (activeMenu) {
      case 'qa':
        return <QAPanel />
      case 'documents':
        return <DocumentManagement />
      case 'roles':
        return <RoleList />
      case 'feedback':
        return <FeedbackPanel />
      case 'collaboration':
        return <CollaborationPanel />
      case 'wish':
        return <WishList />
      case 'aiService':
        return <ServiceMarket />
      case 'workflowMarket':
        return <WorkflowMarket />
      case 'workflowBuilder':
        return <WorkflowBuilder />
      case 'profile':
        return <UserProfile />
      case 'admin':
        return <AdminPanel />
      default:
        return <QAPanel />
    }
  }

  // 配置Ant Design主题 / Configure Ant Design theme
  const antdThemeConfig = {
    algorithm: themeName === 'dark' ? antdTheme.darkAlgorithm : antdTheme.defaultAlgorithm,
    token: {
      colorPrimary: currentTheme.primary,
      colorBgContainer: currentTheme.surface,
      colorBgElevated: currentTheme.surface,
      colorText: currentTheme.text,
      colorTextSecondary: currentTheme.textSecondary,
      colorBorder: currentTheme.border,
    },
  }

  return (
    <ConfigProvider theme={antdThemeConfig}>
      <AntdApp>
        <ErrorBoundary>
        {isDocked ? (
          // 停靠模式：分屏布局
          <div 
            className="app-docked-layout"
            style={{
              display: 'flex',
              flexDirection: dockPosition === DOCK_POSITIONS.LEFT || dockPosition === DOCK_POSITIONS.RIGHT 
                ? 'row' 
                : 'column',
              height: '100vh',
              width: '100vw',
              overflow: 'hidden',
            }}
          >
            {/* 左侧/顶部停靠 */}
            {(dockPosition === DOCK_POSITIONS.LEFT || dockPosition === DOCK_POSITIONS.TOP) && (
              <>
                <div 
                  className="app-docked-panel"
                  style={{
                    width: dockPosition === DOCK_POSITIONS.LEFT ? `${aiPanelConfig.dockSize}px` : '100%',
                    height: dockPosition === DOCK_POSITIONS.TOP ? `${aiPanelConfig.dockSize}px` : '100%',
                    flexShrink: 0,
                    overflow: 'auto',
                  }}
                >
                  <FloatingAIPanel />
                </div>
                {/* 分隔线 */}
                <ResizeSplitter 
                  direction={dockPosition === DOCK_POSITIONS.LEFT ? 'horizontal' : 'vertical'}
                  onResize={handleSplitterResize}
                />
              </>
            )}

            {/* 主内容区 */}
            <div 
              className="app-main-content"
              style={{
                flex: 1,
                overflow: 'auto',
                minWidth: 0,
                minHeight: 0,
              }}
            >
              <ThemeRenderingEngine
                activeKey={activeMenu}
                onMenuChange={handleMenuClick}
              >
                {renderContent()}
              </ThemeRenderingEngine>
            </div>

            {/* 右侧/底部停靠 */}
            {(dockPosition === DOCK_POSITIONS.RIGHT || dockPosition === DOCK_POSITIONS.BOTTOM) && (
              <>
                {/* 分隔线 */}
                <ResizeSplitter 
                  direction={dockPosition === DOCK_POSITIONS.RIGHT ? 'horizontal' : 'vertical'}
                  onResize={handleSplitterResize}
                />
                <div 
                  className="app-docked-panel"
                  style={{
                    width: dockPosition === DOCK_POSITIONS.RIGHT ? `${aiPanelConfig.dockSize}px` : '100%',
                    height: dockPosition === DOCK_POSITIONS.BOTTOM ? `${aiPanelConfig.dockSize}px` : '100%',
                    flexShrink: 0,
                    overflow: 'auto',
                  }}
                >
                  <FloatingAIPanel />
                </div>
              </>
            )}
          </div>
        ) : (
          // 浮动模式：正常布局
          <>
            <ThemeRenderingEngine
              activeKey={activeMenu}
              onMenuChange={handleMenuClick}
            >
              {renderContent()}
            </ThemeRenderingEngine>

            {/* 浮动AI分析按钮和面板 */}
            <FloatingAIButton />
            <FloatingAIPanel />
          </>
        )}
      </ErrorBoundary>
      </AntdApp>
    </ConfigProvider>
  )
}

/**
 * 主应用组件 / Main App Component
 * 包装所有Context Provider / Wraps all Context Providers
 * 顺序：Theme → ErrorBoundary → UIThemeEngine → Language → QA → App Content
 */
function App() {
  return (
    <ThemeProvider>
      <ThemeEngineErrorBoundary>
        <UIThemeEngineProvider>
          <LanguageProvider>
            <QAProvider>
              <AppContent />
            </QAProvider>
          </LanguageProvider>
        </UIThemeEngineProvider>
      </ThemeEngineErrorBoundary>
    </ThemeProvider>
  )
}

export default App
