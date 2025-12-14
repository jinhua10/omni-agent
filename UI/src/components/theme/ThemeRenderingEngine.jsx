/**
 * 主题渲染引擎 / Theme Rendering Engine
 *
 * 根据当前选择的UI主题动态渲染对应的布局和组件
 * Dynamically renders corresponding layout and components based on currently selected UI theme
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { Suspense, lazy } from 'react';
import { Spin } from 'antd';
import { useUIThemeEngine } from '../../contexts/UIThemeEngineContext';
import { ThemeRenderProvider } from '../../engine/ThemeRenderEngine';

// 懒加载不同主题的布局组件 / Lazy load layout components for different themes
const ModernLayout = lazy(() => import('../layout/ModernLayout'));
const BubbleLayout = lazy(() => import('../layout/BubbleLayout')); // 梦幻气泡主题布局 / Dreamy Bubble Theme Layout
// const AnimeLayout = lazy(() => import('../layout/AnimeLayout')); // 未来实现 / Future implementation
// const CyberpunkLayout = lazy(() => import('../layout/CyberpunkLayout')); // 未来实现 / Future implementation

/**
 * 主题布局映射表 / Theme layout mapping
 * 将主题ID映射到对应的布局组件
 * Maps theme IDs to corresponding layout components
 */
const THEME_LAYOUT_MAP = {
  modern: ModernLayout,
  bubble: BubbleLayout, // 梦幻气泡主题 / Dreamy Bubble Theme
  // 未来实现的主题 / Future theme implementations
  anime: null, // AnimeLayout
  cyberpunk: null, // CyberpunkLayout
};

/**
 * 主题渲染引擎组件 / Theme Rendering Engine Component
 *
 * @param {Object} props
 * @param {React.ReactNode} props.children - 子组件（页面内容）/ Child components (page content)
 * @param {string} props.activeKey - 当前激活的菜单项 / Currently active menu item
 * @param {Function} props.onMenuChange - 菜单切换回调 / Menu change callback
 */
function ThemeRenderingEngine({ children, activeKey, onMenuChange }) {
  const { currentUITheme, currentThemeConfig } = useUIThemeEngine();

  // 获取当前主题的布局组件 / Get layout component for current theme
  const LayoutComponent = THEME_LAYOUT_MAP[currentUITheme];

  // 如果主题布局组件不存在，回退到默认布局 / Fallback to default layout if theme component doesn't exist
  if (!LayoutComponent) {
    console.warn(`Theme layout not found for: ${currentUITheme}, falling back to modern layout`);
    const DefaultLayout = THEME_LAYOUT_MAP.modern;
    return (
      <Suspense fallback={<ThemeLoadingFallback />}>
        <DefaultLayout activeKey={activeKey} onMenuChange={onMenuChange}>
          {children}
        </DefaultLayout>
      </Suspense>
    );
  }

  // 检查当前主题是否有对应页面的Shell / Check if current theme has shell for current page
  const shellMapping = currentThemeConfig?.shellMapping;
  const hasShellForPage = shellMapping && shellMapping[activeKey];

  // 渲染对应主题的布局 / Render layout for corresponding theme
  return (
    <Suspense fallback={<ThemeLoadingFallback />}>
      <ThemeRenderProvider>
        <div className={`theme-container theme-container--${currentUITheme}`} data-ui-theme={currentUITheme}>
          <LayoutComponent activeKey={activeKey} onMenuChange={onMenuChange} themeConfig={currentThemeConfig}>
            {/* 如果主题有Shell，则使用Shell渲染；否则使用原有的children */}
            {/* If theme has Shell, use Shell rendering; otherwise use original children */}
            {hasShellForPage ? <ThemeShellRenderer activeKey={activeKey} /> : children}
          </LayoutComponent>
        </div>
      </ThemeRenderProvider>
    </Suspense>
  );
}

/**
 * 主题Shell渲染器 / Theme Shell Renderer
 * 动态加载并渲染对应页面的Shell组件
 */
function ThemeShellRenderer({ activeKey }) {
  const { currentThemeConfig } = useUIThemeEngine();
  const [ShellComponent, setShellComponent] = React.useState(null);
  const [loading, setLoading] = React.useState(true);

  React.useEffect(() => {
    async function loadShell() {
      setLoading(true);
      try {
        const shellMapping = currentThemeConfig?.shellMapping;
        if (shellMapping && shellMapping[activeKey]) {
          const shellModule = await shellMapping[activeKey]();
          setShellComponent(() => shellModule.default);
        }
      } catch (error) {
        console.error(`Failed to load shell for ${activeKey}:`, error);
      } finally {
        setLoading(false);
      }
    }

    loadShell();
  }, [activeKey, currentThemeConfig]);

  if (loading) {
    return (
      <div style={{ display: 'flex', justifyContent: 'center', alignItems: 'center', minHeight: '400px' }}>
        <Spin size="large" tip="加载页面中... / Loading page...">
          <div style={{ padding: 50 }} />
        </Spin>
      </div>
    );
  }

  if (!ShellComponent) {
    return null;
  }

  return <ShellComponent />;
}

/**
 * 主题加载中的占位组件 / Loading fallback component for theme
 */
function ThemeLoadingFallback() {
  return (
    <div
      style={{
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        height: '100vh',
        background: 'var(--theme-background, #fff)',
      }}
    >
      <Spin size="large" tip="加载主题中... / Loading theme...">
        <div style={{ padding: 50 }} />
      </Spin>
    </div>
  );
}

export default ThemeRenderingEngine;

