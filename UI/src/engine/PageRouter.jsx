/**
 * 页面路由器 / Page Router
 *
 * 根据当前主题动态加载对应的UI壳子
 * Dynamically loads corresponding UI shell based on current theme
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { Suspense, lazy, useEffect, useState } from 'react';
import { Spin } from 'antd';
import { useUIThemeEngine } from '../contexts/UIThemeEngineContext';
import { ThemeRenderProvider } from '../engine/ThemeRenderEngine';

/**
 * 页面路由器 / Page Router
 *
 * @param {string} pageId - 页面ID / Page ID
 * @param {React.Component} fallbackComponent - 回退组件（如果主题不支持该页面）/ Fallback component
 */
function PageRouter({ pageId, fallbackComponent: FallbackComponent }) {
  const { currentUITheme, currentThemeConfig } = useUIThemeEngine();
  const [ShellComponent, setShellComponent] = useState(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    async function loadShell() {
      setLoading(true);

      try {
        // 获取当前主题的页面壳子映射 / Get current theme's page shell mapping
        const shellMapping = currentThemeConfig?.shellMapping;

        if (shellMapping && shellMapping[pageId]) {
          // 动态导入对应的UI壳子 / Dynamically import corresponding UI shell
          const shellModule = await shellMapping[pageId]();
          setShellComponent(() => shellModule.default);
        } else {
          // 如果主题不支持该页面，使用回退组件 / Use fallback component if theme doesn't support this page
          console.warn(`Theme '${currentUITheme}' doesn't support page '${pageId}', using fallback`);
          setShellComponent(() => FallbackComponent);
        }
      } catch (error) {
        console.error(`Failed to load shell for page '${pageId}' in theme '${currentUITheme}':`, error);
        setShellComponent(() => FallbackComponent);
      } finally {
        setLoading(false);
      }
    }

    loadShell();
  }, [currentUITheme, pageId, FallbackComponent, currentThemeConfig]);

  if (loading) {
    return (
      <div style={{
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        minHeight: '400px',
      }}>
        <Spin size="large" tip="加载主题UI中... / Loading theme UI...">
          <div style={{ padding: 50 }} />
        </Spin>
      </div>
    );
  }

  if (!ShellComponent) {
    return <FallbackComponent />;
  }

  return (
    <Suspense
      fallback={
        <div style={{
          display: 'flex',
          justifyContent: 'center',
          alignItems: 'center',
          minHeight: '400px',
        }}>
          <Spin size="large" tip="渲染UI中... / Rendering UI...">
            <div style={{ padding: 50 }} />
          </Spin>
        </div>
      }
    >
      <ShellComponent />
    </Suspense>
  );
}

/**
 * 带引擎的页面路由器 / Page Router with Engine
 *
 * 包装了ThemeRenderProvider，确保数据引擎可用
 * Wrapped with ThemeRenderProvider to ensure data engine is available
 */
export function EnginePageRouter({ pageId, fallbackComponent }) {
  return (
    <ThemeRenderProvider>
      <PageRouter pageId={pageId} fallbackComponent={fallbackComponent} />
    </ThemeRenderProvider>
  );
}

export default PageRouter;

