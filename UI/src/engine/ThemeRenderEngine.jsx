/**
 * 主题渲染引擎核心 / Theme Rendering Engine Core
 *
 * 实现数据与UI完全分离，通过引擎将数据和actions绑定到UI壳子
 * Implements complete separation of data and UI, binding data and actions to UI shell via engine
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { createContext, useContext, useState, useCallback, useEffect } from 'react';

// ========== 主题渲染引擎上下文 / Theme Rendering Engine Context ==========

const ThemeRenderContext = createContext(null);

/**
 * 使用主题渲染引擎 / Use theme rendering engine
 */
export const useThemeRender = () => {
  const context = useContext(ThemeRenderContext);
  if (!context) {
    throw new Error('useThemeRender must be used within ThemeRenderProvider');
  }
  return context;
};

/**
 * 主题渲染引擎Provider / Theme Rendering Engine Provider
 */
export function ThemeRenderProvider({ children }) {
  // 页面状态数据存储 / Page state data storage
  const [pageStates, setPageStates] = useState({});

  // 全局actions注册表 / Global actions registry
  const [actionsRegistry, setActionsRegistry] = useState({});

  /**
   * 注册页面状态 / Register page state
   * @param {string} pageId - 页面ID / Page ID
   * @param {object} initialState - 初始状态 / Initial state
   */
  const registerPageState = useCallback((pageId, initialState) => {
    setPageStates(prev => ({
      ...prev,
      [pageId]: initialState
    }));
  }, []);

  /**
   * 更新页面状态 / Update page state
   * @param {string} pageId - 页面ID / Page ID
   * @param {object|function} updater - 状态更新器 / State updater
   */
  const updatePageState = useCallback((pageId, updater) => {
    setPageStates(prev => ({
      ...prev,
      [pageId]: typeof updater === 'function' ? updater(prev[pageId]) : { ...prev[pageId], ...updater }
    }));
  }, []);

  /**
   * 获取页面状态 / Get page state
   * @param {string} pageId - 页面ID / Page ID
   */
  const getPageState = useCallback((pageId) => {
    return pageStates[pageId] || {};
  }, [pageStates]);

  /**
   * 注册actions / Register actions
   * @param {string} pageId - 页面ID / Page ID
   * @param {object} actions - actions对象 / Actions object
   */
  const registerActions = useCallback((pageId, actions) => {
    setActionsRegistry(prev => ({
      ...prev,
      [pageId]: actions
    }));
  }, []);

  /**
   * 获取actions / Get actions
   * @param {string} pageId - 页面ID / Page ID
   */
  const getActions = useCallback((pageId) => {
    return actionsRegistry[pageId] || {};
  }, [actionsRegistry]);

  /**
   * 创建绑定的UI数据 / Create bound UI data
   * 将数据和actions绑定在一起，传递给UI壳子
   * Bind data and actions together, pass to UI shell
   *
   * @param {string} pageId - 页面ID / Page ID
   * @returns {object} 绑定的数据和actions / Bound data and actions
   */
  const createUIBinding = useCallback((pageId) => {
    return {
      state: getPageState(pageId),
      actions: getActions(pageId),
      updateState: (updater) => updatePageState(pageId, updater),
    };
  }, [getPageState, getActions, updatePageState]);

  const value = {
    // 状态管理 / State management
    registerPageState,
    updatePageState,
    getPageState,

    // Actions管理 / Actions management
    registerActions,
    getActions,

    // UI绑定 / UI binding
    createUIBinding,
  };

  return (
    <ThemeRenderContext.Provider value={value}>
      {children}
    </ThemeRenderContext.Provider>
  );
}

// ========== 页面Hook / Page Hook ==========

/**
 * 使用页面绑定 / Use page binding
 *
 * 在页面组件中使用，自动管理数据和actions的绑定
 * Use in page component, automatically manage data and actions binding
 *
 * @param {string} pageId - 页面ID / Page ID
 * @param {object} initialState - 初始状态 / Initial state
 * @param {object} actions - actions对象 / Actions object
 * @returns {object} UI绑定对象 / UI binding object
 */
export function usePageBinding(pageId, initialState, actions) {
  const engine = useThemeRender();

  // 注册页面状态和actions / Register page state and actions
  useEffect(() => {
    engine.registerPageState(pageId, initialState);
    engine.registerActions(pageId, actions);
  }, [pageId]); // eslint-disable-line

  // 返回UI绑定 / Return UI binding
  return engine.createUIBinding(pageId);
}

// ========== UI壳子基础组件 / UI Shell Base Component ==========

/**
 * 主题壳子基础组件 / Theme Shell Base Component
 *
 * 所有主题UI壳子的基础组件，接收绑定的数据和actions
 * Base component for all theme UI shells, receives bound data and actions
 */
export function ThemeShellBase({ pageId, children, renderShell }) {
  const binding = useThemeRender().createUIBinding(pageId);

  // 如果提供了renderShell函数，使用它来渲染
  // If renderShell function is provided, use it to render
  if (renderShell) {
    return renderShell(binding);
  }

  // 否则，将binding作为props传递给children
  // Otherwise, pass binding as props to children
  return typeof children === 'function' ? children(binding) : children;
}

