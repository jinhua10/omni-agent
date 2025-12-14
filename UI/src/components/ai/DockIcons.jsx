/**
 * 停靠按钮图标组件 (Dock Button Icons)
 *
 * 提供更直观的停靠方向图标
 * (Provides more intuitive dock direction icons)
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React from 'react'

// 左侧停靠图标
export const DockLeftIcon = () => (
  <svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor">
    <rect x="2" y="4" width="8" height="16" rx="1" fill="currentColor" fillOpacity="0.8" />
    <rect x="11" y="4" width="11" height="16" rx="1" fill="currentColor" fillOpacity="0.3" />
    <path d="M10 12 L11 12 M10 8 L11 8 M10 16 L11 16" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" />
  </svg>
)

// 右侧停靠图标
export const DockRightIcon = () => (
  <svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor">
    <rect x="2" y="4" width="11" height="16" rx="1" fill="currentColor" fillOpacity="0.3" />
    <rect x="14" y="4" width="8" height="16" rx="1" fill="currentColor" fillOpacity="0.8" />
    <path d="M13 12 L14 12 M13 8 L14 8 M13 16 L14 16" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" />
  </svg>
)

// 顶部停靠图标
export const DockTopIcon = () => (
  <svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor">
    <rect x="4" y="2" width="16" height="8" rx="1" fill="currentColor" fillOpacity="0.8" />
    <rect x="4" y="11" width="16" height="11" rx="1" fill="currentColor" fillOpacity="0.3" />
    <path d="M12 10 L12 11 M8 10 L8 11 M16 10 L16 11" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" />
  </svg>
)

// 底部停靠图标
export const DockBottomIcon = () => (
  <svg width="18" height="18" viewBox="0 0 24 24" fill="currentColor">
    <rect x="4" y="2" width="16" height="11" rx="1" fill="currentColor" fillOpacity="0.3" />
    <rect x="4" y="14" width="16" height="8" rx="1" fill="currentColor" fillOpacity="0.8" />
    <path d="M12 13 L12 14 M8 13 L8 14 M16 13 L16 14" stroke="currentColor" strokeWidth="1.5" strokeLinecap="round" />
  </svg>
)
