/**
 * 文档管理主页面 - 支持列表和浏览器双视图切换
 * Document Management Main Page - Support List and Browser View Toggle
 *
 * 提供两种文档管理视图：
 * 1. 列表视图 - 传统的表格列表，带搜索和过滤
 * 2. 浏览器视图 - FTP风格的文件浏览器
 *
 * @author OmniAgent Team
 * @since 2025-12-19
 */

import React, { useState } from 'react'
import { Button, Space, Segmented } from 'antd'
import {
  UnorderedListOutlined,
  FolderOpenOutlined,
  AppstoreOutlined
} from '@ant-design/icons'
import DocumentList from './DocumentList'
import DocumentBrowser from './DocumentBrowser'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/document/document-management.css'

function DocumentManagement() {
  // ============================================================================
  // Hooks / 钩子
  // ============================================================================
  const { t } = useLanguage()

  // ============================================================================
  // State / 状态管理
  // ============================================================================

  // 视图模式: 'list' | 'browser'
  const [viewMode, setViewMode] = useState(() => {
    // 从 localStorage 读取用户偏好 / Read user preference from localStorage
    return localStorage.getItem('documentViewMode') || 'browser'
  })

  // ============================================================================
  // Functions / 函数
  // ============================================================================

  /**
   * 切换视图模式 / Switch view mode
   *
   * @param {string} mode - 视图模式 / View mode
   */
  const handleViewModeChange = (mode) => {
    setViewMode(mode)
    localStorage.setItem('documentViewMode', mode)
  }

  // ============================================================================
  // Render / 渲染
  // ============================================================================

  return (
    <div className="document-management">
      {/* 页面标题和视图切换器 / Page title and view switcher */}
      <div className="document-management-header">
        <h2 className="page-title">
          📁 {t('document.title')}
        </h2>

        {/* 视图切换器 / View switcher */}
        <Space size="middle">
          <Segmented
            value={viewMode}
            onChange={handleViewModeChange}
            options={[
              {
                label: (
                  <Space>
                    <FolderOpenOutlined />
                    <span>{t('document.viewMode.browser')}</span>
                  </Space>
                ),
                value: 'browser',
              },
              {
                label: (
                  <Space>
                    <UnorderedListOutlined />
                    <span>{t('document.viewMode.list')}</span>
                  </Space>
                ),
                value: 'list',
              },
            ]}
            size="large"
          />
        </Space>
      </div>

      {/* 内容区域 / Content area */}
      <div className="document-management-content">
        {viewMode === 'browser' ? (
          <DocumentBrowser />
        ) : (
          <DocumentList />
        )}
      </div>
    </div>
  )
}

export default DocumentManagement

