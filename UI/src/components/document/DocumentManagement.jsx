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
  AppstoreOutlined,
  SyncOutlined,  // ⭐ 流程图标
  SettingOutlined,  // ⭐ 配置图标
  ThunderboltOutlined,  // ⭐ 查询扩展图标
  SearchOutlined  // ⭐ 检索图标
} from '@ant-design/icons'
import DocumentList from './DocumentList'
import DocumentBrowser from './DocumentBrowser'
import DocumentProcessingFlow from '../rag-flow/DocumentProcessingFlow'  // ⭐ 导入流程组件
import ChunkingConfig from './ChunkingConfig'  // ⭐ 分块配置组件
import QueryExpansionConfig from './QueryExpansionConfig'  // ⭐ 查询扩展配置组件
import RetrievalConfig from './RetrievalConfig'  // ⭐ 检索配置组件
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

  // 视图模式: 'list' | 'browser' | 'flow'
  const [viewMode, setViewMode] = useState(() => {
    // 从 localStorage 读取用户偏好 / Read user preference from localStorage
    return localStorage.getItem('documentViewMode') || 'browser'
  })

  // ⭐ 当前正在处理的文档ID（用于流程视图）
  const [processingDocumentId, setProcessingDocumentId] = useState(null)

  // ⭐ 处理文档上传成功
  const handleDocumentUploaded = (documentId) => {
    console.log('📄 文档上传成功，documentId:', documentId)
    setProcessingDocumentId(documentId)
    // 自动切换到流程视图
    setViewMode('flow')
    // 更新localStorage
    localStorage.setItem('documentViewMode', 'flow')
  }

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
              {
                label: (
                  <Space>
                    <SyncOutlined />
                    <span>{t('document.viewMode.flow')}</span>
                  </Space>
                ),
                value: 'flow',
              },
              {
                label: (
                  <Space>
                    <SettingOutlined />
                    <span>{t('document.viewMode.chunking')}</span>
                  </Space>
                ),
                value: 'chunking',
              },
              {
                label: (
                  <Space>
                    <ThunderboltOutlined />
                    <span>{t('document.viewMode.queryExpansion')}</span>
                  </Space>
                ),
                value: 'queryExpansion',
              },
              {
                label: (
                  <Space>
                    <SearchOutlined />
                    <span>{t('document.viewMode.retrieval')}</span>
                  </Space>
                ),
                value: 'retrieval',
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
        ) : viewMode === 'list' ? (
          <DocumentList />
        ) : viewMode === 'flow' ? (
          // 流程视图：显示文档处理进度
          <div className="document-flow-view">
            <DocumentProcessingFlow
              documentId={processingDocumentId || 'demo'}
              autoStart={true}
              showDemo={!processingDocumentId}
              onComplete={(progress) => {
                console.log('✅ 文档处理完成:', progress)
              }}
              onError={(error) => {
                console.error('❌ 文档处理失败:', error)
              }}
            />
          </div>
        ) : viewMode === 'chunking' ? (
          // ⭐ 分块配置视图：交互式配置分块策略
          <div className="document-chunking-view">
            <ChunkingConfig />
          </div>
        ) : viewMode === 'queryExpansion' ? (
          // ⭐ 查询扩展配置视图：交互式配置查询扩展策略
          <div className="document-query-expansion-view">
            <QueryExpansionConfig />
          </div>
        ) : viewMode === 'retrieval' ? (
          // ⭐ 检索配置视图：交互式配置检索参数
          <div className="document-retrieval-view">
            <RetrievalConfig />
          </div>
        ) : null}
      </div>
    </div>
  )
}

export default DocumentManagement

