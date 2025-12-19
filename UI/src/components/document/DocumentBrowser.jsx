/**
 * FTP风格文档浏览器组件 / FTP-Style Document Browser Component
 *
 * 提供类似FTP的文档管理界面，包括：
 * - 目录树导航（面包屑）
 * - 文件和文件夹列表
 * - 文件上传、下载、删除
 * - 创建文件夹
 * - 文档AI交互
 * - 文档详情查看
 *
 * Provides FTP-like document management interface including:
 * - Directory tree navigation (breadcrumb)
 * - Files and folders list
 * - File upload, download, delete
 * - Create folder
 * - Document AI interaction
 * - Document detail viewing
 *
 * @author OmniAgent Team
 * @since 2025-12-19
 */

import React, { useState, useEffect, useCallback, useMemo } from 'react'
import {
  Button,
  Space,
  Table,
  Modal,
  Input,
  Breadcrumb,
  Dropdown,
  Tooltip,
  Tag,
  message as antdMessage
} from 'antd'
import {
  FolderOutlined,
  FileOutlined,
  UploadOutlined,
  FolderAddOutlined,
  DeleteOutlined,
  DownloadOutlined,
  EyeOutlined,
  MessageOutlined,
  ReloadOutlined,
  HomeOutlined,
  FolderOpenOutlined,
  SearchOutlined,
  CloseCircleOutlined,
  FilterOutlined,
  SyncOutlined,
  CheckCircleOutlined,
  CloseCircleOutlined as ErrorIcon,
  ClockCircleOutlined
} from '@ant-design/icons'
import DocumentUpload from './DocumentUpload'
import DocumentDetail from './DocumentDetail'
import { useLanguage } from '../../contexts/LanguageContext'
import { useMessage } from '../../hooks/useMessage'
import axios from 'axios'
import '../../assets/css/document/document-browser.css'

/**
 * 格式化文件大小 / Format file size
 *
 * @param {number} bytes - 字节数 / Bytes
 * @returns {string} 格式化后的大小 / Formatted size
 */
const formatFileSize = (bytes) => {
  if (!bytes || bytes === 0) return '-'
  if (bytes < 1024) return `${bytes} B`
  if (bytes < 1024 * 1024) return `${(bytes / 1024).toFixed(2)} KB`
  if (bytes < 1024 * 1024 * 1024) return `${(bytes / (1024 * 1024)).toFixed(2)} MB`
  return `${(bytes / (1024 * 1024 * 1024)).toFixed(2)} GB`
}

/**
 * 格式化日期时间 / Format date time
 *
 * @param {number} timestamp - 时间戳 / Timestamp
 * @returns {string} 格式化后的日期时间 / Formatted date time
 */
const formatDateTime = (timestamp) => {
  if (!timestamp) return '-'
  const date = new Date(timestamp)
  return date.toLocaleString('zh-CN', {
    year: 'numeric',
    month: '2-digit',
    day: '2-digit',
    hour: '2-digit',
    minute: '2-digit',
    second: '2-digit'
  })
}

function DocumentBrowser() {
  // ============================================================================
  // Hooks / 钩子
  // ============================================================================
  const { t } = useLanguage()
  const message = useMessage()

  // ============================================================================
  // State / 状态管理
  // ============================================================================

  const [currentPath, setCurrentPath] = useState('') // 当前路径 / Current path
  const [items, setItems] = useState([]) // 文件和文件夹列表 / Files and folders list
  const [loading, setLoading] = useState(false) // 加载状态 / Loading state
  const [selectedItems, setSelectedItems] = useState([]) // 选中的项 / Selected items
  const [searchKeyword, setSearchKeyword] = useState('') // 搜索关键词 / Search keyword
  const [statusFilter, setStatusFilter] = useState('all') // 状态过滤器 / Status filter: all, indexing, done, failed

  // UI 状态 / UI state
  const [uploadVisible, setUploadVisible] = useState(false) // 上传对话框 / Upload dialog
  const [detailVisible, setDetailVisible] = useState(false) // 详情对话框 / Detail dialog
  const [createFolderVisible, setCreateFolderVisible] = useState(false) // 创建文件夹对话框 / Create folder dialog
  const [selectedDocument, setSelectedDocument] = useState(null) // 选中的文档 / Selected document
  const [newFolderName, setNewFolderName] = useState('') // 新文件夹名称 / New folder name

  // 统计信息 / Statistics
  const [stats, setStats] = useState({
    totalFiles: 0,
    totalFolders: 0,
    totalSize: 0,
    totalSizeFormatted: '0 B'
  })

  // ============================================================================
  // API Functions / API 函数
  // ============================================================================

  /**
   * 加载目录内容 / Load directory content
   *
   * @param {string} path - 路径 / Path
   */
  const loadDirectory = useCallback(async (path = '') => {
    setLoading(true)
    try {
      const response = await axios.get('/api/documents/browse/list', {
        params: { path }
      })

      if (response.data && response.data.success) {
        setItems(response.data.items || [])
        setCurrentPath(response.data.path || '')
      } else {
        antdMessage.error(t('document.browse.loadFailed'))
      }
    } catch (error) {
      console.error('Failed to load directory:', error)
      antdMessage.error(t('document.browse.loadFailed'))
    } finally {
      setLoading(false)
    }
  }, [t])

  /**
   * 加载统计信息 / Load statistics
   */
  const loadStats = useCallback(async () => {
    try {
      const response = await axios.get('/api/documents/browse/stats')
      if (response.data && response.data.success) {
        setStats(response.data)
      }
    } catch (error) {
      console.error('Failed to load stats:', error)
    }
  }, [])

  /**
   * 初始化 - 加载根目录 / Initialize - load root directory
   */
  useEffect(() => {
    loadDirectory('')
    loadStats()
  }, [loadDirectory, loadStats])

  /**
   * 导航到指定路径 / Navigate to path
   *
   * @param {string} path - 路径 / Path
   */
  const navigateTo = useCallback((path) => {
    loadDirectory(path)
  }, [loadDirectory])

  /**
   * 处理项点击 / Handle item click
   *
   * @param {object} item - 项 / Item
   */
  const handleItemClick = useCallback((item) => {
    if (item.type === 'directory') {
      // 进入文件夹 / Enter folder
      navigateTo(item.path)
    } else {
      // 查看文件详情 / View file details
      handleViewDetail(item)
    }
  }, [navigateTo])

  /**
   * 下载文件 / Download file
   *
   * @param {object} item - 文件项 / File item
   */
  const handleDownload = useCallback(async (item) => {
    try {
      const url = `/api/documents/browse/download?path=${encodeURIComponent(item.path)}`
      window.open(url, '_blank')
      antdMessage.success(t('document.browse.downloadStarted'))
    } catch (error) {
      console.error('Failed to download:', error)
      antdMessage.error(t('document.browse.downloadFailed'))
    }
  }, [t])

  /**
   * 删除文件或文件夹 / Delete file or folder
   *
   * @param {object} item - 项 / Item
   */
  const handleDelete = useCallback(async (item) => {
    Modal.confirm({
      title: t('document.browse.confirmDelete'),
      content: `${t('document.browse.deleteWarning')}: ${item.name}`,
      okText: t('common.confirm'),
      cancelText: t('common.cancel'),
      okButtonProps: { danger: true },
      onOk: async () => {
        try {
          const response = await axios.delete('/api/documents/browse/delete', {
            params: { path: item.path }
          })

          if (response.data && response.data.success) {
            antdMessage.success(t('document.browse.deleteSuccess'))
            loadDirectory(currentPath) // 刷新当前目录 / Refresh current directory
            loadStats() // 刷新统计信息 / Refresh statistics
          } else {
            antdMessage.error(response.data.message || t('document.browse.deleteFailed'))
          }
        } catch (error) {
          console.error('Failed to delete:', error)
          antdMessage.error(t('document.browse.deleteFailed'))
        }
      }
    })
  }, [currentPath, loadDirectory, loadStats, t])

  /**
   * 查看文档详情 / View document details
   *
   * @param {object} item - 文件项 / File item
   */
  const handleViewDetail = useCallback((item) => {
    setSelectedDocument({
      fileName: item.name,
      filePath: item.path,
      fileSize: item.size,
      uploadTime: item.modified
    })
    setDetailVisible(true)
  }, [])

  /**
   * AI交互 / AI interaction
   *
   * @param {object} item - 文件项 / File item
   */
  const handleAIChat = useCallback((item) => {
    // TODO: 实现AI交互功能 / Implement AI interaction
    antdMessage.info(`AI交互功能开发中: ${item.name}`)
  }, [])

  /**
   * 创建文件夹 / Create folder
   */
  const handleCreateFolder = useCallback(async () => {
    if (!newFolderName.trim()) {
      antdMessage.warning(t('document.browse.folderNameRequired'))
      return
    }

    const folderPath = currentPath ? `${currentPath}/${newFolderName}` : newFolderName

    try {
      const response = await axios.post('/api/documents/browse/mkdir', null, {
        params: { path: folderPath }
      })

      if (response.data && response.data.success) {
        antdMessage.success(t('document.browse.createFolderSuccess'))
        setCreateFolderVisible(false)
        setNewFolderName('')
        loadDirectory(currentPath)
        loadStats()
      } else {
        antdMessage.error(response.data.message || t('document.browse.createFolderFailed'))
      }
    } catch (error) {
      console.error('Failed to create folder:', error)
      antdMessage.error(t('document.browse.createFolderFailed'))
    }
  }, [currentPath, newFolderName, loadDirectory, loadStats, t])

  /**
   * 上传成功回调 / Upload success callback
   */
  const handleUploadSuccess = useCallback(() => {
    loadDirectory(currentPath)
    loadStats()
  }, [currentPath, loadDirectory, loadStats])

  // ============================================================================
  // 面包屑导航 / Breadcrumb navigation
  // ============================================================================

  const breadcrumbItems = useMemo(() => {
    const items = [
      {
        title: (
          <span onClick={() => navigateTo('')} style={{ cursor: 'pointer' }}>
            <HomeOutlined /> {t('document.browse.root')}
          </span>
        )
      }
    ]

    if (currentPath) {
      const parts = currentPath.split('/')
      parts.forEach((part, index) => {
        const path = parts.slice(0, index + 1).join('/')
        items.push({
          title: (
            <span onClick={() => navigateTo(path)} style={{ cursor: 'pointer' }}>
              {part}
            </span>
          )
        })
      })
    }

    return items
  }, [currentPath, navigateTo, t])

  // ============================================================================
  // 搜索和状态过滤 / Search and status filtering
  // ============================================================================

  const filteredItems = useMemo(() => {
    let result = items

    // 搜索过滤 / Search filter
    if (searchKeyword.trim()) {
      const keyword = searchKeyword.toLowerCase()
      result = result.filter(item => {
        const name = item.name.toLowerCase()
        return name.includes(keyword)
      })
    }

    // 状态过滤 / Status filter
    if (statusFilter !== 'all') {
      result = result.filter(item => {
        // 文件夹不参与状态过滤
        if (item.type === 'directory') return true

        // 根据索引状态过滤
        const status = item.indexStatus || 'pending'
        if (statusFilter === 'failed') {
          return status === 'failed' || status === 'error'
        }
        if (statusFilter === 'indexing') {
          return status === 'indexing' || status === 'pending'
        }
        if (statusFilter === 'done') {
          return status === 'done' || status === 'completed'
        }
        return true
      })
    }

    return result
  }, [items, searchKeyword, statusFilter])

  /**
   * 清除搜索 / Clear search
   */
  const handleClearSearch = useCallback(() => {
    setSearchKeyword('')
  }, [])

  /**
   * 重建索引 / Rebuild index
   */
  const handleRebuildIndex = useCallback(async (items) => {
    const fileItems = Array.isArray(items) ? items : [items]
    const filePaths = fileItems
      .filter(item => item.type === 'file')
      .map(item => item.path)

    if (filePaths.length === 0) {
      antdMessage.warning(t('document.browse.noFilesSelected'))
      return
    }

    try {
      const response = await axios.post('/api/documents/rebuild-index', {
        filePaths
      })

      if (response.data && response.data.success) {
        antdMessage.success(t('document.browse.rebuildIndexStarted'))
        // 刷新列表
        loadDirectory(currentPath)
      } else {
        antdMessage.error(response.data.message || t('document.browse.rebuildIndexFailed'))
      }
    } catch (error) {
      console.error('Failed to rebuild index:', error)
      antdMessage.error(t('document.browse.rebuildIndexFailed'))
    }
  }, [currentPath, loadDirectory, t])

  /**
   * 批量重建索引 / Batch rebuild index
   */
  const handleBatchRebuild = useCallback(() => {
    if (selectedItems.length === 0) {
      antdMessage.warning(t('document.browse.noFilesSelected'))
      return
    }

    Modal.confirm({
      title: t('document.browse.confirmRebuildIndex'),
      content: t('document.browse.rebuildIndexWarning', { count: selectedItems.length })
        .replace('{count}', selectedItems.length),
      onOk: () => handleRebuildIndex(selectedItems.map(key =>
        items.find(item => item.path === key)
      ))
    })
  }, [selectedItems, items, handleRebuildIndex, t])

  // ============================================================================
  // 表格列定义 / Table columns definition
  // ============================================================================

  const columns = [
    {
      title: t('document.browse.name'),
      dataIndex: 'name',
      key: 'name',
      render: (name, record) => (
        <Space>
          {record.type === 'directory' ? (
            <FolderOutlined style={{ color: '#faad14', fontSize: '18px' }} />
          ) : (
            <FileOutlined style={{ color: '#1890ff', fontSize: '18px' }} />
          )}
          <span
            onClick={() => handleItemClick(record)}
            style={{ cursor: 'pointer', color: '#1890ff' }}
          >
            {name}
          </span>
        </Space>
      )
    },
    {
      title: t('document.browse.type'),
      dataIndex: 'type',
      key: 'type',
      width: 100,
      render: (type) => (
        <Tag color={type === 'directory' ? 'gold' : 'blue'}>
          {type === 'directory' ? t('document.browse.folder') : t('document.browse.file')}
        </Tag>
      )
    },
    {
      title: t('document.browse.indexStatus'),
      dataIndex: 'indexStatus',
      key: 'indexStatus',
      width: 120,
      render: (status, record) => {
        if (record.type === 'directory') return '-'

        const statusInfo = {
          'pending': { icon: <ClockCircleOutlined />, color: 'default', text: t('document.browse.statusPending') },
          'indexing': { icon: <SyncOutlined spin />, color: 'processing', text: t('document.browse.statusIndexing') },
          'done': { icon: <CheckCircleOutlined />, color: 'success', text: t('document.browse.statusDone') },
          'completed': { icon: <CheckCircleOutlined />, color: 'success', text: t('document.browse.statusDone') },
          'failed': { icon: <ErrorIcon />, color: 'error', text: t('document.browse.statusFailed') },
          'error': { icon: <ErrorIcon />, color: 'error', text: t('document.browse.statusFailed') }
        }

        const info = statusInfo[status] || statusInfo['pending']

        return (
          <Tag icon={info.icon} color={info.color}>
            {info.text}
          </Tag>
        )
      }
    },
    {
      title: t('document.browse.size'),
      dataIndex: 'size',
      key: 'size',
      width: 120,
      render: (size, record) => (
        record.type === 'file' ? formatFileSize(size) : '-'
      )
    },
    {
      title: t('document.browse.modified'),
      dataIndex: 'modified',
      key: 'modified',
      width: 180,
      render: (modified, record) => (
        record.type === 'file' ? formatDateTime(modified) : '-'
      )
    },
    {
      title: t('document.browse.actions'),
      key: 'actions',
      width: 300,
      render: (_, record) => (
        <Space>
          {record.type === 'file' && (
            <>
              <Tooltip title={t('document.browse.download')}>
                <Button
                  type="text"
                  size="small"
                  icon={<DownloadOutlined />}
                  onClick={(e) => {
                    e.stopPropagation()
                    handleDownload(record)
                  }}
                />
              </Tooltip>
              <Tooltip title={t('document.browse.viewDetail')}>
                <Button
                  type="text"
                  size="small"
                  icon={<EyeOutlined />}
                  onClick={(e) => {
                    e.stopPropagation()
                    handleViewDetail(record)
                  }}
                />
              </Tooltip>
              <Tooltip title={t('document.browse.aiChat')}>
                <Button
                  type="text"
                  size="small"
                  icon={<MessageOutlined />}
                  onClick={(e) => {
                    e.stopPropagation()
                    handleAIChat(record)
                  }}
                />
              </Tooltip>
              <Tooltip title={t('document.browse.rebuildIndex')}>
                <Button
                  type="text"
                  size="small"
                  icon={<SyncOutlined />}
                  onClick={(e) => {
                    e.stopPropagation()
                    handleRebuildIndex(record)
                  }}
                  disabled={record.indexStatus === 'indexing'}
                />
              </Tooltip>
            </>
          )}
          <Tooltip title={t('document.browse.delete')}>
            <Button
              type="text"
              size="small"
              danger
              icon={<DeleteOutlined />}
              onClick={(e) => {
                e.stopPropagation()
                handleDelete(record)
              }}
            />
          </Tooltip>
        </Space>
      )
    }
  ]

  // ============================================================================
  // Render / 渲染
  // ============================================================================

  return (
    <div className="document-browser">
      {/* 工具栏 / Toolbar */}
      <div className="browser-toolbar">
        <Space size="middle" wrap>
          <Button
            type="primary"
            icon={<UploadOutlined />}
            onClick={() => setUploadVisible(true)}
          >
            {t('document.browse.upload')}
          </Button>
          <Button
            icon={<FolderAddOutlined />}
            onClick={() => setCreateFolderVisible(true)}
          >
            {t('document.browse.createFolder')}
          </Button>
          <Button
            icon={<ReloadOutlined />}
            onClick={() => loadDirectory(currentPath)}
          >
            {t('common.refresh')}
          </Button>

          {/* 状态过滤器 / Status filter */}
          <Space.Compact>
            <Button
              icon={<FilterOutlined />}
              type={statusFilter === 'all' ? 'primary' : 'default'}
              onClick={() => setStatusFilter('all')}
            >
              {t('document.browse.filterAll')}
            </Button>
            <Button
              icon={<SyncOutlined />}
              type={statusFilter === 'indexing' ? 'primary' : 'default'}
              onClick={() => setStatusFilter('indexing')}
            >
              {t('document.browse.filterIndexing')}
            </Button>
            <Button
              icon={<CheckCircleOutlined />}
              type={statusFilter === 'done' ? 'primary' : 'default'}
              onClick={() => setStatusFilter('done')}
            >
              {t('document.browse.filterDone')}
            </Button>
            <Button
              icon={<ErrorIcon />}
              type={statusFilter === 'failed' ? 'primary' : 'default'}
              onClick={() => setStatusFilter('failed')}
              danger={statusFilter === 'failed'}
            >
              {t('document.browse.filterFailed')}
            </Button>
          </Space.Compact>

          {/* 批量重建按钮 / Batch rebuild button */}
          {selectedItems.length > 0 && (
            <Button
              icon={<SyncOutlined />}
              onClick={handleBatchRebuild}
            >
              {t('document.browse.batchRebuild')} ({selectedItems.length})
            </Button>
          )}

          {/* 搜索框 / Search box */}
          <Input
            placeholder={t('document.searchPlaceholder')}
            prefix={<SearchOutlined />}
            suffix={
              searchKeyword ? (
                <CloseCircleOutlined
                  onClick={handleClearSearch}
                  style={{ cursor: 'pointer', color: '#999' }}
                />
              ) : null
            }
            value={searchKeyword}
            onChange={(e) => setSearchKeyword(e.target.value)}
            style={{ width: 300 }}
            allowClear
          />
        </Space>

        {/* 统计信息 / Statistics */}
        <Space size="middle" className="browser-stats">
          <Tag color="blue">{t('document.browse.files')}: {stats.totalFiles}</Tag>
          <Tag color="gold">{t('document.browse.folders')}: {stats.totalFolders}</Tag>
          <Tag color="green">{t('document.browse.totalSize')}: {stats.totalSizeFormatted}</Tag>
        </Space>
      </div>

      {/* 面包屑导航 / Breadcrumb navigation */}
      <div className="browser-breadcrumb">
        <Breadcrumb items={breadcrumbItems} />
        {searchKeyword && (
          <Tag color="blue" style={{ marginLeft: 16 }}>
            {t('document.browse.searchResults')}: {filteredItems.length}
          </Tag>
        )}
      </div>

      {/* 文件列表 / File list */}
      <Table
        className="browser-table"
        columns={columns}
        dataSource={filteredItems}
        loading={loading}
        rowKey="path"
        pagination={false}
        rowSelection={{
          selectedRowKeys: selectedItems,
          onChange: setSelectedItems,
          getCheckboxProps: (record) => ({
            disabled: record.type === 'directory', // 文件夹不可选
          }),
        }}
        onRow={(record) => ({
          onDoubleClick: () => handleItemClick(record)
        })}
      />

      {/* 上传对话框 / Upload dialog */}
      <DocumentUpload
        visible={uploadVisible}
        onSuccess={handleUploadSuccess}
        onCancel={() => setUploadVisible(false)}
      />

      {/* 详情对话框 / Detail dialog */}
      <Modal
        title={t('document.browse.detailTitle')}
        open={detailVisible}
        onCancel={() => setDetailVisible(false)}
        footer={null}
        width={800}
      >
        <DocumentDetail document={selectedDocument} />
      </Modal>

      {/* 创建文件夹对话框 / Create folder dialog */}
      <Modal
        title={t('document.browse.createFolderTitle')}
        open={createFolderVisible}
        onOk={handleCreateFolder}
        onCancel={() => {
          setCreateFolderVisible(false)
          setNewFolderName('')
        }}
        okText={t('common.confirm')}
        cancelText={t('common.cancel')}
      >
        <Input
          placeholder={t('document.browse.folderNamePlaceholder')}
          value={newFolderName}
          onChange={(e) => setNewFolderName(e.target.value)}
          onPressEnter={handleCreateFolder}
        />
      </Modal>
    </div>
  )
}

export default DocumentBrowser

