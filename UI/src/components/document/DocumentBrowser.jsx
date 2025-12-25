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
import { useQA } from '../../contexts/QAContext'
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
  const {
    aiAnalysisDocs,
    addDocToAIAnalysis,
    removeDocFromAIAnalysis,
    setShowFloatingAI
  } = useQA()

  // ============================================================================
  // State / 状态管理
  // ============================================================================

  const [currentPath, setCurrentPath] = useState('') // 当前路径 / Current path
  const [items, setItems] = useState([]) // 文件和文件夹列表 / Files and folders list
  const [loading, setLoading] = useState(false) // 加载状态 / Loading state
  const [selectedItems, setSelectedItems] = useState([]) // 选中的项 / Selected items
  const [searchKeyword, setSearchKeyword] = useState('') // 搜索关键词 / Search keyword

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
   * AI交互 - 添加/移除文件到AI分析面板 / AI interaction - Add/Remove files to AI analysis panel
   *
   * 点击切换：第一次点击加入，第二次点击取消
   * Click to toggle: first click adds, second click removes
   *
   * @param {object} item - 文件项 / File item
   */
  const handleAIChat = useCallback((item) => {
    const files = Array.isArray(item) ? item : [item]
    const fileItems = files.filter(file => file.type === 'file')

    if (fileItems.length === 0) {
      antdMessage.warning(t('document.browse.noFilesSelected'))
      return
    }

    // 检查每个文件是否已在AI面板中
    fileItems.forEach(file => {
      const docId = file.path || file.name
      const isInPanel = aiAnalysisDocs.some(doc => {
        const existingId = doc.id || doc.filePath || doc.path || doc.name || doc.fileName || doc.title
        return existingId === docId || existingId === file.name
      })

      const docData = {
        id: file.path,
        name: file.name,
        fileName: file.name,
        filePath: file.path,
        fileSize: file.size,
        title: file.name
      }

      if (isInPanel) {
        // 已在面板中，移除
        removeDocFromAIAnalysis(docId)
        antdMessage.success(
          t('document.browse.removeFromAIPanelSuccess', { name: file.name })
            .replace('{name}', file.name)
        )
      } else {
        // 不在面板中，添加
        addDocToAIAnalysis(docData)
        antdMessage.success(
          t('document.browse.addToAIPanelSuccess', { count: 1 })
            .replace('{count}', '1')
        )
      }
    })

    // 显示AI面板
    setShowFloatingAI(true)
  }, [aiAnalysisDocs, addDocToAIAnalysis, removeDocFromAIAnalysis, setShowFloatingAI, t])

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

  /**
   * 加入流程视图 / Add to flow view
   *
   * 将文档从存储目录复制到临时目录，并跳转到流程视图
   *
   * @param {object} item - 文件项 / File item
   */
  const handleAddToFlowView = useCallback(async (item) => {
    try {
      antdMessage.loading({ content: t('document.browse.addingToFlow'), key: 'addToFlow' })

      // 调用后端API：从storage复制到documents临时目录
      const response = await axios.post('/api/documents/copy-to-pending', {
        path: item.path,
        fileName: item.name
      })

      if (response.data && response.data.success) {
        antdMessage.success({
          content: t('document.browse.addToFlowSuccess'),
          key: 'addToFlow'
        })

        // 跳转到流程视图，并传递文档ID
        const documentId = response.data.documentId || item.name
        window.location.hash = `#/documents?view=flow&docId=${encodeURIComponent(documentId)}`
      } else {
        antdMessage.error({
          content: response.data.message || t('document.browse.addToFlowFailed'),
          key: 'addToFlow'
        })
      }
    } catch (error) {
      console.error('Failed to add to flow view:', error)
      antdMessage.error({
        content: t('document.browse.addToFlowFailed'),
        key: 'addToFlow'
      })
    }
  }, [t])

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

    return result
  }, [items, searchKeyword])

  /**
   * 清除搜索 / Clear search
   */
  const handleClearSearch = useCallback(() => {
    setSearchKeyword('')
  }, [])

  /**
   * 批量添加到AI分析 / Batch add to AI analysis
   */
  const handleBatchAddToAI = useCallback(() => {
    if (selectedItems.length === 0) {
      antdMessage.warning(t('document.browse.noFilesSelected'))
      return
    }

    const selectedFiles = selectedItems.map(key =>
      items.find(item => item.path === key)
    ).filter(Boolean)

    handleAIChat(selectedFiles)
  }, [selectedItems, items, handleAIChat])

  /**
   * 处理拖拽开始 / Handle drag start
   */
  const handleDragStart = useCallback((e, record) => {
    // 如果拖拽的是选中的文件之一，拖拽所有选中的文件
    // 否则只拖拽当前文件
    let filesToDrag
    if (selectedItems.includes(record.path)) {
      filesToDrag = selectedItems.map(key =>
        items.find(item => item.path === key)
      ).filter(Boolean)
    } else {
      filesToDrag = [record]
    }

    const dragData = {
      type: 'documents',
      documents: filesToDrag
        .filter(item => item.type === 'file')
        .map(item => ({
          fileName: item.name,
          filePath: item.path,
          fileSize: item.size
        }))
    }

    e.dataTransfer.effectAllowed = 'copy'
    e.dataTransfer.setData('text/plain', JSON.stringify(dragData))
    e.dataTransfer.setData('application/json', JSON.stringify(dragData))
  }, [selectedItems, items])

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
            draggable={record.type === 'file'}
            onDragStart={(e) => record.type === 'file' && handleDragStart(e, record)}
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
      title: t('document.browse.size'),
      dataIndex: 'size',
      key: 'size',
      width: 120,
      sorter: (a, b) => (a.size || 0) - (b.size || 0),
      render: (size, record) => (
        record.type === 'file' ? formatFileSize(size) : '-'
      )
    },
    {
      title: t('document.browse.modified'),
      dataIndex: 'modified',
      key: 'modified',
      width: 180,
      sorter: (a, b) => (a.modified || 0) - (b.modified || 0),
      render: (modified) => formatDateTime(modified)
    },
    {
      title: t('document.browse.actions'),
      key: 'actions',
      width: 350,
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
              <Tooltip title={
                aiAnalysisDocs.some(doc => {
                  const existingId = doc.id || doc.filePath || doc.path || doc.name || doc.fileName || doc.title
                  return existingId === record.path || existingId === record.name
                })
                  ? t('document.browse.removeFromAIPanel')
                  : t('document.browse.addToAIPanel')
              }>
                <Button
                  type={
                    aiAnalysisDocs.some(doc => {
                      const existingId = doc.id || doc.filePath || doc.path || doc.name || doc.fileName || doc.title
                      return existingId === record.path || existingId === record.name
                    })
                      ? 'primary'
                      : 'text'
                  }
                  size="small"
                  icon={<MessageOutlined />}
                  onClick={(e) => {
                    e.stopPropagation()
                    handleAIChat(record)
                  }}
                />
              </Tooltip>
              <Tooltip title={t('document.browse.chunkingConfig')}>
                <Button
                  type="text"
                  size="small"
                  icon={<FilterOutlined />}
                  onClick={(e) => {
                    e.stopPropagation()
                    // 跳转到分块配置页面
                    window.location.hash = `#/documents?view=chunking&docId=${encodeURIComponent(record.path)}`
                  }}
                />
              </Tooltip>
              <Tooltip title={t('document.browse.addToFlowView')}>
                <Button
                  type="text"
                  size="small"
                  icon={<SyncOutlined />}
                  onClick={(e) => {
                    e.stopPropagation()
                    handleAddToFlowView(record)
                  }}
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

          {/* 批量操作按钮 / Batch operation buttons */}
          {selectedItems.length > 0 && (
            <Button
                type="primary"
                icon={<MessageOutlined />}
                onClick={handleBatchAddToAI}
              >
                {t('document.browse.batchAddToAI')} ({selectedItems.length})
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
        maskClosable={false}
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

