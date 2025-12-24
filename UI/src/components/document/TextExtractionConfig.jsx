/**
 * 文本提取配置组件
 * (Text Extraction Configuration Component)
 *
 * 提供文本提取模型的选择和配置
 * (Provides text extraction model selection and configuration)
 *
 * Phase 4 - 文档处理精细化控制
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */

import React, { useState, useEffect } from 'react'
import {
  Card,
  Select,
  Button,
  Space,
  Divider,
  Alert,
  Descriptions,
  Tag,
  Spin,
  App,
  Input,
  Switch,
  Tooltip,
  Collapse,
  Dropdown,
} from 'antd'
import {
  FileTextOutlined,
  EyeOutlined,
  ScanOutlined,
  ThunderboltOutlined,
  ThunderboltFilled,
  CheckCircleOutlined,
  EditOutlined,
  EyeOutlined as ViewOutlined,
  DownloadOutlined,
  SaveOutlined,
  ClockCircleOutlined,
  CheckCircleFilled,
  LoadingOutlined,
  MergeCellsOutlined,
  ExpandOutlined,
  ShrinkOutlined,
} from '@ant-design/icons'
import MarkdownRenderer from '../common/MarkdownRenderer'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/document/TextExtractionConfig.css'

const { Option } = Select
const { TextArea } = Input
const EXTRACTION_MODELS = {
  standard: {
    name: '标准提取',
    nameEn: 'Standard Extraction',
    icon: <FileTextOutlined />,
    description: '适用于纯文本文档（TXT、Markdown等）',
    descriptionEn: 'For plain text documents (TXT, Markdown, etc.)',
    color: '#1890ff',
    features: ['快速', '准确', '低资源消耗'],
    featuresEn: ['Fast', 'Accurate', 'Low resource'],
  },
  'vision-llm': {
    name: 'Vision LLM',
    nameEn: 'Vision LLM',
    icon: <EyeOutlined />,
    description: '适用于复杂文档（PPT、图片、PDF等），支持图表理解',
    descriptionEn: 'For complex documents (PPT, images, PDF), supports chart understanding',
    color: '#722ed1',
    features: ['图表理解', '智能分析', '高准确度'],
    featuresEn: ['Chart understanding', 'Smart analysis', 'High accuracy'],
  },
  ocr: {
    name: 'OCR识别',
    nameEn: 'OCR Recognition',
    icon: <ScanOutlined />,
    description: '适用于扫描文档和图片',
    descriptionEn: 'For scanned documents and images',
    color: '#52c41a',
    features: ['扫描件支持', '多语言', '图片识别'],
    featuresEn: ['Scan support', 'Multilingual', 'Image recognition'],
  },
}

function TextExtractionConfig({ documentId }) {
  const { t, language } = useLanguage()
  const { message } = App.useApp()
  const [selectedModel, setSelectedModel] = useState('standard')
  const [loading, setLoading] = useState(false)
  const [systemConfig, setSystemConfig] = useState(null)
  const [documentConfig, setDocumentConfig] = useState(null)
  const [extracting, setExtracting] = useState(false)
  const [extractionProgress, setExtractionProgress] = useState(null)
  const [extractionResult, setExtractionResult] = useState('')
  const [streamingMode, setStreamingMode] = useState(true) // ⭐ 新增：流式/非流式开关
  const [batchInfo, setBatchInfo] = useState(null) // ⭐ 批次信息
  const [isEditing, setIsEditing] = useState(false) // ⭐ 是否为编辑模式（查看源码）
  const [activeTab, setActiveTab] = useState('preview') // ⭐ 当前标签页
  const [batches, setBatches] = useState([]) // ⭐ 批次数据 [{index, content, status}]
  const [autoSaveEnabled, setAutoSaveEnabled] = useState(true) // ⭐ 自动保存开关
  const [lastSaved, setLastSaved] = useState(null) // ⭐ 最后保存时间
  const [isMerged, setIsMerged] = useState(false) // ⭐ 是否已合并批次
  const [expandedBatches, setExpandedBatches] = useState([]) // ⭐ 展开的批次索引列表

  // 加载系统配置
  useEffect(() => {
    loadSystemConfig()
    if (documentId) {
      loadDocumentConfig()
    }
  }, [documentId])

  // ⭐ 自动保存功能：内容变化后 3 秒自动保存
  useEffect(() => {
    if (!autoSaveEnabled || !extractionResult || !documentId) return

    const timer = setTimeout(() => {
      saveExtractionResult()
    }, 3000) // 3秒防抖

    return () => clearTimeout(timer)
  }, [extractionResult, autoSaveEnabled, documentId])

  // ⭐ 保存提取结果
  const saveExtractionResult = async () => {
    if (!documentId || !extractionResult) return

    try {
      const encodedDocId = encodeURIComponent(documentId)
      await fetch(`/api/system/rag-config/document/${encodedDocId}`, {
        method: 'PUT',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          extractedText: extractionResult,
          textExtractionModel: selectedModel,
        }),
      })
      setLastSaved(new Date())
      console.log(t('textExtractionConfig.extraction.autoSaveSuccess'))
    } catch (error) {
      console.error(t('textExtractionConfig.extraction.autoSaveFailed'), ':', error)
    }
  }

  // ⭐ 导出为 Markdown 文件
  const exportAsMarkdown = () => {
    const blob = new Blob([extractionResult], { type: 'text/markdown;charset=utf-8' })
    const url = URL.createObjectURL(blob)
    const link = document.createElement('a')
    link.href = url
    link.download = `${documentId || 'extraction'}.md`
    link.click()
    URL.revokeObjectURL(url)
    message.success(t('textExtractionConfig.export.successMarkdown'))
  }

  // ⭐ 导出为 HTML 文件
  const exportAsHTML = () => {
    const ReactMarkdown = require('react-markdown').default
    const { renderToString } = require('react-dom/server')

    const htmlContent = `<!DOCTYPE html>
<html lang="zh-CN">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>${documentId || t('textExtractionConfig.export.documentResult')}</title>
  <style>
    body {
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif;
      line-height: 1.6;
      max-width: 900px;
      margin: 40px auto;
      padding: 20px;
      color: #333;
    }
    h1, h2, h3 { margin-top: 24px; margin-bottom: 16px; }
    h2 { border-bottom: 1px solid #eaecef; padding-bottom: 0.3em; }
    code { background-color: #f6f8fa; padding: 2px 6px; border-radius: 3px; }
    pre { background-color: #f6f8fa; padding: 16px; border-radius: 6px; overflow: auto; }
    blockquote { border-left: 4px solid #dfe2e5; padding-left: 16px; color: #6a737d; }
    hr { border: none; height: 2px; background-color: #e1e4e8; margin: 24px 0; }
  </style>
</head>
<body>
  ${extractionResult.replace(/\n/g, '<br>').replace(/#{1,6} /g, (match) => `<h${match.length - 1}>`)}
</body>
</html>`

    const blob = new Blob([htmlContent], { type: 'text/html;charset=utf-8' })
    const url = URL.createObjectURL(blob)
    const link = document.createElement('a')
    link.href = url
    link.download = `${documentId || 'extraction'}.html`
    link.click()
    URL.revokeObjectURL(url)
    message.success(t('textExtractionConfig.export.successHTML'))
  }

  // ⭐ 合并所有批次内容
  const mergeBatches = () => {
    const mergedContent = batches
      .sort((a, b) => a.index - b.index) // 按索引排序
      .map(b => b.content)
      .join('\n\n')

    setExtractionResult(mergedContent)
    setIsMerged(true)
    setBatches([]) // 清空批次，切换到合并视图
    message.success(t('textExtractionConfig.batches.mergeSuccess'))
  }

  // ⭐ 检查是否所有批次都已完成
  useEffect(() => {
    if (batches.length > 0 && batches.every(b => b.status === 'completed') && !isMerged) {
      // 所有批次完成后，提示用户可以合并
      message.info({
        content: t('textExtractionConfig.batches.allCompletedTip'),
        duration: 5,
      })
    }
  }, [batches, isMerged])

  const loadDocumentConfig = async () => {
    if (!documentId) return
    try {
      // ⭐ 对URL中的documentId进行编码
      const encodedDocId = encodeURIComponent(documentId)
      // 使用旧API保持兼容
      const response = await fetch(`/api/system/rag-config/document/${encodedDocId}`)
      const result = await response.json()
      if (result.success) {
        setDocumentConfig(result.data)
        setSelectedModel(result.data.textExtractionModel || 'standard')
        
        // ⭐ 如果已经有提取的内容，直接显示
        if (result.data.extractedText) {
          setExtractionResult(result.data.extractedText)
          setExtractionProgress({ 
            status: 'success', 
            percent: 100,
            accuracy: result.data.extractionAccuracy || 0.85
          })
          console.log(t('textExtractionConfig.extraction.loadedExtractedContent'), ':', result.data.extractedText.length, t('textExtractionConfig.extraction.characters'))
        }
      }
    } catch (error) {
      console.error('Failed to load document config:', error)
    }
  }

  const loadSystemConfig = async () => {
    try {
      const response = await fetch('/api/system/rag-config')
      const result = await response.json()
      if (result.success) {
        setSystemConfig(result.data)
        if (!documentId) {
          setSelectedModel(result.data.defaultTextExtractionModel || 'standard')
        }
      }
    } catch (error) {
      console.error('Failed to load system config:', error)
    }
  }

  const handleModelChange = (value) => {
    setSelectedModel(value)
  }

  // 自动提取处理（支持流式/非流式）
  const handleAutoExtract = async () => {
    if (!documentId || extracting) return
    
    setExtracting(true)
    setExtractionProgress({ status: 'processing', percent: 0 })
    setExtractionResult('') // ⭐ 清空之前的结果
    setBatches([]) // ⭐ 清空批次
    message.info(streamingMode ? t('textExtractionConfig.extraction.streamingStart') : t('textExtractionConfig.extraction.batchStart'))

    let currentBatchIndex = -1 // ⭐ 跟踪当前批次

    try {
      // ⭐ 对URL中的documentId进行编码
      const encodedDocId = encodeURIComponent(documentId)
      // 新API：迁移到 DocumentProcessingController
      const response = await fetch(`/api/documents/processing/${encodedDocId}/extract`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          model: selectedModel,
          streaming: streamingMode  // ⭐ 使用开关控制
        }),
      })

      if (!response.ok) {
        throw new Error('Extraction request failed')
      }

      // 处理流式响应
      const reader = response.body.getReader()
      const decoder = new TextDecoder()
      let buffer = ''

      while (true) {
        const { done, value } = await reader.read()
        if (done) break

        buffer += decoder.decode(value, { stream: true })
        const lines = buffer.split('\n')
        buffer = lines.pop() // 保留不完整的行

        for (const line of lines) {
          const trimmedLine = line.trim()
          // 处理SSE格式：忽略event行，只处理data行
          if (trimmedLine.startsWith('data:')) {
            try {
              // 提取data:后面的JSON内容（处理有无空格的情况）
              const jsonStr = trimmedLine.startsWith('data: ') ? trimmedLine.slice(6) : trimmedLine.slice(5)
              const data = JSON.parse(jsonStr)
              
              console.log('📥 SSE event received:', data.type, data)
              
              if (data.type === 'progress') {
                setExtractionProgress({
                  status: 'processing',
                  percent: data.percent || 0,
                  message: data.message
                })
              } else if (data.type === 'batchInfo') {
                // ⭐ 收到批次信息，初始化批次数组
                console.log('📦 Batch info received:', data)
                setBatchInfo({
                  totalBatches: data.totalBatches,
                  totalPages: data.totalPages,
                })
                // 初始化批次数组
                const initialBatches = Array.from({ length: data.totalBatches }, (_, i) => ({
                  index: i,
                  number: i + 1,
                  content: '',
                  status: 'pending', // pending, processing, completed
                }))
                setBatches(initialBatches)
                // ⭐ 默认展开所有批次（用户可以手动收起）
                setExpandedBatches(initialBatches.map(b => b.index))
              } else if (data.type === 'batchStart') {
                // ⭐ 批次开始，更新当前批次索引
                console.log('🚀 Batch started:', data)
                currentBatchIndex = data.batchIndex
                setBatches(prev => prev.map(b =>
                  b.index === data.batchIndex
                    ? { ...b, status: 'processing' }
                    : b
                ))
              } else if (data.type === 'batchEnd') {
                // ⭐ 批次完成
                console.log('✅ Batch completed:', data)
                setBatches(prev => prev.map(b =>
                  b.index === data.batchIndex
                    ? { ...b, status: 'completed' }
                    : b
                ))
              } else if (data.type === 'accuracy') {
                // ⭐ 保存精度信息
                setExtractionProgress(prev => ({
                  ...prev,
                  accuracy: data.value,
                  message: data.message
                }))
              } else if (data.type === 'content') {
                // ⭐ 流式/非流式都实时累加显示（前端体验一致）
                const newContent = data.content || ''
                // ⭐ 优先使用消息中的 batchIndex，解决并行处理时的混乱问题
                const batchIdx = typeof data.batchIndex === 'number' ? data.batchIndex : currentBatchIndex

                console.log('📄 Text content accumulated:', {
                  length: newContent.length,
                  mode: streamingMode ? 'streaming' : 'batch',
                  batchIndex: batchIdx,
                  indexInMessage: data.batchIndex,
                  globalIndex: currentBatchIndex,
                  contentPreview: newContent.substring(0, 50)
                })

                // ⭐ 只更新对应批次的内容（不再累加到 extractionResult，避免并行混乱）
                if (batchIdx >= 0) {
                  setBatches(prev => {
                    const updated = prev.map(b =>
                      b.index === batchIdx
                        ? { ...b, content: b.content + newContent }
                        : b
                    )
                    console.log('📊 Batch status updated:', updated.map(b => ({
                      batch: b.number,
                      status: b.status,
                      contentLength: b.content.length
                    })))
                    return updated
                  })
                } else {
                  // 如果没有批次信息（旧协议），才累加到总内容
                  console.warn('⚠️ 未找到批次索引，使用旧协议')
                  setExtractionResult(prev => prev + newContent)
                }
              } else if (data.type === 'complete') {
                setExtractionProgress({ 
                  status: 'success', 
                  percent: 100,
                  accuracy: data.accuracy || 0.85
                })
                message.success(streamingMode ? t('textExtractionConfig.extraction.streamingComplete') : t('textExtractionConfig.extraction.batchComplete'))
              }
            } catch (e) {
              console.error('Failed to parse SSE data:', e, 'Original line:', trimmedLine)
            }
          }
        }
      }
    } catch (error) {
      console.error('Auto extraction failed:', error)
      setExtractionProgress({ status: 'error', percent: 0 })
      message.error(t('textExtractionConfig.tips.extractionFailed') || '提取失败')
    } finally {
      setExtracting(false)
    }
  }

  const handleApply = async () => {
    setLoading(true)
    try {
      if (documentId) {
        // 手动触发提取
        await handleAutoExtract()
      } else {
        // 更新系统配置
        const response = await fetch('/api/system/rag-config', {
          method: 'PUT',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            defaultTextExtractionModel: selectedModel,
          }),
        })

        const result = await response.json()
        if (result.success) {
          message.success(t('textExtractionConfig.tips.saveSuccess'))
          loadSystemConfig()
        } else {
          message.error(result.message || t('textExtractionConfig.tips.saveFailed'))
        }
      }
    } catch (error) {
      console.error('Operation failed:', error)
      message.error(t('textExtractionConfig.tips.operationFailed'))
    } finally {
      setLoading(false)
    }
  }

  const currentModel = EXTRACTION_MODELS[selectedModel]

  return (
    <div className="text-extraction-config">
      <div className="config-layout">
        {/* 左侧：配置面板 */}
        <div className="config-panel">
          <Card title={documentId ? `${t('textExtractionConfig.documentTitle')} - ${documentId}` : t('textExtractionConfig.title')}>
            <Space vertical style={{ width: '100%' }} size="large">
              {documentId && extractionProgress && (
                <Alert
                  title={
                    extractionProgress.status === 'processing' ? t('textExtractionConfig.progress.extracting') : 
                    extractionProgress.status === 'success' ? t('textExtractionConfig.progress.completed') : 
                    t('textExtractionConfig.progress.failed')
                  }
                  description={
                    <div>
                      {extractionProgress.message || `进度: ${extractionProgress.percent}%`}
                      {extractionProgress.accuracy && (
                        <div className="accuracy-display">
                          📊 {t('textExtractionConfig.progress.accuracy')}: {(extractionProgress.accuracy * 100).toFixed(1)}%
                        </div>
                      )}
                    </div>
                  }
                  type={extractionProgress.status === 'processing' ? 'info' : extractionProgress.status === 'success' ? 'success' : 'error'}
                  showIcon
                />
              )}
              {documentId && !extractionProgress ? (
                <Alert
                  title={t('textExtractionConfig.alerts.documentConfigTitle')}
                  description={t('textExtractionConfig.alerts.documentConfigDesc').replace('{docId}', documentId)}
                  type="warning"
                  showIcon
                />
              ) : !documentId && (
                <Alert
                  title={t('textExtractionConfig.alerts.systemConfigTitle')}
                  description={t('textExtractionConfig.alerts.systemConfigDesc')}
                  type="info"
                  showIcon
                />
              )}

              <div className="model-selector">
                <label className="config-label">{t('textExtractionConfig.labels.selectModel')}:</label>
                <Select
                  value={selectedModel}
                  onChange={handleModelChange}
                  style={{ width: '100%' }}
                  size="large"
                >
                  {Object.entries(EXTRACTION_MODELS).map(([key, model]) => (
                    <Option key={key} value={key}>
                      <Space>
                        <span style={{ color: model.color }}>{model.icon}</span>
                        <span>{language === 'zh' ? model.name : model.nameEn}</span>
                      </Space>
                    </Option>
                  ))}
                </Select>
              </div>

              {/* ⭐ 流式/非流式开关 */}
              {documentId && (
                <div className="streaming-mode-selector">
                  <Space align="center" className="streaming-mode-selector-space">
                    <Space>
                      <ThunderboltFilled style={{ color: streamingMode ? '#1890ff' : '#8c8c8c' }} />
                      <span className="config-label">
                        {streamingMode ? t('textExtractionConfig.streamingMode.streamingMode') : t('textExtractionConfig.streamingMode.batchOutput')}
                      </span>
                    </Space>
                    <Tooltip title={streamingMode ? t('textExtractionConfig.streamingMode.streamingTip') : t('textExtractionConfig.streamingMode.batchTip')}>
                      <Switch
                        checked={streamingMode}
                        onChange={setStreamingMode}
                        disabled={extracting}
                        checkedChildren="流式"
                        unCheckedChildren="批量"
                      />
                    </Tooltip>
                  </Space>
                  <div className="streaming-mode-description">
                    {streamingMode
                      ? `💡 ${t('textExtractionConfig.streamingMode.streamingTip')}`
                      : `💡 ${t('textExtractionConfig.streamingMode.batchTip')}`}
                  </div>
                </div>
              )}

              {/* 系统配置选项 */}
              {systemConfig && (
                <div className="system-config">
                  <Divider />
                  <Space vertical style={{ width: '100%' }}>
                    <div className="config-item">
                      <Space>
                        <CheckCircleOutlined style={{ color: '#52c41a' }} />
                        <span>{t('textExtractionConfig.labels.defaultModel')}:</span>
                        <Tag color={currentModel.color}>
                          {language === 'zh' ? currentModel.name : currentModel.nameEn}
                        </Tag>
                      </Space>
                    </div>
                  </Space>
                </div>
              )}

              <div className="action-buttons">
                <Space>
                  <Button
                    type="primary"
                    icon={<ThunderboltOutlined />}
                    onClick={handleApply}
                    loading={loading || extracting}
                    disabled={extracting}
                    size="large"
                  >
                    {documentId ? (extracting ? t('textExtractionConfig.buttons.extractionInProgress') : t('textExtractionConfig.buttons.startExtraction')) : t('textExtractionConfig.buttons.applyConfig')}
                  </Button>
                  <Button onClick={loadSystemConfig} size="large" disabled={extracting}>
                    {t('textExtractionConfig.buttons.reset')}
                  </Button>
                  {documentId && (
                    <Button
                      onClick={() => window.location.hash = '#/documents?view=flow&docId=' + documentId}
                      size="large"
                    >
                      {t('textExtractionConfig.buttons.backToFlow')}
                    </Button>
                  )}
                </Space>
              </div>
            </Space>
          </Card>
        </div>

        {/* 右侧：如果有文档ID且（有提取结果或有批次），显示提取结果；否则显示模型说明 */}
        {documentId && (extractionResult || batches.length > 0 || extracting) ? (
          <div className="preview-panel">
            <Card 
              title={
                <Space>
                  <span>📄 {t('textExtractionConfig.batches.title')}</span>
                  <Tag color="blue">
                    {batches.length > 0 && !isMerged
                      ? `${batches.reduce((sum, b) => sum + b.content.length, 0)} ${t('textExtractionConfig.progress.characters')}`
                      : `${extractionResult.length} ${t('textExtractionConfig.progress.characters')}`
                    }
                  </Tag>
                  {extractionProgress?.accuracy && (
                    <Tag color="green">{t('textExtractionConfig.progress.accuracy')}: {(extractionProgress.accuracy * 100).toFixed(1)}%</Tag>
                  )}
                  {batchInfo && (
                    <Tag color="purple">{batchInfo.totalPages} {t('textExtractionConfig.progress.pages')} / {batchInfo.totalBatches} {t('textExtractionConfig.progress.batches')}</Tag>
                  )}
                </Space>
              }
              extra={
                <Space>
                  {batches.length > 0 && !isMerged && (
                    <>
                      {batches.every(b => b.status === 'completed') && (
                        <Button
                          type="primary"
                          icon={<MergeCellsOutlined />}
                          onClick={mergeBatches}
                          size="small"
                        >
                          {t('textExtractionConfig.batches.mergeBatches')}
                        </Button>
                      )}
                      <Tooltip title={expandedBatches.length === batches.length ? t('textExtractionConfig.batches.collapseAll') : t('textExtractionConfig.batches.expandAll')}>
                        <Button
                          icon={expandedBatches.length === batches.length ? <ShrinkOutlined /> : <ExpandOutlined />}
                          onClick={() => {
                            if (expandedBatches.length === batches.length) {
                              // 全部收起
                              setExpandedBatches([])
                            } else {
                              // 全部展开
                              setExpandedBatches(batches.map(b => b.index))
                            }
                          }}
                          size="small"
                        >
                          {expandedBatches.length === batches.length ? t('textExtractionConfig.batches.collapseAll') : t('textExtractionConfig.batches.expandAll')}
                        </Button>
                      </Tooltip>
                      <Divider type="vertical" />
                    </>
                  )}
                  <Button
                    type={activeTab === 'preview' ? 'primary' : 'default'}
                    icon={<ViewOutlined />}
                    onClick={() => setActiveTab('preview')}
                    size="small"
                  >
                    {t('textExtractionConfig.preview.title')}
                  </Button>
                  <Button
                    type={activeTab === 'source' ? 'primary' : 'default'}
                    icon={<EditOutlined />}
                    onClick={() => setActiveTab('source')}
                    size="small"
                  >
                    {t('textExtractionConfig.preview.source')}
                  </Button>
                  <Divider type="vertical" />
                  <Tooltip title={autoSaveEnabled ? t('textExtractionConfig.autoSave.enabled') : t('textExtractionConfig.autoSave.disabled')}>
                    <Switch
                      checked={autoSaveEnabled}
                      onChange={setAutoSaveEnabled}
                      checkedChildren={<SaveOutlined />}
                      unCheckedChildren={<SaveOutlined />}
                      size="small"
                    />
                  </Tooltip>
                  {lastSaved && (
                    <Tooltip title={`${t('textExtractionConfig.autoSave.lastSaved')}: ${lastSaved.toLocaleTimeString()}`}>
                      <Tag icon={<CheckCircleFilled />} color="success" className="tag-no-margin">
                        {t('textExtractionConfig.autoSave.saved')}
                      </Tag>
                    </Tooltip>
                  )}
                  <Dropdown
                    menu={{
                      items: [
                        {
                          key: 'markdown',
                          label: t('textExtractionConfig.export.markdown'),
                          icon: <DownloadOutlined />,
                          onClick: exportAsMarkdown,
                        },
                        {
                          key: 'html',
                          label: t('textExtractionConfig.export.html'),
                          icon: <DownloadOutlined />,
                          onClick: exportAsHTML,
                        },
                      ],
                    }}
                  >
                    <Button size="small" icon={<DownloadOutlined />}>
                      {t('textExtractionConfig.export.label')}
                    </Button>
                  </Dropdown>
                </Space>
              }
              className="preview-panel"
            >
              {activeTab === 'preview' ? (
                <div className="markdown-preview" style={{ minHeight: '600px' }}>
                  {batches.length > 0 && !isMerged ? (
                    // ⭐ 批次级别显示（固定高度，滚动查看，用户可收起/展开）
                    <Collapse
                      className="batch-collapse-panel"
                      activeKey={expandedBatches}
                      onChange={(keys) => {
                        console.log('📂 Batch expand status changed:', keys)
                        setExpandedBatches(keys)
                      }}
                      items={batches.map(batch => ({
                        key: batch.index,
                        label: (
                          <Space>
                            <span>{t('textExtractionConfig.batches.batch')} {batch.number}</span>
                            {batch.status === 'pending' && <Tag color="default">{t('textExtractionConfig.batches.waiting')}</Tag>}
                            {batch.status === 'processing' && <Tag icon={<LoadingOutlined />} color="processing">{t('textExtractionConfig.batches.processing')}</Tag>}
                            {batch.status === 'completed' && <Tag icon={<CheckCircleFilled />} color="success">{t('textExtractionConfig.batches.completed')}</Tag>}
                          </Space>
                        ),
                        children: (
                          <MarkdownRenderer
                            content={batch.content || t('textExtractionConfig.batches.waiting')}
                          />
                        ),
                      }))}
                    />
                  ) : (
                    // 没有批次信息时，或已合并后，显示全部内容
                    <MarkdownRenderer
                      content={extractionResult || t('textExtractionConfig.batches.waiting')}
                    />
                  )}
                </div>
              ) : (
                <TextArea
                  value={
                    batches.length > 0 && !isMerged
                      ? batches.sort((a, b) => a.index - b.index).map(b => b.content).join('\n\n')
                      : extractionResult
                  }
                  onChange={(e) => setExtractionResult(e.target.value)}
                  style={{
                    minHeight: '600px',
                    height: '100%',
                    fontFamily: 'monospace',
                    fontSize: '13px',
                    lineHeight: '1.6',
                    border: 'none',
                    resize: 'vertical'
                  }}
                  placeholder={t('textExtractionConfig.preview.sourcePlaceholder')}
                  className="source-editor"
                />
              )}
            </Card>
          </div>
        ) : (
          <div className="preview-panel">
          <Card
            title={
              <Space>
                {currentModel.icon}
                <span>{language === 'zh' ? currentModel.name : currentModel.nameEn}</span>
              </Space>
            }
            variant="borderless"
            className="model-info-card"
            style={{
              borderLeft: `4px solid ${currentModel.color}`,
            }}
          >
            <Space vertical size="large" style={{ width: '100%' }}>
              {/* 模型描述 */}
              <div className="model-description">
                <h4>{t('textExtractionConfig.labels.modelDescription')}</h4>
                <p>{language === 'zh' ? currentModel.description : currentModel.descriptionEn}</p>
              </div>

              {/* 特性列表 */}
              <div className="model-features">
                <h4>{t('textExtractionConfig.labels.mainFeatures')}</h4>
                <Space wrap>
                  {(language === 'zh' ? currentModel.features : currentModel.featuresEn).map(
                    (feature, index) => (
                      <Tag key={index} color={currentModel.color}>
                        {feature}
                      </Tag>
                    )
                  )}
                </Space>
              </div>

              {/* 使用场景 */}
              <div className="model-scenarios">
                <h4>{t('textExtractionConfig.labels.applicableScenarios')}</h4>
                <Descriptions column={1} size="small">
                  {selectedModel === 'standard' && (
                    <>
                      <Descriptions.Item label={t('textExtractionConfig.labels.applicableFiles')}>
                        {t('textExtractionConfig.scenarios.standard.files')}
                      </Descriptions.Item>
                      <Descriptions.Item label={t('textExtractionConfig.labels.processingSpeed')}>
                        {t('textExtractionConfig.scenarios.standard.speed')}
                      </Descriptions.Item>
                      <Descriptions.Item label={t('textExtractionConfig.labels.resourceConsumption')}>
                        {t('textExtractionConfig.scenarios.standard.resource')}
                      </Descriptions.Item>
                    </>
                  )}
                  {selectedModel === 'vision-llm' && (
                    <>
                      <Descriptions.Item label={t('textExtractionConfig.labels.applicableFiles')}>
                        {t('textExtractionConfig.scenarios.visionLlm.files')}
                      </Descriptions.Item>
                      <Descriptions.Item label={t('textExtractionConfig.labels.processingSpeed')}>
                        {t('textExtractionConfig.scenarios.visionLlm.speed')}
                      </Descriptions.Item>
                      <Descriptions.Item label={t('textExtractionConfig.labels.resourceConsumption')}>
                        {t('textExtractionConfig.scenarios.visionLlm.resource')}
                      </Descriptions.Item>
                      <Descriptions.Item label={t('textExtractionConfig.labels.specialAbility')}>
                        {t('textExtractionConfig.scenarios.visionLlm.ability')}
                      </Descriptions.Item>
                    </>
                  )}
                  {selectedModel === 'ocr' && (
                    <>
                      <Descriptions.Item label={t('textExtractionConfig.labels.applicableFiles')}>
                        {t('textExtractionConfig.scenarios.ocr.files')}
                      </Descriptions.Item>
                      <Descriptions.Item label={t('textExtractionConfig.labels.processingSpeed')}>
                        {t('textExtractionConfig.scenarios.ocr.speed')}
                      </Descriptions.Item>
                      <Descriptions.Item label={t('textExtractionConfig.labels.resourceConsumption')}>
                        {t('textExtractionConfig.scenarios.ocr.resource')}
                      </Descriptions.Item>
                      <Descriptions.Item label={t('textExtractionConfig.labels.languageSupport')}>
                        {t('textExtractionConfig.scenarios.ocr.language')}
                      </Descriptions.Item>
                    </>
                  )}
                </Descriptions>
              </div>

              {/* 提示信息 */}
              <Alert
                title={t('textExtractionConfig.alerts.finalTipTitle')}
                description={t('textExtractionConfig.alerts.finalTipDesc')}
                type="warning"
                showIcon
              />
            </Space>
          </Card>
        </div>
        )}
      </div>
    </div>
  )
}

export default TextExtractionConfig

