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
} from '@ant-design/icons'
import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/document/TextExtractionConfig.css'

const { Option } = Select
const { TextArea } = Input

/**
 * 文本提取模型配置
 */
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
      console.log('💾 自动保存成功')
    } catch (error) {
      console.error('自动保存失败:', error)
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
    message.success('已导出为 Markdown 文件')
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
  <title>${documentId || '文档提取结果'}</title>
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
    message.success('已导出为 HTML 文件')
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
    message.success('批次已合并为完整文档')
  }

  // ⭐ 检查是否所有批次都已完成
  useEffect(() => {
    if (batches.length > 0 && batches.every(b => b.status === 'completed') && !isMerged) {
      // 所有批次完成后，提示用户可以合并
      message.info({
        content: '所有批次已完成，您可以合并查看完整文档',
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
          console.log('📄 加载已保存的提取内容:', result.data.extractedText.length, '字符')
        }
      }
    } catch (error) {
      console.error('加载文档配置失败:', error)
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
      console.error('加载系统配置失败:', error)
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
    message.info(streamingMode ? '开始流式提取...' : '开始提取...')

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
        throw new Error('提取请求失败')
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
              
              console.log('📥 收到SSE事件:', data.type, data)
              
              if (data.type === 'progress') {
                setExtractionProgress({
                  status: 'processing',
                  percent: data.percent || 0,
                  message: data.message
                })
              } else if (data.type === 'batchInfo') {
                // ⭐ 收到批次信息，初始化批次数组
                console.log('📦 收到批次信息:', data)
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
              } else if (data.type === 'batchStart') {
                // ⭐ 批次开始，更新当前批次索引
                console.log('🚀 批次开始:', data)
                currentBatchIndex = data.batchIndex
                setBatches(prev => prev.map(b =>
                  b.index === data.batchIndex
                    ? { ...b, status: 'processing' }
                    : b
                ))
              } else if (data.type === 'batchEnd') {
                // ⭐ 批次完成
                console.log('✅ 批次完成:', data)
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

                console.log('📄 累加文本内容:', {
                  长度: newContent.length,
                  模式: streamingMode ? '流式' : '非流式',
                  批次索引: batchIdx,
                  消息中的索引: data.batchIndex,
                  全局索引: currentBatchIndex,
                  内容预览: newContent.substring(0, 50)
                })

                // ⭐ 只更新对应批次的内容（不再累加到 extractionResult，避免并行混乱）
                if (batchIdx >= 0) {
                  setBatches(prev => {
                    const updated = prev.map(b =>
                      b.index === batchIdx
                        ? { ...b, content: b.content + newContent }
                        : b
                    )
                    console.log('📊 批次状态更新:', updated.map(b => ({
                      批次: b.number,
                      状态: b.status,
                      内容长度: b.content.length
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
                message.success(streamingMode ? '流式提取完成' : '提取完成')
              }
            } catch (e) {
              console.error('解析SSE数据失败:', e, '原始行:', trimmedLine)
            }
          }
        }
      }
    } catch (error) {
      console.error('自动提取失败:', error)
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
      console.error('操作失败:', error)
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
                    extractionProgress.status === 'processing' ? '正在提取文本...' : 
                    extractionProgress.status === 'success' ? '✅ 提取完成' : 
                    '❌ 提取失败'
                  }
                  description={
                    <div>
                      {extractionProgress.message || `进度: ${extractionProgress.percent}%`}
                      {extractionProgress.accuracy && (
                        <div style={{ marginTop: 8, fontSize: 16, fontWeight: 'bold', color: '#52c41a' }}>
                          📊 提取精度: {(extractionProgress.accuracy * 100).toFixed(1)}%
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
                  <Space align="center" style={{ width: '100%', justifyContent: 'space-between' }}>
                    <Space>
                      <ThunderboltFilled style={{ color: streamingMode ? '#1890ff' : '#8c8c8c' }} />
                      <span className="config-label">
                        {streamingMode ? '流式输出' : '批量输出'}
                      </span>
                    </Space>
                    <Tooltip title={streamingMode ? '实时显示提取内容，适合大文档' : '批量显示提取内容，适合小文档'}>
                      <Switch
                        checked={streamingMode}
                        onChange={setStreamingMode}
                        disabled={extracting}
                        checkedChildren="流式"
                        unCheckedChildren="批量"
                      />
                    </Tooltip>
                  </Space>
                  <div style={{ marginTop: 8, fontSize: 12, color: '#8c8c8c' }}>
                    {streamingMode
                      ? '💡 边提取边显示，减少等待时间，适合PPT、PDF等大文档'
                      : '💡 提取完成后统一显示，适合TXT、Markdown等小文档'}
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
                    {documentId ? (extracting ? '提取中...' : t('textExtractionConfig.buttons.startExtraction')) : t('textExtractionConfig.buttons.applyConfig')}
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

        {/* 右侧：如果有文档ID且有提取结果，显示提取结果；否则显示模型说明 */}
        {documentId && extractionResult ? (
          <div className="preview-panel">
            <Card 
              title={
                <Space>
                  <span>📄 提取结果</span>
                  <Tag color="blue">{extractionResult.length} 字符</Tag>
                  {extractionProgress?.accuracy && (
                    <Tag color="green">精度: {(extractionProgress.accuracy * 100).toFixed(1)}%</Tag>
                  )}
                  {batchInfo && (
                    <Tag color="purple">{batchInfo.totalPages} 页 / {batchInfo.totalBatches} 批</Tag>
                  )}
                </Space>
              }
              extra={
                <Space>
                  {batches.length > 0 && batches.every(b => b.status === 'completed') && !isMerged && (
                    <>
                      <Button
                        type="primary"
                        icon={<MergeCellsOutlined />}
                        onClick={mergeBatches}
                        size="small"
                      >
                        合并批次
                      </Button>
                      <Divider type="vertical" />
                    </>
                  )}
                  <Button
                    type={activeTab === 'preview' ? 'primary' : 'default'}
                    icon={<ViewOutlined />}
                    onClick={() => setActiveTab('preview')}
                    size="small"
                  >
                    预览
                  </Button>
                  <Button
                    type={activeTab === 'source' ? 'primary' : 'default'}
                    icon={<EditOutlined />}
                    onClick={() => setActiveTab('source')}
                    size="small"
                  >
                    源码
                  </Button>
                  <Divider type="vertical" />
                  <Tooltip title={autoSaveEnabled ? '已启用自动保存' : '已禁用自动保存'}>
                    <Switch
                      checked={autoSaveEnabled}
                      onChange={setAutoSaveEnabled}
                      checkedChildren={<SaveOutlined />}
                      unCheckedChildren={<SaveOutlined />}
                      size="small"
                    />
                  </Tooltip>
                  {lastSaved && (
                    <Tooltip title={`上次保存: ${lastSaved.toLocaleTimeString()}`}>
                      <Tag icon={<CheckCircleFilled />} color="success" style={{ margin: 0 }}>
                        已保存
                      </Tag>
                    </Tooltip>
                  )}
                  <Dropdown
                    menu={{
                      items: [
                        {
                          key: 'markdown',
                          label: '导出 Markdown',
                          icon: <DownloadOutlined />,
                          onClick: exportAsMarkdown,
                        },
                        {
                          key: 'html',
                          label: '导出 HTML',
                          icon: <DownloadOutlined />,
                          onClick: exportAsHTML,
                        },
                      ],
                    }}
                  >
                    <Button size="small" icon={<DownloadOutlined />}>
                      导出
                    </Button>
                  </Dropdown>
                </Space>
              }
              style={{ height: '100%' }}
              bodyStyle={{ height: 'calc(100% - 57px)', padding: activeTab === 'preview' ? '20px' : 0, overflow: 'auto' }}
            >
              {activeTab === 'preview' ? (
                <div className="markdown-preview">
                  {batches.length > 0 ? (
                    // ⭐ 批次级别显示（固定高度，滚动查看）
                    <Collapse
                      className="batch-collapse-panel"
                      defaultActiveKey={batches.map(b => b.index)}
                      items={batches.map(batch => ({
                        key: batch.index,
                        label: (
                          <Space>
                            <span>批次 {batch.number}</span>
                            {batch.status === 'pending' && <Tag color="default">等待中</Tag>}
                            {batch.status === 'processing' && <Tag icon={<LoadingOutlined />} color="processing">处理中</Tag>}
                            {batch.status === 'completed' && <Tag icon={<CheckCircleFilled />} color="success">已完成</Tag>}
                          </Space>
                        ),
                        children: (
                          <ReactMarkdown remarkPlugins={[remarkGfm]}>
                            {batch.content || '等待内容...'}
                          </ReactMarkdown>
                        ),
                      }))}
                    />
                  ) : (
                    // 没有批次信息时，显示全部内容
                    <ReactMarkdown remarkPlugins={[remarkGfm]}>
                      {extractionResult}
                    </ReactMarkdown>
                  )}
                </div>
              ) : (
                <TextArea
                  value={extractionResult}
                  onChange={(e) => setExtractionResult(e.target.value)}
                  style={{
                    height: '100%',
                    fontFamily: 'monospace',
                    fontSize: '13px',
                    lineHeight: '1.6',
                    border: 'none',
                    resize: 'none'
                  }}
                  placeholder="提取的 Markdown 源码将显示在这里..."
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

