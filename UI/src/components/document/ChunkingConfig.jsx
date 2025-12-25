/**
 * 分块策略配置组件 (Chunking Strategy Configuration Component)
 *
 * 提供交互式的分块策略配置和实时预览功能
 * (Provides interactive configuration and real-time preview of chunking strategies)
 *
 * Phase 4.2.1 - 分块策略配置界面
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */

import React, { useState, useEffect } from 'react'
import {
  Card,
  Row,
  Col,
  Select,
  Form,
  InputNumber,
  Switch,
  Button,
  Input,
  Divider,
  Space,
  Statistic,
  Tag,
  Tooltip,
  App,
  Spin,
  Alert,
} from 'antd'
import {
  SettingOutlined,
  EyeOutlined,
  SwapOutlined,
  ReloadOutlined,
  InfoCircleOutlined,
  CheckCircleOutlined,
  ArrowLeftOutlined,
  ArrowRightOutlined,
  ThunderboltOutlined,
} from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/document/ChunkingConfig.css'

const { Option } = Select
const { TextArea } = Input

function ChunkingConfig({ documentId }) {
  const { t } = useLanguage()
  const [form] = Form.useForm()
  const { message } = App.useApp() // ⭐ 使用App钩子获取message API

  // 添加调试日志
  useEffect(() => {
    console.log('🔍 ChunkingConfig received documentId:', documentId)
  }, [documentId])

  // 状态管理
  const [loading, setLoading] = useState(false)
  const [strategies, setStrategies] = useState([])
  const [currentStrategy, setCurrentStrategy] = useState(null)
  const [documentConfig, setDocumentConfig] = useState(null)
  const [documentInfo, setDocumentInfo] = useState(null)
  const [previewText, setPreviewText] = useState('')
  const [previewResult, setPreviewResult] = useState(null)
  const [comparisonMode, setComparisonMode] = useState(false)
  const [comparisonStrategies, setComparisonStrategies] = useState([])
  const [comparisonResults, setComparisonResults] = useState([])

  // 加载可用策略和文档配置
  useEffect(() => {
    loadStrategies()
    if (documentId) {
      loadDocumentConfig()
    }
  }, [documentId])

  // 加载文档配置
  const loadDocumentConfig = async () => {
    if (!documentId) return
    try {
      // ⭐ 对URL中的documentId进行编码
      const encodedDocId = encodeURIComponent(documentId)
      
      // 加载文档配置
      const configResponse = await fetch(`/api/system/rag-config/document/${encodedDocId}`)
      const configResult = await configResponse.json()
      if (configResult.success && configResult.data) {
        setDocumentConfig(configResult.data)
        // 如果有文档配置，应用其分块策略
        if (configResult.data.chunkingStrategy) {
          form.setFieldsValue(configResult.data.chunkingStrategy)
        }
        // 如果有提取的文本，自动填充到预览文本框
        if (configResult.data.extractedText) {
          console.log('📄 加载已提取的文本，长度:', configResult.data.extractedText.length)
          setPreviewText(configResult.data.extractedText)
        }
      }
      
      // 加载文档详情（文件名等信息）
      const docResponse = await fetch(`/api/documents/${encodedDocId}`)
      const docResult = await docResponse.json()
      if (docResult.success) {
        // 后端直接返回文档信息字段，不是嵌套在data中
        const info = {
          fileName: docResult.fileName,
          fileSize: docResult.fileSize,
          mimeType: docResult.mimeType,
          uploadTime: docResult.uploadTime,
          extractedLength: docResult.extractedLength
        }
        setDocumentInfo(info)
        console.log('📋 加载文档信息:', info.fileName)
      }
    } catch (error) {
      console.error('加载文档配置失败:', error)
    }
  }

  // 加载策略列表
  const loadStrategies = async () => {
    setLoading(true)
    try {
      const response = await fetch('/api/chunking/strategies')
      const result = await response.json()

      if (result.success && result.data) {
        setStrategies(result.data)
        if (result.data.length > 0 && !documentConfig) {
          // 仅在没有文档配置时默认选择第一个策略
          selectStrategy(result.data[0])
        }
        message.success(t('chunkingConfig.message.loadSuccess'))
      } else {
        message.error(t('chunkingConfig.message.loadFailed'))
      }
    } catch (error) {
      console.error('Failed to load strategies:', error)
      message.error(t('chunkingConfig.message.loadFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 选择策略
  const selectStrategy = (strategy) => {
    setCurrentStrategy(strategy)
    // 设置表单默认值
    if (strategy.defaultParams) {
      form.setFieldsValue(strategy.defaultParams)
    }
  }

  // 获取策略显示名称
  const getStrategyDisplayName = (strategyName) => {
    const nameMap = {
      fixed_size: t('chunkingConfig.strategy.fixedSize'),
      semantic: t('chunkingConfig.strategy.semantic'),
      ppl: t('chunkingConfig.strategy.ppl'),
      paragraph: t('chunkingConfig.strategy.paragraph'),
      sentence_boundary: t('chunkingConfig.strategy.sentence_boundary'),
    }
    return nameMap[strategyName] || strategyName
  }

  // 获取策略描述
  const getStrategyDescription = (strategyName) => {
    const descMap = {
      fixed_size: t('chunkingConfig.strategy.description.fixedSize'),
      semantic: t('chunkingConfig.strategy.description.semantic'),
      ppl: t('chunkingConfig.strategy.description.ppl'),
      paragraph: t('chunkingConfig.strategy.description.paragraph'),
    }
    return descMap[strategyName] || ''
  }

  // 预览分块
  const handlePreview = async () => {
    if (!previewText.trim()) {
      message.warning(t('chunkingConfig.message.inputRequired'))
      return
    }

    if (!currentStrategy) {
      message.warning(t('chunkingConfig.message.selectStrategyRequired'))
      return
    }

    setLoading(true)
    try {
      const params = form.getFieldsValue()
      const response = await fetch('/api/chunking/preview', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          strategyName: currentStrategy.name,
          content: previewText,
          params: params,
        }),
      })

      const result = await response.json()

      if (result.success && result.data) {
        setPreviewResult(result.data)
        message.success(t('chunkingConfig.message.previewSuccess'))
      } else {
        message.error(result.message || t('chunkingConfig.message.previewFailed'))
      }
    } catch (error) {
      console.error('Failed to preview:', error)
      message.error(t('chunkingConfig.message.previewFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 策略对比
  const handleComparison = async () => {
    if (comparisonStrategies.length < 2) {
      message.warning(t('chunkingConfig.comparison.noComparison'))
      return
    }

    if (!previewText.trim()) {
      message.warning(t('chunkingConfig.message.inputRequired'))
      return
    }

    setLoading(true)
    try {
      const params = form.getFieldsValue()
      const response = await fetch('/api/chunking/compare', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          strategyNames: comparisonStrategies,
          content: previewText,
          params: params,
        }),
      })

      const result = await response.json()

      if (result.success && result.data) {
        setComparisonResults(result.data)
        message.success(t('chunkingConfig.message.comparisonSuccess'))
      } else {
        message.error(result.message || t('chunkingConfig.message.comparisonFailed'))
      }
    } catch (error) {
      console.error('Failed to compare:', error)
      message.error(t('chunkingConfig.message.comparisonFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 重置表单
  const handleReset = () => {
    form.resetFields()
    if (currentStrategy && currentStrategy.defaultParams) {
      form.setFieldsValue(currentStrategy.defaultParams)
    }
  }

  // 清除预览
  const handleClearPreview = () => {
    setPreviewText('')
    setPreviewResult(null)
    setComparisonResults([])
  }

  // ⭐ 返回上一步（文本提取）
  const handlePreviousStep = async () => {
    if (!documentId) return

    try {
      const encodedDocId = encodeURIComponent(documentId)
      const response = await fetch(`/api/documents/processing/${encodedDocId}/step/goto`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ step: 'TEXT_EXTRACTION' }),
      })

      const result = await response.json()
      if (result.success) {
        message.success(t('chunkingConfig.navigation.returnSuccess'))
        // 跳转到文本提取页面
        window.location.hash = `#/documents/extract?docId=${documentId}`
      } else {
        message.error(result.message || t('chunkingConfig.navigation.returnFailed'))
      }
    } catch (error) {
      console.error('返回上一步失败:', error)
      message.error(t('chunkingConfig.navigation.returnFailed') + ': ' + error.message)
    }
  }

  // ⭐ 执行分块并索引
  const handleExecuteChunking = async () => {
    if (!documentId) {
      message.warning(t('chunkingConfig.navigation.selectDocumentFirst'))
      return
    }

    if (!currentStrategy) {
      message.warning(t('chunkingConfig.navigation.selectStrategyFirst'))
      return
    }

    setLoading(true)
    try {
      const params = form.getFieldsValue()
      const encodedDocId = encodeURIComponent(documentId)

      message.info(t('chunkingConfig.navigation.chunkingStarted'))

      // 调用分块API（流式）
      const response = await fetch(`/api/documents/processing/${encodedDocId}/chunk`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          strategy: currentStrategy.name,
          params: params,
        }),
      })

      if (!response.ok) {
        throw new Error(t('chunkingConfig.navigation.chunkingFailed'))
      }

      // 处理SSE流式响应
      const reader = response.body.getReader()
      const decoder = new TextDecoder()
      let buffer = ''

      while (true) {
        const { done, value } = await reader.read()
        if (done) break

        buffer += decoder.decode(value, { stream: true })
        const lines = buffer.split('\n')
        buffer = lines.pop() || '' // 保留不完整的行

        for (const line of lines) {
          if (line.startsWith('data: ')) {
            try {
              const data = JSON.parse(line.slice(6))
              if (data.type === 'progress') {
                message.info(data.message)
              } else if (data.type === 'complete') {
                message.success(
                  t('chunkingConfig.navigation.chunkingComplete', { count: data.chunkCount })
                )
              } else if (data.type === 'error') {
                message.error(data.message)
              }
            } catch (e) {
              console.warn('解析SSE消息失败:', e)
            }
          }
        }
      }

      // 完成后刷新配置
      await loadDocumentConfig()
      message.success(t('chunkingConfig.message.previewSuccess'))

    } catch (error) {
      console.error('执行分块失败:', error)
      message.error(t('chunkingConfig.navigation.chunkingFailed') + ': ' + error.message)
    } finally {
      setLoading(false)
    }
  }

  // 渲染参数配置表单
  const renderParamsForm = () => {
    if (!currentStrategy) return null

    const { defaultParams } = currentStrategy

    return (
      <Form form={form} layout="vertical" initialValues={defaultParams}>
        {defaultParams.chunkSize !== undefined && (
          <Form.Item
            name="chunkSize"
            label={
              <Space>
                {t('chunkingConfig.params.chunkSize')}
                <Tooltip title={t('chunkingConfig.params.help.chunkSize')}>
                  <InfoCircleOutlined />
                </Tooltip>
              </Space>
            }
          >
            <InputNumber min={100} max={10000} step={100} style={{ width: '100%' }} />
          </Form.Item>
        )}

        {defaultParams.chunkOverlap !== undefined && (
          <Form.Item
            name="chunkOverlap"
            label={
              <Space>
                {t('chunkingConfig.params.chunkOverlap')}
                <Tooltip title={t('chunkingConfig.params.help.chunkOverlap')}>
                  <InfoCircleOutlined />
                </Tooltip>
              </Space>
            }
          >
            <InputNumber min={0} max={1000} step={50} style={{ width: '100%' }} />
          </Form.Item>
        )}

        {defaultParams.minChunkSize !== undefined && (
          <Form.Item
            name="minChunkSize"
            label={
              <Space>
                {t('chunkingConfig.params.minChunkSize')}
                <Tooltip title={t('chunkingConfig.params.help.minChunkSize')}>
                  <InfoCircleOutlined />
                </Tooltip>
              </Space>
            }
          >
            <InputNumber min={50} max={5000} step={50} style={{ width: '100%' }} />
          </Form.Item>
        )}

        {defaultParams.maxChunkSize !== undefined && (
          <Form.Item
            name="maxChunkSize"
            label={
              <Space>
                {t('chunkingConfig.params.maxChunkSize')}
                <Tooltip title={t('chunkingConfig.params.help.maxChunkSize')}>
                  <InfoCircleOutlined />
                </Tooltip>
              </Space>
            }
          >
            <InputNumber min={100} max={20000} step={100} style={{ width: '100%' }} />
          </Form.Item>
        )}

        {defaultParams.similarityThreshold !== undefined && (
          <Form.Item
            name="similarityThreshold"
            label={
              <Space>
                {t('chunkingConfig.params.similarityThreshold')}
                <Tooltip title={t('chunkingConfig.params.help.similarityThreshold')}>
                  <InfoCircleOutlined />
                </Tooltip>
              </Space>
            }
          >
            <InputNumber min={0} max={1} step={0.1} style={{ width: '100%' }} />
          </Form.Item>
        )}

        {defaultParams.perplexityThreshold !== undefined && (
          <Form.Item
            name="perplexityThreshold"
            label={
              <Space>
                {t('chunkingConfig.params.perplexityThreshold')}
                <Tooltip title={t('chunkingConfig.params.help.perplexityThreshold')}>
                  <InfoCircleOutlined />
                </Tooltip>
              </Space>
            }
          >
            <InputNumber min={0} max={100} step={1} style={{ width: '100%' }} />
          </Form.Item>
        )}

        {defaultParams.respectParagraph !== undefined && (
          <Form.Item
            name="respectParagraph"
            label={
              <Space>
                {t('chunkingConfig.params.respectParagraph')}
                <Tooltip title={t('chunkingConfig.params.help.respectParagraph')}>
                  <InfoCircleOutlined />
                </Tooltip>
              </Space>
            }
            valuePropName="checked"
          >
            <Switch />
          </Form.Item>
        )}

        <Form.Item>
          <Space>
            <Button type="primary" icon={<EyeOutlined />} onClick={handlePreview}>
              {t('chunkingConfig.actions.preview')}
            </Button>
            <Button icon={<ReloadOutlined />} onClick={handleReset}>
              {t('chunkingConfig.actions.reset')}
            </Button>
          </Space>
        </Form.Item>
      </Form>
    )
  }

  // 渲染统计信息
  const renderStatistics = () => {
    if (!previewResult || !previewResult.statistics) return null

    const { statistics } = previewResult

    return (
      <Card
        title={t('chunkingConfig.preview.stats.title')}
        size="small"
        className="statistics-card"
      >
        <Row gutter={16}>
          <Col span={8}>
            <Statistic
              title={t('chunkingConfig.preview.stats.totalChunks')}
              value={statistics.totalChunks}
              prefix={<CheckCircleOutlined />}
            />
          </Col>
          <Col span={8}>
            <Statistic
              title={t('chunkingConfig.preview.stats.avgLength')}
              value={statistics.avgLength}
              suffix={t('common.characters')}
            />
          </Col>
          <Col span={8}>
            <Statistic
              title={t('chunkingConfig.preview.stats.totalChars')}
              value={statistics.totalChars}
            />
          </Col>
        </Row>
        <Divider style={{ margin: '12px 0' }} />
        <Row gutter={16}>
          <Col span={12}>
            <Statistic
              title={t('chunkingConfig.preview.stats.minLength')}
              value={statistics.minLength}
              valueStyle={{ fontSize: '16px' }}
            />
          </Col>
          <Col span={12}>
            <Statistic
              title={t('chunkingConfig.preview.stats.maxLength')}
              value={statistics.maxLength}
              valueStyle={{ fontSize: '16px' }}
            />
          </Col>
        </Row>
      </Card>
    )
  }

  // 渲染分块结果
  const renderChunkResults = () => {
    if (!previewResult || !previewResult.chunks || previewResult.chunks.length === 0) {
      return (
        <Alert
          title={t('chunkingConfig.preview.noChunks')}
          type="info"
          showIcon
        />
      )
    }

    return (
      <div className="chunk-results">
        <div className="chunk-results-header">
          <Tag color="blue">
            {t('chunkingConfig.preview.chunkCount', { count: previewResult.chunks.length })}
          </Tag>
        </div>
        <div className="chunk-list">
          {previewResult.chunks.map((chunk, index) => (
            <Card
              key={index}
              size="small"
              title={
                <Space>
                  <Tag color="green">{t('chunkingConfig.preview.chunkIndex', { index: index + 1 })}</Tag>
                  <Tag>{t('chunkingConfig.preview.chunkLength', { length: chunk.length })}</Tag>
                </Space>
              }
              className="chunk-card"
            >
              <pre className="chunk-content">{chunk}</pre>
            </Card>
          ))}
        </div>
      </div>
    )
  }

  // 渲染对比结果
  const renderComparisonResults = () => {
    if (comparisonResults.length === 0) return null

    return (
      <Card title={t('chunkingConfig.comparison.title')} className="comparison-card">
        <div className="comparison-results">
          {comparisonResults.map((result, index) => (
            <Card
              key={index}
              size="small"
              title={getStrategyDisplayName(result.strategyName)}
              className="comparison-item"
            >
              <Row gutter={16}>
                <Col span={6}>
                  <Statistic
                    title={t('chunkingConfig.comparison.result.chunks')}
                    value={result.statistics.totalChunks}
                  />
                </Col>
                <Col span={6}>
                  <Statistic
                    title={t('chunkingConfig.comparison.result.avgLength')}
                    value={result.statistics.avgLength}
                  />
                </Col>
                <Col span={6}>
                  <Statistic
                    title={t('chunkingConfig.preview.stats.minLength')}
                    value={result.statistics.minLength}
                  />
                </Col>
                <Col span={6}>
                  <Statistic
                    title={t('chunkingConfig.preview.stats.maxLength')}
                    value={result.statistics.maxLength}
                  />
                </Col>
              </Row>
            </Card>
          ))}
        </div>
      </Card>
    )
  }

  return (
    <div className="chunking-config-container">
      <Spin spinning={loading}>
        {/* 页面标题 */}
        <div className="page-header">
          <h1>{t('chunkingConfig.title')}</h1>
          <p className="subtitle">{t('chunkingConfig.subtitle')}</p>
          
          {/* 文档信息卡片 */}
          {documentId && documentInfo && (
            <Card
              size="small"
              style={{ marginTop: 16, background: '#f0f5ff', borderColor: '#adc6ff' }}
              bodyStyle={{ padding: '12px 16px' }}
            >
              <Space direction="vertical" style={{ width: '100%' }} size="small">
                <div style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
                  <span style={{ fontSize: 16, fontWeight: 600, color: '#1890ff' }}>
                    📄 {t('chunkingConfig.currentDocument')}
                  </span>
                  <Tag color="blue" style={{ fontSize: 14 }}>{documentInfo.fileName}</Tag>
                </div>
                
                <Space size="large" wrap>
                  {documentInfo.fileSize && (
                    <Space size={4}>
                      <span style={{ color: '#666' }}>{t('common.fileSize')}:</span>
                      <Tag color="green">{(documentInfo.fileSize / 1024).toFixed(2)} KB</Tag>
                    </Space>
                  )}
                  
                  {documentInfo.mimeType && (
                    <Space size={4}>
                      <span style={{ color: '#666' }}>{t('common.fileType')}:</span>
                      <Tag color="cyan">{documentInfo.mimeType}</Tag>
                    </Space>
                  )}
                  
                  {documentConfig?.extractedText && (
                    <Space size={4}>
                      <span style={{ color: '#666' }}>{t('chunkingConfig.extractedLength')}:</span>
                      <Tag color="purple">{documentConfig.extractedText.length} {t('common.characters')}</Tag>
                    </Space>
                  )}
                  
                  {documentInfo.uploadTime && (
                    <Space size={4}>
                      <span style={{ color: '#666' }}>{t('common.uploadTime')}:</span>
                      <Tag color="orange">{new Date(documentInfo.uploadTime).toLocaleString()}</Tag>
                    </Space>
                  )}
                </Space>
              </Space>
            </Card>
          )}
          
          {/* 无文档时的提示 */}
          {!documentId && (
            <Alert
              message={t('chunkingConfig.message.noDocumentSelected')}
              description={t('chunkingConfig.message.selectDocumentFirst')}
              type="warning"
              showIcon
              style={{ marginTop: 16 }}
            />
          )}
        </div>

        <Row gutter={24}>
          {/* 左侧：策略选择和参数配置 */}
          <Col xs={24} lg={8}>
            <Card
              title={
                <Space>
                  <SettingOutlined />
                  {t('chunkingConfig.strategyList.title')}
                </Space>
              }
              className="strategy-card"
            >
              {/* 策略选择 */}
              <div className="strategy-selector">
                <Select
                  style={{ width: '100%' }}
                  placeholder={t('chunkingConfig.strategyList.selectStrategy')}
                  value={currentStrategy?.name}
                  onChange={(value) => {
                    const strategy = strategies.find((s) => s.name === value)
                    selectStrategy(strategy)
                  }}
                >
                  {strategies.map((strategy) => (
                    <Option key={strategy.name} value={strategy.name}>
                      <Space>
                        {getStrategyDisplayName(strategy.name)}
                        <Tag color="blue">{strategy.name}</Tag>
                      </Space>
                    </Option>
                  ))}
                </Select>

                {currentStrategy && (
                  <Alert
                    title={getStrategyDescription(currentStrategy.name)}
                    type="info"
                    showIcon
                    style={{ marginTop: 16 }}
                  />
                )}
              </div>

              <Divider />

              {/* 参数配置 */}
              <div className="params-config">
                <h3>{t('chunkingConfig.params.title')}</h3>
                {renderParamsForm()}
              </div>

              <Divider />

              {/* 对比模式 */}
              <div className="comparison-mode">
                <Space vertical style={{ width: '100%' }}>
                  <Space>
                    <SwapOutlined />
                    <span>{t('chunkingConfig.comparison.title')}</span>
                    <Switch
                      checked={comparisonMode}
                      onChange={setComparisonMode}
                    />
                  </Space>

                  {comparisonMode && (
                    <>
                      <Select
                        mode="multiple"
                        style={{ width: '100%' }}
                        placeholder={t('chunkingConfig.comparison.selectStrategies')}
                        value={comparisonStrategies}
                        onChange={setComparisonStrategies}
                      >
                        {strategies.map((strategy) => (
                          <Option key={strategy.name} value={strategy.name}>
                            {getStrategyDisplayName(strategy.name)}
                          </Option>
                        ))}
                      </Select>

                      <Button
                        type="primary"
                        icon={<SwapOutlined />}
                        onClick={handleComparison}
                        block
                      >
                        {t('chunkingConfig.comparison.compareButton')}
                      </Button>
                    </>
                  )}
                </Space>
              </div>
            </Card>
          </Col>

          {/* 右侧：预览区域 */}
          <Col xs={24} lg={16}>
            <Card
              title={
                <Space>
                  <EyeOutlined />
                  {t('chunkingConfig.preview.title')}
                </Space>
              }
              extra={
                <Button
                  icon={<ReloadOutlined />}
                  onClick={handleClearPreview}
                >
                  {t('chunkingConfig.preview.clearButton')}
                </Button>
              }
              className="preview-card"
            >
              {/* 输入文本 */}
              <div className="preview-input">
                <h3>{t('chunkingConfig.preview.inputText')}</h3>
                <TextArea
                  rows={15}
                  placeholder={t('chunkingConfig.preview.inputPlaceholder')}
                  value={previewText}
                  onChange={(e) => setPreviewText(e.target.value)}
                  style={{ minHeight: '400px' }}
                />
              </div>

              <Divider />

              {/* 统计信息 */}
              {renderStatistics()}

              {/* 对比结果 */}
              {comparisonMode && renderComparisonResults()}

              {/* 分块结果 */}
              {!comparisonMode && (
                <>
                  <Divider />
                  <div className="preview-results">
                    <h3>{t('chunkingConfig.preview.chunkResult')}</h3>
                    {renderChunkResults()}
                  </div>
                </>
              )}
            </Card>
          </Col>
        </Row>

        {/* ⭐ 步骤导航按钮 */}
        {documentId && (
          <Card style={{ marginTop: 24 }}>
            <Space style={{ width: '100%', justifyContent: 'space-between' }}>
              <Button
                icon={<ArrowLeftOutlined />}
                onClick={handlePreviousStep}
                disabled={loading}
              >
                {t('chunkingConfig.actions.previousStep')}
              </Button>
              <Button
                type="primary"
                icon={<ThunderboltOutlined />}
                onClick={handleExecuteChunking}
                size="large"
                disabled={loading || !currentStrategy}
                loading={loading}
              >
                {t('chunkingConfig.actions.executeChunking')}
              </Button>
            </Space>
          </Card>
        )}
      </Spin>
    </div>
  )
}

export default ChunkingConfig

