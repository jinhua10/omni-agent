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
} from 'antd'
import {
  FileTextOutlined,
  EyeOutlined,
  ScanOutlined,
  ThunderboltOutlined,
  CheckCircleOutlined,
} from '@ant-design/icons'
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

  // 加载系统配置
  useEffect(() => {
    loadSystemConfig()
    if (documentId) {
      loadDocumentConfig()
    }
  }, [documentId])

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

  // 自动提取处理（流式）
  const handleAutoExtract = async () => {
    if (!documentId || extracting) return
    
    setExtracting(true)
    setExtractionProgress({ status: 'processing', percent: 0 })
    message.info(t('textExtractionConfig.tips.autoExtractionStarted') || '开始自动提取...')

    try {
      // ⭐ 对URL中的documentId进行编码
      const encodedDocId = encodeURIComponent(documentId)
      // 新API：迁移到 DocumentProcessingController
      const response = await fetch(`/api/documents/processing/${encodedDocId}/extract`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          model: selectedModel,
          streaming: true
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
              } else if (data.type === 'accuracy') {
                // ⭐ 保存精度信息
                setExtractionProgress(prev => ({
                  ...prev,
                  accuracy: data.value,
                  message: data.message
                }))
              } else if (data.type === 'content') {
                console.log('📄 累加文本内容，长度:', data.content?.length || 0)
                setExtractionResult(prev => prev + (data.content || ''))
              } else if (data.type === 'complete') {
                setExtractionProgress({ 
                  status: 'success', 
                  percent: 100,
                  accuracy: data.accuracy || 0.85
                })
                message.success(t('textExtractionConfig.tips.extractionComplete') || '提取完成')
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
                </Space>
              }
              style={{ height: '100%' }}
              bodyStyle={{ height: 'calc(100% - 57px)', padding: 0 }}
            >
              <TextArea
                value={extractionResult}
                readOnly
                style={{ 
                  height: '100%',
                  fontFamily: 'monospace',
                  fontSize: '13px',
                  lineHeight: '1.6',
                  border: 'none',
                  resize: 'none'
                }}
                placeholder="提取的文本内容将显示在这里..."
              />
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

