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
  message,
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

function TextExtractionConfig() {
  const { t, language } = useLanguage()
  const [selectedModel, setSelectedModel] = useState('standard')
  const [loading, setLoading] = useState(false)
  const [systemConfig, setSystemConfig] = useState(null)

  // 加载系统配置
  useEffect(() => {
    loadSystemConfig()
  }, [])

  const loadSystemConfig = async () => {
    try {
      const response = await fetch('/api/system/rag-config')
      const result = await response.json()
      if (result.success) {
        setSystemConfig(result.data)
        setSelectedModel(result.data.defaultTextExtractionModel || 'standard')
      }
    } catch (error) {
      console.error('加载系统配置失败:', error)
    }
  }

  const handleModelChange = (value) => {
    setSelectedModel(value)
  }

  const handleApply = async () => {
    setLoading(true)
    try {
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
        message.success('配置已保存')
        loadSystemConfig()
      } else {
        message.error(result.message || '保存失败')
      }
    } catch (error) {
      console.error('保存配置失败:', error)
      message.error('保存失败')
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
          <Card title="文本提取模型选择">
            <Space direction="vertical" style={{ width: '100%' }} size="large">
              <Alert
                message="提示"
                description="文本提取是RAG流程的第一步，选择合适的提取模型可以提高后续处理的准确度。"
                type="info"
                showIcon
              />

              <div className="model-selector">
                <label className="config-label">选择提取模型:</label>
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
                  <Space direction="vertical" style={{ width: '100%' }}>
                    <div className="config-item">
                      <Space>
                        <CheckCircleOutlined style={{ color: '#52c41a' }} />
                        <span>默认文本提取模型:</span>
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
                    loading={loading}
                    size="large"
                  >
                    应用配置
                  </Button>
                  <Button onClick={loadSystemConfig} size="large">
                    重置
                  </Button>
                </Space>
              </div>
            </Space>
          </Card>
        </div>

        {/* 右侧：预览/说明 */}
        <div className="preview-panel">
          <Card
            title={
              <Space>
                {currentModel.icon}
                <span>{language === 'zh' ? currentModel.name : currentModel.nameEn}</span>
              </Space>
            }
            bordered={false}
            className="model-info-card"
            style={{
              borderLeft: `4px solid ${currentModel.color}`,
            }}
          >
            <Space direction="vertical" size="large" style={{ width: '100%' }}>
              {/* 模型描述 */}
              <div className="model-description">
                <h4>模型说明</h4>
                <p>{language === 'zh' ? currentModel.description : currentModel.descriptionEn}</p>
              </div>

              {/* 特性列表 */}
              <div className="model-features">
                <h4>主要特性</h4>
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
                <h4>适用场景</h4>
                <Descriptions column={1} size="small">
                  {selectedModel === 'standard' && (
                    <>
                      <Descriptions.Item label="适用文件">
                        TXT, MD, 纯文本文档
                      </Descriptions.Item>
                      <Descriptions.Item label="处理速度">
                        ⚡ 非常快
                      </Descriptions.Item>
                      <Descriptions.Item label="资源消耗">
                        💾 低
                      </Descriptions.Item>
                    </>
                  )}
                  {selectedModel === 'vision-llm' && (
                    <>
                      <Descriptions.Item label="适用文件">
                        PPT, PPTX, PDF(图表), 图片
                      </Descriptions.Item>
                      <Descriptions.Item label="处理速度">
                        🐢 较慢（需要LLM推理）
                      </Descriptions.Item>
                      <Descriptions.Item label="资源消耗">
                        💾 高（需要GPU）
                      </Descriptions.Item>
                      <Descriptions.Item label="特殊能力">
                        🎯 可以理解图表、流程图、架构图
                      </Descriptions.Item>
                    </>
                  )}
                  {selectedModel === 'ocr' && (
                    <>
                      <Descriptions.Item label="适用文件">
                        扫描件PDF, 图片
                      </Descriptions.Item>
                      <Descriptions.Item label="处理速度">
                        🚀 快
                      </Descriptions.Item>
                      <Descriptions.Item label="资源消耗">
                        💾 中等
                      </Descriptions.Item>
                      <Descriptions.Item label="语言支持">
                        🌍 多语言（中英日韩等）
                      </Descriptions.Item>
                    </>
                  )}
                </Descriptions>
              </div>

              {/* 提示信息 */}
              <Alert
                message="提示"
                description="保存配置后，新上传的文档将使用选择的模型进行文本提取。已处理的文档可以在文档管理中重新提取。"
                type="warning"
                showIcon
              />
            </Space>
          </Card>
        </div>
      </div>
    </div>
  )
}

export default TextExtractionConfig

