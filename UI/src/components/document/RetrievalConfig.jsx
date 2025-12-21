/**
 * 检索参数配置组件 (Retrieval Configuration Component)
 *
 * 提供交互式的检索参数配置和实时测试功能
 * (Provides interactive configuration and real-time testing of retrieval parameters)
 *
 * Phase 4.2.3 - 检索参数配置界面
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */

import React, { useState, useEffect } from 'react'
import {
  Card,
  Row,
  Col,
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
  message,
  Spin,
  Alert,
  Slider,
  Select,
  List,
  Empty,
} from 'antd'
import {
  SettingOutlined,
  ThunderboltOutlined,
  ReloadOutlined,
  InfoCircleOutlined,
  CheckCircleOutlined,
  ClockCircleOutlined,
} from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/document/RetrievalConfig.css'

const { TextArea } = Input
const { Option } = Select

function RetrievalConfig() {
  const { t } = useLanguage()
  const [form] = Form.useForm()

  // 状态管理
  const [loading, setLoading] = useState(false)
  const [config, setConfig] = useState(null)
  const [strategies, setStrategies] = useState([])
  const [testQuery, setTestQuery] = useState('')
  const [testResult, setTestResult] = useState(null)

  // 加载配置和策略
  useEffect(() => {
    loadConfig()
    loadStrategies()
  }, [])

  // 加载当前配置
  const loadConfig = async () => {
    setLoading(true)
    try {
      const response = await fetch('/api/retrieval/config')
      const result = await response.json()

      if (result.success && result.data) {
        setConfig(result.data)
        form.setFieldsValue(result.data)
        message.success(t('retrievalConfig.message.configLoadSuccess'))
      } else {
        message.error(t('retrievalConfig.message.configLoadFailed'))
      }
    } catch (error) {
      console.error('Failed to load config:', error)
      message.error(t('retrievalConfig.message.configLoadFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 加载检索策略
  const loadStrategies = async () => {
    try {
      const response = await fetch('/api/retrieval/strategies')
      const result = await response.json()

      if (result.success && result.data) {
        setStrategies(result.data)
      }
    } catch (error) {
      console.error('Failed to load strategies:', error)
    }
  }

  // 保存配置
  const handleSaveConfig = async () => {
    try {
      const values = form.getFieldsValue()
      setLoading(true)

      const response = await fetch('/api/retrieval/config', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(values),
      })

      const result = await response.json()

      if (result.success) {
        message.success(t('retrievalConfig.message.configSaveSuccess'))
        setConfig(values)
      } else {
        message.error(result.message || t('retrievalConfig.message.configSaveFailed'))
      }
    } catch (error) {
      console.error('Failed to save config:', error)
      message.error(t('retrievalConfig.message.configSaveFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 测试检索
  const handleTest = async () => {
    if (!testQuery.trim()) {
      message.warning(t('retrievalConfig.message.inputRequired'))
      return
    }

    setLoading(true)
    try {
      const values = form.getFieldsValue()
      const response = await fetch('/api/retrieval/test', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          query: testQuery,
          topK: values.topK,
          similarityThreshold: values.similarityThreshold,
          retrievalStrategy: values.retrievalStrategy,
          rerankerEnabled: values.rerankerEnabled,
        }),
      })

      const result = await response.json()

      if (result.success && result.data) {
        setTestResult(result.data)
        message.success(t('retrievalConfig.message.testSuccess'))
      } else {
        message.error(result.message || t('retrievalConfig.message.testFailed'))
      }
    } catch (error) {
      console.error('Failed to test:', error)
      message.error(t('retrievalConfig.message.testFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 重置表单
  const handleReset = () => {
    form.resetFields()
    if (config) {
      form.setFieldsValue(config)
    }
  }

  // 清除测试
  const handleClearTest = () => {
    setTestQuery('')
    setTestResult(null)
  }

  // 获取策略描述
  const getStrategyDescription = (strategyName) => {
    const descMap = {
      vector: t('retrievalConfig.strategy.description.vector'),
      fulltext: t('retrievalConfig.strategy.description.fulltext'),
      hybrid: t('retrievalConfig.strategy.description.hybrid'),
    }
    return descMap[strategyName] || ''
  }

  // 渲染配置表单
  const renderConfigForm = () => {
    return (
      <Form form={form} layout="vertical">
        {/* 基础配置 */}
        <h3>{t('retrievalConfig.basicConfig.title')}</h3>

        <Form.Item
          name="topK"
          label={
            <Space>
              {t('retrievalConfig.basicConfig.topK')}
              <Tooltip title={t('retrievalConfig.basicConfig.topKHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <InputNumber min={1} max={100} style={{ width: '100%' }} />
        </Form.Item>

        <Form.Item
          name="similarityThreshold"
          label={
            <Space>
              {t('retrievalConfig.basicConfig.similarityThreshold')}
              <Tooltip title={t('retrievalConfig.basicConfig.similarityThresholdHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <Slider min={0} max={1} step={0.05} marks={{ 0: '0', 0.5: '0.5', 1: '1' }} />
        </Form.Item>

        <Form.Item
          name="timeoutSeconds"
          label={
            <Space>
              {t('retrievalConfig.basicConfig.timeout')}
              <Tooltip title={t('retrievalConfig.basicConfig.timeoutHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <InputNumber min={1} max={60} style={{ width: '100%' }} />
        </Form.Item>

        <Divider />

        {/* 检索策略 */}
        <h3>{t('retrievalConfig.strategy.title')}</h3>

        <Form.Item
          name="retrievalStrategy"
          label={t('retrievalConfig.strategy.select')}
        >
          <Select>
            {strategies.map((strategy) => (
              <Option key={strategy.name} value={strategy.name}>
                {strategy.displayName}
              </Option>
            ))}
          </Select>
        </Form.Item>

        {form.getFieldValue('retrievalStrategy') && (
          <Alert
            message={getStrategyDescription(form.getFieldValue('retrievalStrategy'))}
            type="info"
            showIcon
            style={{ marginBottom: 16 }}
          />
        )}

        {/* 混合检索权重 */}
        {form.getFieldValue('retrievalStrategy') === 'hybrid' && (
          <>
            <Divider />
            <h3>{t('retrievalConfig.hybridWeights.title')}</h3>
            <Alert
              message={t('retrievalConfig.hybridWeights.weightTip')}
              type="info"
              showIcon
              style={{ marginBottom: 16 }}
            />

            <Form.Item
              name="vectorWeight"
              label={
                <Space>
                  {t('retrievalConfig.hybridWeights.vectorWeight')}
                  <Tooltip title={t('retrievalConfig.hybridWeights.vectorWeightHelp')}>
                    <InfoCircleOutlined />
                  </Tooltip>
                </Space>
              }
            >
              <Slider min={0} max={1} step={0.1} marks={{ 0: '0', 0.5: '0.5', 1: '1' }} />
            </Form.Item>

            <Form.Item
              name="fullTextWeight"
              label={
                <Space>
                  {t('retrievalConfig.hybridWeights.fulltextWeight')}
                  <Tooltip title={t('retrievalConfig.hybridWeights.fulltextWeightHelp')}>
                    <InfoCircleOutlined />
                  </Tooltip>
                </Space>
              }
            >
              <Slider min={0} max={1} step={0.1} marks={{ 0: '0', 0.5: '0.5', 1: '1' }} />
            </Form.Item>
          </>
        )}

        <Divider />

        {/* 重排序 */}
        <h3>{t('retrievalConfig.reranker.title')}</h3>

        <Form.Item
          name="rerankerEnabled"
          label={
            <Space>
              {t('retrievalConfig.reranker.enable')}
              <Tooltip title={t('retrievalConfig.reranker.enableHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
          valuePropName="checked"
        >
          <Switch />
        </Form.Item>

        {form.getFieldValue('rerankerEnabled') && (
          <Form.Item
            name="rerankerModel"
            label={
              <Space>
                {t('retrievalConfig.reranker.model')}
                <Tooltip title={t('retrievalConfig.reranker.modelHelp')}>
                  <InfoCircleOutlined />
                </Tooltip>
              </Space>
            }
          >
            <Select>
              <Option value="bge-reranker">{t('retrievalConfig.reranker.models.bgeReranker')}</Option>
              <Option value="cross-encoder">{t('retrievalConfig.reranker.models.crossEncoder')}</Option>
              <Option value="colbert">{t('retrievalConfig.reranker.models.colbert')}</Option>
            </Select>
          </Form.Item>
        )}

        <Divider />

        {/* 并行配置 */}
        <h3>{t('retrievalConfig.parallel.title')}</h3>

        <Form.Item
          name="parallelEnabled"
          label={
            <Space>
              {t('retrievalConfig.parallel.enable')}
              <Tooltip title={t('retrievalConfig.parallel.enableHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
          valuePropName="checked"
        >
          <Switch />
        </Form.Item>

        <Form.Item>
          <Space>
            <Button type="primary" icon={<SettingOutlined />} onClick={handleSaveConfig}>
              {t('retrievalConfig.actions.save')}
            </Button>
            <Button icon={<ReloadOutlined />} onClick={handleReset}>
              {t('retrievalConfig.actions.reset')}
            </Button>
          </Space>
        </Form.Item>
      </Form>
    )
  }

  // 渲染测试结果
  const renderTestResult = () => {
    if (!testResult) {
      return <Empty description={t('retrievalConfig.test.noResults')} />
    }

    return (
      <div className="test-result">
        {/* 统计信息 */}
        {testResult.statistics && (
          <Card size="small" className="test-stats" style={{ marginBottom: 16 }}>
            <Row gutter={16}>
              <Col span={6}>
                <Statistic
                  title={t('retrievalConfig.test.stats.totalResults')}
                  value={testResult.totalResults}
                  prefix={<CheckCircleOutlined />}
                />
              </Col>
              <Col span={6}>
                <Statistic
                  title={t('retrievalConfig.test.stats.retrievalTime')}
                  value={testResult.retrievalTime}
                  suffix="ms"
                  prefix={<ClockCircleOutlined />}
                />
              </Col>
              <Col span={6}>
                <Statistic
                  title={t('retrievalConfig.test.stats.vectorResults')}
                  value={testResult.statistics.vectorResults}
                />
              </Col>
              <Col span={6}>
                <Statistic
                  title={t('retrievalConfig.test.stats.fulltextResults')}
                  value={testResult.statistics.fulltextResults}
                />
              </Col>
            </Row>
            <Divider style={{ margin: '12px 0' }} />
            <Row gutter={16}>
              <Col span={8}>
                <Statistic
                  title={t('retrievalConfig.test.stats.avgScore')}
                  value={testResult.statistics.avgScore}
                  precision={2}
                  valueStyle={{ fontSize: '16px' }}
                />
              </Col>
              <Col span={8}>
                <Statistic
                  title={t('retrievalConfig.test.stats.minScore')}
                  value={testResult.statistics.minScore}
                  precision={2}
                  valueStyle={{ fontSize: '16px' }}
                />
              </Col>
              <Col span={8}>
                <Statistic
                  title={t('retrievalConfig.test.stats.maxScore')}
                  value={testResult.statistics.maxScore}
                  precision={2}
                  valueStyle={{ fontSize: '16px' }}
                />
              </Col>
            </Row>
          </Card>
        )}

        {/* 结果列表 */}
        <List
          header={
            <Space>
              <strong>{t('retrievalConfig.test.resultCount', { count: testResult.results?.length || 0 })}</strong>
            </Space>
          }
          bordered
          dataSource={testResult.results || []}
          renderItem={(item, index) => (
            <List.Item>
              <List.Item.Meta
                title={
                  <Space>
                    <Tag color="green">#{index + 1}</Tag>
                    <span>{item.documentName}</span>
                    <Tag color={item.source === 'vector' ? 'blue' : 'orange'}>
                      {item.source === 'vector'
                        ? t('retrievalConfig.test.result.vectorSource')
                        : t('retrievalConfig.test.result.fulltextSource')}
                    </Tag>
                    <Tag color="purple">{t('retrievalConfig.test.result.score')}: {item.score.toFixed(3)}</Tag>
                  </Space>
                }
                description={item.content}
              />
            </List.Item>
          )}
        />
      </div>
    )
  }

  return (
    <div className="retrieval-config-container">
      <Spin spinning={loading}>
        {/* 页面标题 */}
        <div className="page-header">
          <h1>{t('retrievalConfig.title')}</h1>
          <p className="subtitle">{t('retrievalConfig.subtitle')}</p>
        </div>

        <Row gutter={24}>
          {/* 左侧：配置面板 */}
          <Col xs={24} lg={10}>
            <Card
              title={
                <Space>
                  <SettingOutlined />
                  {t('retrievalConfig.basicConfig.title')}
                </Space>
              }
              className="config-card"
            >
              {renderConfigForm()}
            </Card>
          </Col>

          {/* 右侧：测试区域 */}
          <Col xs={24} lg={14}>
            <Card
              title={
                <Space>
                  <ThunderboltOutlined />
                  {t('retrievalConfig.test.title')}
                </Space>
              }
              extra={
                <Button
                  icon={<ReloadOutlined />}
                  onClick={handleClearTest}
                >
                  {t('retrievalConfig.test.clearButton')}
                </Button>
              }
              className="test-card"
            >
              {/* 输入区域 */}
              <div className="test-input">
                <h3>{t('retrievalConfig.test.inputQuery')}</h3>
                <TextArea
                  rows={3}
                  placeholder={t('retrievalConfig.test.inputPlaceholder')}
                  value={testQuery}
                  onChange={(e) => setTestQuery(e.target.value)}
                />

                <Button
                  type="primary"
                  icon={<ThunderboltOutlined />}
                  onClick={handleTest}
                  style={{ marginTop: 16 }}
                  block
                >
                  {t('retrievalConfig.test.testButton')}
                </Button>
              </div>

              <Divider />

              {/* 测试结果 */}
              <div className="test-results">
                <h3>{t('retrievalConfig.test.stats.title')}</h3>
                {renderTestResult()}
              </div>
            </Card>
          </Col>
        </Row>
      </Spin>
    </div>
  )
}

export default RetrievalConfig

