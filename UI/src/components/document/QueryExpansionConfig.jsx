/**
 * 查询扩展配置组件 (Query Expansion Configuration Component)
 *
 * 提供交互式的查询扩展策略配置和实时预览功能
 * (Provides interactive configuration and real-time preview of query expansion strategies)
 *
 * Phase 4.2.2 - 查询扩展配置界面
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
  Progress,
  Modal,
  List,
  Empty,
} from 'antd'
import {
  SettingOutlined,
  EyeOutlined,
  ReloadOutlined,
  InfoCircleOutlined,
  ThunderboltOutlined,
  DatabaseOutlined,
  BookOutlined,
  PlusOutlined,
  DeleteOutlined,
  EditOutlined,
  ExportOutlined,
  ImportOutlined,
} from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/document/QueryExpansionConfig.css'

const { TextArea } = Input

function QueryExpansionConfig() {
  const { t } = useLanguage()
  const [form] = Form.useForm()

  // 状态管理
  const [loading, setLoading] = useState(false)
  const [config, setConfig] = useState(null)
  const [previewQuery, setPreviewQuery] = useState('')
  const [previewResult, setPreviewResult] = useState(null)
  const [cacheStats, setCacheStats] = useState(null)
  const [dictionary, setDictionary] = useState({ domainTerms: {}, totalTerms: 0 })
  const [enableSynonym, setEnableSynonym] = useState(true)
  const [enableLlm, setEnableLlm] = useState(true)
  const [enableDomain, setEnableDomain] = useState(true)
  const [dictionaryModalVisible, setDictionaryModalVisible] = useState(false)

  // 加载配置
  useEffect(() => {
    loadConfig()
    loadCacheStats()
    loadDictionary()
  }, [])

  // 加载当前配置
  const loadConfig = async () => {
    setLoading(true)
    try {
      const response = await fetch('/api/query-expansion/config')
      const result = await response.json()

      if (result.success && result.data) {
        setConfig(result.data)
        form.setFieldsValue(result.data)
        message.success(t('queryExpansionConfig.message.configLoadSuccess'))
      } else {
        message.error(t('queryExpansionConfig.message.configLoadFailed'))
      }
    } catch (error) {
      console.error('Failed to load config:', error)
      message.error(t('queryExpansionConfig.message.configLoadFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 加载缓存统计
  const loadCacheStats = async () => {
    try {
      const response = await fetch('/api/query-expansion/cache/stats')
      const result = await response.json()

      if (result.success && result.data) {
        setCacheStats(result.data)
      }
    } catch (error) {
      console.error('Failed to load cache stats:', error)
    }
  }

  // 加载领域词典
  const loadDictionary = async () => {
    try {
      const response = await fetch('/api/query-expansion/dictionary')
      const result = await response.json()

      if (result.success && result.data) {
        setDictionary(result.data)
      }
    } catch (error) {
      console.error('Failed to load dictionary:', error)
    }
  }

  // 保存配置
  const handleSaveConfig = async () => {
    try {
      const values = form.getFieldsValue()
      setLoading(true)

      const response = await fetch('/api/query-expansion/config', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(values),
      })

      const result = await response.json()

      if (result.success) {
        message.success(t('queryExpansionConfig.message.configSaveSuccess'))
        setConfig(values)
      } else {
        message.error(result.message || t('queryExpansionConfig.message.configSaveFailed'))
      }
    } catch (error) {
      console.error('Failed to save config:', error)
      message.error(t('queryExpansionConfig.message.configSaveFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 预览查询扩展
  const handlePreview = async () => {
    if (!previewQuery.trim()) {
      message.warning(t('queryExpansionConfig.message.inputRequired'))
      return
    }

    setLoading(true)
    try {
      const response = await fetch('/api/query-expansion/preview', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          query: previewQuery,
          enableSynonym,
          enableLlm,
          enableDomain,
        }),
      })

      const result = await response.json()

      if (result.success && result.data) {
        setPreviewResult(result.data)
        message.success(t('queryExpansionConfig.message.previewSuccess'))
      } else {
        message.error(result.message || t('queryExpansionConfig.message.previewFailed'))
      }
    } catch (error) {
      console.error('Failed to preview:', error)
      message.error(t('queryExpansionConfig.message.previewFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 清除缓存
  const handleClearCache = () => {
    Modal.confirm({
      title: t('queryExpansionConfig.cacheConfig.clearCache'),
      content: t('queryExpansionConfig.cacheConfig.clearCacheConfirm'),
      onOk: async () => {
        try {
          const response = await fetch('/api/query-expansion/cache/clear', {
            method: 'POST',
          })

          const result = await response.json()

          if (result.success) {
            message.success(t('queryExpansionConfig.message.cacheCleared'))
            loadCacheStats()
          } else {
            message.error(t('queryExpansionConfig.message.cacheClearFailed'))
          }
        } catch (error) {
          console.error('Failed to clear cache:', error)
          message.error(t('queryExpansionConfig.message.cacheClearFailed'))
        }
      },
    })
  }

  // 重置表单
  const handleReset = () => {
    form.resetFields()
    if (config) {
      form.setFieldsValue(config)
    }
  }

  // 清除预览
  const handleClearPreview = () => {
    setPreviewQuery('')
    setPreviewResult(null)
  }

  // 渲染配置表单
  const renderConfigForm = () => {
    return (
      <Form form={form} layout="vertical">
        {/* 基础配置 */}
        <h3>{t('queryExpansionConfig.basicConfig.title')}</h3>

        <Form.Item
          name="llmExpansionEnabled"
          label={
            <Space>
              {t('queryExpansionConfig.basicConfig.llmExpansion')}
              <Tooltip title={t('queryExpansionConfig.basicConfig.llmExpansionHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
          valuePropName="checked"
        >
          <Switch />
        </Form.Item>

        <Form.Item
          name="maxExpandedQueries"
          label={
            <Space>
              {t('queryExpansionConfig.basicConfig.maxQueries')}
              <Tooltip title={t('queryExpansionConfig.basicConfig.maxQueriesHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <InputNumber min={1} max={10} style={{ width: '100%' }} />
        </Form.Item>

        <Divider />

        {/* 策略权重 */}
        <h3>{t('queryExpansionConfig.strategyWeights.title')}</h3>
        <Alert
          message={t('queryExpansionConfig.strategyWeights.weightTip')}
          type="info"
          showIcon
          style={{ marginBottom: 16 }}
        />

        <Form.Item
          name="synonymWeight"
          label={
            <Space>
              {t('queryExpansionConfig.strategyWeights.synonym')}
              <Tooltip title={t('queryExpansionConfig.strategyWeights.synonymHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <Slider min={0} max={1} step={0.1} marks={{ 0: '0', 0.5: '0.5', 1: '1' }} />
        </Form.Item>

        <Form.Item
          name="llmWeight"
          label={
            <Space>
              {t('queryExpansionConfig.strategyWeights.llm')}
              <Tooltip title={t('queryExpansionConfig.strategyWeights.llmHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <Slider min={0} max={1} step={0.1} marks={{ 0: '0', 0.5: '0.5', 1: '1' }} />
        </Form.Item>

        <Form.Item
          name="domainWeight"
          label={
            <Space>
              {t('queryExpansionConfig.strategyWeights.domain')}
              <Tooltip title={t('queryExpansionConfig.strategyWeights.domainHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <Slider min={0} max={1} step={0.1} marks={{ 0: '0', 0.5: '0.5', 1: '1' }} />
        </Form.Item>

        <Divider />

        {/* 缓存配置 */}
        <h3>{t('queryExpansionConfig.cacheConfig.title')}</h3>

        <Form.Item
          name="cacheEnabled"
          label={
            <Space>
              {t('queryExpansionConfig.basicConfig.enableCache')}
              <Tooltip title={t('queryExpansionConfig.basicConfig.enableCacheHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
          valuePropName="checked"
        >
          <Switch />
        </Form.Item>

        <Form.Item
          name="cacheSize"
          label={
            <Space>
              {t('queryExpansionConfig.cacheConfig.cacheSize')}
              <Tooltip title={t('queryExpansionConfig.cacheConfig.cacheSizeHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <InputNumber min={100} max={100000} step={1000} style={{ width: '100%' }} />
        </Form.Item>

        <Form.Item
          name="cacheTtl"
          label={
            <Space>
              {t('queryExpansionConfig.cacheConfig.cacheTtl')}
              <Tooltip title={t('queryExpansionConfig.cacheConfig.cacheTtlHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <InputNumber min={1} max={1440} style={{ width: '100%' }} />
        </Form.Item>

        <Divider />

        {/* 并行配置 */}
        <h3>{t('queryExpansionConfig.parallelConfig.title')}</h3>

        <Form.Item
          name="parallelEnabled"
          label={
            <Space>
              {t('queryExpansionConfig.basicConfig.parallelExecution')}
              <Tooltip title={t('queryExpansionConfig.basicConfig.parallelExecutionHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
          valuePropName="checked"
        >
          <Switch />
        </Form.Item>

        <Form.Item
          name="parallelThreads"
          label={
            <Space>
              {t('queryExpansionConfig.parallelConfig.threads')}
              <Tooltip title={t('queryExpansionConfig.parallelConfig.threadsHelp')}>
                <InfoCircleOutlined />
              </Tooltip>
            </Space>
          }
        >
          <InputNumber min={1} max={32} style={{ width: '100%' }} />
        </Form.Item>

        <Form.Item>
          <Space>
            <Button type="primary" icon={<SettingOutlined />} onClick={handleSaveConfig}>
              {t('queryExpansionConfig.actions.save')}
            </Button>
            <Button icon={<ReloadOutlined />} onClick={handleReset}>
              {t('queryExpansionConfig.actions.reset')}
            </Button>
          </Space>
        </Form.Item>
      </Form>
    )
  }

  // 渲染缓存统计
  const renderCacheStats = () => {
    if (!cacheStats) return null

    return (
      <Card
        title={
          <Space>
            <DatabaseOutlined />
            {t('queryExpansionConfig.cacheStats.title')}
          </Space>
        }
        extra={
          <Space>
            <Button
              size="small"
              icon={<ReloadOutlined />}
              onClick={loadCacheStats}
            >
              {t('queryExpansionConfig.cacheStats.refreshStats')}
            </Button>
            <Button
              size="small"
              danger
              icon={<DeleteOutlined />}
              onClick={handleClearCache}
            >
              {t('queryExpansionConfig.actions.clear')}
            </Button>
          </Space>
        }
        className="cache-stats-card"
      >
        <Row gutter={16}>
          <Col span={8}>
            <Statistic
              title={t('queryExpansionConfig.cacheStats.hitRate')}
              value={cacheStats.hitRate * 100}
              precision={2}
              suffix="%"
              valueStyle={{ color: cacheStats.hitRate > 0.8 ? '#3f8600' : '#cf1322' }}
            />
            <Progress
              percent={cacheStats.hitRate * 100}
              status={cacheStats.hitRate > 0.8 ? 'success' : 'exception'}
              showInfo={false}
            />
          </Col>
          <Col span={8}>
            <Statistic
              title={t('queryExpansionConfig.cacheStats.cacheSize')}
              value={cacheStats.cacheSize}
              suffix={`/ ${cacheStats.maxSize}`}
            />
            <Progress
              percent={(cacheStats.cacheSize / cacheStats.maxSize) * 100}
              showInfo={false}
            />
          </Col>
          <Col span={8}>
            <Statistic
              title={t('queryExpansionConfig.cacheStats.totalRequests')}
              value={cacheStats.totalRequests}
            />
          </Col>
        </Row>
        <Divider style={{ margin: '12px 0' }} />
        <Row gutter={16}>
          <Col span={12}>
            <Statistic
              title={t('queryExpansionConfig.cacheStats.hitCount')}
              value={cacheStats.hitCount}
              valueStyle={{ fontSize: '16px', color: '#3f8600' }}
            />
          </Col>
          <Col span={12}>
            <Statistic
              title={t('queryExpansionConfig.cacheStats.missCount')}
              value={cacheStats.missCount}
              valueStyle={{ fontSize: '16px', color: '#cf1322' }}
            />
          </Col>
        </Row>
      </Card>
    )
  }

  // 渲染预览结果
  const renderPreviewResult = () => {
    if (!previewResult) {
      return (
        <Empty description={t('queryExpansionConfig.preview.noResults')} />
      )
    }

    return (
      <div className="preview-result">
        {/* 统计信息 */}
        {previewResult.statistics && (
          <Card size="small" className="preview-stats" style={{ marginBottom: 16 }}>
            <Row gutter={16}>
              <Col span={6}>
                <Statistic
                  title={t('queryExpansionConfig.preview.stats.originalLength')}
                  value={previewResult.statistics.originalLength}
                />
              </Col>
              <Col span={6}>
                <Statistic
                  title={t('queryExpansionConfig.preview.stats.avgLength')}
                  value={previewResult.statistics.avgExpandedLength}
                  precision={0}
                />
              </Col>
              <Col span={6}>
                <Statistic
                  title={t('queryExpansionConfig.preview.stats.expansionRate')}
                  value={previewResult.statistics.expansionRate}
                  suffix="x"
                />
              </Col>
              <Col span={6}>
                <Tag color="blue" style={{ fontSize: '16px', padding: '8px 16px' }}>
                  {t('queryExpansionConfig.preview.queryCount', { count: previewResult.totalQueries })}
                </Tag>
              </Col>
            </Row>
          </Card>
        )}

        {/* 扩展查询列表 */}
        <List
          header={<strong>{t('queryExpansionConfig.preview.expandedQueries')}</strong>}
          bordered
          dataSource={previewResult.expandedQueries}
          renderItem={(query, index) => (
            <List.Item>
              <Space>
                <Tag color="green">{index + 1}</Tag>
                <span>{query}</span>
              </Space>
            </List.Item>
          )}
        />
      </div>
    )
  }

  // 渲染领域词典
  const renderDictionary = () => {
    return (
      <Card
        title={
          <Space>
            <BookOutlined />
            {t('queryExpansionConfig.dictionary.title')}
          </Space>
        }
        extra={
          <Space>
            <Button size="small" icon={<PlusOutlined />} onClick={() => setDictionaryModalVisible(true)}>
              {t('queryExpansionConfig.dictionary.addDomain')}
            </Button>
            <Button size="small" icon={<ExportOutlined />}>
              {t('queryExpansionConfig.dictionary.exportDictionary')}
            </Button>
            <Button size="small" icon={<ImportOutlined />}>
              {t('queryExpansionConfig.dictionary.importDictionary')}
            </Button>
          </Space>
        }
        className="dictionary-card"
      >
        <Space direction="vertical" size="small" style={{ width: '100%', marginBottom: 16 }}>
          <Tag color="blue">{t('queryExpansionConfig.dictionary.totalDomains', { count: Object.keys(dictionary.domainTerms || {}).length })}</Tag>
          <Tag color="green">{t('queryExpansionConfig.dictionary.totalTerms', { count: dictionary.totalTerms })}</Tag>
        </Space>

        <List
          dataSource={Object.entries(dictionary.domainTerms || {})}
          renderItem={([domain, terms]) => (
            <List.Item
              actions={[
                <Button size="small" icon={<EditOutlined />}>
                  {t('queryExpansionConfig.dictionary.editDomain')}
                </Button>,
                <Button size="small" danger icon={<DeleteOutlined />}>
                  {t('queryExpansionConfig.dictionary.deleteDomain')}
                </Button>,
              ]}
            >
              <List.Item.Meta
                title={<Tag color="purple">{domain}</Tag>}
                description={
                  <Space wrap>
                    {terms.map((term, idx) => (
                      <Tag key={idx}>{term}</Tag>
                    ))}
                  </Space>
                }
              />
            </List.Item>
          )}
        />
      </Card>
    )
  }

  return (
    <div className="query-expansion-config-container">
      <Spin spinning={loading}>
        {/* 页面标题 */}
        <div className="page-header">
          <h1>{t('queryExpansionConfig.title')}</h1>
          <p className="subtitle">{t('queryExpansionConfig.subtitle')}</p>
        </div>

        <Row gutter={24}>
          {/* 左侧：配置面板 */}
          <Col xs={24} lg={10}>
            <Card
              title={
                <Space>
                  <SettingOutlined />
                  {t('queryExpansionConfig.basicConfig.title')}
                </Space>
              }
              className="config-card"
            >
              {renderConfigForm()}
            </Card>

            <div style={{ marginTop: 24 }}>
              {renderCacheStats()}
            </div>
          </Col>

          {/* 右侧：预览和词典 */}
          <Col xs={24} lg={14}>
            {/* 预览区域 */}
            <Card
              title={
                <Space>
                  <EyeOutlined />
                  {t('queryExpansionConfig.preview.title')}
                </Space>
              }
              extra={
                <Button
                  icon={<ReloadOutlined />}
                  onClick={handleClearPreview}
                >
                  {t('queryExpansionConfig.actions.clear')}
                </Button>
              }
              className="preview-card"
            >
              {/* 输入区域 */}
              <div className="preview-input">
                <h3>{t('queryExpansionConfig.preview.originalQuery')}</h3>
                <TextArea
                  rows={3}
                  placeholder={t('queryExpansionConfig.preview.inputPlaceholder')}
                  value={previewQuery}
                  onChange={(e) => setPreviewQuery(e.target.value)}
                />

                <Space style={{ marginTop: 16 }} wrap>
                  <span>{t('queryExpansionConfig.preview.enableStrategies')}:</span>
                  <Switch
                    checked={enableSynonym}
                    onChange={setEnableSynonym}
                    checkedChildren={t('queryExpansionConfig.preview.synonymExpansion')}
                    unCheckedChildren={t('queryExpansionConfig.preview.synonymExpansion')}
                  />
                  <Switch
                    checked={enableLlm}
                    onChange={setEnableLlm}
                    checkedChildren={t('queryExpansionConfig.preview.llmExpansion')}
                    unCheckedChildren={t('queryExpansionConfig.preview.llmExpansion')}
                  />
                  <Switch
                    checked={enableDomain}
                    onChange={setEnableDomain}
                    checkedChildren={t('queryExpansionConfig.preview.domainExpansion')}
                    unCheckedChildren={t('queryExpansionConfig.preview.domainExpansion')}
                  />
                </Space>

                <Button
                  type="primary"
                  icon={<ThunderboltOutlined />}
                  onClick={handlePreview}
                  style={{ marginTop: 16 }}
                  block
                >
                  {t('queryExpansionConfig.preview.previewButton')}
                </Button>
              </div>

              <Divider />

              {/* 预览结果 */}
              <div className="preview-results">
                {renderPreviewResult()}
              </div>
            </Card>

            {/* 领域词典 */}
            <div style={{ marginTop: 24 }}>
              {renderDictionary()}
            </div>
          </Col>
        </Row>
      </Spin>
    </div>
  )
}

export default QueryExpansionConfig

