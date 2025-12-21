/**
 * 缓存管理组件 (Cache Management Component)
 *
 * 独立的缓存管理页面，提供多级缓存统计、热点分析和管理功能
 * (Standalone cache management page with multi-level statistics, hotkey analysis and management)
 *
 * Phase 4.2.4 - 缓存管理界面
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */

import React, { useState, useEffect } from 'react'
import {
  Card,
  Row,
  Col,
  Statistic,
  Progress,
  Table,
  Button,
  Space,
  Modal,
  Input,
  message,
  Spin,
  Tag,
  Tabs,
  Select,
  Divider,
  Alert,
} from 'antd'
import {
  DatabaseOutlined,
  FireOutlined,
  LineChartOutlined,
  DeleteOutlined,
  ReloadOutlined,
  ThunderboltOutlined,
  ExportOutlined,
  CheckCircleOutlined,
  CloseCircleOutlined,
} from '@ant-design/icons'
import { Line } from '@ant-design/plots'
import { useLanguage } from '../../contexts/LanguageContext'
import './CacheManagement.css'

const { TextArea } = Input
const { Option } = Select
const { TabPane } = Tabs

function CacheManagement() {
  const { t } = useLanguage()

  // 状态管理
  const [loading, setLoading] = useState(false)
  const [overview, setOverview] = useState(null)
  const [selectedCache, setSelectedCache] = useState('query')
  const [hotKeys, setHotKeys] = useState([])
  const [trends, setTrends] = useState(null)
  const [clearModalVisible, setClearModalVisible] = useState(false)
  const [warmupModalVisible, setWarmupModalVisible] = useState(false)
  const [warmupKeys, setWarmupKeys] = useState('')
  const [warmupResult, setWarmupResult] = useState(null)

  // 加载数据
  useEffect(() => {
    loadCacheStats()
  }, [])

  useEffect(() => {
    if (selectedCache) {
      loadHotKeys(selectedCache)
      loadTrends(selectedCache)
    }
  }, [selectedCache])

  // 加载缓存统计
  const loadCacheStats = async () => {
    setLoading(true)
    try {
      const response = await fetch('/api/cache/stats')
      const result = await response.json()

      if (result.success && result.data) {
        setOverview(result.data)
        message.success(t('cacheManagement.message.loadSuccess'))
      } else {
        message.error(t('cacheManagement.message.loadFailed'))
      }
    } catch (error) {
      console.error('Failed to load cache stats:', error)
      message.error(t('cacheManagement.message.loadFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 加载热点数据
  const loadHotKeys = async (cacheName) => {
    try {
      const response = await fetch(`/api/cache/hotkeys?cacheName=${cacheName}`)
      const result = await response.json()

      if (result.success && result.data) {
        setHotKeys(result.data)
      }
    } catch (error) {
      console.error('Failed to load hot keys:', error)
    }
  }

  // 加载趋势数据
  const loadTrends = async (cacheName) => {
    try {
      const response = await fetch(`/api/cache/trends?cacheName=${cacheName}`)
      const result = await response.json()

      if (result.success && result.data) {
        setTrends(result.data)
      }
    } catch (error) {
      console.error('Failed to load trends:', error)
    }
  }

  // 清除缓存
  const handleClearCache = async (clearType) => {
    try {
      setLoading(true)
      const response = await fetch('/api/cache/clear', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          cacheName: selectedCache,
          clearType: clearType,
        }),
      })

      const result = await response.json()

      if (result.success) {
        message.success(t('cacheManagement.message.clearSuccess'))
        setClearModalVisible(false)
        loadCacheStats()
      } else {
        message.error(t('cacheManagement.message.clearFailed'))
      }
    } catch (error) {
      console.error('Failed to clear cache:', error)
      message.error(t('cacheManagement.message.clearFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 预热缓存
  const handleWarmup = async () => {
    if (!warmupKeys.trim()) {
      message.warning(t('cacheManagement.warmup.inputKeys'))
      return
    }

    try {
      setLoading(true)
      const keys = warmupKeys.split(/[,\n]/).map(k => k.trim()).filter(k => k)

      const response = await fetch('/api/cache/warmup', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({
          cacheName: selectedCache,
          keys: keys,
        }),
      })

      const result = await response.json()

      if (result.success && result.data) {
        setWarmupResult(result.data)
        message.success(t('cacheManagement.message.warmupSuccess'))
      } else {
        message.error(t('cacheManagement.message.warmupFailed'))
      }
    } catch (error) {
      console.error('Failed to warmup cache:', error)
      message.error(t('cacheManagement.message.warmupFailed'))
    } finally {
      setLoading(false)
    }
  }

  // 渲染概览卡片
  const renderOverview = () => {
    if (!overview) return null

    return (
      <Row gutter={16}>
        <Col span={8}>
          <Card className="overview-card">
            <Statistic
              title={t('cacheManagement.overview.totalHitRate')}
              value={overview.totalHitRate * 100}
              precision={2}
              suffix="%"
              valueStyle={{ color: overview.totalHitRate > 0.8 ? '#3f8600' : '#cf1322' }}
            />
            <Progress
              percent={overview.totalHitRate * 100}
              status={overview.totalHitRate > 0.8 ? 'success' : 'exception'}
              showInfo={false}
            />
          </Card>
        </Col>
        <Col span={8}>
          <Card className="overview-card">
            <Statistic
              title={t('cacheManagement.overview.totalSize')}
              value={overview.totalSize}
              suffix={`/ ${overview.totalMaxSize}`}
            />
            <Progress
              percent={(overview.totalSize / overview.totalMaxSize) * 100}
              showInfo={false}
            />
          </Card>
        </Col>
        <Col span={8}>
          <Card className="overview-card">
            <Statistic
              title={t('cacheManagement.overview.totalRequests')}
              value={overview.totalHitCount + overview.totalMissCount}
              suffix={
                <Space>
                  <Tag color="green">{t('cacheManagement.stats.hitCount')}: {overview.totalHitCount}</Tag>
                  <Tag color="red">{t('cacheManagement.stats.missCount')}: {overview.totalMissCount}</Tag>
                </Space>
              }
            />
          </Card>
        </Col>
      </Row>
    )
  }

  // 渲染缓存列表
  const renderCacheList = () => {
    if (!overview || !overview.caches) return null

    const columns = [
      {
        title: t('cacheManagement.cacheTypes.query'),
        dataIndex: 'displayName',
        key: 'displayName',
      },
      {
        title: t('cacheManagement.stats.hitRate'),
        dataIndex: 'hitRate',
        key: 'hitRate',
        render: (rate) => (
          <Progress
            percent={rate * 100}
            status={rate > 0.8 ? 'success' : 'exception'}
            format={percent => `${percent.toFixed(2)}%`}
          />
        ),
      },
      {
        title: t('cacheManagement.stats.size'),
        key: 'size',
        render: (_, record) => (
          <span>{record.size} / {record.maxSize}</span>
        ),
      },
      {
        title: t('cacheManagement.stats.usagePercent'),
        key: 'usage',
        render: (_, record) => (
          <Progress
            percent={(record.size / record.maxSize) * 100}
            size="small"
          />
        ),
      },
      {
        title: t('cacheManagement.stats.hitCount'),
        dataIndex: 'hitCount',
        key: 'hitCount',
      },
      {
        title: t('cacheManagement.stats.missCount'),
        dataIndex: 'missCount',
        key: 'missCount',
      },
      {
        title: t('cacheManagement.stats.evictionCount'),
        dataIndex: 'evictionCount',
        key: 'evictionCount',
      },
      {
        title: t('cacheManagement.stats.avgLoadTime'),
        dataIndex: 'avgLoadTime',
        key: 'avgLoadTime',
        render: (time) => `${time}ms`,
      },
    ]

    return (
      <Table
        dataSource={overview.caches}
        columns={columns}
        rowKey="name"
        pagination={false}
        onRow={(record) => ({
          onClick: () => setSelectedCache(record.name),
          style: { cursor: 'pointer', background: selectedCache === record.name ? '#e6f7ff' : 'transparent' }
        })}
      />
    )
  }

  // 渲染热点表格
  const renderHotKeys = () => {
    const columns = [
      {
        title: t('cacheManagement.hotkeys.key'),
        dataIndex: 'key',
        key: 'key',
      },
      {
        title: t('cacheManagement.hotkeys.hitCount'),
        dataIndex: 'hitCount',
        key: 'hitCount',
        sorter: (a, b) => a.hitCount - b.hitCount,
      },
      {
        title: t('cacheManagement.hotkeys.lastAccess'),
        dataIndex: 'lastAccessTime',
        key: 'lastAccessTime',
        render: (time) => new Date(time).toLocaleString(),
      },
      {
        title: t('cacheManagement.hotkeys.size'),
        dataIndex: 'size',
        key: 'size',
        render: (size) => `${(size / 1024).toFixed(2)} KB`,
      },
    ]

    return (
      <Table
        dataSource={hotKeys}
        columns={columns}
        rowKey="key"
        pagination={{ pageSize: 10 }}
      />
    )
  }

  // 渲染趋势图
  const renderTrends = () => {
    if (!trends) return null

    const hitRateConfig = {
      data: trends.hitRateTrend.map(p => ({
        time: new Date(p.timestamp).toLocaleTimeString(),
        value: p.value,
      })),
      xField: 'time',
      yField: 'value',
      smooth: true,
      color: '#1890ff',
    }

    const sizeConfig = {
      data: trends.sizeTrend.map(p => ({
        time: new Date(p.timestamp).toLocaleTimeString(),
        value: p.value,
      })),
      xField: 'time',
      yField: 'value',
      smooth: true,
      color: '#52c41a',
    }

    return (
      <Row gutter={16}>
        <Col span={12}>
          <Card title={t('cacheManagement.trends.hitRateTrend')}>
            <Line {...hitRateConfig} />
          </Card>
        </Col>
        <Col span={12}>
          <Card title={t('cacheManagement.trends.sizeTrend')}>
            <Line {...sizeConfig} />
          </Card>
        </Col>
      </Row>
    )
  }

  return (
    <div className="cache-management-container">
      <Spin spinning={loading}>
        {/* 页面标题 */}
        <div className="page-header">
          <h1>{t('cacheManagement.title')}</h1>
          <p className="subtitle">{t('cacheManagement.subtitle')}</p>
          <Space>
            <Button icon={<ReloadOutlined />} onClick={loadCacheStats}>
              {t('cacheManagement.actions.refresh')}
            </Button>
            <Button icon={<ExportOutlined />}>
              {t('cacheManagement.actions.export')}
            </Button>
          </Space>
        </div>

        {/* 概览 */}
        <div style={{ marginBottom: 24 }}>
          <h2>{t('cacheManagement.overview.title')}</h2>
          {renderOverview()}
        </div>

        <Divider />

        {/* 缓存列表 */}
        <div style={{ marginBottom: 24 }}>
          {renderCacheList()}
        </div>

        {/* 详细信息标签页 */}
        {selectedCache && (
          <Card
            title={
              <Space>
                <DatabaseOutlined />
                <span>{overview?.caches.find(c => c.name === selectedCache)?.displayName}</span>
              </Space>
            }
            extra={
              <Space>
                <Button
                  icon={<ThunderboltOutlined />}
                  onClick={() => setWarmupModalVisible(true)}
                >
                  {t('cacheManagement.actions.warmup')}
                </Button>
                <Button
                  danger
                  icon={<DeleteOutlined />}
                  onClick={() => setClearModalVisible(true)}
                >
                  {t('cacheManagement.actions.clearAll')}
                </Button>
              </Space>
            }
          >
            <Tabs defaultActiveKey="hotkeys">
              <TabPane
                tab={
                  <span>
                    <FireOutlined />
                    {t('cacheManagement.hotkeys.title')}
                  </span>
                }
                key="hotkeys"
              >
                {renderHotKeys()}
              </TabPane>
              <TabPane
                tab={
                  <span>
                    <LineChartOutlined />
                    {t('cacheManagement.trends.title')}
                  </span>
                }
                key="trends"
              >
                {renderTrends()}
              </TabPane>
            </Tabs>
          </Card>
        )}

        {/* 清除确认对话框 */}
        <Modal
          title={t('cacheManagement.clearConfirm.title')}
          open={clearModalVisible}
          onCancel={() => setClearModalVisible(false)}
          footer={[
            <Button key="cancel" onClick={() => setClearModalVisible(false)}>
              {t('common.cancel')}
            </Button>,
            <Button key="expired" onClick={() => handleClearCache('expired')}>
              {t('cacheManagement.actions.clearExpired')}
            </Button>,
            <Button key="all" type="primary" danger onClick={() => handleClearCache('all')}>
              {t('cacheManagement.actions.clearAll')}
            </Button>,
          ]}
        >
          <Alert
            message={t('cacheManagement.clearConfirm.allMessage')}
            type="warning"
            showIcon
          />
        </Modal>

        {/* 预热对话框 */}
        <Modal
          title={t('cacheManagement.warmup.title')}
          open={warmupModalVisible}
          onOk={handleWarmup}
          onCancel={() => {
            setWarmupModalVisible(false)
            setWarmupResult(null)
          }}
          width={600}
        >
          <Space direction="vertical" style={{ width: '100%' }}>
            <h4>{t('cacheManagement.warmup.inputKeys')}</h4>
            <TextArea
              rows={6}
              placeholder={t('cacheManagement.warmup.inputPlaceholder')}
              value={warmupKeys}
              onChange={(e) => setWarmupKeys(e.target.value)}
            />

            {warmupResult && (
              <Alert
                message={t('cacheManagement.warmup.result.title')}
                description={
                  <Space direction="vertical">
                    <div>
                      {t('cacheManagement.warmup.result.total')}: {warmupResult.totalKeys}
                    </div>
                    <div>
                      <CheckCircleOutlined style={{ color: '#52c41a' }} />
                      {' '}{t('cacheManagement.warmup.result.success')}: {warmupResult.successCount}
                    </div>
                    <div>
                      <CloseCircleOutlined style={{ color: '#ff4d4f' }} />
                      {' '}{t('cacheManagement.warmup.result.failure')}: {warmupResult.failureCount}
                    </div>
                    <div>
                      {t('cacheManagement.warmup.result.duration')}: {warmupResult.duration}ms
                    </div>
                  </Space>
                }
                type="info"
                showIcon
              />
            )}
          </Space>
        </Modal>
      </Spin>
    </div>
  )
}

export default CacheManagement

