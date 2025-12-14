/**
 * 同步监控组件 (Sync Monitor Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect } from 'react'
import { Card, Row, Col, Statistic, Progress, Space, Tag, Spin, Divider } from 'antd'
import { SyncOutlined, CheckCircleOutlined, CloseCircleOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import collaborationApi from '../../api/modules/collaboration'
import '../../assets/css/collaboration/sync-monitor.css'

function SyncMonitor() {
  const { t } = useLanguage()
  const [loading, setLoading] = useState(false)
  const [data, setData] = useState(null)

  useEffect(() => {
    loadSyncData()
    const interval = setInterval(loadSyncData, 10000)
    return () => clearInterval(interval)
  }, [])

  const loadSyncData = async () => {
    setLoading(true)
    try {
      const response = await collaborationApi.getSyncStatus()
      if (response) {
        // axios 拦截器已返回 response.data (Axios interceptor returns response.data)
        // 确保 recentSyncs 是数组 (Ensure recentSyncs is an array)
        const syncData = {
          ...response,
          recentSyncs: Array.isArray(response.recentSyncs) ? response.recentSyncs : []
        }
        setData(syncData)
      }
    } catch (error) {
      console.error('Failed to load sync data:', error)
      setData(null) // 设置为null防止崩溃 (Set to null to prevent crash)
    } finally {
      setLoading(false)
    }
  }

  if (loading && !data) {
    return (
      <div className="sync-monitor__loading">
        <Spin tip={t('common.loading')} size="large">
          <div style={{ padding: 50 }} />
        </Spin>
      </div>
    )
  }

  if (!data) {
    return null
  }

  return (
    <div className="sync-monitor">
      <Row gutter={[16, 16]} className="sync-monitor__stats">
        <Col xs={24} sm={12} lg={6}>
          <Card>
            <Statistic
              title={t('collaboration.totalSyncs')}
              value={data.totalSyncs}
              prefix={<SyncOutlined />}
              styles={{ content: { color: '#667eea' } }}
            />
          </Card>
        </Col>
        <Col xs={24} sm={12} lg={6}>
          <Card>
            <Statistic
              title={t('collaboration.successSyncs')}
              value={data.successSyncs}
              prefix={<CheckCircleOutlined />}
              styles={{ content: { color: '#52c41a' } }}
            />
          </Card>
        </Col>
        <Col xs={24} sm={12} lg={6}>
          <Card>
            <Statistic
              title={t('collaboration.failedSyncs')}
              value={data.failedSyncs}
              prefix={<CloseCircleOutlined />}
              styles={{ content: { color: '#ff4d4f' } }}
            />
          </Card>
        </Col>
        <Col xs={24} sm={12} lg={6}>
          <Card>
            <Statistic
              title={t('collaboration.syncRate')}
              value={Math.round((data.successSyncs / (data.totalSyncs || 1)) * 100)}
              suffix="%"
              styles={{ content: { color: '#667eea' } }}
            />
          </Card>
        </Col>
      </Row>

      <Card
        className="sync-monitor__activity"
        title={t('collaboration.recentActivity')}
      >
        <Space direction="vertical" size="middle" style={{ width: '100%' }}>
          {(data.recentSyncs || []).map((item, index) => (
            <div key={item.id || index}>
              <div className="sync-monitor__activity-item">
                <div className="sync-monitor__activity-header">
                  <div className="sync-monitor__activity-title">
                    <span>{item.peerName}</span>
                    <Tag color={(item.status || 'success') === 'success' ? 'green' : 'red'}>
                      {t(`collaboration.syncStatus.${item.status || 'success'}`)}
                    </Tag>
                  </div>
                </div>
                <div className="sync-monitor__activity-desc">
                  <span>{item.description}</span>
                  <span className="sync-monitor__activity-time">
                    {new Date(item.timestamp).toLocaleString()}
                  </span>
                </div>
                {item.progress !== undefined && (
                  <div className="sync-monitor__progress">
                    <Progress
                      percent={item.progress}
                      size="small"
                      status={(item.status || 'success') === 'success' ? 'success' : 'active'}
                    />
                  </div>
                )}
              </div>
              {index < (data.recentSyncs || []).length - 1 && <Divider style={{ margin: '12px 0' }} />}
            </div>
          ))}
        </Space>
      </Card>
    </div>
  )
}

export default SyncMonitor

