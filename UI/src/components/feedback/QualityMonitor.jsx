/**
 * 质量监控组件 (Quality Monitor Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect } from 'react'
import { Card, Row, Col, Statistic, Progress, Table, Spin } from 'antd'
import {
  CheckCircleOutlined,
  CloseCircleOutlined,
  SyncOutlined,
  TrophyOutlined,
} from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import feedbackApi from '../../api/modules/feedback'
import '../../assets/css/feedback/quality-monitor.css'

function QualityMonitor() {
  const { t } = useLanguage()
  const [loading, setLoading] = useState(false)
  const [data, setData] = useState(null)

  useEffect(() => {
    loadQualityData()
  }, [])

  const loadQualityData = async () => {
    setLoading(true)
    try {
      const response = await feedbackApi.getQualityMonitor()
      if (response) {
        // axios 拦截器已返回 response.data (Axios interceptor returns response.data)
        setData(response)
      }
    } catch (error) {
      console.error('Failed to load quality data:', error)
    } finally {
      setLoading(false)
    }
  }

  if (loading) {
    return (
      <div className="quality-monitor__loading">
        <Spin tip={t('common.loading')} size="large">
          <div style={{ padding: 50 }} />
        </Spin>
      </div>
    )
  }

  if (!data) {
    return null
  }

  const columns = [
    {
      title: t('feedback.concept'),
      dataIndex: 'concept',
      key: 'concept',
    },
    {
      title: t('feedback.conflicts'),
      dataIndex: 'conflictCount',
      key: 'conflictCount',
      sorter: (a, b) => a.conflictCount - b.conflictCount,
    },
    {
      title: t('feedback.resolved'),
      dataIndex: 'resolvedCount',
      key: 'resolvedCount',
      sorter: (a, b) => a.resolvedCount - b.resolvedCount,
    },
    {
      title: t('feedback.quality'),
      dataIndex: 'qualityScore',
      key: 'qualityScore',
      render: (score) => (
        <Progress
          percent={Math.round(score * 100)}
          size="small"
          status={score > 0.8 ? 'success' : score > 0.6 ? 'normal' : 'exception'}
        />
      ),
      sorter: (a, b) => a.qualityScore - b.qualityScore,
    },
  ]

  return (
    <div className="quality-monitor">
      <Row gutter={[16, 16]} className="quality-monitor__stats">
        <Col xs={24} sm={12} lg={6}>
          <Card>
            <Statistic
              title={t('feedback.totalConflicts')}
              value={data.totalConflicts}
              prefix={<SyncOutlined />}
              styles={{ value: { color: '#667eea' } }}
            />
          </Card>
        </Col>
        <Col xs={24} sm={12} lg={6}>
          <Card>
            <Statistic
              title={t('feedback.resolvedConflicts')}
              value={data.resolvedConflicts}
              prefix={<CheckCircleOutlined />}
              styles={{ value: { color: '#52c41a' } }}
            />
          </Card>
        </Col>
        <Col xs={24} sm={12} lg={6}>
          <Card>
            <Statistic
              title={t('feedback.pendingConflicts')}
              value={data.pendingConflicts}
              prefix={<CloseCircleOutlined />}
              styles={{ value: { color: '#faad14' } }}
            />
          </Card>
        </Col>
        <Col xs={24} sm={12} lg={6}>
          <Card>
            <Statistic
              title={t('feedback.avgQuality')}
              value={Math.round((data.averageQuality || 0) * 100)}
              suffix="%"
              prefix={<TrophyOutlined />}
              styles={{ value: { color: '#667eea' } }}
            />
          </Card>
        </Col>
      </Row>

      <Card
        className="quality-monitor__table"
        title={t('feedback.conceptQuality')}
      >
        <Table
          dataSource={data.concepts || []}
          columns={columns}
          rowKey="concept"
          pagination={{ pageSize: 10 }}
        />
      </Card>
    </div>
  )
}

export default QualityMonitor

