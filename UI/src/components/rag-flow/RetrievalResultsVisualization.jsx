/**
 * 检索结果可视化组件 (Retrieval Results Visualization Component)
 *
 * 可视化展示检索结果的分布和质量
 * (Visualizes the distribution and quality of retrieval results)
 *
 * Phase 4.1 - 检索结果可视化
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */

import React, { useState, useEffect } from 'react'
import {
  Card,
  List,
  Tag,
  Space,
  Progress,
  Row,
  Col,
  Statistic,
  Empty,
  Divider,
  Badge,
  Tooltip,
} from 'antd'
import {
  FileTextOutlined,
  ThunderboltOutlined,
  CheckCircleOutlined,
  ClockCircleOutlined,
  StarOutlined,
} from '@ant-design/icons'
import { Column, Pie } from '@ant-design/plots'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/rag-flow/RetrievalResultsVisualization.css'

function RetrievalResultsVisualization({ results, query }) {
  const { t } = useLanguage()

  // 模拟数据（如果没有传入）
  const mockResults = results || [
    {
      id: 1,
      documentName: t('retrievalResults.mockData.doc1'),
      score: 0.95,
      source: 'vector',
      content: t('retrievalResults.mockData.content1'),
      timestamp: Date.now() - 3600000,
    },
    {
      id: 2,
      documentName: t('retrievalResults.mockData.doc2'),
      score: 0.88,
      source: 'fulltext',
      content: t('retrievalResults.mockData.content2'),
      timestamp: Date.now() - 7200000,
    },
    {
      id: 3,
      documentName: t('retrievalResults.mockData.doc3'),
      score: 0.82,
      source: 'vector',
      content: t('retrievalResults.mockData.content3'),
      timestamp: Date.now() - 86400000,
    },
    {
      id: 4,
      documentName: t('retrievalResults.mockData.doc4'),
      score: 0.76,
      source: 'hybrid',
      content: t('retrievalResults.mockData.content4'),
      timestamp: Date.now() - 172800000,
    },
    {
      id: 5,
      documentName: t('retrievalResults.mockData.doc5'),
      score: 0.71,
      source: 'vector',
      content: t('retrievalResults.mockData.content5'),
      timestamp: Date.now() - 259200000,
    },
  ]

  // 统计数据
  const statistics = {
    totalResults: mockResults.length,
    avgScore: mockResults.reduce((sum, r) => sum + r.score, 0) / mockResults.length,
    maxScore: Math.max(...mockResults.map(r => r.score)),
    minScore: Math.min(...mockResults.map(r => r.score)),
    vectorCount: mockResults.filter(r => r.source === 'vector').length,
    fulltextCount: mockResults.filter(r => r.source === 'fulltext').length,
    hybridCount: mockResults.filter(r => r.source === 'hybrid').length,
  }

  // 渲染统计卡片
  const renderStatistics = () => {
    return (
      <Row gutter={16}>
        <Col span={6}>
          <Card className="stat-card">
            <Statistic
              title={t('retrievalResults.statistics.totalResults')}
              value={statistics.totalResults}
              prefix={<FileTextOutlined />}
            />
          </Card>
        </Col>
        <Col span={6}>
          <Card className="stat-card">
            <Statistic
              title={t('retrievalResults.statistics.avgScore')}
              value={statistics.avgScore}
              precision={3}
              prefix={<StarOutlined />}
              valueStyle={{ color: '#3f8600' }}
            />
          </Card>
        </Col>
        <Col span={6}>
          <Card className="stat-card">
            <Statistic
              title={t('retrievalResults.statistics.maxScore')}
              value={statistics.maxScore}
              precision={3}
              valueStyle={{ color: '#cf1322' }}
            />
          </Card>
        </Col>
        <Col span={6}>
          <Card className="stat-card">
            <Statistic
              title={t('retrievalResults.statistics.minScore')}
              value={statistics.minScore}
              precision={3}
              valueStyle={{ color: '#666' }}
            />
          </Card>
        </Col>
      </Row>
    )
  }

  // 来源分布饼图
  const renderSourceDistribution = () => {
    const data = [
      { type: t('retrievalResults.source.vector'), value: statistics.vectorCount },
      { type: t('retrievalResults.source.fulltext'), value: statistics.fulltextCount },
      { type: t('retrievalResults.source.hybrid'), value: statistics.hybridCount },
    ].filter(d => d.value > 0)

    const config = {
      data,
      angleField: 'value',
      colorField: 'type',
      radius: 0.8,
      label: {
        type: 'outer',
        content: '{name} {percentage}',
      },
      interactions: [{ type: 'element-active' }],
    }

    return (
      <Card title={t('retrievalResults.sourceDistribution.title')}>
        <Pie {...config} />
      </Card>
    )
  }

  // 相似度分布柱状图
  const renderScoreDistribution = () => {
    const data = mockResults.map((r, index) => ({
      name: `${t('retrievalResults.result')} ${index + 1}`,
      score: r.score,
    }))

    const config = {
      data,
      xField: 'name',
      yField: 'score',
      label: {
        position: 'top',
        style: {
          fill: '#000',
          opacity: 0.6,
        },
      },
      xAxis: {
        label: {
          autoHide: true,
          autoRotate: false,
        },
      },
      meta: {
        score: {
          alias: t('retrievalResults.score'),
        },
      },
    }

    return (
      <Card title={t('retrievalResults.scoreDistribution.title')}>
        <Column {...config} />
      </Card>
    )
  }

  // 渲染结果列表
  const renderResultsList = () => {
    return (
      <List
        header={
          <Space>
            <strong>{t('retrievalResults.resultsList.title')}</strong>
            <Badge count={mockResults.length} showZero />
          </Space>
        }
        bordered
        dataSource={mockResults}
        renderItem={(item, index) => (
          <List.Item>
            <List.Item.Meta
              avatar={
                <div className="rank-badge">
                  <span className="rank-number">#{index + 1}</span>
                </div>
              }
              title={
                <Space>
                  <FileTextOutlined />
                  <span>{item.documentName}</span>
                  <Tag color={getSourceColor(item.source)}>
                    {getSourceLabel(item.source)}
                  </Tag>
                </Space>
              }
              description={
                <Space direction="vertical" className="retrieval-results-container__item-description">
                  <div>
                    <Tooltip title={t('retrievalResults.similarity')}>
                      <Progress
                        percent={item.score * 100}
                        size="small"
                        format={(percent) => `${(percent / 100).toFixed(3)}`}
                      />
                    </Tooltip>
                  </div>
                  <div className="retrieval-results-container__item-content">{item.content}</div>
                  <div className="retrieval-results-container__item-timestamp">
                    <ClockCircleOutlined /> {new Date(item.timestamp).toLocaleString()}
                  </div>
                </Space>
              }
            />
          </List.Item>
        )}
      />
    )
  }

  // 获取来源颜色
  const getSourceColor = (source) => {
    const colors = {
      vector: 'blue',
      fulltext: 'orange',
      hybrid: 'purple',
    }
    return colors[source] || 'default'
  }

  // 获取来源标签
  const getSourceLabel = (source) => {
    const labels = {
      vector: t('retrievalResults.source.vector'),
      fulltext: t('retrievalResults.source.fulltext'),
      hybrid: t('retrievalResults.source.hybrid'),
    }
    return labels[source] || source
  }

  if (!mockResults || mockResults.length === 0) {
    return (
      <div className="retrieval-results-container">
        <Empty description={t('retrievalResults.noResults')} />
      </div>
    )
  }

  return (
    <div className="retrieval-results-container">
      {/* 查询信息 */}
      {query && (
        <Card className="query-card">
          <Space>
            <ThunderboltOutlined />
            <strong>{t('retrievalResults.query')}:</strong>
            <span>{query}</span>
          </Space>
        </Card>
      )}

      {/* 统计信息 */}
      <div className="retrieval-results-container__statistics">
        {renderStatistics()}
      </div>

      {/* 图表区域 */}
      <Row gutter={16} className="retrieval-results-container__charts">
        <Col span={12}>
          {renderSourceDistribution()}
        </Col>
        <Col span={12}>
          {renderScoreDistribution()}
        </Col>
      </Row>

      <Divider />

      {/* 结果列表 */}
      <div className="retrieval-results-container__results-list">
        {renderResultsList()}
      </div>
    </div>
  )
}

export default RetrievalResultsVisualization

