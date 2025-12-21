/**
 * 查询过程可视化组件 (Query Process Visualization Component)
 *
 * 可视化展示查询处理的完整流程
 * (Visualizes the complete query processing flow)
 *
 * Phase 4.1 - 查询过程可视化
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */

import React, { useState, useEffect } from 'react'
import {
  Card,
  Steps,
  Tag,
  Space,
  Statistic,
  Progress,
  Timeline,
  Row,
  Col,
  Spin,
  Alert,
  Button,
  Input,
} from 'antd'
import {
  SearchOutlined,
  ThunderboltOutlined,
  DatabaseOutlined,
  SortAscendingOutlined,
  CheckCircleOutlined,
  ClockCircleOutlined,
  FireOutlined,
} from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import './QueryProcessVisualization.css'

const { Step } = Steps
const { TextArea } = Input

function QueryProcessVisualization({ autoStart = false }) {
  const { t } = useLanguage()

  // 状态管理
  const [loading, setLoading] = useState(false)
  const [currentStep, setCurrentStep] = useState(0)
  const [queryText, setQueryText] = useState('')
  const [processData, setProcessData] = useState(null)

  // 模拟查询处理流程
  const startQueryProcess = async () => {
    if (!queryText.trim()) {
      return
    }

    setLoading(true)
    setCurrentStep(0)

    const steps = [
      { name: 'query_received', duration: 50 },
      { name: 'query_expansion', duration: 200 },
      { name: 'embedding', duration: 300 },
      { name: 'retrieval', duration: 400 },
      { name: 'reranking', duration: 250 },
      { name: 'completed', duration: 50 },
    ]

    for (let i = 0; i < steps.length; i++) {
      setCurrentStep(i)
      await new Promise(resolve => setTimeout(resolve, steps[i].duration))
    }

    // 设置处理结果
    const result = {
      originalQuery: queryText,
      expandedQueries: [
        queryText,
        queryText + ' 相关概念',
        queryText + ' 实现方法',
      ],
      embeddingTime: 120,
      retrievalResults: 15,
      retrievalTime: 385,
      rerankedResults: 5,
      rerankingTime: 230,
      totalTime: 1235,
      cacheHit: Math.random() > 0.5,
    }

    setProcessData(result)
    setCurrentStep(steps.length - 1)
    setLoading(false)
  }

  // 流程步骤定义
  const steps = [
    {
      title: t('queryProcess.steps.received.title'),
      icon: <SearchOutlined />,
      description: t('queryProcess.steps.received.description'),
    },
    {
      title: t('queryProcess.steps.expansion.title'),
      icon: <ThunderboltOutlined />,
      description: t('queryProcess.steps.expansion.description'),
    },
    {
      title: t('queryProcess.steps.embedding.title'),
      icon: <FireOutlined />,
      description: t('queryProcess.steps.embedding.description'),
    },
    {
      title: t('queryProcess.steps.retrieval.title'),
      icon: <DatabaseOutlined />,
      description: t('queryProcess.steps.retrieval.description'),
    },
    {
      title: t('queryProcess.steps.reranking.title'),
      icon: <SortAscendingOutlined />,
      description: t('queryProcess.steps.reranking.description'),
    },
    {
      title: t('queryProcess.steps.completed.title'),
      icon: <CheckCircleOutlined />,
      description: t('queryProcess.steps.completed.description'),
    },
  ]

  // 渲染统计信息
  const renderStatistics = () => {
    if (!processData) return null

    return (
      <Card title={t('queryProcess.statistics.title')} className="statistics-card">
        <Row gutter={16}>
          <Col span={6}>
            <Statistic
              title={t('queryProcess.statistics.totalTime')}
              value={processData.totalTime}
              suffix="ms"
              prefix={<ClockCircleOutlined />}
            />
          </Col>
          <Col span={6}>
            <Statistic
              title={t('queryProcess.statistics.expandedQueries')}
              value={processData.expandedQueries.length}
              prefix={<ThunderboltOutlined />}
            />
          </Col>
          <Col span={6}>
            <Statistic
              title={t('queryProcess.statistics.retrievalResults')}
              value={processData.retrievalResults}
              prefix={<DatabaseOutlined />}
            />
          </Col>
          <Col span={6}>
            <Statistic
              title={t('queryProcess.statistics.finalResults')}
              value={processData.rerankedResults}
              prefix={<CheckCircleOutlined />}
            />
          </Col>
        </Row>
        <div style={{ marginTop: 16 }}>
          {processData.cacheHit && (
            <Tag color="green" icon={<CheckCircleOutlined />}>
              {t('queryProcess.statistics.cacheHit')}
            </Tag>
          )}
          <Tag color="blue">
            {t('queryProcess.statistics.embeddingTime')}: {processData.embeddingTime}ms
          </Tag>
          <Tag color="purple">
            {t('queryProcess.statistics.retrievalTime')}: {processData.retrievalTime}ms
          </Tag>
          <Tag color="orange">
            {t('queryProcess.statistics.rerankingTime')}: {processData.rerankingTime}ms
          </Tag>
        </div>
      </Card>
    )
  }

  // 渲染时间线
  const renderTimeline = () => {
    if (!processData) return null

    const timelineItems = [
      {
        color: 'green',
        label: t('queryProcess.timeline.received'),
        time: '0ms',
        content: processData.originalQuery,
      },
      {
        color: 'blue',
        label: t('queryProcess.timeline.expanded'),
        time: '50ms',
        content: processData.expandedQueries.join(', '),
      },
      {
        color: 'purple',
        label: t('queryProcess.timeline.embedded'),
        time: '250ms',
        content: `${t('queryProcess.timeline.vectorGenerated')}`,
      },
      {
        color: 'orange',
        label: t('queryProcess.timeline.retrieved'),
        time: '550ms',
        content: `${processData.retrievalResults} ${t('queryProcess.timeline.results')}`,
      },
      {
        color: 'red',
        label: t('queryProcess.timeline.reranked'),
        time: '985ms',
        content: `${t('queryProcess.timeline.topResults')}: ${processData.rerankedResults}`,
      },
      {
        color: 'green',
        label: t('queryProcess.timeline.completed'),
        time: `${processData.totalTime}ms`,
        content: t('queryProcess.timeline.ready'),
      },
    ]

    return (
      <Card title={t('queryProcess.timeline.title')} className="timeline-card">
        <Timeline>
          {timelineItems.map((item, index) => (
            <Timeline.Item key={index} color={item.color}>
              <Space direction="vertical" size="small">
                <Space>
                  <strong>{item.label}</strong>
                  <Tag>{item.time}</Tag>
                </Space>
                <span style={{ color: '#666' }}>{item.content}</span>
              </Space>
            </Timeline.Item>
          ))}
        </Timeline>
      </Card>
    )
  }

  return (
    <div className="query-process-container">
      <Spin spinning={loading} tip={t('queryProcess.processing')}>
        {/* 输入区域 */}
        <Card className="input-card">
          <Space direction="vertical" style={{ width: '100%' }}>
            <h3>{t('queryProcess.input.title')}</h3>
            <TextArea
              rows={3}
              placeholder={t('queryProcess.input.placeholder')}
              value={queryText}
              onChange={(e) => setQueryText(e.target.value)}
              disabled={loading}
            />
            <Button
              type="primary"
              icon={<SearchOutlined />}
              onClick={startQueryProcess}
              disabled={!queryText.trim() || loading}
              size="large"
            >
              {t('queryProcess.input.startButton')}
            </Button>
          </Space>
        </Card>

        {/* 流程步骤 */}
        <Card className="steps-card" style={{ marginTop: 24 }}>
          <Steps current={currentStep} status={loading ? 'process' : 'finish'}>
            {steps.map((step, index) => (
              <Step
                key={index}
                title={step.title}
                description={step.description}
                icon={step.icon}
              />
            ))}
          </Steps>
        </Card>

        {/* 统计信息 */}
        {processData && (
          <div style={{ marginTop: 24 }}>
            {renderStatistics()}
          </div>
        )}

        {/* 时间线 */}
        {processData && (
          <div style={{ marginTop: 24 }}>
            {renderTimeline()}
          </div>
        )}

        {/* 进度提示 */}
        {loading && (
          <Alert
            message={t('queryProcess.progress.title')}
            description={
              <Space direction="vertical" style={{ width: '100%' }}>
                <span>{steps[currentStep]?.description}</span>
                <Progress
                  percent={((currentStep + 1) / steps.length) * 100}
                  status="active"
                />
              </Space>
            }
            type="info"
            showIcon
            style={{ marginTop: 24 }}
          />
        )}
      </Spin>
    </div>
  )
}

export default QueryProcessVisualization

