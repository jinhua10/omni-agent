/**
 * 演化时间线组件 (Evolution Timeline Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect } from 'react'
import { Timeline, Card, Tag, Empty, Spin } from 'antd'
import { HistoryOutlined, CheckCircleOutlined, ClockCircleOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import feedbackApi from '../../api/modules/feedback'
import '../../assets/css/feedback/evolution-timeline.css'

function EvolutionTimeline() {
  const { t } = useLanguage()
  const [loading, setLoading] = useState(false)
  const [timeline, setTimeline] = useState([])

  useEffect(() => {
    loadTimeline()
  }, [])

  const loadTimeline = async () => {
    setLoading(true)
    try {
      const response = await feedbackApi.getEvolutionHistory()
      if (response) {
        // Backend returns {success, conceptId, history: [...]} structure
        const historyData = response.history || response || []
        setTimeline(Array.isArray(historyData) ? historyData : [])
      }
    } catch (error) {
      console.error('Failed to load timeline:', error)
    } finally {
      setLoading(false)
    }
  }

  const getTimelineIcon = (type) => {
    const icons = {
      created: <ClockCircleOutlined />,
      updated: <HistoryOutlined />,
      resolved: <CheckCircleOutlined />,
    }
    return icons[type] || <ClockCircleOutlined />
  }

  const getTimelineColor = (type) => {
    const colors = {
      created: 'blue',
      updated: 'orange',
      resolved: 'green',
    }
    return colors[type] || 'gray'
  }

  if (loading) {
    return (
      <div className="evolution-timeline__loading">
        <Spin tip={t('common.loading')}>
          <div style={{ padding: 50 }} />
        </Spin>
      </div>
    )
  }

  if (timeline.length === 0) {
    return (
      <Empty
        image={Empty.PRESENTED_IMAGE_SIMPLE}
        description={t('feedback.noEvolution')}
      />
    )
  }

  const timelineItems = timeline.map((item) => {
    const itemType = item.type || 'created'
    return {
      key: item.id,
      dot: getTimelineIcon(itemType),
      color: getTimelineColor(itemType),
      children: (
        <Card className="evolution-timeline__card">
          <div className="evolution-timeline__header">
            <Tag color={getTimelineColor(itemType)}>
              {t(`feedback.timeline.${itemType}`)}
            </Tag>
            <span className="evolution-timeline__time">
              {new Date(item.timestamp).toLocaleString()}
            </span>
          </div>

          <div className="evolution-timeline__content">
            <h4 className="evolution-timeline__title">{item.title}</h4>
            <p className="evolution-timeline__description">{item.description}</p>

            {item.changes && (
              <div className="evolution-timeline__changes">
                <div className="evolution-timeline__change evolution-timeline__change--old">
                  <div className="evolution-timeline__change-label">
                    {t('feedback.before')}
                  </div>
                  <div className="evolution-timeline__change-text">
                    {item.changes.before}
                  </div>
                </div>
                <div className="evolution-timeline__change evolution-timeline__change--new">
                  <div className="evolution-timeline__change-label">
                    {t('feedback.after')}
                  </div>
                  <div className="evolution-timeline__change-text">
                    {item.changes.after}
                  </div>
                </div>
              </div>
            )}
          </div>
        </Card>
      )
    }
  })

  return (
    <div className="evolution-timeline">
      <Timeline items={timelineItems} />
    </div>
  )
}

export default EvolutionTimeline

