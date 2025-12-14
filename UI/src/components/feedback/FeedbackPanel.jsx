/**
 * 反馈管理主面板 (Feedback Panel Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { Tabs } from 'antd'
import { SwapOutlined, HistoryOutlined, DashboardOutlined } from '@ant-design/icons'
import ConflictList from './ConflictList'
import EvolutionTimeline from './EvolutionTimeline'
import QualityMonitor from './QualityMonitor'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/feedback/feedback-panel.css'

function FeedbackPanel() {
  const { t } = useLanguage()
  const [activeTab, setActiveTab] = useState('conflicts')

  const items = [
    {
      key: 'conflicts',
      label: (
        <span>
          <SwapOutlined />
          {t('feedback.conflictList')}
        </span>
      ),
      children: <ConflictList />,
    },
    {
      key: 'evolution',
      label: (
        <span>
          <HistoryOutlined />
          {t('feedback.evolution')}
        </span>
      ),
      children: <EvolutionTimeline />,
    },
    {
      key: 'quality',
      label: (
        <span>
          <DashboardOutlined />
          {t('feedback.quality')}
        </span>
      ),
      children: <QualityMonitor />,
    },
  ]

  return (
    <div className="feedback-panel">
      <div className="feedback-panel__header">
        <h2>{t('feedback.title')}</h2>
      </div>

      <div className="feedback-panel__content">
        <Tabs
          activeKey={activeTab}
          onChange={setActiveTab}
          items={items}
          size="large"
          className="feedback-panel__tabs"
        />
      </div>
    </div>
  )
}

export default FeedbackPanel

