/**
 * 协作网络主面板 (Collaboration Panel Component)
 *
 * 使用主题引擎实现真正的UI与数据分离
 * Uses theme engine to achieve true separation of UI and data
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { Tabs } from 'antd'
import { TeamOutlined, SwapOutlined, ClusterOutlined, SyncOutlined } from '@ant-design/icons'
import PeerList from './PeerList'
import ExchangeHistory from './ExchangeHistory'
import NetworkTopology from './NetworkTopology'
import SyncMonitor from './SyncMonitor'
import { useLanguage } from '../../contexts/LanguageContext'
import { EnginePageRouter } from '../../engine/PageRouter'
import '../../assets/css/collaboration/collaboration-panel.css'

/**
 * 回退组件 - 当主题不支持时使用传统UI / Fallback Component - Traditional UI when theme doesn't support
 */
function TraditionalCollaborationPanel() {
  const { t } = useLanguage()
  const [activeTab, setActiveTab] = useState('peers')

  const items = [
    {
      key: 'peers',
      label: (
        <span>
          <TeamOutlined />
          {t('collaboration.peers')}
        </span>
      ),
      children: <PeerList />,
    },
    {
      key: 'exchange',
      label: (
        <span>
          <SwapOutlined />
          {t('collaboration.exchange')}
        </span>
      ),
      children: <ExchangeHistory />,
    },
    {
      key: 'topology',
      label: (
        <span>
          <ClusterOutlined />
          {t('collaboration.topology')}
        </span>
      ),
      children: <NetworkTopology />,
    },
    {
      key: 'sync',
      label: (
        <span>
          <SyncOutlined />
          {t('collaboration.sync')}
        </span>
      ),
      children: <SyncMonitor />,
    },
  ]

  return (
    <div className="collaboration-panel">
      <div className="collaboration-panel__header">
        <h2>{t('collaboration.title')}</h2>
      </div>

      <div className="collaboration-panel__content">
        <Tabs
          activeKey={activeTab}
          onChange={setActiveTab}
          items={items}
          size="large"
          className="collaboration-panel__tabs"
        />
      </div>
    </div>
  )
}

/**
 * 协作面板 - 主题引擎驱动 / Collaboration Panel - Theme Engine Driven
 *
 * 根据当前主题动态加载对应的UI壳子
 * Dynamically loads corresponding UI shell based on current theme
 */
function CollaborationPanel() {
  return (
    <EnginePageRouter
      pageId="collaboration"
      fallbackComponent={TraditionalCollaborationPanel}
    />
  )
}

export default CollaborationPanel

