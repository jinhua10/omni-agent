/**
 * 伙伴卡片组件 (Peer Card Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Card, Tag, Button, Space, Tooltip, Avatar } from 'antd'
import { DisconnectOutlined, SyncOutlined, UserOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/collaboration/peer-card.css'

function PeerCard(props) {
  const { peer, onDisconnect, onSync } = props
  const { t } = useLanguage()

  const getStatusColor = (status) => {
    const colors = {
      online: 'green',
      offline: 'gray',
      syncing: 'blue',
    }
    return colors[status] || 'default'
  }

  return (
    <Card className="peer-card" hoverable>
      <div className="peer-card__header">
        <div className="peer-card__avatar-wrapper">
          <Avatar size={48} icon={<UserOutlined />} className="peer-card__avatar" />
          <Tag color={getStatusColor(peer.status)} className="peer-card__status">
            {t(`collaboration.status.${peer.status}`)}
          </Tag>
        </div>
      </div>

      <div className="peer-card__info">
        <h3 className="peer-card__name">{peer.name}</h3>
        <p className="peer-card__id">ID: {peer.id?.substring(0, 8)}...</p>

        <div className="peer-card__stats">
          <div className="peer-card__stat">
            <span className="peer-card__stat-label">{t('collaboration.sharedDocs')}</span>
            <span className="peer-card__stat-value">{peer.sharedDocs || 0}</span>
          </div>
          <div className="peer-card__stat">
            <span className="peer-card__stat-label">{t('collaboration.lastSync')}</span>
            <span className="peer-card__stat-value">
              {peer.lastSync ? new Date(peer.lastSync).toLocaleDateString() : '-'}
            </span>
          </div>
        </div>
      </div>

      <div className="peer-card__footer">
        <Space>
          <Tooltip title={t('collaboration.sync')}>
            <Button
              type="primary"
              size="small"
              icon={<SyncOutlined />}
              onClick={() => onSync && onSync(peer)}
              disabled={peer.status !== 'online'}
            >
              {t('collaboration.sync')}
            </Button>
          </Tooltip>
          <Tooltip title={t('collaboration.disconnect')}>
            <Button
              danger
              size="small"
              icon={<DisconnectOutlined />}
              onClick={() => onDisconnect && onDisconnect(peer)}
            >
              {t('collaboration.disconnect')}
            </Button>
          </Tooltip>
        </Space>
      </div>
    </Card>
  )
}

export default PeerCard

