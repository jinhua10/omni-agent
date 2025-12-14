/**
 * 冲突卡片组件 (Conflict Card Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Card, Tag, Button, Progress } from 'antd'
import { SwapOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/feedback/conflict-card.css'

function ConflictCard(props) {
  const { conflict, onVote } = props
  const { t } = useLanguage()

  const getStatusColor = (status) => {
    const colors = {
      pending: 'orange',
      voting: 'blue',
      resolved: 'green',
    }
    return colors[status] || 'default'
  }

  const calculateVoteProgress = () => {
    const total = (conflict.voteA || 0) + (conflict.voteB || 0)
    if (total === 0) return 0
    return Math.round((conflict.voteA / total) * 100)
  }

  return (
    <Card
      className="conflict-card"
      hoverable
      onClick={() => onVote && onVote(conflict)}
    >
      <div className="conflict-card__header">
        <Tag color={getStatusColor(conflict.status)}>
          {t(`feedback.status.${conflict.status}`)}
        </Tag>
        <span className="conflict-card__date">
          {new Date(conflict.createdAt).toLocaleDateString()}
        </span>
      </div>

      <div className="conflict-card__content">
        <h3 className="conflict-card__title">
          <SwapOutlined className="conflict-card__icon" />
          {conflict.question || t('feedback.conceptConflict')}
        </h3>

        <div className="conflict-card__concepts">
          <div className="conflict-card__concept conflict-card__concept--a">
            <div className="conflict-card__concept-label">
              {t('feedback.conceptA')}
            </div>
            <div className="conflict-card__concept-text">
              {conflict.conceptA}
            </div>
          </div>

          <div className="conflict-card__vs">VS</div>

          <div className="conflict-card__concept conflict-card__concept--b">
            <div className="conflict-card__concept-label">
              {t('feedback.conceptB')}
            </div>
            <div className="conflict-card__concept-text">
              {conflict.conceptB}
            </div>
          </div>
        </div>

        {conflict.status !== 'pending' && (
          <div className="conflict-card__votes">
            <div className="conflict-card__vote-info">
              <span>{t('feedback.voteA')}: {conflict.voteA || 0}</span>
              <span>{t('feedback.voteB')}: {conflict.voteB || 0}</span>
            </div>
            <Progress
              percent={calculateVoteProgress()}
              showInfo={false}
              strokeColor="#667eea"
              railColor="#f0f0f0"
            />
          </div>
        )}
      </div>

      <div className="conflict-card__footer">
        <Button
          type="primary"
          block
          onClick={(e) => {
            e.stopPropagation()
            onVote && onVote(conflict)
          }}
          disabled={conflict.status === 'resolved'}
        >
          {conflict.status === 'resolved' ? t('feedback.resolved') : t('feedback.vote')}
        </Button>
      </div>
    </Card>
  )
}

export default ConflictCard

