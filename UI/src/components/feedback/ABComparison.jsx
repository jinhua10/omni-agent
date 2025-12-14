/**
 * A/B对比组件 (AB Comparison Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Button, Card } from 'antd'
import { CheckOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/feedback/ab-comparison.css'

function ABComparison(props) {
  const { conflict, onVote, voting } = props
  const { t } = useLanguage()

  return (
    <div className="ab-comparison">
      <div className="ab-comparison__question">
        <h3>{conflict.question || t('feedback.whichBetter')}</h3>
      </div>

      <div className="ab-comparison__options">
        <Card
          className="ab-comparison__option ab-comparison__option--a"
          hoverable={!voting}
        >
          <div className="ab-comparison__option-header">
            <span className="ab-comparison__option-label">
              {t('feedback.conceptA')}
            </span>
          </div>
          <div className="ab-comparison__option-content">
            {conflict.conceptA}
          </div>
          <div className="ab-comparison__option-footer">
            <Button
              type="primary"
              size="large"
              block
              icon={<CheckOutlined />}
              onClick={() => onVote && onVote('A')}
              loading={voting}
              className="ab-comparison__vote-btn ab-comparison__vote-btn--a"
            >
              {t('feedback.voteA')}
            </Button>
          </div>
        </Card>

        <div className="ab-comparison__divider">
          <span>VS</span>
        </div>

        <Card
          className="ab-comparison__option ab-comparison__option--b"
          hoverable={!voting}
        >
          <div className="ab-comparison__option-header">
            <span className="ab-comparison__option-label">
              {t('feedback.conceptB')}
            </span>
          </div>
          <div className="ab-comparison__option-content">
            {conflict.conceptB}
          </div>
          <div className="ab-comparison__option-footer">
            <Button
              type="primary"
              size="large"
              block
              icon={<CheckOutlined />}
              onClick={() => onVote && onVote('B')}
              loading={voting}
              className="ab-comparison__vote-btn ab-comparison__vote-btn--b"
            >
              {t('feedback.voteB')}
            </Button>
          </div>
        </Card>
      </div>

      {conflict.context && (
        <div className="ab-comparison__context">
          <h4>{t('feedback.context')}</h4>
          <p>{conflict.context}</p>
        </div>
      )}
    </div>
  )
}

export default ABComparison

