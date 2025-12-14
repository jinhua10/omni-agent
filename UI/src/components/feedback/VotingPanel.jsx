/**
 * 投票面板组件 (Voting Panel Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { Modal } from 'antd'
import ABComparison from './ABComparison'
import { useLanguage } from '../../contexts/LanguageContext'
import feedbackApi from '../../api/modules/feedback'

function VotingPanel(props) {
  const { visible, conflict, onCancel, onSuccess } = props
  const { t } = useLanguage()
  const [voting, setVoting] = useState(false)

  const handleVote = async (choice) => {
    if (!conflict) return

    setVoting(true)
    try {
      await feedbackApi.vote({
        conflictId: conflict.id,
        choice,
      })
      onSuccess && onSuccess()
    } catch (error) {
      console.error('Failed to vote:', error)
    } finally {
      setVoting(false)
    }
  }

  return (
    <Modal
      title={t('feedback.voting')}
      open={visible}
      onCancel={onCancel}
      footer={null}
      width={800}
      centered
    >
      {conflict && (
        <ABComparison
          conflict={conflict}
          onVote={handleVote}
          voting={voting}
        />
      )}
    </Modal>
  )
}

export default VotingPanel

