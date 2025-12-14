/**
 * 冲突列表组件 (Conflict List Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect, useCallback } from 'react'
import { Space, Button, Select, message, Empty } from 'antd'
import { ReloadOutlined, FilterOutlined } from '@ant-design/icons'
import ConflictCard from './ConflictCard'
import VotingPanel from './VotingPanel'
import { Loading } from '../common'
import { useLanguage } from '../../contexts/LanguageContext'
import feedbackApi from '../../api/modules/feedback'
import '../../assets/css/feedback/conflict-list.css'

function ConflictList() {
  const { t } = useLanguage()
  const [conflicts, setConflicts] = useState([])
  const [loading, setLoading] = useState(false)
  const [votingVisible, setVotingVisible] = useState(false)
  const [selectedConflict, setSelectedConflict] = useState(null)
  const [filterStatus, setFilterStatus] = useState('all')

  const loadConflicts = useCallback(async () => {
    setLoading(true)
    try {
      const response = await feedbackApi.getConflicts({
        status: filterStatus !== 'all' ? filterStatus : undefined,
      })
      if (response) {
        // axios 拦截器已返回 response.data (Axios interceptor returns response.data)
        setConflicts(response.list || response || [])
      }
    } catch (error) {
      console.error('Failed to load conflicts:', error)
      message.error(t('feedback.loadFailed'))
    } finally {
      setLoading(false)
    }
  }, [filterStatus, t])

  useEffect(() => {
    loadConflicts()
  }, [loadConflicts])

  const handleVote = (conflict) => {
    setSelectedConflict(conflict)
    setVotingVisible(true)
  }

  const handleVoteSuccess = () => {
    setVotingVisible(false)
    loadConflicts()
    message.success(t('feedback.voteSuccess'))
  }

  return (
    <div className="conflict-list">
      <div className="conflict-list__toolbar">
        <Space>
          <FilterOutlined />
          <Select
            value={filterStatus}
            onChange={setFilterStatus}
            style={{ width: 150 }}
            options={[
              { label: t('feedback.all'), value: 'all' },
              { label: t('feedback.pending'), value: 'pending' },
              { label: t('feedback.status.voting'), value: 'voting' },
              { label: t('feedback.resolved'), value: 'resolved' },
            ]}
          />
        </Space>
        <Button
          icon={<ReloadOutlined />}
          onClick={loadConflicts}
          loading={loading}
        >
          {t('common.refresh')}
        </Button>
      </div>

      <div className="conflict-list__content">
        {loading ? (
          <Loading spinning={true} tip={t('common.loading')} />
        ) : conflicts.length === 0 ? (
          <Empty
            image={Empty.PRESENTED_IMAGE_SIMPLE}
            description={t('feedback.noConflicts')}
          />
        ) : (
          <div className="conflict-list__grid">
            {conflicts.map((conflict) => (
              <ConflictCard
                key={conflict.id}
                conflict={conflict}
                onVote={handleVote}
              />
            ))}
          </div>
        )}
      </div>

      <VotingPanel
        visible={votingVisible}
        conflict={selectedConflict}
        onCancel={() => setVotingVisible(false)}
        onSuccess={handleVoteSuccess}
      />
    </div>
  )
}

export default ConflictList

