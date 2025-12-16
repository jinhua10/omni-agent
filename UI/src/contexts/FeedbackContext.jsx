/**
 * FeedbackContext 反馈状态上下文 (Feedback State Context)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { createContext, useContext, useState, useCallback } from 'react'
import PropTypes from 'prop-types'
import api from '@api/modules'

const FeedbackContext = createContext()

export function FeedbackProvider({ children }) {
  const [feedbacks, setFeedbacks] = useState([])
  const [conflicts, setConflicts] = useState([])
  const [loading, setLoading] = useState(false)

  const submitFeedback = useCallback(async (feedbackData) => {
    try {
      setLoading(true)
      const result = await api.feedback.submit(feedbackData)
      setFeedbacks((prev) => [result, ...prev])
      return result
    } catch (error) {
      console.error('Failed to submit feedback:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  const fetchConflicts = useCallback(async (params) => {
    try {
      setLoading(true)
      const data = await api.feedback.getConflicts(params)
      setConflicts(data.list || data)
      return data
    } catch (error) {
      // 如果是404错误（接口未实现），返回空数据而不报错
      if (error.response?.status === 404) {
        console.warn('⚠️ Feedback conflicts endpoint not implemented, using empty data')
        setConflicts([])
        return { list: [] }
      }
      console.error('Failed to fetch conflicts:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  const vote = useCallback(async (voteData) => {
    try {
      await api.feedback.vote(voteData)
      // 更新冲突列表中的投票数
      setConflicts((prev) =>
        prev.map((conflict) => {
          if (conflict.id === voteData.conflictId) {
            return {
              ...conflict,
              votes: (conflict.votes || 0) + 1,
            }
          }
          return conflict
        })
      )
    } catch (error) {
      console.error('Failed to vote:', error)
      throw error
    }
  }, [])

  const value = {
    feedbacks,
    conflicts,
    loading,
    submitFeedback,
    fetchConflicts,
    vote,
  }

  return (
    <FeedbackContext.Provider value={value}>
      {children}
    </FeedbackContext.Provider>
  )
}

FeedbackProvider.propTypes = {
  children: PropTypes.node.isRequired,
}

export function useFeedback() {
  const context = useContext(FeedbackContext)
  if (!context) {
    throw new Error('useFeedback must be used within FeedbackProvider')
  }
  return context
}

export default FeedbackContext

