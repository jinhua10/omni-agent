/**
 * 会话信息显示组件 (Session Info Display Component)
 *
 * 显示RAG检索的会话信息，包括文档数量、分页控制等
 * (Displays RAG retrieval session info, including document count, pagination controls, etc.)
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React, { useState, useEffect } from 'react'
import { Button, Space, Tag, Spin } from 'antd'
import {
  LeftOutlined,
  RightOutlined,
  ReloadOutlined,
} from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import { useQA } from '../../contexts/QAContext'
import axios from 'axios'
import '../../assets/css/qa/session-info-display.css'

/**
 * 会话信息显示组件
 */
function SessionInfoDisplay({ sessionId, onLoadMore }) {
  const { t } = useLanguage()
  const { updateSessionInfo, sessionInfo } = useQA()
  const [loading, setLoading] = useState(false)
  const [loadingNext, setLoadingNext] = useState(false)
  const [loadingPrev, setLoadingPrev] = useState(false)

  /**
   * 获取会话信息
   */
  useEffect(() => {
    if (sessionId) {
      fetchSessionInfo()
    }
  }, [sessionId])

  const fetchSessionInfo = async () => {
    if (!sessionId) return

    setLoading(true)
    try {
      const response = await axios.get(`/api/search/session/${sessionId}/info`)
      updateSessionInfo(response.data)
    } catch (error) {
      console.error('❌ Failed to fetch session info:', error)
    } finally {
      setLoading(false)
    }
  }

  /**
   * 加载下一批文档
   */
  const handleLoadNext = async () => {
    if (!sessionId || !sessionInfo?.hasNext) return

    setLoadingNext(true)
    try {
      const response = await axios.post(`/api/search/session/${sessionId}/next`)
      // 通知父组件重新提问
      if (onLoadMore) {
        onLoadMore('next', response.data)
      }
      // 更新会话信息
      await fetchSessionInfo()
    } catch (error) {
      console.error('❌ Failed to load next batch:', error)
    } finally {
      setLoadingNext(false)
    }
  }

  /**
   * 加载上一批文档
   */
  const handleLoadPrevious = async () => {
    if (!sessionId || !sessionInfo?.hasPrevious) return

    setLoadingPrev(true)
    try {
      const response = await axios.post(`/api/search/session/${sessionId}/previous`)
      // 通知父组件重新提问
      if (onLoadMore) {
        onLoadMore('previous', response.data)
      }
      // 更新会话信息
      await fetchSessionInfo()
    } catch (error) {
      console.error('❌ Failed to load previous batch:', error)
    } finally {
      setLoadingPrev(false)
    }
  }

  if (!sessionId || !sessionInfo) {
    return null
  }

  if (loading) {
    return (
      <div className="session-info-display session-info-display--loading">
        <Spin size="small" />
        <span>加载会话信息...</span>
      </div>
    )
  }

  return (
    <div className="session-info-display">
      <div className="session-info-display__stats">
        <Tag color="blue" className="session-info-display__stat">
          📊 检索到 <strong>{sessionInfo.totalDocuments}</strong> 个文档
        </Tag>
        <Tag color="green" className="session-info-display__stat">
          📄 当前使用 <strong>{sessionInfo.documentsPerQuery}</strong> 个
        </Tag>
        {sessionInfo.remainingDocuments > 0 && (
          <Tag color="orange" className="session-info-display__stat">
            📝 剩余 <strong>{sessionInfo.remainingDocuments}</strong> 个未引用
          </Tag>
        )}
        <Tag color="purple" className="session-info-display__stat">
          📑 第 <strong>{sessionInfo.currentPage}</strong> / <strong>{sessionInfo.totalPages}</strong> 页
        </Tag>
      </div>

      {(sessionInfo.hasPrevious || sessionInfo.hasNext) && (
        <div className="session-info-display__pagination">
          <Button
            icon={<LeftOutlined />}
            onClick={handleLoadPrevious}
            disabled={!sessionInfo.hasPrevious || loadingPrev}
            loading={loadingPrev}
            className="session-info-display__pagination-btn"
          >
            上一批
          </Button>

          <span className="session-info-display__pagination-info">
            {sessionInfo.currentPage} / {sessionInfo.totalPages}
          </span>

          <Button
            type="primary"
            icon={<RightOutlined />}
            onClick={handleLoadNext}
            disabled={!sessionInfo.hasNext || loadingNext}
            loading={loadingNext}
            className="session-info-display__pagination-btn"
          >
            下一批
          </Button>

          <Button
            icon={<ReloadOutlined />}
            onClick={fetchSessionInfo}
            disabled={loading}
            title="刷新会话信息"
            className="session-info-display__refresh-btn"
          />
        </div>
      )}

      {sessionInfo.remainingDocuments === 0 && !sessionInfo.hasNext && (
        <div className="session-info-display__complete">
          ✅ 所有相关文档已引用完毕
        </div>
      )}
    </div>
  )
}

export default SessionInfoDisplay
