/**
 * 对话历史组件 (Conversation History Component)
 *
 * 展示历史对话记录，支持搜索和选择
 * (Displays conversation history with search and selection)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect } from 'react'
import { List, Input, Button, Empty, Spin } from 'antd'
import { SearchOutlined, CloseOutlined, ClockCircleOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import qaApi from '../../api/modules/qa'
import '../../assets/css/qa/conversation-history.css'

function ConversationHistory(props) {
  const { onClose, onSelectQuestion } = props
  const { t } = useLanguage()
  const [history, setHistory] = useState([])
  const [loading, setLoading] = useState(false)
  const [searchText, setSearchText] = useState('')
  const [page, setPage] = useState(1)
  const [hasMore, setHasMore] = useState(true)

  const loadHistory = async (pageNum = 1, search = '') => {
    setLoading(true)
    try {
      const response = await qaApi.getHistory({
        page: pageNum,
        pageSize: 20,
        keyword: search,
      })

      if (response) {
        // axios 拦截器已返回 response.data (Axios interceptor returns response.data)
        if (pageNum === 1) {
          setHistory(response.list || [])
        } else {
          setHistory(prev => [...prev, ...(response.list || [])])
        }
        setHasMore(response.hasMore || false)
      }
    } catch (error) {
      console.error('Failed to load history:', error)
    } finally {
      setLoading(false)
    }
  }

  useEffect(() => {
    loadHistory(1, searchText)
  }, [])

  const handleSearch = (value) => {
    setSearchText(value)
    setPage(1)
    loadHistory(1, value)
  }

  const handleLoadMore = () => {
    const nextPage = page + 1
    setPage(nextPage)
    loadHistory(nextPage, searchText)
  }

  const handleQuestionClick = (item) => {
    if (onSelectQuestion) {
      onSelectQuestion(item.question)
    }
  }

  const formatTime = (timestamp) => {
    const date = new Date(timestamp)
    const now = new Date()
    const diff = now - date
    const days = Math.floor(diff / (1000 * 60 * 60 * 24))

    if (days === 0) {
      return t('qa.history.today')
    } else if (days === 1) {
      return t('qa.history.yesterday')
    } else if (days < 7) {
      return `${days} ${t('qa.history.daysAgo')}`
    } else {
      return date.toLocaleDateString()
    }
  }

  return (
    <div className="conversation-history">
      <div className="conversation-history__header">
        <div className="conversation-history__title-wrapper">
          <ClockCircleOutlined className="conversation-history__icon" />
          <h3 className="conversation-history__title">{t('qa.history.title')}</h3>
        </div>
        <Button
          type="text"
          icon={<CloseOutlined />}
          onClick={onClose}
          className="conversation-history__close-btn"
        />
      </div>

      <div className="conversation-history__search">
        <Input
          placeholder={t('qa.history.searchPlaceholder')}
          prefix={<SearchOutlined />}
          value={searchText}
          onChange={(e) => handleSearch(e.target.value)}
          allowClear
        />
      </div>

      <div className="conversation-history__content">
        {loading && history.length === 0 ? (
          <div className="conversation-history__loading">
            <Spin tip={t('common.loading')}>
              <div style={{ padding: 50 }} />
            </Spin>
          </div>
        ) : history.length === 0 ? (
          <Empty
            image={Empty.PRESENTED_IMAGE_SIMPLE}
            description={t('qa.history.noResults')}
            className="conversation-history__empty"
          />
        ) : (
          <>
            <List
              dataSource={history}
              renderItem={(item) => (
                <div
                  key={item.id}
                  className="conversation-history__item"
                  onClick={() => handleQuestionClick(item)}
                >
                  <div className="conversation-history__item-time">
                    {formatTime(item.timestamp)}
                  </div>
                  <p className="conversation-history__item-question">{item.question}</p>
                  {item.answerPreview && (
                    <p className="conversation-history__item-answer">{item.answerPreview}</p>
                  )}
                </div>
              )}
            />

            {hasMore && (
              <div className="conversation-history__load-more">
                <Button
                  onClick={handleLoadMore}
                  loading={loading}
                  className="conversation-history__load-more-btn"
                >
                  {t('common.loadMore')}
                </Button>
              </div>
            )}
          </>
        )}
      </div>
    </div>
  )
}

export default ConversationHistory

