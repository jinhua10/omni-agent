/**
 * 答案卡片组件 (Answer Card Component)
 *
 * 展示 AI 回答，支持 Markdown 渲染、代码高亮、反馈
 * (Displays AI answers with Markdown rendering, code highlighting, feedback)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { Button, Space, Tooltip } from 'antd'
import { LikeOutlined, DislikeOutlined, CopyOutlined, LikeFilled, DislikeFilled } from '@ant-design/icons'
import StreamingAnswer from './StreamingAnswer'
import MarkdownRenderer from './MarkdownRenderer'
import DocumentReferences from './DocumentReferences'
import SessionInfoDisplay from './SessionInfoDisplay'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/qa/answer-card.css'

/**
 * 答案卡片组件
 */
function AnswerCard(props) {
  const { answer, onFeedback } = props
  const { t } = useLanguage()
  const [feedback, setFeedback] = useState(null)
  const [copied, setCopied] = useState(false)

  const handleLike = () => {
    const newFeedback = feedback === 'like' ? null : 'like'
    setFeedback(newFeedback)
    if (onFeedback) {
      onFeedback(answer.id, newFeedback === 'like' ? 5 : 3)
    }
  }

  const handleDislike = () => {
    const newFeedback = feedback === 'dislike' ? null : 'dislike'
    setFeedback(newFeedback)
    if (onFeedback) {
      onFeedback(answer.id, newFeedback === 'dislike' ? 1 : 3)
    }
  }

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(answer.content)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    } catch (error) {
      console.error('Failed to copy:', error)
    }
  }

  return (
    <div className="answer-card">
      <div className="answer-card__avatar">
        <span className="answer-card__avatar-icon">🤖</span>
      </div>

      <div className="answer-card__content">
        <div className="answer-card__text">
          {answer.thinking ? (
            <div className="answer-card__thinking">
              <div className="answer-card__thinking-dots">
                <span></span>
                <span></span>
                <span></span>
              </div>
              <span className="answer-card__thinking-text">Thinking...</span>
            </div>
          ) : answer.dualTrack ? (
            // 双轨模式：左右双面板显示
            <div className="answer-card__dual-track">
              <div className="answer-card__panel answer-card__panel--left">
                <div className="answer-card__panel-header">
                  <span className="answer-card__panel-icon">🤖</span>
                  <span className="answer-card__panel-title">{t('qa.dualTrack.leftPanelTitle')}</span>
                </div>
                <div className="answer-card__panel-content">
                  {answer.streaming ? (
                    <StreamingAnswer content={answer.leftPanel || ''} streaming={answer.streaming} />
                  ) : (
                    <MarkdownRenderer content={answer.leftPanel || ''} isStreaming={false} />
                  )}
                </div>
              </div>

              <div className="answer-card__panel answer-card__panel--right">
                <div className="answer-card__panel-header">
                  <span className="answer-card__panel-icon">📚</span>
                  <span className="answer-card__panel-title">{t('qa.dualTrack.rightPanelTitle')}</span>
                </div>
                <div className="answer-card__panel-content">
                  {answer.streaming ? (
                    <StreamingAnswer content={answer.rightPanel || ''} streaming={answer.streaming} />
                  ) : (
                    <MarkdownRenderer content={answer.rightPanel || ''} isStreaming={false} />
                  )}
                </div>
              </div>
            </div>
          ) : answer.streaming ? (
            // 单轨模式：单面板流式显示
            <StreamingAnswer content={answer.content} streaming={answer.streaming} />
          ) : (
            // 单轨模式：单面板静态显示
            <MarkdownRenderer content={answer.content} isStreaming={false} />
          )}
          
          {answer.stopped && !answer.streaming && (
            <div className="answer-card__stopped-badge">
              ⚠️ {t('qa.generationStopped')}
            </div>
          )}
        </div>

        {/* 会话信息（分页文档） */}
        {!answer.streaming && answer.sessionId && (
          <SessionInfoDisplay sessionId={answer.sessionId} />
        )}

        {/* 引用文档列表 */}
        {!answer.streaming && answer.sources && answer.sources.length > 0 && (
          <DocumentReferences
            sources={answer.sources}
            chunks={answer.chunks}
          />
        )}

        {!answer.streaming && (
          <div className="answer-card__footer">
            <div className="answer-card__time">
              {new Date(answer.timestamp).toLocaleTimeString()}
            </div>

            <Space className="answer-card__actions">
              <Tooltip title={t('qa.feedback.like')}>
                <Button
                  type="text"
                  icon={feedback === 'like' ? <LikeFilled /> : <LikeOutlined />}
                  onClick={handleLike}
                  className={`answer-card__action-btn ${feedback === 'like' ? 'answer-card__action-btn--active' : ''}`}
                />
              </Tooltip>

              <Tooltip title={t('qa.feedback.dislike')}>
                <Button
                  type="text"
                  icon={feedback === 'dislike' ? <DislikeFilled /> : <DislikeOutlined />}
                  onClick={handleDislike}
                  className={`answer-card__action-btn ${feedback === 'dislike' ? 'answer-card__action-btn--active' : ''}`}
                />
              </Tooltip>

              <Tooltip title={copied ? t('qa.feedback.copied') : t('qa.feedback.copy')}>
                <Button
                  type="text"
                  icon={<CopyOutlined />}
                  onClick={handleCopy}
                  className="answer-card__action-btn"
                />
              </Tooltip>
            </Space>
          </div>
        )}
      </div>
    </div>
  )
}

export default AnswerCard

