/**
 * 聊天框组件 (Chat Box Component)
 *
 * 展示问答消息列表，支持滚动、加载状态
 * (Displays Q&A message list with scrolling and loading states)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useRef, useEffect } from 'react'
import { Button, Radio, Select } from 'antd'
import { HistoryOutlined } from '@ant-design/icons'
import AnswerCard from './AnswerCard'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/qa/chat-box.css'

// 角色列表 (Role list)
const ROLES = [
  { value: 'general', labelKey: 'qa.role.general' },
  { value: 'developer', labelKey: 'qa.role.developer' },
  { value: 'devops', labelKey: 'qa.role.devops' },
  { value: 'architect', labelKey: 'qa.role.architect' },
  { value: 'researcher', labelKey: 'qa.role.researcher' },
  { value: 'product_manager', labelKey: 'qa.role.productManager' },
  { value: 'data_scientist', labelKey: 'qa.role.dataScientist' },
  { value: 'security_engineer', labelKey: 'qa.role.securityEngineer' },
  { value: 'tester', labelKey: 'qa.role.tester' },
]

function ChatBox(props) {
  const { 
    messages, 
    loading, 
    onFeedback, 
    onToggleHistory, 
    onStopGeneration, 
    isGenerating,
    isStreamingMode,
    onToggleStreamingMode,
    knowledgeMode,        // 'none' | 'rag' | 'role'
    onKnowledgeModeChange,
    roleName,
    onRoleNameChange
  } = props
  const { t } = useLanguage()
  const messagesEndRef = useRef(null)
  const toolbarRef = useRef(null)

  useEffect(() => {
    messagesEndRef.current?.scrollIntoView({ behavior: 'smooth' })
  }, [messages])

  // 监听滚动，添加阴影效果
  useEffect(() => {
    const handleScroll = () => {
      if (toolbarRef.current) {
        const scrollTop = window.pageYOffset || document.documentElement.scrollTop
        if (scrollTop > 10) {
          toolbarRef.current.classList.add('scrolled')
        } else {
          toolbarRef.current.classList.remove('scrolled')
        }
      }
    }

    window.addEventListener('scroll', handleScroll, { passive: true })
    return () => window.removeEventListener('scroll', handleScroll)
  }, [])

  return (
    <div className="chat-box">
      <div className="chat-box__toolbar" ref={toolbarRef}>
        <div className="chat-box__toolbar-left">
          <Button
            icon={<HistoryOutlined />}
            onClick={onToggleHistory}
            className="chat-box__history-btn"
          >
            {t('qa.history.title')}
          </Button>
        </div>
        
        <div className="chat-box__toolbar-right">
          {/* 知识库模式选择 (Knowledge Mode Selection) */}
          <div className="chat-box__kb-mode">
            <span className="chat-box__kb-mode-label">{t('qa.knowledgeMode.label')}:</span>
            <Radio.Group
              value={knowledgeMode}
              onChange={(e) => onKnowledgeModeChange(e.target.value)}
              size="small"
              className="chat-box__kb-mode-group"
            >
              <Radio.Button value="none">{t('qa.knowledgeMode.none')}</Radio.Button>
              <Radio.Button value="rag">{t('qa.knowledgeMode.rag')}</Radio.Button>
              <Radio.Button value="role">{t('qa.knowledgeMode.role')}</Radio.Button>
            </Radio.Group>
          </div>

          {/* 角色选择（仅在角色模式下显示） (Role Selection) */}
          {knowledgeMode === 'role' && (
            <Select
              value={roleName}
              onChange={onRoleNameChange}
              size="small"
              className="chat-box__role-select"
              showSearch
              filterOption={(input, option) =>
                option.children.toLowerCase().indexOf(input.toLowerCase()) >= 0
              }
            >
              {ROLES.map(role => (
                <Select.Option key={role.value} value={role.value}>
                  {t(role.labelKey)}
                </Select.Option>
              ))}
            </Select>
          )}

          <Button
            onClick={onToggleStreamingMode}
            className="chat-box__mode-toggle"
            title={isStreamingMode ? t('qa.mode.switchToNonStreaming') : t('qa.mode.switchToStreaming')}
            size="small"
          >
            {isStreamingMode ? '⚡ ' + t('qa.mode.streaming') : '💭 ' + t('qa.mode.nonStreaming')}
          </Button>
        </div>
      </div>

      <div className="chat-box__messages">
        {messages.length === 0 ? (
          <div className="chat-box__empty">
            <div className="chat-box__empty-icon">💬</div>
            <p className="chat-box__empty-text">{t('qa.emptyMessage')}</p>
          </div>
        ) : (
          messages.map((message) => (
            <div
              key={message.id}
              className={`chat-box__message chat-box__message--${message.type}`}
            >
              {message.type === 'question' ? (
                <div className="chat-box__question">
                  <div className="chat-box__question-avatar">👤</div>
                  <div className="chat-box__question-content">
                    <div className="chat-box__question-text">{message.content}</div>
                    <div className="chat-box__question-time">
                      {new Date(message.timestamp).toLocaleTimeString()}
                    </div>
                  </div>
                </div>
              ) : message.type === 'error' ? (
                <div className="chat-box__error">
                  <div className="chat-box__error-icon">⚠️</div>
                  <div className="chat-box__error-content">{message.content}</div>
                </div>
              ) : (
                <AnswerCard answer={message} onFeedback={onFeedback} />
              )}
            </div>
          ))
        )}

        {loading && (
          <div className="chat-box__loading">
            <div className="chat-box__loading-dots">
              <span></span>
              <span></span>
              <span></span>
            </div>
          </div>
        )}

        {isGenerating && (
          <div className="chat-box__stop-btn-wrapper">
            <Button
              danger
              onClick={onStopGeneration}
              className="chat-box__stop-btn"
            >
              🛑 {t('qa.stopGeneration')}
            </Button>
          </div>
        )}

        <div ref={messagesEndRef} />
      </div>
    </div>
  )
}

export default ChatBox

