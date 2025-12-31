/**
 * 信件选择模态框组件 (Letter Selection Modal Component)
 *
 * 用于在首页展示三封信供用户选择
 * (Display three letters for user selection on homepage)
 *
 * @author omni-agent team
 * @since 2026-01-01
 */

import React, { useState, useEffect } from 'react'
import { Modal, Card, Button, Space, Radio } from 'antd'
import {
  UserOutlined,
  CodeOutlined,
  TeamOutlined,
  MailOutlined,
  CloseOutlined,
  GlobalOutlined
} from '@ant-design/icons'
import ReactMarkdown from 'react-markdown'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { tomorrow } from 'react-syntax-highlighter/dist/esm/styles/prism'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/common/LetterModal.css'

// 获取信件配置（使用国际化）
const getLetters = (t, language) => [
  {
    key: 'user',
    title: t('letter.user.title'),
    icon: <UserOutlined />,
    description: t('letter.user.description'),
    color: '#1890ff',
    emoji: '👤',
    fileZh: 'user',
    fileEn: 'user-en'
  },
  {
    key: 'developer',
    title: t('letter.developer.title'),
    icon: <CodeOutlined />,
    description: t('letter.developer.description'),
    color: '#52c41a',
    emoji: '💻',
    fileZh: 'developer',
    fileEn: 'developer-en'
  },
  {
    key: 'enterprise',
    title: t('letter.enterprise.title'),
    icon: <TeamOutlined />,
    description: t('letter.enterprise.description'),
    color: '#722ed1',
    emoji: '💼',
    fileZh: 'enterprise',
    fileEn: 'enterprise-en'
  }
]

/**
 * 角色卡片组件 - 场景式设计
 */
const CharacterCard = ({ letter, onSelect, t }) => {
  const [isHovered, setIsHovered] = useState(false)

  return (
    <div
      className="character-wrapper"
      onMouseEnter={() => setIsHovered(true)}
      onMouseLeave={() => setIsHovered(false)}
      onClick={() => onSelect(letter)}
    >
      {/* 悬挂的信封气泡 */}
      <div className={`envelope-bubble ${isHovered ? 'envelope-bubble-hover' : ''}`}>
        <div className="envelope-icon">
          <MailOutlined style={{ fontSize: 32, color: letter.color }} />
          <div className="envelope-badge">1</div>
        </div>
        <div className="bubble-arrow" style={{ borderTopColor: '#fff' }}></div>
      </div>

      {/* 角色小人 */}
      <div className={`character-avatar ${isHovered ? 'character-avatar-hover' : ''}`}>
        <div
          className="avatar-circle"
          style={{
            background: `linear-gradient(135deg, ${letter.color}15 0%, ${letter.color}30 100%)`,
            borderColor: letter.color
          }}
        >
          <div className="avatar-emoji">{letter.emoji}</div>
        </div>

        {/* 角色名称 */}
        <div className="character-name" style={{ color: letter.color }}>
          {letter.key === 'user' && (t('letter.user.title').split('的')[0] || 'User')}
          {letter.key === 'developer' && (t('letter.developer.title').split('的')[0] || 'Developer')}
          {letter.key === 'enterprise' && (t('letter.enterprise.title').split('的')[0] || 'Enterprise')}
        </div>

        {/* 悬停时显示描述 */}
        {isHovered && (
          <div className="character-description">
            {letter.description}
          </div>
        )}
      </div>

      {/* 点击提示 */}
      {isHovered && (
        <div className="click-hint" style={{ color: letter.color }}>
          ✨ {t('letter.user.buttonText')}
        </div>
      )}
    </div>
  )
}

/**
 * 信件内容查看器组件
 */
const LetterViewer = ({ letter, onClose, t, language }) => {
  const [content, setContent] = useState('')
  const [loading, setLoading] = useState(true)

  useEffect(() => {
    if (letter) {
      setLoading(true)

      // 动态导入对应的 Markdown 文件
      const loadMarkdown = async () => {
        try {
          let module
          const fileKey = language === 'en' ? letter.fileEn : letter.fileZh

          if (fileKey === 'user') {
            module = await import('../../assets/md/致普通用户的一封信.md?raw')
          } else if (fileKey === 'user-en') {
            module = await import('../../assets/md/letter-to-users-en.md?raw')
          } else if (fileKey === 'developer') {
            module = await import('../../assets/md/致开发者的一封信.md?raw')
          } else if (fileKey === 'developer-en') {
            module = await import('../../assets/md/letter-to-developers-en.md?raw')
          } else if (fileKey === 'enterprise') {
            module = await import('../../assets/md/致企业用户的一封信.md?raw')
          } else if (fileKey === 'enterprise-en') {
            module = await import('../../assets/md/letter-to-enterprise-en.md?raw')
          }

          if (module && module.default) {
            setContent(module.default)
          } else {
            throw new Error('Failed to load markdown content')
          }
          setLoading(false)
        } catch (err) {
          console.error('Failed to load letter:', err)
          setContent(`# ${t('letter.loadFailed')}\n\n${t('letter.loadFailedMessage')}`)
          setLoading(false)
        }
      }

      loadMarkdown()
    }
  }, [letter, language, t])

  return (
    <Modal
      open={!!letter}
      onCancel={onClose}
      footer={null}
      width="80%"
      className="letter-viewer-modal"
      closeIcon={<CloseOutlined />}
    >
      <div className="letter-viewer-content">
        {loading ? (
          <div className="letter-viewer-loading">
            <p>{t('letter.loading')}</p>
          </div>
        ) : (
          <ReactMarkdown
            components={{
              code({ node, inline, className, children, ...props }) {
                const match = /language-(\w+)/.exec(className || '')
                return !inline && match ? (
                  <SyntaxHighlighter
                    style={tomorrow}
                    language={match[1]}
                    PreTag="div"
                    {...props}
                  >
                    {String(children).replace(/\n$/, '')}
                  </SyntaxHighlighter>
                ) : (
                  <code className={className} {...props}>
                    {children}
                  </code>
                )
              }
            }}
          >
            {content}
          </ReactMarkdown>
        )}
      </div>
    </Modal>
  )
}

/**
 * 信件选择模态框主组件
 */
const LetterModal = ({ open, onClose, onLetterRead }) => {
  const { t, language, setLanguage } = useLanguage()
  const [selectedLetter, setSelectedLetter] = useState(null)
  const [showConfetti, setShowConfetti] = useState(false)
  const [isFirstVisit, setIsFirstVisit] = useState(false)

  // 获取信件列表
  const LETTERS = getLetters(t, language)

  // 检查是否首次访问
  useEffect(() => {
    const hasSeenLetter = localStorage.getItem('omni_agent_letter_seen')
    setIsFirstVisit(!hasSeenLetter)
  }, [])

  // 当模态框打开时显示庆祝动画
  useEffect(() => {
    if (open) {
      setShowConfetti(true)
      const timer = setTimeout(() => setShowConfetti(false), 2000)
      return () => clearTimeout(timer)
    }
  }, [open])

  const handleSelectLetter = (letter) => {
    setSelectedLetter(letter)
  }

  const handleCloseViewer = () => {
    setSelectedLetter(null)
    // 如果是首次访问，阅读完一封信后标记为已读
    if (isFirstVisit && onLetterRead) {
      onLetterRead()
    }
  }

  const handleCloseAll = () => {
    // 只有非首次访问才能直接关闭
    if (!isFirstVisit) {
      setSelectedLetter(null)
      onClose()
    }
  }

  const handleLanguageChange = (e) => {
    setLanguage(e.target.value)
  }

  return (
    <>
      <Modal
        open={open && !selectedLetter}
        onCancel={handleCloseAll}
        footer={null}
        width={1000}
        className="letter-selection-modal letter-scene-modal"
        closeIcon={!isFirstVisit ? <CloseOutlined /> : null}
        maskClosable={!isFirstVisit}
        keyboard={!isFirstVisit}
        centered
      >
        {/* 庆祝动画 */}
        {showConfetti && (
          <div className="confetti-container">
            <span className="confetti">🎉</span>
            <span className="confetti">✨</span>
            <span className="confetti">🎊</span>
            <span className="confetti">💫</span>
            <span className="confetti">⭐</span>
          </div>
        )}

        {/* 语言切换按钮 */}
        <div className="letter-language-switcher">
          <GlobalOutlined className="letter-language-icon" />
          <Radio.Group
            value={language}
            onChange={handleLanguageChange}
            buttonStyle="solid"
            size="small"
          >
            <Radio.Button value="zh">中文</Radio.Button>
            <Radio.Button value="en">English</Radio.Button>
          </Radio.Group>
        </div>

        {/* 艺术标题 */}
        <div className="letter-art-header">
          <h1 className="art-title">
            <span className="title-gradient">
              {t('letter.modalTitle')}
            </span>
          </h1>
          <p className="art-subtitle">
            {t('letter.modalSubtitle')}
          </p>
          {isFirstVisit && (
            <div className="first-visit-badge">
              <span className="badge-icon">📖</span>
              <span className="badge-text">
                {language === 'zh'
                  ? '请选择并阅读一封信后即可进入系统'
                  : 'Please read at least one letter to continue'}
              </span>
            </div>
          )}
        </div>

        {/* 场景区域 - 三个角色 */}
        <div className="character-scene">
          <div className="scene-stage">
            {LETTERS.map((letter, index) => (
              <CharacterCard
                key={letter.key}
                letter={letter}
                onSelect={handleSelectLetter}
                t={t}
                style={{ animationDelay: `${index * 0.15}s` }}
              />
            ))}
          </div>
        </div>

        {/* 底部提示 */}
        {!isFirstVisit && (
          <div className="letter-modal-footer">
            <Button
              onClick={handleCloseAll}
              type="text"
              className="later-button"
            >
              {t('letter.laterButton')} →
            </Button>
          </div>
        )}
      </Modal>

      <LetterViewer
        letter={selectedLetter}
        onClose={handleCloseViewer}
        t={t}
        language={language}
      />
    </>
  )
}

export default LetterModal

