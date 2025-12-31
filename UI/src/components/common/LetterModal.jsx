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
 * 信件选择卡片组件
 */
const LetterCard = ({ letter, onSelect, t }) => {
  const [isHovered, setIsHovered] = useState(false)

  return (
    <Card
      className={`letter-card ${isHovered ? 'letter-card-hovered' : ''}`}
      hoverable
      onMouseEnter={() => setIsHovered(true)}
      onMouseLeave={() => setIsHovered(false)}
      onClick={() => onSelect(letter)}
      style={{
        borderColor: isHovered ? letter.color : undefined,
      }}
    >
      <div className="letter-card-content">
        <div
          className="letter-card-emoji"
          style={{
            animation: isHovered ? 'bounce 0.6s ease' : 'none'
          }}
        >
          {letter.emoji}
        </div>
        <h3 className="letter-card-title" style={{ color: letter.color }}>
          {letter.title}
        </h3>
        <p className="letter-card-description">
          {letter.description}
        </p>
        <Button
          type="primary"
          className="letter-card-button"
          style={{
            backgroundColor: letter.color,
            borderColor: letter.color,
          }}
        >
          {t('letter.user.buttonText')}
        </Button>
      </div>
    </Card>
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
const LetterModal = ({ open, onClose }) => {
  const { t, language, changeLanguage } = useLanguage()
  const [selectedLetter, setSelectedLetter] = useState(null)
  const [showConfetti, setShowConfetti] = useState(false)

  // 获取信件列表
  const LETTERS = getLetters(t, language)

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
  }

  const handleCloseAll = () => {
    setSelectedLetter(null)
    onClose()
  }

  const handleLanguageChange = (e) => {
    changeLanguage(e.target.value)
  }

  return (
    <>
      <Modal
        open={open && !selectedLetter}
        onCancel={handleCloseAll}
        footer={null}
        width={900}
        className="letter-selection-modal"
        closeIcon={<CloseOutlined />}
      >
        <div className="letter-modal-header">
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

          <h2 className="letter-modal-title">
            <MailOutlined className="letter-modal-icon" />
            {t('letter.modalTitle')}
          </h2>
          <p className="letter-modal-subtitle">
            {t('letter.modalSubtitle')}
          </p>
        </div>

        <div className="letter-cards-container">
          <Space size={24} wrap>
            {LETTERS.map(letter => (
              <LetterCard
                key={letter.key}
                letter={letter}
                onSelect={handleSelectLetter}
                t={t}
              />
            ))}
          </Space>
        </div>

        <div className="letter-modal-footer">
          <Button onClick={handleCloseAll}>
            {t('letter.laterButton')}
          </Button>
        </div>
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

