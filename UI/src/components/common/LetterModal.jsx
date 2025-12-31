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
import { Modal, Card, Button, Space, Radio, Switch } from 'antd'
import {
  UserOutlined,
  CodeOutlined,
  TeamOutlined,
  MailOutlined,
  CloseOutlined,
  GlobalOutlined
} from '@ant-design/icons'
import MarkdownRenderer from './MarkdownRenderer'
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
 * 信件卡片组件 - 现代卡片设计
 */
const LetterCard = ({ letter, onSelect, t, language }) => {
  const [isHovered, setIsHovered] = useState(false)

  // 根据角色类型获取图标和背景
  const getCardStyle = () => {
    switch(letter.key) {
      case 'user':
        return {
          icon: '👤',
          gradient: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
          shadow: '0 20px 60px rgba(102, 126, 234, 0.3)'
        }
      case 'developer':
        return {
          icon: '💻',
          gradient: 'linear-gradient(135deg, #f093fb 0%, #f5576c 100%)',
          shadow: '0 20px 60px rgba(245, 87, 108, 0.3)'
        }
      case 'enterprise':
        return {
          icon: '💼',
          gradient: 'linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)',
          shadow: '0 20px 60px rgba(79, 172, 254, 0.3)'
        }
      default:
        return {
          icon: '📧',
          gradient: 'linear-gradient(135deg, #a8edea 0%, #fed6e3 100%)',
          shadow: '0 20px 60px rgba(168, 237, 234, 0.3)'
        }
    }
  }

  const cardStyle = getCardStyle()

  return (
    <Card
      className={`modern-letter-card ${isHovered ? 'card-hovered' : ''}`}
      onMouseEnter={() => setIsHovered(true)}
      onMouseLeave={() => setIsHovered(false)}
      onClick={() => onSelect(letter)}
      hoverable
      bordered={false}
    >
      {/* 新信徽章 */}
      <div className="card-badge">
        <span className="badge-dot"></span>
        <span className="badge-text">NEW</span>
      </div>

      {/* 卡片图标 */}
      <div
        className="card-icon-wrapper"
        style={{ background: cardStyle.gradient }}
      >
        <div className="card-icon">{cardStyle.icon}</div>
      </div>

      {/* 卡片标题 */}
      <h3 className="card-title">
        {letter.title}
      </h3>

      {/* 卡片描述 */}
      <p className="card-description">
        {letter.description}
      </p>

      {/* 阅读按钮 */}
      <div className="card-action">
        <Button
          type="primary"
          size="large"
          icon={<MailOutlined />}
          className="read-button"
          style={{ background: cardStyle.gradient, border: 'none' }}
        >
          {t('letter.user.buttonText')}
        </Button>
      </div>

      {/* 装饰性元素 */}
      <div className="card-decoration">
        <div className="decoration-circle decoration-1"></div>
        <div className="decoration-circle decoration-2"></div>
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
          <MarkdownRenderer content={content} />
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
  const [autoShowEnabled, setAutoShowEnabled] = useState(true)

  // 获取信件列表
  const LETTERS = getLetters(t, language)

  // 检查是否首次访问和自动显示设置
  useEffect(() => {
    const hasSeenLetter = localStorage.getItem('omni_agent_letter_seen')
    const autoShow = localStorage.getItem('omni_agent_auto_show_letter')
    setIsFirstVisit(!hasSeenLetter)
    setAutoShowEnabled(autoShow !== 'false')
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

  const handleAutoShowChange = (checked) => {
    setAutoShowEnabled(checked)
    localStorage.setItem('omni_agent_auto_show_letter', checked ? 'true' : 'false')
  }

  return (
    <>
      <Modal
        open={open && !selectedLetter}
        onCancel={handleCloseAll}
        footer={null}
        width={1200}
        className="letter-selection-modal modern-letter-modal"
        closeIcon={!isFirstVisit ? <CloseOutlined /> : null}
        maskClosable={!isFirstVisit}
        keyboard={!isFirstVisit}
        centered
      >
        {/* 语言切换按钮 */}
        <div className="modal-language-switcher">
          <GlobalOutlined className="language-icon" />
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

        {/* 头部区域 */}
        <div className="modal-header">
          {/* 欢迎图标 */}
          <div className="welcome-icon">
            <div className="icon-animation">
              <MailOutlined style={{ fontSize: 48 }} />
            </div>
          </div>

          {/* 标题 */}
          <h1 className="modal-title">
            {t('letter.modalTitle')}
          </h1>

          {/* 副标题 */}
          <p className="modal-subtitle">
            {t('letter.modalSubtitle')}
          </p>

          {/* 首次访问提示 */}
          {isFirstVisit && (
            <div className="first-visit-tip">
              <span className="tip-icon">💡</span>
              <span className="tip-text">
                {language === 'zh'
                  ? '请选择一封信件阅读后即可进入系统'
                  : 'Please read one letter to continue'}
              </span>
            </div>
          )}
        </div>

        {/* 卡片区域 */}
        <div className="cards-container">
          <div className="cards-grid">
            {LETTERS.map((letter, index) => (
              <div
                key={letter.key}
                className="card-wrapper"
                style={{ animationDelay: `${index * 0.1}s` }}
              >
                <LetterCard
                  letter={letter}
                  onSelect={handleSelectLetter}
                  t={t}
                  language={language}
                />
              </div>
            ))}
          </div>
        </div>

        {/* 底部提示 */}
        {!isFirstVisit && (
          <div className="modal-footer">
            <Button
              type="text"
              onClick={handleCloseAll}
              className="skip-button"
            >
              {t('letter.laterButton')}
            </Button>
          </div>
        )}

        {/* 自动显示设置（所有用户可见） */}
        <div className="modal-settings">
          <div className="auto-show-setting">
            <Space size={8}>
              <Switch
                checked={autoShowEnabled}
                onChange={handleAutoShowChange}
                size="small"
              />
              <span className="setting-text">
                {language === 'zh'
                  ? '下次自动显示欢迎信'
                  : 'Auto-show welcome letter next time'}
              </span>
            </Space>
          </div>
        </div>

        {/* 庆祝动画 */}
        {showConfetti && (
          <div className="confetti-animation">
            {[...Array(30)].map((_, i) => (
              <div
                key={i}
                className="confetti-piece"
                style={{
                  left: `${Math.random() * 100}%`,
                  animationDelay: `${Math.random() * 0.5}s`,
                  background: ['#667eea', '#764ba2', '#f093fb', '#f5576c', '#4facfe', '#00f2fe'][i % 6]
                }}
              />
            ))}
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

