/**
 * Header 导航栏组件 (Header Navigation Component)
 *
 * 提供应用顶部导航栏
 * (Provides application top navigation bar)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Menu, Button } from 'antd'
import {
  HomeOutlined,
  MessageOutlined,
  FileOutlined,
  TeamOutlined,
  GlobalOutlined,
  LikeOutlined,
  ShareAltOutlined,
  StarOutlined,
  RocketOutlined,
  UserOutlined,
  SettingOutlined,
  BulbOutlined
} from '@ant-design/icons'
import { useLanguage } from '@contexts/LanguageContext'
import { useTheme } from '@contexts/ThemeContext'
import PropTypes from 'prop-types'

/**
 * Header 组件
 *
 * @param {Object} props - 组件属性
 * @param {string} props.activeKey - 当前激活的菜单项
 * @param {Function} props.onMenuClick - 菜单点击回调
 * @param {boolean} props.showLanguageToggle - 是否显示语言切换按钮
 *
 * @example
 * <Header
 *   activeKey="qa"
 *   onMenuClick={(key) => console.log(key)}
 *   showLanguageToggle={true}
 * />
 */
function Header({
  activeKey = 'home',
  onMenuClick,
  showLanguageToggle = true
}) {
  const { t, language, toggleLanguage } = useLanguage()
  const { theme, toggleTheme } = useTheme()

  // 菜单项配置 (Menu items configuration)
  const menuItems = [
    {
      key: 'home',
      icon: <HomeOutlined />,
      label: t('nav.home'),
    },
    {
      key: 'qa',
      icon: <MessageOutlined />,
      label: t('nav.qa'),
    },
    {
      key: 'documents',
      icon: <FileOutlined />,
      label: t('nav.documents'),
    },
    {
      key: 'roles',
      icon: <TeamOutlined />,
      label: t('nav.roles'),
    },
    {
      key: 'feedback',
      icon: <LikeOutlined />,
      label: t('nav.feedback'),
    },
    {
      key: 'collaboration',
      icon: <ShareAltOutlined />,
      label: t('nav.collaboration'),
    },
    {
      key: 'wish',
      icon: <StarOutlined />,
      label: t('nav.wish'),
    },
    {
      key: 'aiService',
      icon: <RocketOutlined />,
      label: t('nav.aiService'),
    },
    {
      key: 'profile',
      icon: <UserOutlined />,
      label: t('nav.profile'),
    },
    {
      key: 'admin',
      icon: <SettingOutlined />,
      label: t('nav.admin'),
    },
  ]

  return (
    <header className="app-header">
      <div className="app-header__container">
        {/* Logo 和标题 (Logo and title) */}
        <div className="app-header__logo">
          <span className="app-header__logo-icon">🤖</span>
          <span className="app-header__logo-text">Omni Agent</span>
        </div>

        {/* 导航菜单 (Navigation menu) */}
        <Menu
          mode="horizontal"
          selectedKeys={[activeKey]}
          items={menuItems}
          onClick={({ key }) => onMenuClick?.(key)}
          className="app-header__menu"
        />

        {/* 右侧操作区 (Right actions) */}
        <div className="app-header__actions">
          <Button
            icon={<BulbOutlined />}
            onClick={toggleTheme}
            className="app-header__theme-btn"
            title={theme === 'light' ? t('common.switchToDark') : t('common.switchToLight')}
          >
            {theme === 'light' ? '🌙' : '☀️'}
          </Button>
          {showLanguageToggle && (
            <Button
              icon={<GlobalOutlined />}
              onClick={toggleLanguage}
              className="app-header__language-btn"
            >
              {language === 'zh' ? 'EN' : '中文'}
            </Button>
          )}
        </div>
      </div>
    </header>
  )
}

Header.propTypes = {
  activeKey: PropTypes.string,
  onMenuClick: PropTypes.func,
  showLanguageToggle: PropTypes.bool,
}

export default Header

