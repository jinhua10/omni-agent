/**
 * 梦幻气泡头部 / Dreamy Bubble Header
 * 
 * 沉浸式玻璃态头部栏
 * Immersive Glassmorphic Header Bar
 */

import React, { useState } from 'react';
import { 
  SearchOutlined, 
  BellOutlined, 
  UserOutlined,
  GlobalOutlined,
  SkinOutlined
} from '@ant-design/icons';
import { Badge, Dropdown, Avatar } from 'antd';
import { useLanguage } from '../../../contexts/LanguageContext';
import { useUIThemeEngine, UI_THEMES } from '../../../contexts/UIThemeEngineContext';

/**
 * 梦幻气泡头部组件
 */
function BubbleHeader({ scrollProgress }) {
  const { t, currentLanguage, changeLanguage } = useLanguage();
  const { currentUITheme, switchUITheme } = useUIThemeEngine();
  const [searchFocused, setSearchFocused] = useState(false);

  // 语言选项 / Language options - Labels in their own languages (no i18n needed)
  const languageItems = [
    {
      key: 'zh',
      label: '简体中文',  // Chinese label should be in Chinese
      onClick: () => changeLanguage('zh')
    },
    {
      key: 'en',
      label: 'English',  // English label should be in English
      onClick: () => changeLanguage('en')
    }
  ];

  // 主题选项 / Theme options - Using theme names from UI_THEMES
  const themeItems = Object.entries(UI_THEMES).map(([key, theme]) => ({
    key: key,
    label: theme.name[currentLanguage] || theme.name.zh,
    onClick: () => switchUITheme(key)
  }));

  // 用户菜单 / User menu - Using i18n
  const userMenuItems = [
    {
      key: 'profile',
      label: t('user.menu.profile')
    },
    {
      key: 'settings',
      label: t('user.menu.accountSettings')
    },
    {
      type: 'divider'
    },
    {
      key: 'logout',
      label: t('user.menu.logout'),
      danger: true
    }
  ];

  return (
    <header 
      className="bubble-header"
      style={{
        '--header-opacity': Math.min(scrollProgress * 2, 1)
      }}
    >
      {/* 玻璃背景层 */}
      <div className="header-glass-backdrop"></div>
      
      {/* 内容容器 */}
      <div className="header-content">
        {/* Logo区域 */}
        <div className="header-logo-section">
          <div className="logo-orb">
            <div className="logo-glow"></div>
            <div className="logo-inner">
              <span className="logo-icon">🫧</span>
            </div>
          </div>
          <h1 className="logo-text">
            <span className="text-gradient">Omni Agent</span>
          </h1>
        </div>

        {/* 搜索栏 */}
        <div className={`header-search-section ${searchFocused ? 'focused' : ''}`}>
          <div className="search-orb-container">
            <div className="search-glass-shell">
              <SearchOutlined className="search-icon" />
              <input
                type="text"
                className="search-input"
                placeholder={t('common.search') + '...'}
                onFocus={() => setSearchFocused(true)}
                onBlur={() => setSearchFocused(false)}
              />
              <div className="search-glow-effect"></div>
            </div>
            {searchFocused && (
              <div className="search-focus-ring"></div>
            )}
          </div>
        </div>

        {/* 操作区域 */}
        <div className="header-actions-section">
          {/* 通知气泡 */}
          <div className="action-orb notification-orb">
            <Badge count={5} size="small" offset={[-2, 2]}>
              <div className="orb-button">
                <div className="orb-glass"></div>
                <BellOutlined className="orb-icon" />
                <div className="orb-pulse"></div>
              </div>
            </Badge>
          </div>

          {/* 语言切换气泡 */}
          <Dropdown menu={{ items: languageItems }} placement="bottomRight">
            <div className="action-orb language-orb">
              <div className="orb-button">
                <div className="orb-glass"></div>
                <GlobalOutlined className="orb-icon" />
                <span className="orb-label">{currentLanguage?.toUpperCase()}</span>
              </div>
            </div>
          </Dropdown>

          {/* 主题切换气泡 - 突出显示 / Theme switch bubble - highlighted */}
          <Dropdown menu={{ items: themeItems }} placement="bottomRight" trigger={['click']}>
            <div className="action-orb theme-orb theme-switch-highlight">
              <div className="orb-button">
                <div className="orb-glass"></div>
                <SkinOutlined className="orb-icon" />
                <span className="orb-label">{t('theme.colorTheme')}</span>
              </div>
              <div className="theme-indicator-pulse"></div>
            </div>
          </Dropdown>

          {/* 用户头像气泡 */}
          <Dropdown menu={{ items: userMenuItems }} placement="bottomRight">
            <div className="action-orb user-orb">
              <div className="orb-button">
                <div className="orb-glass"></div>
                <Avatar 
                  size={32} 
                  icon={<UserOutlined />}
                  className="user-avatar"
                />
                <div className="user-status-dot"></div>
              </div>
            </div>
          </Dropdown>
        </div>
      </div>

      {/* 装饰性元素 */}
      <div className="header-decorations">
        <div className="deco-particle p-1"></div>
        <div className="deco-particle p-2"></div>
        <div className="deco-particle p-3"></div>
        <div className="deco-wave wave-1"></div>
        <div className="deco-wave wave-2"></div>
      </div>

      {/* 底部光线 */}
      <div className="header-bottom-glow"></div>
    </header>
  );
}

export default BubbleHeader;
