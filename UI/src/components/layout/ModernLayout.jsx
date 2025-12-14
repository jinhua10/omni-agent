/**
 * 现代化应用布局组件 / Modern App Layout Component
 *
 * 提供侧边栏导航和主内容区的现代化布局
 * Provides modern layout with sidebar navigation and main content area
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useCallback } from 'react';
import { Layout, Menu, Button, Drawer, Dropdown, Modal, Checkbox } from 'antd';
import {
  MenuFoldOutlined,
  MenuUnfoldOutlined,
  MessageOutlined,
  FileOutlined,
  TeamOutlined,
  LikeOutlined,
  ShareAltOutlined,
  StarOutlined,
  RocketOutlined,
  UserOutlined,
  SettingOutlined,
  BulbOutlined,
  GlobalOutlined,
  BgColorsOutlined,
  AppstoreOutlined,
  ClearOutlined,
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { useTheme } from '../../contexts/ThemeContext';
import { useUIThemeEngine } from '../../contexts/UIThemeEngineContext';
import ThemeCustomizer from './ThemeCustomizer';
import { UIThemeSwitcher } from '../theme';
import './modern-layout.css';

const { Header, Sider, Content } = Layout;

/**
 * 现代化布局组件 / Modern Layout Component
 */
function ModernLayout({ children, activeKey, onMenuChange }) {
  const { t, language, toggleLanguage } = useLanguage();
  const { themeName, setTheme, presetThemes } = useTheme();
  const { currentUITheme } = useUIThemeEngine();
  const [collapsed, setCollapsed] = useState(false);
  const [mobileMenuOpen, setMobileMenuOpen] = useState(false);
  const [customizerOpen, setCustomizerOpen] = useState(false);
  const [uiThemeSwitcherOpen, setUiThemeSwitcherOpen] = useState(false);
  const [clearCacheModalOpen, setClearCacheModalOpen] = useState(false);
  
  // 从 localStorage 读取上次的选择 / Load last selection from localStorage
  const [clearOptions, setClearOptions] = useState(() => {
    try {
      const saved = localStorage.getItem('clearCacheOptions');
      if (saved) {
        return JSON.parse(saved);
      }
    } catch (e) {
      console.error('Failed to load clear cache options:', e);
    }
    // 默认值 / Default values
    return {
      floatingPanel: true,
      theme: true,
      uiTheme: true,
      other: true,
    };
  });

  /**
   * 处理复选框变化 / Handle checkbox change
   */
  const handleClearOptionChange = useCallback((key) => (e) => {
    setClearOptions(prev => {
      const newOptions = { ...prev, [key]: e.target.checked };
      // 保存到 localStorage / Save to localStorage
      try {
        localStorage.setItem('clearCacheOptions', JSON.stringify(newOptions));
      } catch (e) {
        console.error('Failed to save clear cache options:', e);
      }
      return newOptions;
    });
  }, []);

  /**
   * 处理清除缓存 / Handle clear cache
   */
  const handleClearCache = useCallback(() => {
    const keysToRemove = [];
    
    if (clearOptions.floatingPanel) {
      keysToRemove.push('floating_ai_panel_config');
    }
    if (clearOptions.theme) {
      keysToRemove.push('selectedTheme');
      keysToRemove.push('customTheme');
    }
    if (clearOptions.uiTheme) {
      keysToRemove.push('selectedUITheme');
      keysToRemove.push('uiThemeConfig');
    }
    
    if (clearOptions.other) {
      // 保存选项设置，不要被清除 / Save options before clearing
      const savedOptions = localStorage.getItem('clearCacheOptions');
      
      // 清除所有缓存
      localStorage.clear();
      sessionStorage.clear();
      
      // 恢复选项设置 / Restore options
      if (savedOptions) {
        localStorage.setItem('clearCacheOptions', savedOptions);
      }
      
      console.log('🧹 All cache cleared (except clear options)');
    } else if (keysToRemove.length > 0) {
      // 只清除选中的项
      keysToRemove.forEach(key => localStorage.removeItem(key));
      console.log('🧹 Selected cache cleared:', keysToRemove);
    }
    
    setClearCacheModalOpen(false);
    
    // 延迟一下刷新，让用户看到模态框关闭
    setTimeout(() => {
      window.location.reload();
    }, 300);
  }, [clearOptions]);

  // 菜单项配置 / Menu items configuration
  const menuItems = [
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
  ];

  // 主题切换菜单 / Theme switch menu
  const themeMenuItems = Object.keys(presetThemes).map(key => ({
    key,
    label: (
      <div style={{ display: 'flex', alignItems: 'center', gap: '8px' }}>
        <div
          style={{
            width: 16,
            height: 16,
            borderRadius: 4,
            background: presetThemes[key].primary,
          }}
        />
        {presetThemes[key].name}
      </div>
    ),
    onClick: () => setTheme(key),
  }));

  return (
    <Layout className="modern-layout">
      {/* 侧边栏 / Sidebar */}
      <Sider
        trigger={null}
        collapsible
        collapsed={collapsed}
        breakpoint="lg"
        collapsedWidth={80}
        width={240}
        className="modern-layout__sider"
        style={{
          background: 'var(--theme-surface)',
          borderRight: '1px solid var(--theme-border)',
        }}
      >
        {/* Logo区域 / Logo area */}
        <div className="modern-layout__logo">
          <span className="modern-layout__logo-icon">🤖</span>
          {!collapsed && (
            <span className="modern-layout__logo-text">AI Reviewer</span>
          )}
        </div>

        {/* 菜单 / Menu */}
        <Menu
          mode="inline"
          selectedKeys={[activeKey]}
          items={menuItems}
          onClick={({ key }) => onMenuChange(key)}
          className="modern-layout__menu"
          style={{
            background: 'var(--theme-surface)',
            color: 'var(--theme-text)',
            borderRight: 'none',
          }}
        />
      </Sider>

      <Layout className="modern-layout__main">
        {/* 顶部导航栏 / Top navigation bar */}
        <Header
          className="modern-layout__header"
          style={{
            background: 'var(--theme-surface)',
            borderBottom: '1px solid var(--theme-border)',
            color: 'var(--theme-text)',
          }}
        >
          <div className="modern-layout__header-left">
            <Button
              type="text"
              icon={collapsed ? <MenuUnfoldOutlined /> : <MenuFoldOutlined />}
              onClick={() => setCollapsed(!collapsed)}
              className="modern-layout__trigger"
            />
          </div>

          <div className="modern-layout__header-right">
            {/* 清除缓存按钮 / Clear cache button */}
            <Button
              type="text"
              icon={<ClearOutlined />}
              onClick={() => setClearCacheModalOpen(true)}
              title={t('common.clearCache')}
            />
            
            {/* UI主题切换器 / UI theme switcher */}
            <Button
              type="text"
              icon={<AppstoreOutlined />}
              onClick={() => setUiThemeSwitcherOpen(true)}
              title={t('uiTheme.switcher.title') || 'UI主题切换器'}
            />

            {/* 颜色主题选择器 / Color theme selector */}
            <Dropdown
              menu={{ items: themeMenuItems }}
              placement="bottomRight"
            >
              <Button
                type="text"
                icon={<BgColorsOutlined />}
                title={t('theme.colorTheme') || '颜色主题'}
              />
            </Dropdown>

            {/* 主题定制器 / Theme customizer */}
            <Button
              type="text"
              icon={<BulbOutlined />}
              onClick={() => setCustomizerOpen(true)}
              title={t('theme.customizer.title') || '主题定制器'}
            />

            {/* 语言切换 / Language toggle */}
            <Button
              type="text"
              icon={<GlobalOutlined />}
              onClick={toggleLanguage}
            >
              {language === 'zh' ? 'EN' : '中文'}
            </Button>
          </div>
        </Header>

        {/* 主内容区 / Main content area */}
        <Content
          className="modern-layout__content"
          style={{
            background: 'var(--theme-background)',
            color: 'var(--theme-text)',
          }}
        >
          {children}
        </Content>
      </Layout>

      {/* 主题定制器抽屉 / Theme customizer drawer */}
      <ThemeCustomizer
        open={customizerOpen}
        onClose={() => setCustomizerOpen(false)}
      />

      {/* UI主题切换器 / UI theme switcher */}
      <UIThemeSwitcher
        open={uiThemeSwitcherOpen}
        onClose={() => setUiThemeSwitcherOpen(false)}
      />

      {/* 清除缓存模态框 / Clear cache modal */}
      <Modal
        title={t('common.clearCacheTitle')}
        open={clearCacheModalOpen}
        onOk={handleClearCache}
        onCancel={() => setClearCacheModalOpen(false)}
        okText={t('common.clearCacheConfirm')}
        cancelText={t('common.cancel')}
        okButtonProps={{ danger: true }}
      >
        <div style={{ marginBottom: 16 }}>
          <p style={{ marginBottom: 12, color: 'var(--theme-text-secondary)' }}>
            {t('common.clearCacheDescription')}
          </p>
          <Checkbox
            checked={clearOptions.floatingPanel}
            onChange={handleClearOptionChange('floatingPanel')}
            style={{ display: 'block', marginBottom: 8 }}
          >
            {t('common.floatingPanelConfig')}
          </Checkbox>
          <Checkbox
            checked={clearOptions.theme}
            onChange={handleClearOptionChange('theme')}
            style={{ display: 'block', marginBottom: 8 }}
          >
            {t('common.themeSettings')}
          </Checkbox>
          <Checkbox
            checked={clearOptions.uiTheme}
            onChange={handleClearOptionChange('uiTheme')}
            style={{ display: 'block', marginBottom: 8 }}
          >
            {t('common.uiThemeConfig')}
          </Checkbox>
          <Checkbox
            checked={clearOptions.other}
            onChange={handleClearOptionChange('other')}
            style={{ display: 'block', marginBottom: 8 }}
          >
            {t('common.otherCacheData')}
          </Checkbox>
        </div>
        <p style={{ color: 'var(--theme-text-secondary)', fontSize: 12, margin: 0 }}>
          {t('common.clearCacheWarning')}
        </p>
      </Modal>
    </Layout>
  );
}

export default ModernLayout;

