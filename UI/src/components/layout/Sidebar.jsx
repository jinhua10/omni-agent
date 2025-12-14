/**
 * Sidebar 侧边栏组件 (Sidebar Component)
 *
 * 提供可折叠的侧边导航栏
 * (Provides collapsible side navigation)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { Layout, Menu } from 'antd'
import {
  HomeOutlined,
  MessageOutlined,
  FileOutlined,
  TeamOutlined,
  SettingOutlined,
  MenuFoldOutlined,
  MenuUnfoldOutlined,
} from '@ant-design/icons'
import { useLanguage } from '@contexts/LanguageContext'
import PropTypes from 'prop-types'

const { Sider } = Layout

/**
 * Sidebar 组件
 *
 * @param {Object} props - 组件属性
 * @param {string} props.activeKey - 当前激活的菜单项
 * @param {Function} props.onMenuClick - 菜单点击回调
 * @param {boolean} props.defaultCollapsed - 默认是否折叠
 *
 * @example
 * <Sidebar
 *   activeKey="qa"
 *   onMenuClick={handleMenuClick}
 * />
 */
function Sidebar({
  activeKey = 'home',
  onMenuClick,
  defaultCollapsed = false,
}) {
  const { t } = useLanguage()
  const [collapsed, setCollapsed] = useState(defaultCollapsed)

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
      type: 'divider',
    },
    {
      key: 'settings',
      icon: <SettingOutlined />,
      label: t('nav.settings'),
    },
  ]

  return (
    <Sider
      collapsible
      collapsed={collapsed}
      onCollapse={setCollapsed}
      trigger={null}
      className="app-sidebar"
      width={220}
    >
      {/* 折叠按钮 (Collapse button) */}
      <div className="app-sidebar__trigger" onClick={() => setCollapsed(!collapsed)}>
        {collapsed ? <MenuUnfoldOutlined /> : <MenuFoldOutlined />}
      </div>

      {/* 菜单 (Menu) */}
      <Menu
        mode="inline"
        selectedKeys={[activeKey]}
        items={menuItems}
        onClick={({ key }) => onMenuClick?.(key)}
        className="app-sidebar__menu"
      />
    </Sider>
  )
}

Sidebar.propTypes = {
  activeKey: PropTypes.string,
  onMenuClick: PropTypes.func,
  defaultCollapsed: PropTypes.bool,
}

export default Sidebar

