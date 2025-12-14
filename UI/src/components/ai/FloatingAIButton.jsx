/**
 * 浮动AI分析按钮 (Floating AI Analysis Button)
 *
 * 固定在屏幕右侧的快速访问按钮
 * (Fixed button on the right side of screen for quick access)
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React, { useState } from 'react'
import { Button, Badge, Tooltip, Dropdown } from 'antd'
import {
  RobotOutlined,
  CloseOutlined,
  ExpandOutlined,
  BorderOutlined,
  ColumnHeightOutlined,
  ReloadOutlined,
} from '@ant-design/icons'
import { useQA } from '../../contexts/QAContext'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/ai/floating-ai-button.css'

// 重置到默认位置的辅助函数
const resetPanelPosition = () => {
  const DEFAULT_CONFIG = {
    x: Math.max(50, window.innerWidth - 500),
    y: 100,
    width: 450,
    height: 600,
    dockPosition: 'none',
    dockSize: 450,
    lastFloatingConfig: null,
  }
  localStorage.setItem('floating_ai_panel_config', JSON.stringify(DEFAULT_CONFIG))
  console.log('🔄 Panel position reset to:', DEFAULT_CONFIG)
  // 不刷新页面，让组件自动重新加载
}

/**
 * 浮动AI分析按钮组件
 */
function FloatingAIButton() {
  const { t } = useLanguage()
  const { showFloatingAI, setShowFloatingAI, aiAnalysisDocs } = useQA()
  const [dropdownVisible, setDropdownVisible] = useState(false)

  const togglePanel = () => {
    console.log('🔘 FloatingAIButton clicked, current showFloatingAI:', showFloatingAI)
    setShowFloatingAI(!showFloatingAI)
    console.log('🔘 FloatingAIButton toggled to:', !showFloatingAI)
  }

  const docCount = aiAnalysisDocs.length

  // 快捷操作菜单
  const menuItems = [
    {
      key: 'open',
      icon: <RobotOutlined />,
      label: '打开面板',
      onClick: () => setShowFloatingAI(true),
    },
    {
      type: 'divider',
    },
    {
      key: 'reset',
      icon: <ReloadOutlined />,
      label: '重置位置',
      onClick: resetPanelPosition,
    },
  ]

  return (
    <div className="floating-ai-button">
      <Dropdown
        menu={{ items: menuItems }}
        trigger={['contextMenu']}
        open={dropdownVisible}
        onOpenChange={setDropdownVisible}
      >
        <div onContextMenu={(e) => e.preventDefault()}>
          <Tooltip
            placement="left"
            title={
              showFloatingAI
                ? 'AI分析面板已打开 (右键更多选项)'
                : docCount > 0
                ? `AI分析面板 (${docCount} 个文档) - 右键更多选项`
                : 'AI分析面板 (右键更多选项)'
            }
          >
            <Badge
              count={docCount}
              offset={[-8, 8]}
              className="floating-ai-button__badge"
            >
              <Button
                type={showFloatingAI ? 'primary' : 'default'}
                shape="circle"
                size="large"
                icon={showFloatingAI ? <CloseOutlined /> : <RobotOutlined />}
                onClick={togglePanel}
                className={`floating-ai-button__btn ${
                  showFloatingAI ? 'floating-ai-button__btn--active' : ''
                } ${docCount > 0 ? 'floating-ai-button__btn--has-docs' : ''}`}
              />
            </Badge>
          </Tooltip>
        </div>
      </Dropdown>

      {/* 脉冲动画提示（有文档但面板未打开时显示） */}
      {docCount > 0 && !showFloatingAI && (
        <div className="floating-ai-button__pulse" />
      )}
    </div>
  )
}

export default FloatingAIButton
