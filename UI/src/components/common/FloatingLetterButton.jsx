/**
 * 悬浮信件按钮组件 (Floating Letter Button Component)
 *
 * 显示在屏幕右下角的悬浮按钮，点击可打开信件选择模态框
 * (Floating button in bottom-right corner to open letter selection modal)
 *
 * @author omni-agent team
 * @since 2026-01-01
 */

import React, { useState } from 'react'
import { Button, Tooltip, Badge } from 'antd'
import { MailOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/common/FloatingLetterButton.css'

/**
 * 悬浮信件按钮组件
 */
const FloatingLetterButton = ({ onClick, showBadge = false }) => {
  const { t } = useLanguage()
  const [isHovered, setIsHovered] = useState(false)

  return (
    <div className="floating-letter-button-container">
      <Tooltip
        title={t('letter.floatingButtonTooltip')}
        placement="left"
      >
        <Badge dot={showBadge} offset={[-5, 5]}>
          <Button
            type="primary"
            shape="circle"
            size="large"
            icon={<MailOutlined />}
            onClick={onClick}
            onMouseEnter={() => setIsHovered(true)}
            onMouseLeave={() => setIsHovered(false)}
            className={`floating-letter-button ${isHovered ? 'floating-letter-button-hovered' : ''}`}
            style={{
              width: '56px',
              height: '56px',
              fontSize: '24px',
              boxShadow: '0 4px 12px rgba(24, 144, 255, 0.4)',
            }}
          />
        </Badge>
      </Tooltip>
      {isHovered && (
        <div className="floating-letter-button-label">
          {t('letter.floatingButtonLabel')}
        </div>
      )}
    </div>
  )
}

export default FloatingLetterButton

