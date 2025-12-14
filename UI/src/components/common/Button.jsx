/**
 * Button 按钮组件 (Button Component)
 *
 * 封装 Ant Design Button，提供统一的按钮样式
 * (Wraps Ant Design Button for consistent button styles)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Button as AntButton } from 'antd'
import PropTypes from 'prop-types'

/**
 * Button 组件
 *
 * @param {Object} props - 组件属性
 * @param {string} props.type - 按钮类型
 * @param {string} props.size - 按钮大小
 * @param {boolean} props.loading - 加载状态
 * @param {boolean} props.disabled - 禁用状态
 * @param {ReactNode} props.icon - 图标
 * @param {ReactNode} props.children - 按钮文本
 * @param {Function} props.onClick - 点击回调
 *
 * @example
 * <Button type="primary" onClick={handleClick}>
 *   确认
 * </Button>
 */
function Button({
  type = 'default',
  size = 'middle',
  loading = false,
  disabled = false,
  icon,
  children,
  onClick,
  className = '',
  ...restProps
}) {
  return (
    <AntButton
      type={type}
      size={size}
      loading={loading}
      disabled={disabled}
      icon={icon}
      onClick={onClick}
      className={`app-button ${className}`}
      {...restProps}
    >
      {children}
    </AntButton>
  )
}

Button.propTypes = {
  type: PropTypes.oneOf(['primary', 'default', 'dashed', 'text', 'link']),
  size: PropTypes.oneOf(['large', 'middle', 'small']),
  loading: PropTypes.bool,
  disabled: PropTypes.bool,
  icon: PropTypes.node,
  children: PropTypes.node,
  onClick: PropTypes.func,
  className: PropTypes.string,
}

export default Button

