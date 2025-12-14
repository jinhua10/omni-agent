/**
 * Loading 加载组件 (Loading Component)
 *
 * 提供多种样式的加载动画
 * (Provides various loading animations)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Spin } from 'antd'
import { LoadingOutlined } from '@ant-design/icons'
import { useLanguage } from '@contexts/LanguageContext'
import PropTypes from 'prop-types'

/**
 * Loading 组件
 *
 * @param {Object} props - 组件属性
 * @param {boolean} props.spinning - 是否显示加载状态
 * @param {string} props.tip - 加载提示文字
 * @param {string} props.size - 大小 (small/default/large)
 * @param {boolean} props.fullscreen - 是否全屏显示
 * @param {ReactNode} props.children - 子组件
 *
 * @example
 * // 简单加载
 * <Loading spinning={true} />
 *
 * // 全屏加载
 * <Loading spinning={true} fullscreen={true} />
 *
 * // 包裹内容
 * <Loading spinning={isLoading}>
 *   <YourContent />
 * </Loading>
 */
function Loading({
  spinning = true,
  tip,
  size = 'default',
  fullscreen = false,
  children,
}) {
  const { t } = useLanguage()

  // 默认提示文字 (Default tip text)
  const defaultTip = tip || t('common.loading')

  // 自定义加载图标 (Custom loading icon)
  const loadingIcon = <LoadingOutlined style={{ fontSize: getSizeValue(size) }} spin />

  // 全屏加载 (Fullscreen loading)
  if (fullscreen) {
    return spinning ? (
      <div className="app-loading--fullscreen">
        <Spin
          indicator={loadingIcon}
          tip={defaultTip}
          size={size}
        />
      </div>
    ) : null
  }

  // 包裹式加载 (Wrapper loading)
  if (children) {
    return (
      <Spin
        spinning={spinning}
        indicator={loadingIcon}
        tip={defaultTip}
        size={size}
        className="app-loading--wrapper"
      >
        {children}
      </Spin>
    )
  }

  // 简单加载 (Simple loading)
  // 注意：tip 只能在 nest 或 fullscreen 模式下使用
  return (
    <div className="app-loading">
      <Spin
        indicator={loadingIcon}
        size={size}
      />
      {defaultTip && <div className="app-loading__tip">{defaultTip}</div>}
    </div>
  )
}

/**
 * 获取大小对应的数值 (Get size value)
 * @param {string} size - 大小
 * @returns {number} 数值
 */
function getSizeValue(size) {
  const sizeMap = {
    small: 16,
    default: 24,
    large: 32,
  }
  return sizeMap[size] || 24
}

Loading.propTypes = {
  spinning: PropTypes.bool,
  tip: PropTypes.string,
  size: PropTypes.oneOf(['small', 'default', 'large']),
  fullscreen: PropTypes.bool,
  children: PropTypes.node,
}

export default Loading

