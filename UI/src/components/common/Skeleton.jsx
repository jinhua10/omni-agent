/**
 * Skeleton 骨架屏组件 (Skeleton Screen Component)
 *
 * 封装 Ant Design Skeleton，提供加载占位符
 * (Wraps Ant Design Skeleton for loading placeholders)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Skeleton as AntSkeleton } from 'antd'
import PropTypes from 'prop-types'

/**
 * Skeleton 组件
 *
 * @param {Object} props - 组件属性
 * @param {boolean} props.loading - 是否显示骨架屏
 * @param {boolean} props.active - 是否显示动画效果
 * @param {boolean} props.avatar - 是否显示头像占位符
 * @param {boolean} props.title - 是否显示标题占位符
 * @param {number} props.paragraph - 段落占位符行数
 * @param {ReactNode} props.children - 实际内容
 * @param {string} props.type - 类型 (default/card/list)
 *
 * @example
 * <Skeleton loading={isLoading}>
 *   <YourContent />
 * </Skeleton>
 */
function Skeleton({
  loading = true,
  active = true,
  avatar = false,
  title = true,
  paragraph = 3,
  children,
  type = 'default',
  ...restProps
}) {
  // 根据类型返回不同的骨架屏 (Return different skeleton based on type)
  const renderSkeleton = () => {
    switch (type) {
      case 'card':
        return (
          <div className="app-skeleton app-skeleton--card">
            <AntSkeleton
              active={active}
              avatar={{ size: 64, shape: 'square' }}
              title={title}
              paragraph={{ rows: paragraph }}
              {...restProps}
            />
          </div>
        )

      case 'list':
        return (
          <div className="app-skeleton app-skeleton--list">
            <AntSkeleton
              active={active}
              avatar={{ size: 40 }}
              title={title}
              paragraph={{ rows: 2 }}
              {...restProps}
            />
          </div>
        )

      default:
        return (
          <div className="app-skeleton">
            <AntSkeleton
              active={active}
              avatar={avatar}
              title={title}
              paragraph={{ rows: paragraph }}
              {...restProps}
            />
          </div>
        )
    }
  }

  if (!loading && children) {
    return children
  }

  return renderSkeleton()
}

Skeleton.propTypes = {
  loading: PropTypes.bool,
  active: PropTypes.bool,
  avatar: PropTypes.bool,
  title: PropTypes.bool,
  paragraph: PropTypes.number,
  children: PropTypes.node,
  type: PropTypes.oneOf(['default', 'card', 'list']),
}

/**
 * Button 骨架屏 (Button skeleton)
 */
Skeleton.Button = ({ active = true, size = 'default', ...props }) => (
  <div className="app-skeleton app-skeleton--button">
    <AntSkeleton.Button active={active} size={size} {...props} />
  </div>
)

/**
 * Image 骨架屏 (Image skeleton)
 */
Skeleton.Image = ({ active = true, ...props }) => (
  <div className="app-skeleton app-skeleton--image">
    <AntSkeleton.Image active={active} {...props} />
  </div>
)

/**
 * Input 骨架屏 (Input skeleton)
 */
Skeleton.Input = ({ active = true, size = 'default', ...props }) => (
  <div className="app-skeleton app-skeleton--input">
    <AntSkeleton.Input active={active} size={size} {...props} />
  </div>
)

export default Skeleton

