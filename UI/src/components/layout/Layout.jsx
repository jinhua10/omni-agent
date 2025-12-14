/**
 * Layout 布局组件 (Layout Component)
 *
 * 提供应用的基础布局结构，包含 Header、Content、Footer
 * (Provides basic layout structure with Header, Content, Footer)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Layout as AntLayout } from 'antd'
import PropTypes from 'prop-types'

const { Content } = AntLayout

/**
 * Layout 组件
 *
 * @param {Object} props - 组件属性
 * @param {ReactNode} props.children - 子组件
 * @param {ReactNode} props.header - 头部内容
 * @param {ReactNode} props.footer - 底部内容
 * @param {string} props.className - 自定义类名
 *
 * @example
 * <Layout
 *   header={<Header />}
 *   footer={<Footer />}
 * >
 *   <YourContent />
 * </Layout>
 */
function Layout({ children, header, footer, className = '' }) {
  return (
    <AntLayout className={`app-layout ${className}`}>
      {/* 头部 (Header) */}
      {header && (
        <div className="app-layout__header">
          {header}
        </div>
      )}

      {/* 主内容区 (Main content) */}
      <Content className="app-layout__content">
        {children}
      </Content>

      {/* 底部 (Footer) */}
      {footer && (
        <div className="app-layout__footer">
          {footer}
        </div>
      )}
    </AntLayout>
  )
}

Layout.propTypes = {
  children: PropTypes.node.isRequired,
  header: PropTypes.node,
  footer: PropTypes.node,
  className: PropTypes.string,
}

export default Layout

