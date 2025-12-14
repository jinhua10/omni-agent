/**
 * ErrorBoundary 错误边界组件 (Error Boundary Component)
 *
 * 捕获子组件的错误，防止整个应用崩溃
 * (Catches errors in child components to prevent app crash)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Button, Result } from 'antd'
import PropTypes from 'prop-types'

/**
 * ErrorBoundary 组件
 *
 * @example
 * <ErrorBoundary>
 *   <YourComponent />
 * </ErrorBoundary>
 */
class ErrorBoundary extends React.Component {
  constructor(props) {
    super(props)
    this.state = {
      hasError: false,
      error: null,
      errorInfo: null,
    }
  }

  /**
   * 静态方法：当错误发生时更新状态
   * (Static method: Update state when error occurs)
   */
  static getDerivedStateFromError(error) {
    return {
      hasError: true,
      error,
    }
  }

  /**
   * 组件捕获错误时调用
   * (Called when component catches error)
   */
  componentDidCatch(error, errorInfo) {
    // 记录错误信息 (Log error info)
    console.error('ErrorBoundary caught an error:', error, errorInfo)

    this.setState({
      error,
      errorInfo,
    })

    // 可以将错误发送到错误监控服务 (Can send error to monitoring service)
    // Example: logErrorToService(error, errorInfo)
  }

  /**
   * 重置错误状态 (Reset error state)
   */
  handleReset = () => {
    this.setState({
      hasError: false,
      error: null,
      errorInfo: null,
    })
  }

  /**
   * 刷新页面 (Reload page)
   */
  handleReload = () => {
    window.location.reload()
  }

  render() {
    const { hasError, error } = this.state
    const { children, fallback } = this.props

    // 如果有错误，显示错误界面 (If error exists, show error UI)
    if (hasError) {
      // 自定义错误界面 (Custom error UI)
      if (fallback) {
        return typeof fallback === 'function'
          ? fallback(error, this.handleReset)
          : fallback
      }

      // 默认错误界面 (Default error UI)
      return (
        <div className="error-boundary">
          <Result
            status="error"
            title="Oops! Something went wrong"
            subTitle="抱歉，页面出现了错误 / Sorry, something went wrong with the page"
            extra={[
              <Button type="primary" key="reset" onClick={this.handleReset}>
                Try Again / 重试
              </Button>,
              <Button key="reload" onClick={this.handleReload}>
                Reload Page / 刷新页面
              </Button>,
            ]}
          >
            {process.env.NODE_ENV === 'development' && error && (
              <div className="error-boundary__details">
                <h4>Error Details (Development Only):</h4>
                <pre>{error.toString()}</pre>
              </div>
            )}
          </Result>
        </div>
      )
    }

    // 正常渲染子组件 (Render children normally)
    return children
  }
}

ErrorBoundary.propTypes = {
  children: PropTypes.node.isRequired,
  fallback: PropTypes.oneOfType([
    PropTypes.node,
    PropTypes.func,
  ]),
}

export default ErrorBoundary

