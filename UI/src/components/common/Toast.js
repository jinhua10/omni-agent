/**
 * Toast 消息提示组件 (Toast Message Component)
 *
 * 提供全局消息提示功能
 * (Provides global message notification)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { message } from 'antd'

/**
 * Toast 工具类 (Toast utility class)
 * 封装 Ant Design message API
 */
const Toast = {
  /**
   * 成功提示 (Success message)
   * @param {string} content - 提示内容
   * @param {number} duration - 持续时间（秒）
   */
  success(content, duration = 3) {
    message.success({
      content,
      duration,
      className: 'app-toast app-toast--success',
    })
  },

  /**
   * 错误提示 (Error message)
   * @param {string} content - 提示内容
   * @param {number} duration - 持续时间（秒）
   */
  error(content, duration = 3) {
    message.error({
      content,
      duration,
      className: 'app-toast app-toast--error',
    })
  },

  /**
   * 警告提示 (Warning message)
   * @param {string} content - 提示内容
   * @param {number} duration - 持续时间（秒）
   */
  warning(content, duration = 3) {
    message.warning({
      content,
      duration,
      className: 'app-toast app-toast--warning',
    })
  },

  /**
   * 信息提示 (Info message)
   * @param {string} content - 提示内容
   * @param {number} duration - 持续时间（秒）
   */
  info(content, duration = 3) {
    message.info({
      content,
      duration,
      className: 'app-toast app-toast--info',
    })
  },

  /**
   * 加载提示 (Loading message)
   * @param {string} content - 提示内容
   * @param {number} duration - 持续时间（秒，0 表示不自动关闭）
   * @returns {Function} 关闭函数
   */
  loading(content, duration = 0) {
    return message.loading({
      content,
      duration,
      className: 'app-toast app-toast--loading',
    })
  },

  /**
   * 销毁所有提示 (Destroy all messages)
   */
  destroy() {
    message.destroy()
  },

  /**
   * 配置全局参数 (Configure global settings)
   * @param {Object} config - 配置对象
   */
  config(config) {
    message.config({
      top: 80,
      duration: 3,
      maxCount: 3,
      ...config,
    })
  },
}

export default Toast

