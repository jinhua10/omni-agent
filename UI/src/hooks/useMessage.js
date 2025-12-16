/**
 * 消息通知Hook (Message Notification Hook)
 *
 * 提供统一的消息API，避免antd静态方法的警告
 * (Provides unified message API to avoid antd static method warnings)
 *
 * @author AI Reviewer Team
 * @since 2025-12-17
 */

import { App } from 'antd'

/**
 * 使用消息通知
 * (Use message notification)
 *
 * @returns {Object} message API对象 (message API object)
 */
export function useMessage() {
  const { message } = App.useApp()
  return message
}

export default useMessage

