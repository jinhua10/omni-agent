/**
 * API 模块统一导出 (API Modules Export)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import documentApi from './modules/document'
import qaApi from './modules/qa'
import roleApi from './modules/role'
import feedbackApi from './modules/feedback'
import hopeApi from './modules/hope'
import wishApi from './modules/wish'
import collaborationApi from './modules/collaboration'
import adminApi from './modules/admin'

/**
 * 导出所有 API 模块 (Export all API modules)
 */
export default {
  document: documentApi,
  qa: qaApi,
  role: roleApi,
  feedback: feedbackApi,
  hope: hopeApi,
  wish: wishApi,
  collaboration: collaborationApi,
  admin: adminApi,
}

/**
 * 也可以单独导出 (Also export individually)
 */
export {
  documentApi,
  qaApi,
  roleApi,
  feedbackApi,
  hopeApi,
  wishApi,
  collaborationApi,
  adminApi,
}

