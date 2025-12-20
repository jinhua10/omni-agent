/**
 * API 模块统一导出 (API Modules Export)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import documentApi from './document'
import qaApi from './qa'
import roleApi from './role'
import feedbackApi from './feedback'
import hopeApi from './hope'
import wishApi from './wish'
import collaborationApi from './collaboration'
import adminApi from './admin'
import workflowApi from './workflow'

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
  workflow: workflowApi,
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
  workflowApi,
}

