/**
 * HOPE API 模块 (HOPE API Module)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { request } from '../index'

const hopeApi = {
  /**
   * 获取 HOPE 仪表盘数据 (Get HOPE dashboard data)
   */
  getDashboard() {
    return request.get('/hope/dashboard')
  },

  /**
   * 获取三层记忆数据 (Get three-layer memory data)
   */
  getMemoryLayers() {
    return request.get('/hope/memory-layers')
  },

  /**
   * 获取会话列表 (Get session list)
   */
  getSessions(params) {
    return request.get('/hope/sessions', params)
  },

  /**
   * 获取会话详情 (Get session detail)
   */
  getSessionDetail(sessionId) {
    return request.get(`/hope/sessions/${sessionId}`)
  },

  /**
   * 清除缓存 (Clear cache)
   */
  clearCache(layer) {
    return request.post('/hope/clear-cache', { layer })
  },

  /**
   * 获取统计数据 (Get statistics)
   */
  getStatistics() {
    return request.get('/hope/statistics')
  },
}

export default hopeApi

