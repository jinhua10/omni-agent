/**
 * 反馈 API 模块 (Feedback API Module)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { request } from '../index'

const feedbackApi = {
  /**
   * 提交反馈 (Submit feedback)
   */
  submit(data) {
    return request.post('/feedback', data)
  },

  /**
   * 获取反馈列表 (Get feedback list)
   */
  getList(params) {
    return request.get('/feedback', params)
  },

  /**
   * 获取冲突列表 (Get conflict list)
   */
  getConflicts(params) {
    return request.get('/feedback/conflicts', params)
  },

  /**
   * 投票 (Vote)
   */
  vote(data) {
    return request.post('/feedback/vote', data)
  },

  /**
   * 获取演化历史 (Get evolution history)
   */
  getEvolutionHistory(conceptId) {
    return request.get(`/feedback/evolution/${conceptId}`)
  },

  /**
   * 获取质量监控数据 (Get quality monitoring data)
   */
  getQualityMonitor() {
    return request.get('/feedback/quality-monitor')
  },
}

export default feedbackApi

