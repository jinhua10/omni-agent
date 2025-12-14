/**
 * 角色 API 模块 (Role API Module)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { request } from '../index'

const roleApi = {
  /**
   * 获取角色列表 (Get role list)
   */
  getList(params) {
    return request.get('/roles', params)
  },

  /**
   * 获取角色详情 (Get role detail)
   */
  getDetail(id) {
    return request.get(`/roles/${id}`)
  },

  /**
   * 创建角色 (Create role)
   */
  create(data) {
    return request.post('/roles', data)
  },

  /**
   * 更新角色 (Update role)
   */
  update(id, data) {
    return request.put(`/roles/${id}`, data)
  },

  /**
   * 删除角色 (Delete role)
   */
  delete(id) {
    return request.delete(`/roles/${id}`)
  },

  /**
   * 检测问题所属角色 (Detect question role)
   */
  detect(question) {
    return request.post('/roles/detect', { question })
  },

  /**
   * 获取角色统计 (Get role statistics)
   */
  getStatistics() {
    return request.get('/roles/statistics')
  },
}

export default roleApi

