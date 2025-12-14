/**
 * 协作 API 模块 (Collaboration API Module)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { request } from '../index'

const collaborationApi = {
  /**
   * 获取协作伙伴列表 (Get peer list)
   */
  getPeers(params) {
    return request.get('/collaboration/peers', params)
  },

  /**
   * 生成连接码 (Generate connection code)
   */
  generateCode() {
    return request.post('/collaboration/generate-code')
  },

  /**
   * 使用连接码连接 (Connect with code)
   */
  connect(code) {
    return request.post('/collaboration/connect', { code })
  },

  /**
   * 断开连接 (Disconnect)
   */
  disconnect(peerId) {
    return request.delete(`/collaboration/peers/${peerId}`)
  },

  /**
   * 知识交换 (Exchange knowledge)
   */
  exchange(data) {
    return request.post('/collaboration/exchange', data)
  },

  /**
   * 获取贡献统计 (Get contribution statistics)
   */
  getContribution() {
    return request.get('/collaboration/contribution')
  },

  /**
   * 获取网络拓扑 (Get network graph)
   */
  getNetworkGraph() {
    return request.get('/collaboration/network-graph')
  },

  /**
   * 同步数据 (Sync data with peer)
   */
  syncWith(peerId) {
    return request.post(`/collaboration/peers/${peerId}/sync`)
  },

  /**
   * 获取交换历史 (Get exchange history)
   */
  getExchangeHistory() {
    return request.get('/collaboration/exchange-history')
  },

  /**
   * 获取网络拓扑 (Get topology)
   */
  getTopology() {
    return request.get('/collaboration/topology')
  },

  /**
   * 获取同步状态 (Get sync status)
   */
  getSyncStatus() {
    return request.get('/collaboration/sync-status')
  },
}

export default collaborationApi

