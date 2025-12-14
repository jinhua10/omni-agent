/**
 * 文档 API 模块 (Document API Module)
 *
 * 提供文档相关的 API 接口
 * (Provides document-related API interfaces)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { request } from '../index'

const documentApi = {
  /**
   * 获取文档列表 (Get document list)
   * @param {Object} params - 查询参数
   * @param {number} params.page - 页码
   * @param {number} params.pageSize - 每页条数
   * @param {string} params.keyword - 关键词
   * @returns {Promise} 文档列表
   */
  getList(params) {
    return request.get('/documents/list', params)
  },

  /**
   * 获取文档详情 (Get document detail)
   * @param {string} id - 文档 ID
   * @returns {Promise} 文档详情
   */
  getDetail(id) {
    return request.get(`/documents/${id}`)
  },

  /**
   * 上传文档 (Upload document)
   * @param {FormData} formData - 表单数据
   * @param {Function} onProgress - 上传进度回调
   * @returns {Promise} 上传结果
   */
  upload(formData, onProgress) {
    return request.post('/documents/upload', formData, {
      headers: {
        'Content-Type': 'multipart/form-data',
      },
      onUploadProgress: (progressEvent) => {
        if (onProgress) {
          const percentCompleted = Math.round(
            (progressEvent.loaded * 100) / progressEvent.total
          )
          onProgress(percentCompleted)
        }
      },
    })
  },

  /**
   * 批量上传文档 (Batch upload documents)
   * @param {FormData} formData - 表单数据（包含多个文件）
   * @param {Function} onProgress - 上传进度回调
   * @returns {Promise} 上传结果
   */
  batchUpload(formData, onProgress) {
    return request.post('/documents/upload-batch', formData, {
      headers: {
        'Content-Type': 'multipart/form-data',
      },
      onUploadProgress: (progressEvent) => {
        if (onProgress) {
          const percentCompleted = Math.round(
            (progressEvent.loaded * 100) / progressEvent.total
          )
          onProgress(percentCompleted)
        }
      },
    })
  },

  /**
   * 删除文档 (Delete document)
   * @param {string} fileName - 文档文件名
   * @returns {Promise} 删除结果
   */
  delete(fileName) {
    return request.delete(`/documents/${encodeURIComponent(fileName)}`)
  },

  /**
   * 批量删除文档 (Batch delete documents)
   * @param {Array<string>} fileNames - 文档文件名列表
   * @returns {Promise} 删除结果
   */
  batchDelete(fileNames) {
    return request.delete('/documents/batch', { 
      data: { fileNames }
    })
  },

  /**
   * 下载文档 (Download document)
   * @param {string} fileName - 文档文件名
   * @returns {Promise} 文件 Blob
   */
  download(fileName) {
    return request.get('/documents/download', { fileName }, {
      responseType: 'blob',
    })
  },

  /**
   * 搜索文档 (Search documents)
   * @param {string} keyword - 搜索关键词
   * @param {Object} filters - 过滤条件
   * @returns {Promise} 搜索结果
   */
  search(keyword, filters = {}) {
    return request.post('/documents/search', {
      keyword,
      ...filters,
    })
  },

  /**
   * 获取文档统计信息 (Get document statistics)
   * @returns {Promise} 统计信息
   */
  getStatistics() {
    return request.get('/documents/statistics')
  },
}

export default documentApi

