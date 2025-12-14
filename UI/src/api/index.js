/**
 * Axios 实例配置 (Axios Instance Configuration)
 *
 * 提供统一的 HTTP 请求配置、拦截器、错误处理
 * (Provides unified HTTP request configuration, interceptors, error handling)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import axios from 'axios'
import { Toast } from '@components/common'
import { mockRequest, ENABLE_MOCK } from './mock'

// API 基础路径 (API base URL)
const BASE_URL = import.meta.env.VITE_API_BASE_URL || '/api'

// 请求超时时间（毫秒）(Request timeout in milliseconds)
const TIMEOUT = 30000

// 是否启用自动降级到 Mock（后端不可用时）
const AUTO_FALLBACK_TO_MOCK = true

/**
 * 创建 Axios 实例 (Create Axios instance)
 */
const axiosInstance = axios.create({
  baseURL: BASE_URL,
  timeout: TIMEOUT,
  headers: {
    'Content-Type': 'application/json',
  },
})

/**
 * 请求拦截器 (Request interceptor)
 * 在请求发送前进行处理
 */
axiosInstance.interceptors.request.use(
  (config) => {
    // 添加 Token（如果存在）(Add token if exists)
    const token = localStorage.getItem('token')
    if (token) {
      config.headers.Authorization = `Bearer ${token}`
    }

    // 添加语言标识 (Add language identifier)
    const language = localStorage.getItem('language') || 'zh'
    config.headers['Accept-Language'] = language

    // 添加请求时间戳（用于调试）(Add timestamp for debugging)
    config.metadata = { startTime: new Date() }

    console.log(`📤 Request: ${config.method?.toUpperCase()} ${config.url}`)
    return config
  },
  (error) => {
    console.error('❌ Request error:', error)
    return Promise.reject(error)
  }
)

/**
 * 响应拦截器 (Response interceptor)
 * 在收到响应后进行处理
 */
axiosInstance.interceptors.response.use(
  (response) => {
    // 计算请求耗时 (Calculate request duration)
    const duration = new Date() - response.config.metadata.startTime
    console.log(`✅ Response: ${response.config.url} (${duration}ms)`)

    // 返回数据 (Return data)
    return response.data
  },
  async (error) => {
    // 处理错误响应 (Handle error response)
    console.error('❌ Response error:', error)

    // 如果后端不可用且启用了自动降级，尝试使用 Mock 数据
    if (AUTO_FALLBACK_TO_MOCK && (!error.response || error.code === 'ERR_NETWORK')) {
      console.warn('⚠️ Backend unavailable, falling back to mock data')
      const mockResponse = await mockRequest(
        error.config.url,
        error.config.method?.toUpperCase(),
        error.config.data
      )
      if (mockResponse) {
        console.log('✅ Using mock data:', error.config.url)
        return mockResponse
      }
    }

    // 获取错误信息 (Get error message)
    const message = getErrorMessage(error)

    // 显示错误提示 (Show error toast)
    // Toast.error(message) // 暂时注释，避免在使用 Mock 数据时显示错误

    // 特殊错误处理 (Special error handling)
    if (error.response) {
      const { status } = error.response

      switch (status) {
        case 401:
          // 未授权，清除 Token 并跳转到登录页 (Unauthorized)
          localStorage.removeItem('token')
          window.location.href = '/login'
          break

        case 403:
          // 无权限 (Forbidden)
          console.warn('⚠️ Access forbidden')
          break

        case 404:
          // 资源不存在 (Not found)
          console.warn('⚠️ Resource not found')
          break

        case 500:
          // 服务器错误 (Server error)
          console.error('🔥 Server error')
          break

        default:
          break
      }
    }

    return Promise.reject(error)
  }
)

/**
 * 获取错误消息 (Get error message)
 * @param {Error} error - 错误对象
 * @returns {string} 错误消息
 */
function getErrorMessage(error) {
  if (error.response) {
    // 服务器返回的错误 (Server returned error)
    return error.response.data?.message || error.response.statusText || '请求失败'
  } else if (error.request) {
    // 请求已发送但没有收到响应 (Request sent but no response)
    return '网络错误，请检查连接'
  } else {
    // 请求配置错误 (Request configuration error)
    return error.message || '未知错误'
  }
}

/**
 * 导出 Axios 实例 (Export Axios instance)
 */
export default axiosInstance

/**
 * 导出请求方法 (Export request methods)
 */
export const request = {
  /**
   * GET 请求 (GET request)
   */
  get: (url, params, config) => {
    return axiosInstance.get(url, { params, ...config })
  },

  /**
   * POST 请求 (POST request)
   */
  post: (url, data, config) => {
    return axiosInstance.post(url, data, config)
  },

  /**
   * PUT 请求 (PUT request)
   */
  put: (url, data, config) => {
    return axiosInstance.put(url, data, config)
  },

  /**
   * DELETE 请求 (DELETE request)
   */
  delete: (url, config) => {
    return axiosInstance.delete(url, config)
  },

  /**
   * PATCH 请求 (PATCH request)
   */
  patch: (url, data, config) => {
    return axiosInstance.patch(url, data, config)
  },
}

