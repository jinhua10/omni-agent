/**
 * useApi Hook
 *
 * 封装 API 请求，提供加载状态、错误处理等功能
 * (Wraps API requests with loading state, error handling, etc.)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { useState, useCallback } from 'react'

/**
 * useApi Hook
 *
 * @param {Function} apiFunc - API 函数
 * @param {Object} options - 配置选项
 * @param {boolean} options.immediate - 是否立即执行
 * @param {Function} options.onSuccess - 成功回调
 * @param {Function} options.onError - 错误回调
 *
 * @returns {Object} API 状态和方法
 *
 * @example
 * const { data, loading, error, execute } = useApi(api.document.getList)
 *
 * // 执行请求
 * await execute({ page: 1, pageSize: 10 })
 */
function useApi(apiFunc, options = {}) {
  const { immediate = false, onSuccess, onError } = options

  // 状态管理 (State management)
  const [data, setData] = useState(null)
  const [loading, setLoading] = useState(immediate)
  const [error, setError] = useState(null)

  /**
   * 执行 API 请求 (Execute API request)
   */
  const execute = useCallback(
    async (...args) => {
      try {
        setLoading(true)
        setError(null)

        const result = await apiFunc(...args)
        setData(result)

        // 成功回调 (Success callback)
        if (onSuccess) {
          onSuccess(result)
        }

        return result
      } catch (err) {
        console.error('API Error:', err)
        setError(err)

        // 错误回调 (Error callback)
        if (onError) {
          onError(err)
        }

        throw err
      } finally {
        setLoading(false)
      }
    },
    [apiFunc, onSuccess, onError]
  )

  /**
   * 重置状态 (Reset state)
   */
  const reset = useCallback(() => {
    setData(null)
    setError(null)
    setLoading(false)
  }, [])

  // 立即执行 (Execute immediately)
  if (immediate) {
    execute()
  }

  return {
    data,
    loading,
    error,
    execute,
    reset,
  }
}

export default useApi

