/**
 * useFetch Hook
 *
 * 用于数据获取的 Hook，支持自动请求、缓存、轮询等
 * (Hook for data fetching with auto-request, caching, polling, etc.)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { useState, useEffect, useCallback, useRef } from 'react'

/**
 * useFetch Hook
 *
 * @param {Function} apiFunc - API 函数
 * @param {Object} params - 请求参数
 * @param {Object} options - 配置选项
 * @param {boolean} options.manual - 是否手动触发
 * @param {number} options.pollingInterval - 轮询间隔（毫秒）
 * @param {boolean} options.cache - 是否缓存
 * @param {Function} options.onSuccess - 成功回调
 * @param {Function} options.onError - 错误回调
 *
 * @returns {Object} 数据和方法
 *
 * @example
 * // 自动请求
 * const { data, loading, error, refresh } = useFetch(api.document.getList, { page: 1 })
 *
 * // 手动触发
 * const { data, loading, run } = useFetch(api.document.getDetail, null, { manual: true })
 * run(documentId)
 */
function useFetch(apiFunc, params = null, options = {}) {
  const {
    manual = false,
    pollingInterval = 0,
    cache = false,
    onSuccess,
    onError,
  } = options

  // 状态管理 (State management)
  const [data, setData] = useState(null)
  const [loading, setLoading] = useState(!manual)
  const [error, setError] = useState(null)

  // 引用 (Refs)
  const pollingTimerRef = useRef(null)
  const cacheRef = useRef(new Map())
  const mountedRef = useRef(true)

  /**
   * 生成缓存键 (Generate cache key)
   */
  const getCacheKey = useCallback((params) => {
    return JSON.stringify(params)
  }, [])

  /**
   * 执行请求 (Execute request)
   */
  const fetchData = useCallback(
    async (fetchParams = params) => {
      try {
        setLoading(true)
        setError(null)

        // 检查缓存 (Check cache)
        if (cache) {
          const cacheKey = getCacheKey(fetchParams)
          const cachedData = cacheRef.current.get(cacheKey)
          if (cachedData) {
            console.log('📦 Using cached data')
            setData(cachedData)
            setLoading(false)
            return cachedData
          }
        }

        // 发起请求 (Make request)
        const result = await apiFunc(fetchParams)

        if (!mountedRef.current) return

        setData(result)

        // 缓存数据 (Cache data)
        if (cache) {
          const cacheKey = getCacheKey(fetchParams)
          cacheRef.current.set(cacheKey, result)
        }

        // 成功回调 (Success callback)
        if (onSuccess) {
          onSuccess(result)
        }

        return result
      } catch (err) {
        if (!mountedRef.current) return

        console.error('Fetch Error:', err)
        setError(err)

        // 错误回调 (Error callback)
        if (onError) {
          onError(err)
        }

        throw err
      } finally {
        if (mountedRef.current) {
          setLoading(false)
        }
      }
    },
    [apiFunc, params, cache, getCacheKey, onSuccess, onError]
  )

  /**
   * 刷新数据 (Refresh data)
   */
  const refresh = useCallback(() => {
    return fetchData()
  }, [fetchData])

  /**
   * 手动触发请求 (Manual trigger)
   */
  const run = useCallback(
    (runParams) => {
      return fetchData(runParams)
    },
    [fetchData]
  )

  /**
   * 清除缓存 (Clear cache)
   */
  const clearCache = useCallback(() => {
    cacheRef.current.clear()
  }, [])

  // 自动请求 (Auto request)
  useEffect(() => {
    if (!manual) {
      fetchData()
    }
  }, [manual, fetchData])

  // 轮询 (Polling)
  useEffect(() => {
    if (pollingInterval > 0) {
      pollingTimerRef.current = setInterval(() => {
        fetchData()
      }, pollingInterval)

      return () => {
        if (pollingTimerRef.current) {
          clearInterval(pollingTimerRef.current)
        }
      }
    }
  }, [pollingInterval, fetchData])

  // 清理 (Cleanup)
  useEffect(() => {
    return () => {
      mountedRef.current = false
      if (pollingTimerRef.current) {
        clearInterval(pollingTimerRef.current)
      }
    }
  }, [])

  return {
    data,
    loading,
    error,
    refresh,
    run,
    clearCache,
  }
}

export default useFetch

