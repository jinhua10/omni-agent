/**
 * 问答 API 模块 (Q&A API Module)
 *
 * 提供智能问答相关的 API 接口
 * (Provides Q&A-related API interfaces)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { request, SSE_BASE_URL } from '../index'

const qaApi = {
  /**
   * 提问 (Ask question)
   * @param {Object} params - 问题参数
   * @param {string} params.question - 问题内容
   * @param {string} params.hopeSessionId - HOPE 会话 ID（可选）
   * @param {boolean} params.useKnowledgeBase - 是否使用知识库 RAG（可选，默认 true）
   * @returns {Promise} 回答结果
   */
  ask(params) {
    return request.post('/qa/ask', {
      ...params,
      useKnowledgeBase: params.useKnowledgeBase !== undefined ? params.useKnowledgeBase : true
    })
  },

  /**
   * 使用指定文档批次进行问答 (Ask with specific documents)
   * @param {Object} params - 问题参数
   * @param {string} params.question - 问题内容
   * @param {string} params.sessionId - 会话 ID
   * @param {boolean} params.useKnowledgeBase - 是否使用知识库 RAG（可选，默认 true）
   * @returns {Promise} 回答结果
   */
  askWithSession(params) {
    return request.post('/qa/ask-with-session', {
      ...params,
      useKnowledgeBase: params.useKnowledgeBase !== undefined ? params.useKnowledgeBase : true
    })
  },

  /**
   * 流式问答 - 真正的双轨架构 (Streaming Q&A - True Dual-track Architecture)
   *
   * 在一个 SSE 连接中同时接收 HOPE 快速答案和 LLM 流式输出
   * (Receive both HOPE fast answer and LLM streaming in one SSE connection)
   *
   * @param {Object} params - 问题参数
   * @param {string} params.question - 问题内容
   * @param {string} params.knowledgeMode - 知识库模式: 'none' | 'rag' | 'role'
   * @param {string} params.roleName - 角色名称（当 knowledgeMode='role' 时）
   * @param {boolean} params.useKnowledgeBase - 是否使用知识库（兼容参数）
   * @param {string} params.hopeSessionId - HOPE 会话 ID（可选）
   * @param {Function} onChunk - 数据块回调
   * @returns {Promise<{eventSource, stop}>}
   */
  async askStreaming(params, onChunk) {
    try {
      console.log('🚀 Starting dual-track streaming Q&A:', params.question)
      console.log('📝 Knowledge Mode:', params.knowledgeMode)
      console.log('👤 Role Name:', params.roleName)

      // 构建查询参数
      const queryParams = new URLSearchParams({
        question: params.question,
        knowledgeMode: params.knowledgeMode || 'rag',
        roleName: params.roleName || 'general'
      })

      if (params.hopeSessionId) {
        queryParams.append('sessionId', params.hopeSessionId)
      }

      // 使用单端点双轨流式接口
      // 注意：EventSource 不能使用 Vite 代理，需要直接指向后端
      const eventSourceUrl = `${SSE_BASE_URL}/qa/stream/dual-track?${queryParams}`
      console.log('📡 Connecting to dual-track SSE:', eventSourceUrl)

      // ⭐ 创建 EventSource 连接（withCredentials 确保正确处理跨域）
      const eventSource = new EventSource(eventSourceUrl, {
        withCredentials: false  // 开发环境跨域不需要凭据
      })

      // 监听连接打开事件
      eventSource.onopen = (event) => {
        console.log('✅ SSE connection opened:', event)
      }

      // 监听默认 message 事件
      eventSource.onmessage = (event) => {
        console.log('⚡ Real-time SSE message received:', event.data.substring(0, 100))
        try {
          const data = JSON.parse(event.data)
          console.log('📦 Received SSE data:', data.type, data)

          if (!onChunk) return

          // 根据 type 字段处理不同类型的数据
          switch (data.type) {
            case 'reference':
              // 参考文档
              console.log('📚 Reference:', data.title)
              onChunk({
                type: 'reference',
                title: data.title,
                content: data.content,
                score: data.score,
                done: false
              })
              break

            case 'answer':
              // AI 答案 token
              const tokenContent = data.token || data.content || ''
              console.log('💬 Answer token:', tokenContent)
              onChunk({
                type: 'answer',
                content: tokenContent,
                done: false
              })
              break

            case 'complete':
              // 完成标记
              console.log('✅ Stream completed')
              onChunk({
                type: 'complete',
                content: '',
                done: true
              })
              eventSource.close()
              break

            case 'error':
              // 错误信息
              console.error('❌ Error:', data.message)
              onChunk({
                type: 'error',
                error: data.message,
                done: true
              })
              eventSource.close()
              break

            default:
              console.warn('⚠️ Unknown message type:', data.type)
          }
        } catch (error) {
          console.error('❌ Failed to parse SSE message:', error, event.data)
        }
      }

      // 监听错误事件
      eventSource.addEventListener('error', (event) => {
        console.error('❌ SSE connection error:', event)

        if (eventSource.readyState === EventSource.CLOSED) {
          console.log('🔌 EventSource closed')
        } else {
          eventSource.close()

          if (onChunk) {
            onChunk({
              type: 'error',
              error: 'SSE connection failed'
            })
          }
        }
      })

      // 返回控制对象
      return {
        eventSource,
        stop: () => {
          eventSource.close()
          console.log('🛑 Stream stopped')
        }
      }

    } catch (error) {
      console.error('❌ Failed to ask streaming question:', error)
      if (onChunk) {
        onChunk({
          type: 'error',
          error: error.message
        })
      }
      throw error
    }
  },

  /**
   * 获取问答历史 / Get Q&A history
   * @param {Object} params - 查询参数 / Query parameters
   * @param {number} params.page - 页码 / Page number
   * @param {number} params.pageSize - 每页条数 / Items per page
   * @returns {Promise} 历史记录 / History records
   */
  getHistory(params) {
    return request.get('/qa/history', params)
  },

  /**
   * 获取相似问题 / Get similar questions
   * @param {string} question - 问题内容 / Question content
   * @returns {Promise} 相似问题列表 / Similar questions list
   */
  getSimilarQuestions(question) {
    return request.get('/qa/similar', { question })
  },

  /**
   * 反馈回答质量 / Feedback answer quality
   * @param {Object} params - 反馈参数 / Feedback parameters
   * @param {string} params.answerId - 回答 ID / Answer ID
   * @param {number} params.rating - 评分（1-5）/ Rating (1-5)
   * @param {string} params.comment - 评论（可选）/ Comment (optional)
   * @returns {Promise} 反馈结果 / Feedback result
   */
  feedback(params) {
    return request.post('/qa/feedback', params)
  },

  /**
   * 获取推荐提示词 / Get recommended prompts
   * @returns {Promise} 推荐提示词列表 / Recommended prompts list
   */
  getRecommendedPrompts() {
    return request.get('/qa/prompts/recommended')
  },
}

export default qaApi

