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
import { getUserId } from '../../utils/userManager'

const qaApi = {
  /**
   * 提问 (Ask question)
   * @param {Object} params - 问题参数
   * @param {string} params.question - 问题内容
   * @param {string} params.hopeSessionId - HOPE 会话 ID（可选）
   * @param {boolean} params.useKnowledgeBase - 是否使用知识库 RAG（可选，默认 true）
   * @returns {Promise} 回答结果
   */
  async ask(params) {
    const userId = await getUserId()
    return request.post('/qa/ask', {
      ...params,
      userId,
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
      const userId = await getUserId()
      console.log('🚀 Starting dual-track streaming Q&A:', params.question)
      console.log('👤 User ID:', userId)
      console.log('📝 Knowledge Mode:', params.knowledgeMode)
      console.log('👤 Role Name:', params.roleName)

      // 构建查询参数
      const queryParams = new URLSearchParams({
        question: params.question,
        userId: userId,
        knowledgeMode: params.knowledgeMode || 'rag',
        roleName: params.roleName || 'general'
      })

      if (params.hopeSessionId) {
        queryParams.append('sessionId', params.hopeSessionId)
      }

      // 使用单端点双轨流式接口（已迁移到 AdvancedQAController v2.0）
      // 注意：EventSource 不能使用 Vite 代理，需要直接指向后端
      const eventSourceUrl = `${SSE_BASE_URL}/qa/advanced/dual-track/stream?${queryParams}`
      console.log('📡 Connecting to dual-track SSE:', eventSourceUrl)

      // ⭐ 创建 EventSource 连接（withCredentials 确保正确处理跨域）
      const eventSource = new EventSource(eventSourceUrl, {
        withCredentials: false  // 开发环境跨域不需要凭据
      })

      // 监听连接打开事件
      eventSource.onopen = (event) => {
        console.log('✅ SSE connection opened:', event)
      }

      // 🔥 监听左面板输出（纯 LLM / 单轨模式的 LLM）
      eventSource.addEventListener('left', (event) => {
        try {
          const leftData = JSON.parse(event.data)
          console.log('⬅️ Left panel chunk:', leftData.content?.substring(0, 30))

          if (onChunk) {
            onChunk({
              content: leftData.content,
              done: false,
              type: 'left',  // 左面板
              chunkIndex: leftData.chunkIndex
            })
          }
        } catch (error) {
          console.error('❌ Failed to parse left panel chunk:', error)
        }
      })

      // 🔥 监听右面板输出（RAG 增强 / 角色知识库）
      eventSource.addEventListener('right', (event) => {
        try {
          const rightData = JSON.parse(event.data)
          console.log('➡️ Right panel chunk:', rightData.content?.substring(0, 30))

          if (onChunk) {
            onChunk({
              content: rightData.content,
              done: false,
              type: 'right',  // 右面板
              chunkIndex: rightData.chunkIndex
            })
          }
        } catch (error) {
          console.error('❌ Failed to parse right panel chunk:', error)
        }
      })

      // 🔥 监听 LLM 流式输出（单轨模式：不使用 RAG）
      eventSource.addEventListener('llm', (event) => {
        try {
          const llmData = JSON.parse(event.data)
          console.log('📦 LLM chunk received:', llmData.content?.substring(0, 50))

          if (onChunk) {
            onChunk({
              content: llmData.content,
              done: false,
              type: 'llm',  // 单面板 LLM
              chunkIndex: llmData.chunkIndex
            })
          }
        } catch (error) {
          console.error('❌ Failed to parse LLM chunk:', error)
        }
      })

      // 🔥 监听完成事件
      eventSource.addEventListener('complete', (event) => {
        console.log('✅ Dual-track streaming completed')

        try {
          const stats = JSON.parse(event.data)
          console.log('📊 Streaming stats:', stats)

          if (onChunk) {
            onChunk({
              content: '',
              done: true,
              type: 'complete',
              totalChunks: stats.totalChunks,
              totalTime: stats.totalTime
            })
          }
        } catch (e) {
          if (onChunk) {
            onChunk({
              content: '',
              done: true,
              type: 'complete'
            })
          }
        }

        eventSource.close()
      })

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
   * @param {string} params.keyword - 搜索关键词 / Search keyword
   * @returns {Promise} 历史记录 / History records
   */
  async getHistory(params) {
    const userId = await getUserId()
    return request.get('/qa/history', { ...params, userId })
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

