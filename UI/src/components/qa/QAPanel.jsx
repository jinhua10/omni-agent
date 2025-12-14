/**
 * 问答主面板 (Q&A Main Panel)
 *
 * 智能问答系统的主界面容器
 * (Main interface container for intelligent Q&A system)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useRef } from 'react'
import { Layout } from 'antd'
import ChatBox from './ChatBox'
import QuestionInput from './QuestionInput'
import SimilarQuestions from './SimilarQuestions'
import ConversationHistory from './ConversationHistory'
import { useLanguage } from '../../contexts/LanguageContext'
import { useQA } from '../../contexts/QAContext'
import qaApi from '../../api/modules/qa'
import '../../assets/css/qa/qa-panel.css'

const { Content, Sider } = Layout

/**
 * 问答主面板组件
 * @returns {JSX.Element}
 */
function QAPanel() {
  const { t } = useLanguage()
  const {
    messages,
    setMessages,
    similarQuestions,
    setSimilarQuestions,
    currentQuestion,
    setCurrentQuestion,
  } = useQA()

  // 本地状态（不需要跨Tab保持）
  const [loading, setLoading] = useState(false) // 加载状态
  const [historyVisible, setHistoryVisible] = useState(false) // 历史记录可见性
  const [currentEventSource, setCurrentEventSource] = useState(null) // 当前 EventSource 连接
  
  // 使用ref追踪当前流式消息的内容，避免React批量更新导致重复累加
  const streamingContentRef = useRef('')
  const streamingLLMAnswerRef = useRef('')
  
  // 从 localStorage 读取流式模式偏好（默认为 true）
  const [isStreamingMode, setIsStreamingMode] = useState(() => {
    const saved = localStorage.getItem('qa_streaming_mode')
    return saved !== null ? saved === 'true' : true
  })
  
  // 知识库模式：'none' | 'rag' | 'role'（默认为 'rag'）
  const [knowledgeMode, setKnowledgeMode] = useState(() => {
    const saved = localStorage.getItem('qa_knowledge_mode')
    return saved || 'rag'
  })

  // 角色名称（当 knowledgeMode='role' 时使用）
  const [roleName, setRoleName] = useState(() => {
    const saved = localStorage.getItem('qa_role_name')
    return saved || 'general'
  })

  /**
   * 切换流式/非流式模式
   */
  const toggleStreamingMode = () => {
    const newMode = !isStreamingMode
    setIsStreamingMode(newMode)
    localStorage.setItem('qa_streaming_mode', newMode.toString())
    console.log(`🔄 Switched to ${newMode ? 'streaming' : 'non-streaming'} mode`)
  }
  
  /**
   * 切换知识库模式
   */
  const handleKnowledgeModeChange = (mode) => {
    setKnowledgeMode(mode)
    localStorage.setItem('qa_knowledge_mode', mode)
    console.log(`🔄 Switched knowledge mode to: ${mode}`)
  }

  /**
   * 切换角色
   */
  const handleRoleNameChange = (role) => {
    setRoleName(role)
    localStorage.setItem('qa_role_name', role)
    console.log(`🔄 Switched role to: ${role}`)
  }

  /**
   * 处理问题提交
   * 根据用户选择使用流式或非流式模式
   * @param {string} question - 问题内容
   */
  const handleSubmitQuestion = async (question) => {
    // 根据用户设置决定使用哪种模式
    if (!isStreamingMode) {
      return handleSubmitQuestionNonStreaming(question)
    }
    
    // 默认使用流式模式
    if (!question.trim()) return

    // 添加用户问题到消息列表
    const userMessage = {
      id: Date.now(),
      type: 'question',
      content: question,
      timestamp: new Date().toISOString(),
    }
    setMessages(prev => [...prev, userMessage])
    setCurrentQuestion(question)
    setLoading(true)

    try {
      // 创建答案消息占位符 / Create answer message placeholder
      const answerMessage = {
        id: Date.now() + 1,
        type: 'answer',
        content: '',
        streaming: true,
        timestamp: new Date().toISOString(),
        sessionId: null,
        sources: [],
      }
      setMessages(prev => [...prev, answerMessage])
      
      // 重置ref内容
      streamingContentRef.current = { leftPanel: '', rightPanel: '' }
      streamingLLMAnswerRef.current = ''

      // 调用流式 API（双轨输出）/ Call streaming API (Dual Track)
      const result = await qaApi.askStreaming(
        { 
          question,
          knowledgeMode,      // 知识库模式: 'none' | 'rag' | 'role'
          roleName,           // 角色名称（当 knowledgeMode='role' 时）
          useKnowledgeBase: knowledgeMode !== 'none'  // 兼容旧API
        },
        (data) => {
          // 调试日志
          console.log('📨 Received data:', data.type, data)

          // 累加到ref
          if (data.type === 'left') {
            // 左面板：纯 LLM
            console.log('⬅️ Left panel:', data.content)
            streamingContentRef.current.leftPanel += data.content
          } else if (data.type === 'right') {
            // 右面板：RAG 增强 / 角色知识库
            console.log('➡️ Right panel:', data.content)
            streamingContentRef.current.rightPanel += data.content
          } else if (data.type === 'llm') {
            // 单轨 LLM（不使用 RAG）
            console.log('📦 LLM chunk:', data.content)
            streamingLLMAnswerRef.current += data.content
          }
          
          // 更新 UI
          setMessages(prev => {
            const newMessages = [...prev]
            const lastMessage = newMessages[newMessages.length - 1]
            
            if (lastMessage && lastMessage.streaming) {
              switch (data.type) {
                case 'left':
                case 'right':
                  // 双轨模式
                  lastMessage.dualTrack = true
                  lastMessage.leftPanel = streamingContentRef.current.leftPanel || ''
                  lastMessage.rightPanel = streamingContentRef.current.rightPanel || ''
                  lastMessage.content = `[${t('qa.dualTrack.dualTrackOutput')}]\n${t('qa.dualTrack.leftPanel')}: ${lastMessage.leftPanel.substring(0, 50)}...\n${t('qa.dualTrack.rightPanel')}: ${lastMessage.rightPanel.substring(0, 50)}...`
                  break

                case 'llm':
                  // 单轨模式（不使用 RAG）
                  lastMessage.dualTrack = false
                  lastMessage.content = streamingLLMAnswerRef.current
                  break

                case 'complete':
                  // 完成
                  lastMessage.streaming = false
                  lastMessage.sessionId = data.sessionId
                  break

                case 'error':
                  // 错误
                  lastMessage.type = 'error'
                  lastMessage.content = data.error || t('qa.error.failed')
                  lastMessage.streaming = false
                  break

                default:
                  // 兼容
                  if (data.content) {
                    streamingLLMAnswerRef.current += data.content
                    lastMessage.content = streamingLLMAnswerRef.current
                  }
                  if (data.done) {
                    lastMessage.streaming = false
                  }
              }
            }
            return newMessages
          })
        }
      )


      // 保存 EventSource 引用以便停止生成 / Save EventSource reference for stopping
      if (result && result.eventSource) {
        setCurrentEventSource(result.eventSource)
      }

      // 获取相似问题 / Get similar questions
      try {
        const similarData = await qaApi.getSimilarQuestions(question)
        // axios 拦截器已返回 response.data (Axios interceptor returns response.data)
        if (similarData) {
          setSimilarQuestions(similarData)
        }
      } catch (err) {
        console.warn('⚠️ Failed to get similar questions:', err)
      }

    } catch (error) {
      console.error('❌ Failed to ask question:', error)
      // 添加错误消息 / Add error message
      setMessages(prev => {
        const newMessages = [...prev]
        const lastMessage = newMessages[newMessages.length - 1]
        if (lastMessage && lastMessage.streaming) {
          lastMessage.type = 'error'
          lastMessage.content = error.message || t('qa.error.failed')
          lastMessage.streaming = false
        }
        return newMessages
      })
    } finally {
      setLoading(false)
      setCurrentEventSource(null)
    }
  }

  /**
   * 非流式问答（带 thinking 动画）
   * Non-streaming Q&A with thinking animation
   */
  const handleSubmitQuestionNonStreaming = async (question) => {
    if (!question.trim()) return

    // 添加用户问题
    const userMessage = {
      id: Date.now(),
      type: 'question',
      content: question,
      timestamp: new Date().toISOString(),
    }
    setMessages(prev => [...prev, userMessage])
    setCurrentQuestion(question)
    setLoading(true)

    try {
      // 创建 thinking 状态的答案
      const answerMessage = {
        id: Date.now() + 1,
        type: 'answer',
        content: '',
        thinking: true,  // Thinking 状态
        timestamp: new Date().toISOString(),
        sessionId: null,
        sources: [],
      }
      setMessages(prev => [...prev, answerMessage])

      // 调用非流式 API
      const response = await qaApi.ask({ 
        question,
        knowledgeMode,      // 知识库模式: 'none' | 'rag' | 'role'
        roleName,           // 角色名称（当 knowledgeMode='role' 时）
        useKnowledgeBase: knowledgeMode !== 'none'  // 兼容旧API
      })

      // 更新答案内容
      setMessages(prev => {
        const newMessages = [...prev]
        const lastMessage = newMessages[newMessages.length - 1]
        if (lastMessage && lastMessage.thinking) {
          lastMessage.thinking = false
          lastMessage.content = response.answer
          lastMessage.sessionId = response.sessionId
          lastMessage.sources = response.sources || []
        }
        return newMessages
      })

      // 获取相似问题
      try {
        const similarData = await qaApi.getSimilarQuestions(question)
        if (similarData) {
          setSimilarQuestions(similarData)
        }
      } catch (err) {
        console.warn('⚠️ Failed to get similar questions:', err)
      }

    } catch (error) {
      console.error('❌ Failed to ask question:', error)
      setMessages(prev => {
        const newMessages = [...prev]
        const lastMessage = newMessages[newMessages.length - 1]
        if (lastMessage && lastMessage.thinking) {
          lastMessage.type = 'error'
          lastMessage.content = error.message || t('qa.error.failed')
          lastMessage.thinking = false
        }
        return newMessages
      })
    } finally {
      setLoading(false)
    }
  }

  /**
   * 停止生成
   * Stop generation
   */
  const handleStopGeneration = () => {
    if (currentEventSource) {
      console.log('🛑 Stopping generation...')
      currentEventSource.close()
      setCurrentEventSource(null)
      setLoading(false)

      // 标记最后一条消息为已完成
      // Mark last message as completed
      setMessages(prev => {
        const newMessages = [...prev]
        const lastMessage = newMessages[newMessages.length - 1]
        if (lastMessage && lastMessage.streaming) {
          lastMessage.streaming = false
          lastMessage.stopped = true
        }
        return newMessages
      })
    }
  }

  /**
   * 处理相似问题点击
   * @param {string} question - 问题内容
   */
  const handleSimilarQuestionClick = (question) => {
    handleSubmitQuestion(question)
  }

  /**
   * 处理答案反馈
   * @param {string} answerId - 答案 ID
   * @param {number} rating - 评分
   */
  const handleFeedback = async (answerId, rating) => {
    try {
      await qaApi.feedback({ answerId, rating })
      console.log('✅ Feedback submitted')
    } catch (error) {
      console.error('❌ Failed to submit feedback:', error)
    }
  }

  /**
   * 切换历史记录侧边栏
   */
  const toggleHistory = () => {
    setHistoryVisible(!historyVisible)
  }

  return (
    <>
      <Layout className="qa-panel">
        {/* 左侧：对话历史（可折叠） */}
        {historyVisible && (
          <Sider
            width={280}
            className="qa-panel__history-sider"
            theme="light"
          >
            <ConversationHistory
              onClose={() => setHistoryVisible(false)}
              onSelectQuestion={handleSubmitQuestion}
            />
          </Sider>
        )}

        {/* 中间：主聊天区域 */}
        <Content className="qa-panel__main">
        <div className="qa-panel__container">
          {/* 聊天框 */}
          <ChatBox
            messages={messages}
            loading={loading}
            onFeedback={handleFeedback}
            onToggleHistory={toggleHistory}
            onStopGeneration={handleStopGeneration}
            isGenerating={!!currentEventSource}
            isStreamingMode={isStreamingMode}
            onToggleStreamingMode={toggleStreamingMode}
            knowledgeMode={knowledgeMode}
            onKnowledgeModeChange={handleKnowledgeModeChange}
            roleName={roleName}
            onRoleNameChange={handleRoleNameChange}
          />

          {/* 输入框 */}
          <QuestionInput
            onSubmit={handleSubmitQuestion}
            loading={loading}
            placeholder={t('qa.input.placeholder')}
          />
        </div>
      </Content>

        {/* 右侧：相似问题推荐 */}
        <Sider
          width={300}
          className="qa-panel__similar-sider"
          theme="light"
        >
          <SimilarQuestions
            questions={similarQuestions}
            currentQuestion={currentQuestion}
            onQuestionClick={handleSimilarQuestionClick}
          />
        </Sider>
      </Layout>
    </>
  )
}

export default QAPanel

