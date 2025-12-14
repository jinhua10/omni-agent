/**
 * QAContext 问答状态管理 (Q&A State Management Context)
 *
 * 管理问答相关的全局状态，确保切换Tab时不丢失对话内容
 * (Manages Q&A related global state to preserve conversation when switching tabs)
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React, { createContext, useContext, useState, useCallback, useEffect } from 'react'
import PropTypes from 'prop-types'

const QAContext = createContext()

/**
 * QAProvider 组件
 */
export function QAProvider({ children }) {
  // 消息列表 (Message list)
  const [messages, setMessages] = useState([])
  
  // 会话信息 (Session info)
  const [sessionInfo, setSessionInfo] = useState(null)
  
  // 当前会话ID (Current session ID)
  const [currentSessionId, setCurrentSessionId] = useState(null)
  
  // 当前问题 (Current question)
  const [currentQuestion, setCurrentQuestion] = useState('')
  
  // 相似问题 (Similar questions)
  const [similarQuestions, setSimilarQuestions] = useState([])
  
  // AI分析选中的文档 (Documents selected for AI analysis)
  const [aiAnalysisDocs, setAiAnalysisDocs] = useState([])
  
  // 浮动AI面板显示状态 (Floating AI panel visibility)
  const [showFloatingAI, setShowFloatingAI] = useState(false)

  /**
   * 添加消息
   */
  const addMessage = useCallback((message) => {
    setMessages(prev => [...prev, message])
  }, [])

  /**
   * 更新最后一条消息
   */
  const updateLastMessage = useCallback((updater) => {
    setMessages(prev => {
      const newMessages = [...prev]
      const lastMessage = newMessages[newMessages.length - 1]
      if (lastMessage) {
        Object.assign(lastMessage, typeof updater === 'function' ? updater(lastMessage) : updater)
      }
      return newMessages
    })
  }, [])

  /**
   * 清空消息
   */
  const clearMessages = useCallback(() => {
    setMessages([])
    setSessionInfo(null)
    setCurrentSessionId(null)
  }, [])

  /**
   * 添加文档到AI分析
   */
  const addDocToAIAnalysis = useCallback((doc) => {
    setAiAnalysisDocs(prev => {
      // 检查是否已存在（通过文档名或ID去重）
      const docId = doc.id || doc.name || doc.fileName || doc.title
      const exists = prev.some(d => {
        const existingId = d.id || d.name || d.fileName || d.title
        return existingId === docId
      })
      
      if (exists) {
        console.warn('⚠️ Document already in AI analysis:', docId)
        return prev
      }
      
      console.log('✅ Added document to AI analysis:', docId)
      return [...prev, doc]
    })
    
    // 自动显示浮动面板
    setShowFloatingAI(true)
  }, [])

  /**
   * 从AI分析中移除文档
   */
  const removeDocFromAIAnalysis = useCallback((docIdOrName) => {
    setAiAnalysisDocs(prev => {
      const filtered = prev.filter(d => {
        const id = d.id || d.name || d.fileName || d.title
        return id !== docIdOrName
      })
      console.log('🗑️ Removed document from AI analysis:', docIdOrName)
      return filtered
    })
  }, [])

  /**
   * 批量添加文档
   */
  const addDocsToAIAnalysis = useCallback((docs) => {
    docs.forEach(doc => addDocToAIAnalysis(doc))
  }, [addDocToAIAnalysis])

  /**
   * 清空AI分析文档
   */
  const clearAIAnalysisDocs = useCallback(() => {
    setAiAnalysisDocs([])
  }, [])

  /**
   * 检查文档是否在AI分析中
   */
  const isDocInAIAnalysis = useCallback((docIdOrName) => {
    return aiAnalysisDocs.some(d => {
      const id = d.id || d.name || d.fileName || d.title
      return id === docIdOrName
    })
  }, [aiAnalysisDocs])

  /**
   * 更新会话信息
   */
  const updateSessionInfo = useCallback((info) => {
    setSessionInfo(info)
    if (info?.sessionId) {
      setCurrentSessionId(info.sessionId)
    }
  }, [])

  // Context 值
  const contextValue = {
    // 状态
    messages,
    sessionInfo,
    currentSessionId,
    currentQuestion,
    similarQuestions,
    aiAnalysisDocs,
    showFloatingAI,
    
    // 消息操作
    addMessage,
    updateLastMessage,
    clearMessages,
    setMessages,
    
    // 会话操作
    updateSessionInfo,
    setCurrentSessionId,
    setCurrentQuestion,
    setSimilarQuestions,
    
    // AI分析文档操作
    addDocToAIAnalysis,
    removeDocFromAIAnalysis,
    addDocsToAIAnalysis,
    clearAIAnalysisDocs,
    isDocInAIAnalysis,
    
    // 浮动面板操作
    setShowFloatingAI,
  }

  return (
    <QAContext.Provider value={contextValue}>
      {children}
    </QAContext.Provider>
  )
}

QAProvider.propTypes = {
  children: PropTypes.node.isRequired,
}

/**
 * useQA Hook
 */
export function useQA() {
  const context = useContext(QAContext)
  if (!context) {
    throw new Error('useQA must be used within QAProvider')
  }
  return context
}

export default QAContext
