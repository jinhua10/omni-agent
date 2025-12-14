/**
 * KnowledgeContext 知识库状态上下文 (Knowledge Base State Context)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { createContext, useContext, useState, useCallback } from 'react'
import PropTypes from 'prop-types'
import api from '@api/modules'

const KnowledgeContext = createContext()

export function KnowledgeProvider({ children }) {
  const [documents, setDocuments] = useState([])
  const [currentDocument, setCurrentDocument] = useState(null)
  const [loading, setLoading] = useState(false)
  const [statistics, setStatistics] = useState(null)

  const fetchDocuments = useCallback(async (params) => {
    try {
      setLoading(true)
      const data = await api.document.getList(params)
      setDocuments(data.list || data)
      return data
    } catch (error) {
      console.error('Failed to fetch documents:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  const uploadDocument = useCallback(async (formData, onProgress) => {
    try {
      setLoading(true)
      const result = await api.document.upload(formData, onProgress)
      setDocuments((prev) => [result, ...prev])
      return result
    } catch (error) {
      console.error('Failed to upload document:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  const batchUploadDocuments = useCallback(async (formData, onProgress) => {
    try {
      setLoading(true)
      const result = await api.document.batchUpload(formData, onProgress)
      // 刷新文档列表 / Refresh document list
      await fetchDocuments()
      return result
    } catch (error) {
      console.error('Failed to batch upload documents:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [fetchDocuments])

  const deleteDocument = useCallback(async (documentId) => {
    try {
      setLoading(true)
      await api.document.delete(documentId)
      setDocuments((prev) => prev.filter((doc) => doc.id !== documentId))
    } catch (error) {
      console.error('Failed to delete document:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  const fetchStatistics = useCallback(async () => {
    try {
      const stats = await api.document.getStatistics()
      setStatistics(stats)
      return stats
    } catch (error) {
      console.error('Failed to fetch statistics:', error)
      throw error
    }
  }, [])

  const value = {
    documents,
    currentDocument,
    loading,
    statistics,
    fetchDocuments,
    uploadDocument,
    batchUploadDocuments,
    deleteDocument,
    fetchStatistics,
    setCurrentDocument,
  }

  return (
    <KnowledgeContext.Provider value={value}>
      {children}
    </KnowledgeContext.Provider>
  )
}

KnowledgeProvider.propTypes = {
  children: PropTypes.node.isRequired,
}

export function useKnowledge() {
  const context = useContext(KnowledgeContext)
  if (!context) {
    throw new Error('useKnowledge must be used within KnowledgeProvider')
  }
  return context
}

export default KnowledgeContext

