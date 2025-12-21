/**
 * RAG 流程适配器
 * (RAG Flow Adapter)
 *
 * 提供 RAG 流程可视化的数据绑定和状态管理
 * (Provides data binding and state management for RAG flow visualization)
 *
 * @author AI Reviewer Team
 * @since 2.0.0 (Phase 4)
 */

import { useState, useEffect, useCallback } from 'react';
import axios from 'axios';

/**
 * RAG 流程适配器 Hook
 * (RAG Flow Adapter Hook)
 *
 * @param {string} documentId - 文档ID (Document ID)
 * @returns {object} 状态和操作方法 (State and action methods)
 */
export function useRAGFlowAdapter(documentId) {
    // 状态管理 (State management)
    const [state, setState] = useState({
        documentId: documentId || null,
        progress: null,
        loading: false,
        error: null,
        isProcessing: false
    });

    /**
     * 获取文档处理进度
     * (Get document processing progress)
     */
    const fetchProgress = useCallback(async (docId) => {
        if (!docId) return;

        setState(prev => ({ ...prev, loading: true, error: null }));

        try {
            const response = await axios.get(`/api/rag/progress/${docId}`, {
                headers: {
                    'Accept-Language': localStorage.getItem('language') || 'zh'
                }
            });

            if (response.data.success) {
                setState(prev => ({
                    ...prev,
                    progress: response.data.data,
                    loading: false,
                    isProcessing: response.data.data.status === 'RUNNING'
                }));
            } else {
                throw new Error(response.data.error || '获取进度失败');
            }
        } catch (error) {
            console.error('获取文档进度失败:', error);
            setState(prev => ({
                ...prev,
                loading: false,
                error: error.response?.data?.error || error.message || '获取进度失败'
            }));
        }
    }, []);

    /**
     * 获取文档处理详情
     * (Get document processing details)
     */
    const fetchDetails = useCallback(async (docId) => {
        if (!docId) return;

        try {
            const response = await axios.get(`/api/rag/progress/${docId}/details`, {
                headers: {
                    'Accept-Language': localStorage.getItem('language') || 'zh'
                }
            });

            if (response.data.success) {
                return response.data.data;
            } else {
                throw new Error(response.data.error || '获取详情失败');
            }
        } catch (error) {
            console.error('获取文档详情失败:', error);
            throw error;
        }
    }, []);

    /**
     * 删除进度记录
     * (Delete progress record)
     */
    const deleteProgress = useCallback(async (docId) => {
        if (!docId) return;

        try {
            const response = await axios.delete(`/api/rag/progress/${docId}`, {
                headers: {
                    'Accept-Language': localStorage.getItem('language') || 'zh'
                }
            });

            if (response.data.success) {
                setState(prev => ({
                    ...prev,
                    progress: null,
                    isProcessing: false
                }));
                return true;
            } else {
                throw new Error(response.data.error || '删除失败');
            }
        } catch (error) {
            console.error('删除进度记录失败:', error);
            throw error;
        }
    }, []);

    /**
     * 设置文档ID
     * (Set document ID)
     */
    const setDocumentId = useCallback((docId) => {
        setState(prev => ({ ...prev, documentId: docId }));
    }, []);

    /**
     * 更新进度（来自 WebSocket）
     * (Update progress from WebSocket)
     */
    const updateProgress = useCallback((progressData) => {
        setState(prev => ({
            ...prev,
            progress: progressData,
            isProcessing: progressData.status === 'RUNNING'
        }));
    }, []);

    /**
     * 清除错误
     * (Clear error)
     */
    const clearError = useCallback(() => {
        setState(prev => ({ ...prev, error: null }));
    }, []);

    // 初始化时加载进度 (Load progress on initialization)
    useEffect(() => {
        if (state.documentId) {
            fetchProgress(state.documentId);
        }
    }, [state.documentId, fetchProgress]);

    // 返回状态和操作方法 (Return state and action methods)
    return {
        state,
        actions: {
            fetchProgress,
            fetchDetails,
            deleteProgress,
            setDocumentId,
            updateProgress,
            clearError
        }
    };
}

/**
 * RAG 流程绑定 Hook（简化版）
 * (RAG Flow Binding Hook - Simplified)
 *
 * 用于 UI 壳子直接使用
 * (For direct use in UI shells)
 */
export function useRAGFlowBinding() {
    const [documentId, setDocumentId] = useState(null);
    const adapter = useRAGFlowAdapter(documentId);

    return {
        state: {
            documentId: adapter.state.documentId,
            progress: adapter.state.progress,
            loading: adapter.state.loading,
            error: adapter.state.error,
            isProcessing: adapter.state.isProcessing
        },
        actions: {
            // 设置文档 (Set document)
            setDocument: (docId) => {
                setDocumentId(docId);
                adapter.actions.setDocumentId(docId);
            },

            // 刷新进度 (Refresh progress)
            refresh: () => {
                if (documentId) {
                    adapter.actions.fetchProgress(documentId);
                }
            },

            // 获取详情 (Get details)
            getDetails: () => {
                if (documentId) {
                    return adapter.actions.fetchDetails(documentId);
                }
                return Promise.reject(new Error('No document ID'));
            },

            // 删除进度 (Delete progress)
            deleteProgress: () => {
                if (documentId) {
                    return adapter.actions.deleteProgress(documentId);
                }
                return Promise.reject(new Error('No document ID'));
            },

            // 更新进度（来自 WebSocket）
            updateProgress: adapter.actions.updateProgress,

            // 清除错误 (Clear error)
            clearError: adapter.actions.clearError
        }
    };
}

export default useRAGFlowAdapter;

