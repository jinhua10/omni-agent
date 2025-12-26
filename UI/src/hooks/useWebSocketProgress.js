/**
 * WebSocket 进度管理 Hook
 * (WebSocket Progress Management Hook)
 *
 * 管理 WebSocket 连接和进度更新
 *
 * @author OmniAgent Team
 * @since 2025-12-26
 */

import { useEffect, useState, useCallback, useRef } from 'react';
import WebSocketClient from '../utils/WebSocketClient';

function useWebSocketProgress(documentsList, demoMode, onProgressUpdate) {
    const [wsClient, setWsClient] = useState(null);
    const [documentsProgress, setDocumentsProgress] = useState({});
    const isInitialized = useRef(false); // ⭐ 追踪是否已初始化

    // 处理 WebSocket 消息
    const handleMessage = useCallback((message) => {
        if (message.type === 'progress') {
            const progressData = message.data;
            const docId = progressData.documentId;

            // 通知父组件
            if (onProgressUpdate) {
                onProgressUpdate(progressData);
            }

            // 更新文档列表中该文档的进度
            setDocumentsProgress(prev => ({
                ...prev,
                [docId]: {
                    stage: progressData.stage,
                    percentage: progressData.percentage,
                    message: progressData.message,
                    status: progressData.status
                }
            }));

            // 如果完成，清除该文档的进度信息
            if (progressData.status === 'COMPLETED') {
                setTimeout(() => {
                    setDocumentsProgress(prev => {
                        const newProgress = { ...prev };
                        delete newProgress[docId];
                        return newProgress;
                    });
                }, 1000);
            }
        }
    }, [onProgressUpdate]);

    // WebSocket 连接（暂时禁用）
    useEffect(() => {
        if (documentsList.length === 0 || demoMode) return;

        // ⭐ 只在第一次初始化时输出日志
        if (!isInitialized.current) {
            console.log('📡 准备监听文档进度');
            console.log('💡 使用轮询机制监听文档进度（WebSocket 已禁用）');
            isInitialized.current = true;
        }

        // ⭐ 完全使用轮询机制，不使用 WebSocket
        const pollInterval = setInterval(async () => {
            documentsList.forEach(async (doc) => {
                try {
                    const response = await fetch(`/api/system/rag-config/document/${doc.documentId}`);
                    const result = await response.json();
                    if (result.success && result.data) {
                        const docData = result.data;
                        if (docData.status === 'PROCESSING' && docData.currentStage) {
                            const progressData = {
                                documentId: doc.documentId,
                                stage: docData.currentStage || 'UPLOAD',
                                percentage: docData.percentage || 0,
                                message: docData.message || '处理中...',
                                status: docData.status
                            };

                            setDocumentsProgress(prev => ({
                                ...prev,
                                [doc.documentId]: {
                                    stage: progressData.stage,
                                    percentage: progressData.percentage,
                                    message: progressData.message,
                                    status: progressData.status
                                }
                            }));

                            console.debug('🔄 轮询更新进度:', doc.documentId, progressData.percentage + '%');
                        }
                    }
                } catch (error) {
                    console.debug('轮询检查失败:', doc.documentId, error.message);
                }
            });
        }, 2000); // 每 2 秒轮询一次

        // 清理函数
        return () => {
            if (pollInterval) {
                clearInterval(pollInterval);
            }
        };
    }, [documentsList, demoMode]); // ⭐ 移除 handleMessage 避免重复触发

    return {
        wsClient,
        documentsProgress
    };
}

export default useWebSocketProgress;

