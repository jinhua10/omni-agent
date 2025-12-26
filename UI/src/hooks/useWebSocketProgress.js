/**
 * WebSocket 进度管理 Hook
 * (WebSocket Progress Management Hook)
 *
 * 管理 WebSocket 连接和进度更新
 *
 * @author OmniAgent Team
 * @since 2025-12-26
 */

import { useEffect, useState, useCallback } from 'react';
import WebSocketClient from '../utils/WebSocketClient';

function useWebSocketProgress(documentsList, demoMode, onProgressUpdate) {
    const [wsClient, setWsClient] = useState(null);
    const [documentsProgress, setDocumentsProgress] = useState({});

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

    // WebSocket 连接
    useEffect(() => {
        if (documentsList.length === 0 || demoMode) return;

        console.log('📡 建立 WebSocket 连接，监听所有文档进度');

        let client = null;
        let pollInterval = null;
        let connectionFailed = false;

        try {
            // 动态构建 WebSocket URL
            const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
            const host = window.location.host;
            const wsUrl = `${protocol}//${host}/ws/progress`;

            console.log('🔗 WebSocket URL:', wsUrl);

            client = new WebSocketClient(wsUrl);

            client.on('open', () => {
                console.log('✅ WebSocket 连接已建立');
                connectionFailed = false;
                documentsList.forEach(doc => {
                    try {
                        client.subscribe(doc.documentId);
                        console.log('📝 订阅文档进度:', doc.documentId);
                    } catch (err) {
                        console.warn('⚠️ 订阅失败:', doc.documentId, err);
                    }
                });
            });

            client.on('message', handleMessage);

            client.on('error', (error) => {
                console.warn('⚠️ WebSocket 连接错误:', error);
                connectionFailed = true;
                console.log('💡 将使用轮询机制作为备用方案');
            });

            client.on('close', (event) => {
                console.log('🔌 WebSocket 连接已关闭:', event?.code, event?.reason);
                connectionFailed = true;
            });

            client.connect();
            setWsClient(client);

        } catch (error) {
            console.warn('⚠️ WebSocket 初始化失败，将使用轮询机制:', error);
            connectionFailed = true;
        }

        // 备用轮询
        pollInterval = setInterval(async () => {
            const shouldPoll = connectionFailed || !client || client.ws?.readyState !== WebSocket.OPEN;

            if (shouldPoll) {
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
            }
        }, connectionFailed ? 2000 : 5000);

        return () => {
            if (pollInterval) {
                clearInterval(pollInterval);
            }

            if (client) {
                try {
                    console.log('🔌 正在关闭 WebSocket 连接');
                    if (client.ws && client.ws.readyState === WebSocket.OPEN) {
                        client.unsubscribe();
                    }
                    client.close();
                } catch (error) {
                    console.debug('清理 WebSocket 时出错（可忽略）:', error.message);
                }
            }
        };
    }, [documentsList, demoMode, handleMessage]);

    return {
        wsClient,
        documentsProgress
    };
}

export default useWebSocketProgress;

