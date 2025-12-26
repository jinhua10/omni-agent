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

    // WebSocket 连接和轮询
    useEffect(() => {
        if (documentsList.length === 0 || demoMode) return;

        // ⭐ 只在第一次初始化时输出日志
        if (!isInitialized.current) {
            console.log('📡 准备监听文档进度');
            isInitialized.current = true;
        }

        let client = null;
        let pollInterval = null;
        let connectionFailed = false;

        // ⭐ 尝试建立 WebSocket 连接
        try {
            // 动态构建 WebSocket URL
            let wsUrl;

            // 开发环境：前端在 3000，后端在 8080
            if (import.meta.env.DEV || window.location.port === '3000') {
                const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
                const hostname = window.location.hostname;
                wsUrl = `${protocol}//${hostname}:8080/ws/progress`;
            }
            // 生产环境：前后端同域
            else {
                const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
                const host = window.location.host;
                wsUrl = `${protocol}//${host}/ws/progress`;
            }

            console.log('🔗 WebSocket URL:', wsUrl);

            client = new WebSocketClient(wsUrl);

            client.on('open', () => {
                console.log('✅ WebSocket 连接成功');
                connectionFailed = false;

                // 订阅所有文档的进度
                documentsList.forEach(doc => {
                    try {
                        client.subscribe(doc.documentId);
                        console.log('📝 已订阅文档:', doc.documentId);
                    } catch (err) {
                        console.warn('⚠️ 订阅失败:', doc.documentId, err);
                    }
                });
            });

            client.on('message', handleMessage);

            client.on('error', (error) => {
                console.warn('⚠️ WebSocket 连接错误，切换到轮询模式');
                connectionFailed = true;
            });

            client.on('close', (event) => {
                console.debug('🔌 WebSocket 连接关闭:', event?.code);
                connectionFailed = true;
            });

            client.connect();
            setWsClient(client);

        } catch (error) {
            console.warn('⚠️ WebSocket 初始化失败，使用轮询模式');
            connectionFailed = true;
        }

        // ⭐ 轮询机制作为备用方案
        pollInterval = setInterval(async () => {
            // 只在 WebSocket 未连接时才轮询
            const shouldPoll = connectionFailed || !client || !client.isConnected();

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
        }, 3000); // 每 3 秒轮询一次（WebSocket 连接时不轮询）

        // 清理函数
        return () => {
            if (pollInterval) {
                clearInterval(pollInterval);
            }

            if (client) {
                try {
                    if (client.isConnected()) {
                        client.unsubscribe();
                    }
                    client.close();
                } catch (error) {
                    // 忽略清理错误
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

