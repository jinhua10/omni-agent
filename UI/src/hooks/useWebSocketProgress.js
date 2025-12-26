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

    // WebSocket 连接（完全依赖服务端推送）
    useEffect(() => {
        if (documentsList.length === 0 || demoMode) return;

        // ⭐ 只在第一次初始化时输出日志
        if (!isInitialized.current) {
            // console.log('📡 建立 WebSocket 连接');
            isInitialized.current = true;
        }

        let client = null;

        // ⭐ 建立 WebSocket 连接
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

            // console.log('🔗 WebSocket URL:', wsUrl);

            client = new WebSocketClient(wsUrl);

            client.on('open', () => {
                // console.log('✅ WebSocket 连接成功');

                // 订阅所有文档的进度
                documentsList.forEach(doc => {
                    try {
                        client.subscribe(doc.documentId);
                        // console.log('📝 已订阅文档:', doc.documentId);
                    } catch (err) {
                        console.debug('订阅失败:', doc.documentId, err);
                    }
                });
            });

            client.on('message', handleMessage);

            client.on('error', (error) => {
                console.debug('WebSocket 连接错误（可忽略）');
            });

            client.on('close', (event) => {
                console.debug('🔌 WebSocket 连接关闭:', event?.code);
            });

            client.connect();
            setWsClient(client);

        } catch (error) {
            console.debug('WebSocket 初始化失败（后端服务可能未启动）:', error.message);
        }

        // 清理函数
        return () => {
            if (client) {
                // 延迟关闭，避免频繁的cleanup导致连接中断
                setTimeout(() => {
                    try {
                        if (client.isConnected()) {
                            client.unsubscribe();
                        }
                        client.close();
                    } catch (error) {
                        // 忽略清理错误
                    }
                }, 100);
            }
        };
    }, [documentsList, demoMode, handleMessage]);

    return {
        wsClient,
        documentsProgress
    };
}

export default useWebSocketProgress;

