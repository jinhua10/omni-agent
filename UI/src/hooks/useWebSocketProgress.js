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
    const clientRef = useRef(null); // ⭐ 使用ref避免重复创建
    const mountedRef = useRef(true); // ⭐ 追踪组件是否挂载

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
        console.log('🔍 useWebSocketProgress - documentsList.length:', documentsList.length, 'demoMode:', demoMode);

        if (documentsList.length === 0 || demoMode) {
            console.log('⏭️ 跳过 WebSocket 连接 - 文档列表为空或演示模式');
            return;
        }

        // ⭐ 如果已有连接且状态正常，不重新创建
        if (clientRef.current && clientRef.current.isConnected()) {
            console.log('✅ WebSocket 已连接，跳过重复创建');
            return;
        }

        // ⭐ 只在第一次初始化时输出日志
        if (!isInitialized.current) {
            console.log('📡 建立 WebSocket 连接');
            isInitialized.current = true;
        }

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

            console.log('🔗 WebSocket URL:', wsUrl);

            const client = new WebSocketClient(wsUrl);
            clientRef.current = client; // ⭐ 保存到ref

            client.on('open', () => {
                console.log('✅ WebSocket 连接成功');

                // 订阅所有文档的进度
                documentsList.forEach(doc => {
                    try {
                        client.subscribe(doc.documentId);
                        console.log('📝 已订阅文档:', doc.documentId);
                    } catch (err) {
                        console.debug('订阅失败:', doc.documentId, err);
                    }
                });
            });

            client.on('message', handleMessage);

            client.on('error', (error) => {
                console.warn('⚠️ WebSocket 连接错误:', error);
            });

            client.on('close', (event) => {
                console.log('🔌 WebSocket 连接关闭:', event?.code);
            });

            client.connect();
            setWsClient(client);

        } catch (error) {
            console.error('❌ WebSocket 初始化失败:', error);
        }

        // 清理函数
        return () => {
            mountedRef.current = false;
            
            // 延迟清理，避免频繁重建
            const cleanupTimer = setTimeout(() => {
                if (clientRef.current && !mountedRef.current) {
                    try {
                        if (clientRef.current.isConnected()) {
                            clientRef.current.unsubscribe();
                        }
                        clientRef.current.close();
                        clientRef.current = null;
                    } catch (error) {
                        // 忽略清理错误
                    }
                }
            }, 200);

            return () => clearTimeout(cleanupTimer);
        };
    }, [documentsList.length, demoMode, handleMessage]); // ⭐ 添加 handleMessage 依赖

    // ⭐ 组件卸载时清理
    useEffect(() => {
        return () => {
            mountedRef.current = false;
            if (clientRef.current) {
                try {
                    clientRef.current.close();
                    clientRef.current = null;
                } catch (error) {
                    // 忽略
                }
            }
        };
    }, []);

    return {
        wsClient,
        documentsProgress
    };
}

export default useWebSocketProgress;

