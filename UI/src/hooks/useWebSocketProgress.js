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
    const reconnectAttempts = useRef(0); // ⭐ 重连尝试次数
    const MAX_RECONNECT_ATTEMPTS = 3; // ⭐ 最大重连次数
    const onProgressUpdateRef = useRef(onProgressUpdate); // ⭐ 使用 ref 保存回调

    // 更新回调引用
    useEffect(() => {
        onProgressUpdateRef.current = onProgressUpdate;
    }, [onProgressUpdate]);

    // 处理 WebSocket 消息
    const handleMessage = useCallback((message) => {
        console.log('📨 收到 WebSocket 消息:', message);

        if (message.type === 'progress') {
            const progressData = message.data;
            const docId = progressData.documentId;

            console.log('📊 处理进度数据:', {
                docId,
                stage: progressData.stage,
                percentage: progressData.percentage,
                status: progressData.status
            });

            // 通知父组件
            if (onProgressUpdateRef.current) {
                onProgressUpdateRef.current(progressData);
            }

            // 更新文档列表中该文档的进度
            setDocumentsProgress(prev => {
                const updated = {
                    ...prev,
                    [docId]: {
                        stage: progressData.stage,
                        percentage: progressData.percentage,
                        message: progressData.message,
                        status: progressData.status
                    }
                };
                console.log('✅ documentsProgress 已更新:', updated);
                return updated;
            });

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
    }, []); // ⭐ 空依赖数组，使用 ref 访问最新的回调

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

        // ⭐ 检查重连次数限制
        if (reconnectAttempts.current >= MAX_RECONNECT_ATTEMPTS) {
            console.error('❌ WebSocket 重连次数超过限制，停止尝试');
            return;
        }

        // ⭐ 只在第一次初始化时输出日志
        if (!isInitialized.current) {
            console.log('📡 建立 WebSocket 连接');
            isInitialized.current = true;
        }

        // ⭐ 建立 WebSocket 连接
        try {
            // ...existing code...
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
                reconnectAttempts.current = 0; // ⭐ 连接成功，重置计数器

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

            client.on('message', (msg) => {
                console.log('🔔 WebSocket 原始消息:', msg);
                handleMessage(msg);
            });

            client.on('error', (error) => {
                reconnectAttempts.current++; // ⭐ 错误时增加计数
                console.warn(`⚠️ WebSocket 连接错误 (尝试 ${reconnectAttempts.current}/${MAX_RECONNECT_ATTEMPTS}):`, error);
            });

            client.on('close', (event) => {
                console.log('🔌 WebSocket 连接关闭:', event?.code);
                if (reconnectAttempts.current >= MAX_RECONNECT_ATTEMPTS) {
                    console.error('❌ 达到最大重连次数，不再尝试重连');
                }
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
    }, [documentsList.length, demoMode]); // ⭐ 只依赖长度和 demoMode，避免频繁重建

    // ⭐ 组件卸载时清理
    useEffect(() => {
        return () => {
            mountedRef.current = false;
            if (clientRef.current) {
                try {
                    clientRef.current.close();
                    clientRef.current = null;
                    reconnectAttempts.current = 0; // ⭐ 重置计数器
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

