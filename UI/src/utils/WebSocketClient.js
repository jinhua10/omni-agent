/**
 * WebSocket 客户端工具类
 * (WebSocket Client Utility)
 *
 * 封装 WebSocket 连接和消息处理逻辑
 * (Encapsulates WebSocket connection and message handling logic)
 *
 * @author AI Reviewer Team
 * @since 2.0.0 (Phase 4)
 */

class WebSocketClient {
    constructor(url) {
        this.url = url;
        this.ws = null;
        this.reconnectAttempts = 0;
        this.maxReconnectAttempts = 5;
        this.reconnectDelay = 3000;
        this.isClosing = false; // ⚠️ 防止重复关闭
        this.listeners = {
            open: [],
            message: [],
            error: [],
            close: []
        };
    }

    /**
     * 连接 WebSocket
     * (Connect to WebSocket)
     */
    connect() {
        try {
            this.ws = new WebSocket(this.url);

            // 连接建立 (Connection established)
            this.ws.onopen = (event) => {
                // console.log('✅ WebSocket 连接成功', this.url);
                this.reconnectAttempts = 0;
                this.emit('open', event);
            };

            // 接收消息 (Receive message)
            this.ws.onmessage = (event) => {
                try {
                    const message = JSON.parse(event.data);
                    // console.log('📨 收到 WebSocket 消息:', message);
                    this.emit('message', message);
                } catch (error) {
                    console.error('❌ 解析消息失败:', error);
                }
            };

            // 连接错误 (Connection error)
            this.ws.onerror = (error) => {
                console.debug('WebSocket 错误（可忽略）:', error);
                this.emit('error', error);
            };

            // 连接关闭 (Connection closed)
            this.ws.onclose = (event) => {
                console.debug('🔌 WebSocket 连接关闭', event.code);
                this.emit('close', event);

                // ⭐ 只有在正常关闭（非错误）时才重连，且只重连1次
                // 1000 = 正常关闭，1006 = 连接异常关闭
                if (event.code === 1000 && this.reconnectAttempts < 1) {
                    this.reconnectAttempts++;
                    // console.log(`🔄 尝试重连 (${this.reconnectAttempts}/1)...`);
                    setTimeout(() => this.connect(), 5000); // 延长到5秒
                } else if (event.code !== 1000) {
                    // 异常关闭，不重连，避免频繁错误
                    console.debug('WebSocket 异常关闭，不再重连（后端服务可能未启动）');
                }
            };
        } catch (error) {
            console.error('❌ 创建 WebSocket 连接失败:', error);
        }
    }

    /**
     * 发送消息
     * (Send message)
     */
    send(message) {
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            try {
                const data = typeof message === 'string' ? message : JSON.stringify(message);
                this.ws.send(data);
                // console.log('📤 发送 WebSocket 消息:', message);
            } catch (error) {
                console.warn('⚠️ 发送消息失败:', error.message);
            }
        } else {
            // 降级为 debug 级别，避免控制台过多警告
            console.debug('WebSocket 未连接，无法发送消息');
        }
    }

    /**
     * 订阅文档进度
     * (Subscribe to document progress)
     */
    subscribe(documentId) {
        if (!documentId) {
            console.warn('⚠️ documentId 为空，跳过订阅');
            return;
        }
        this.send({
            action: 'subscribe',
            documentId: documentId
        });
    }

    /**
     * 取消订阅
     * (Unsubscribe)
     */
    unsubscribe() {
        // 只在连接打开时发送取消订阅消息
        if (this.ws && this.ws.readyState === WebSocket.OPEN) {
            this.send({
                action: 'unsubscribe'
            });
        }
    }

    /**
     * 添加事件监听器
     * (Add event listener)
     */
    on(event, callback) {
        if (this.listeners[event]) {
            this.listeners[event].push(callback);
        }
    }

    /**
     * 移除事件监听器
     * (Remove event listener)
     */
    off(event, callback) {
        if (this.listeners[event]) {
            this.listeners[event] = this.listeners[event].filter(cb => cb !== callback);
        }
    }

    /**
     * 触发事件
     * (Emit event)
     */
    emit(event, data) {
        if (this.listeners[event]) {
            this.listeners[event].forEach(callback => {
                try {
                    callback(data);
                } catch (error) {
                    console.error('❌ 事件回调执行失败:', event, error);
                }
            });
        }
    }

    /**
     * 关闭连接
     * (Close connection)
     */
    close() {
        // 防止重复关闭
        if (this.isClosing || !this.ws) {
            return;
        }

        this.isClosing = true;
        this.reconnectAttempts = this.maxReconnectAttempts; // 阻止自动重连

        try {
            // 只关闭已建立或正在连接的WebSocket
            if (this.ws.readyState === WebSocket.OPEN) {
                this.ws.close(1000, 'Client closing'); // 正常关闭
            } else if (this.ws.readyState === WebSocket.CONNECTING) {
                // 如果正在连接，等待一下再关闭
                setTimeout(() => {
                    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
                        this.ws.close(1000, 'Client closing');
                    }
                }, 100);
            }
        } catch (error) {
            console.debug('关闭 WebSocket 时出错（可忽略）:', error.message);
        } finally {
            this.ws = null;
            setTimeout(() => {
                this.isClosing = false; // 重置关闭标志
            }, 500);
        }
    }

    /**
     * 检查连接状态
     * (Check connection status)
     */
    isConnected() {
        return this.ws && this.ws.readyState === WebSocket.OPEN;
    }
}

export default WebSocketClient;

