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
                console.error('❌ WebSocket 错误:', error);
                this.emit('error', error);
            };

            // 连接关闭 (Connection closed)
            this.ws.onclose = (event) => {
                console.debug('🔌 WebSocket 连接关闭', event.code);
                this.emit('close', event);

                // ⭐ 暂时禁用自动重连，避免控制台错误刷屏
                // TODO: 当后端 WebSocket 服务稳定后再启用
                const ENABLE_AUTO_RECONNECT = false;

                if (ENABLE_AUTO_RECONNECT && this.reconnectAttempts < this.maxReconnectAttempts) {
                    this.reconnectAttempts++;
                    console.log(`🔄 尝试重连 (${this.reconnectAttempts}/${this.maxReconnectAttempts})...`);
                    setTimeout(() => this.connect(), this.reconnectDelay);
                } else if (ENABLE_AUTO_RECONNECT) {
                    console.log('❌ 达到最大重连次数，停止重连');
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
        if (this.ws) {
            try {
                this.reconnectAttempts = this.maxReconnectAttempts; // 阻止自动重连
                if (this.ws.readyState === WebSocket.OPEN || this.ws.readyState === WebSocket.CONNECTING) {
                    this.ws.close();
                }
                this.ws = null;
                // console.log('🔌 主动关闭 WebSocket 连接');
            } catch (error) {
                console.debug('关闭 WebSocket 时出错（可忽略）:', error.message);
            }
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

