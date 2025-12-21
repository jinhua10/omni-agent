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
                console.log('✅ WebSocket 连接成功', this.url);
                this.reconnectAttempts = 0;
                this.emit('open', event);
            };

            // 接收消息 (Receive message)
            this.ws.onmessage = (event) => {
                try {
                    const message = JSON.parse(event.data);
                    console.log('📨 收到 WebSocket 消息:', message);
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
                console.log('🔌 WebSocket 连接关闭', event.code, event.reason);
                this.emit('close', event);

                // 自动重连 (Auto reconnect)
                if (this.reconnectAttempts < this.maxReconnectAttempts) {
                    this.reconnectAttempts++;
                    console.log(`🔄 尝试重连 (${this.reconnectAttempts}/${this.maxReconnectAttempts})...`);
                    setTimeout(() => this.connect(), this.reconnectDelay);
                } else {
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
            const data = typeof message === 'string' ? message : JSON.stringify(message);
            this.ws.send(data);
            console.log('📤 发送 WebSocket 消息:', message);
        } else {
            console.warn('⚠️ WebSocket 未连接，无法发送消息');
        }
    }

    /**
     * 订阅文档进度
     * (Subscribe to document progress)
     */
    subscribe(documentId) {
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
        this.send({
            action: 'unsubscribe'
        });
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
            this.listeners[event].forEach(callback => callback(data));
        }
    }

    /**
     * 关闭连接
     * (Close connection)
     */
    close() {
        if (this.ws) {
            this.reconnectAttempts = this.maxReconnectAttempts; // 阻止自动重连
            this.ws.close();
            console.log('🔌 主动关闭 WebSocket 连接');
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

