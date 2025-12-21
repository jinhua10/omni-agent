/**
 * 文档处理流程组件
 * (Document Processing Flow Component)
 *
 * 实时展示文档处理的完整流程（上传→提取→分块→向量化→索引）
 * (Real-time display of complete document processing flow)
 *
 * @author AI Reviewer Team
 * @since 2.0.0 (Phase 4)
 */

import React, { useState, useEffect, useCallback } from 'react';
import { Steps, Card, Progress, Alert, Button, Tag, Space, Divider } from 'antd';
import {
    FileAddOutlined,
    FileTextOutlined,
    ScissorOutlined,
    FunctionOutlined,
    DatabaseOutlined,
    CheckCircleOutlined,
    CloseCircleOutlined,
    LoadingOutlined
} from '@ant-design/icons';
import WebSocketClient from '../../utils/WebSocketClient';
import './DocumentProcessingFlow.css';

/**
 * 处理阶段配置
 * (Processing stage configuration)
 */
const STAGE_CONFIG = {
    UPLOAD: {
        icon: <FileAddOutlined />,
        title: { zh: '文档上传', en: 'Document Upload' },
        color: '#1890ff'
    },
    EXTRACT: {
        icon: <FileTextOutlined />,
        title: { zh: '文本提取', en: 'Text Extraction' },
        color: '#52c41a'
    },
    CHUNK: {
        icon: <ScissorOutlined />,
        title: { zh: '智能分块', en: 'Smart Chunking' },
        color: '#faad14'
    },
    VECTORIZE: {
        icon: <FunctionOutlined />,
        title: { zh: '向量化', en: 'Vectorization' },
        color: '#722ed1'
    },
    INDEX: {
        icon: <DatabaseOutlined />,
        title: { zh: '索引存储', en: 'Index Storage' },
        color: '#eb2f96'
    },
    COMPLETED: {
        icon: <CheckCircleOutlined />,
        title: { zh: '处理完成', en: 'Completed' },
        color: '#52c41a'
    }
};

function DocumentProcessingFlow({ documentId, onComplete, onError }) {
    // 状态管理 (State management)
    const [progress, setProgress] = useState(null);
    const [wsClient, setWsClient] = useState(null);
    const [error, setError] = useState(null);
    const [language, setLanguage] = useState('zh');

    // 初始化 WebSocket 连接 (Initialize WebSocket connection)
    useEffect(() => {
        if (!documentId) return;

        // 创建 WebSocket 客户端 (Create WebSocket client)
        const client = new WebSocketClient('ws://localhost:8080/ws/progress');

        // 监听连接建立 (Listen for connection established)
        client.on('open', () => {
            // 订阅文档进度 (Subscribe to document progress)
            client.subscribe(documentId);
        });

        // 监听进度更新 (Listen for progress updates)
        client.on('message', handleMessage);

        // 监听错误 (Listen for errors)
        client.on('error', (error) => {
            console.error('WebSocket error:', error);
            setError('WebSocket连接错误');
            if (onError) onError(error);
        });

        // 连接 WebSocket (Connect WebSocket)
        client.connect();

        setWsClient(client);

        // 清理函数 (Cleanup function)
        return () => {
            if (client) {
                client.unsubscribe();
                client.close();
            }
        };
    }, [documentId]);

    /**
     * 处理 WebSocket 消息
     * (Handle WebSocket message)
     */
    const handleMessage = useCallback((message) => {
        if (message.type === 'progress') {
            setProgress(message.data);

            // 如果完成，通知父组件 (Notify parent if completed)
            if (message.data.status === 'COMPLETED' && onComplete) {
                onComplete(message.data);
            }

            // 如果失败，通知父组件 (Notify parent if failed)
            if (message.data.status === 'FAILED') {
                setError(message.data.errorMessage || '处理失败');
                if (onError) onError(message.data);
            }
        } else if (message.type === 'error') {
            setError(message.message);
            if (onError) onError(message);
        }
    }, [onComplete, onError]);

    /**
     * 获取当前步骤索引
     * (Get current step index)
     */
    const getCurrentStep = useCallback(() => {
        if (!progress) return 0;

        const stages = ['UPLOAD', 'EXTRACT', 'CHUNK', 'VECTORIZE', 'INDEX', 'COMPLETED'];
        const index = stages.indexOf(progress.stage);
        return index >= 0 ? index : 0;
    }, [progress]);

    /**
     * 获取步骤状态
     * (Get step status)
     */
    const getStepStatus = useCallback((stepIndex) => {
        if (!progress) return 'wait';

        const currentStep = getCurrentStep();

        if (progress.status === 'FAILED') {
            return stepIndex === currentStep ? 'error' : stepIndex < currentStep ? 'finish' : 'wait';
        }

        if (stepIndex < currentStep) return 'finish';
        if (stepIndex === currentStep) return 'process';
        return 'wait';
    }, [progress, getCurrentStep]);

    /**
     * 渲染步骤描述
     * (Render step description)
     */
    const renderStepDescription = useCallback((stage) => {
        if (!progress || progress.stage !== stage) return null;

        const details = progress.details;
        if (!details) return null;

        return (
            <div className="step-description">
                {details.currentStep && (
                    <div className="current-step">{details.currentStep}</div>
                )}
                {details.totalSteps > 0 && (
                    <div className="step-counter">
                        步骤 {details.currentStepIndex + 1} / {details.totalSteps}
                    </div>
                )}
                {details.elapsedTimeMs > 0 && (
                    <div className="elapsed-time">
                        已耗时: {(details.elapsedTimeMs / 1000).toFixed(1)}s
                    </div>
                )}
            </div>
        );
    }, [progress]);

    // 如果没有 documentId，显示提示 (Show message if no documentId)
    if (!documentId) {
        return (
            <Alert
                message="请选择要处理的文档"
                description="上传文档后将自动开始处理流程"
                type="info"
                showIcon
            />
        );
    }

    // 如果有错误，显示错误信息 (Show error if exists)
    if (error) {
        return (
            <Alert
                message="处理失败"
                description={error}
                type="error"
                showIcon
                closable
                onClose={() => setError(null)}
            />
        );
    }

    return (
        <Card
            title={
                <Space>
                    <LoadingOutlined spin={progress && progress.status === 'RUNNING'} />
                    文档处理流程
                </Space>
            }
            className="document-processing-flow"
        >
            {/* 步骤展示 (Steps display) */}
            <Steps
                current={getCurrentStep()}
                status={progress?.status === 'FAILED' ? 'error' : 'process'}
                items={[
                    {
                        title: STAGE_CONFIG.UPLOAD.title[language],
                        icon: STAGE_CONFIG.UPLOAD.icon,
                        status: getStepStatus(0),
                        description: renderStepDescription('UPLOAD')
                    },
                    {
                        title: STAGE_CONFIG.EXTRACT.title[language],
                        icon: STAGE_CONFIG.EXTRACT.icon,
                        status: getStepStatus(1),
                        description: renderStepDescription('EXTRACT')
                    },
                    {
                        title: STAGE_CONFIG.CHUNK.title[language],
                        icon: STAGE_CONFIG.CHUNK.icon,
                        status: getStepStatus(2),
                        description: renderStepDescription('CHUNK')
                    },
                    {
                        title: STAGE_CONFIG.VECTORIZE.title[language],
                        icon: STAGE_CONFIG.VECTORIZE.icon,
                        status: getStepStatus(3),
                        description: renderStepDescription('VECTORIZE')
                    },
                    {
                        title: STAGE_CONFIG.INDEX.title[language],
                        icon: STAGE_CONFIG.INDEX.icon,
                        status: getStepStatus(4),
                        description: renderStepDescription('INDEX')
                    }
                ]}
            />

            <Divider />

            {/* 进度条 (Progress bar) */}
            {progress && progress.status === 'RUNNING' && (
                <div className="progress-section">
                    <div className="progress-header">
                        <span className="progress-label">
                            当前进度: {STAGE_CONFIG[progress.stage]?.title[language]}
                        </span>
                        <span className="progress-percent">{progress.progress}%</span>
                    </div>
                    <Progress
                        percent={progress.progress}
                        status="active"
                        strokeColor={{
                            '0%': STAGE_CONFIG[progress.stage]?.color || '#1890ff',
                            '100%': '#52c41a',
                        }}
                    />
                </div>
            )}

            {/* 文档信息 (Document info) */}
            {progress && (
                <div className="document-info">
                    <Space size="large">
                        <div>
                            <strong>文档名称:</strong> {progress.documentName}
                        </div>
                        <div>
                            <strong>文档ID:</strong> {progress.documentId}
                        </div>
                        <div>
                            <strong>状态:</strong>{' '}
                            <Tag color={
                                progress.status === 'RUNNING' ? 'processing' :
                                progress.status === 'COMPLETED' ? 'success' :
                                'error'
                            }>
                                {progress.status}
                            </Tag>
                        </div>
                    </Space>
                </div>
            )}

            {/* 预览内容 (Preview content) */}
            {progress && progress.preview && (
                <div className="preview-section">
                    <h4>预览</h4>
                    <pre className="preview-content">{progress.preview}</pre>
                </div>
            )}

            {/* 操作按钮 (Action buttons) */}
            {progress && progress.status === 'COMPLETED' && (
                <div className="action-buttons">
                    <Button type="primary" icon={<CheckCircleOutlined />}>
                        查看结果
                    </Button>
                </div>
            )}
        </Card>
    );
}

// 导出到全局 (Export to global)
window.DocumentProcessingFlow = DocumentProcessingFlow;

export default DocumentProcessingFlow;

