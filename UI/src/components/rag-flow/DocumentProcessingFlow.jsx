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
    LoadingOutlined,
    ReloadOutlined,
    PlayCircleOutlined
} from '@ant-design/icons';
import WebSocketClient from '../../utils/WebSocketClient';
import { useLanguage } from '../../contexts/LanguageContext';  // ⭐ 导入国际化Hook
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

function DocumentProcessingFlow({ documentId, onComplete, onError, autoStart = false, showDemo = false }) {
    // 国际化 (Internationalization)
    const { t, language } = useLanguage();

    // 状态管理 (State management)
    const [progress, setProgress] = useState(null);
    const [wsClient, setWsClient] = useState(null);
    const [error, setError] = useState(null);
    const [demoMode, setDemoMode] = useState(showDemo);
    const [demoStep, setDemoStep] = useState(0);

    // 演示模式：模拟处理流程 (Demo mode: simulate processing flow)
    useEffect(() => {
        if (demoMode && autoStart) {
            simulateProcessing();
        }
    }, [demoMode, autoStart]);

    const simulateProcessing = () => {
        const stages = [
            { stage: 'UPLOAD', percentage: 0, message: '正在上传文档...', chunks: 0 },
            { stage: 'EXTRACT', percentage: 20, message: '正在提取文本...', chunks: 0 },
            { stage: 'CHUNK', percentage: 40, message: '正在智能分块...', chunks: 15 },
            { stage: 'VECTORIZE', percentage: 60, message: '正在向量化...', chunks: 15 },
            { stage: 'INDEX', percentage: 80, message: '正在建立索引...', chunks: 15 },
            { stage: 'COMPLETED', percentage: 100, message: '处理完成！', chunks: 15 }
        ];

        let currentStep = 0;
        const interval = setInterval(() => {
            if (currentStep < stages.length) {
                setProgress({
                    ...stages[currentStep],
                    documentId: documentId || 'demo',
                    documentName: '示例文档.pdf',
                    status: currentStep === stages.length - 1 ? 'COMPLETED' : 'PROCESSING',
                    startTime: Date.now() - currentStep * 2000,
                    vectors: currentStep * 15
                });
                setDemoStep(currentStep);
                currentStep++;
            } else {
                clearInterval(interval);
                if (onComplete) {
                    onComplete(progress);
                }
            }
        }, 2000); // 每2秒更新一次

        return () => clearInterval(interval);
    };

    // 初始化 WebSocket 连接 (Initialize WebSocket connection)
    useEffect(() => {
        if (!documentId || demoMode) return;

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
            setError(t('ragFlow.messages.wsError'));
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
                setError(message.data.errorMessage || t('ragFlow.messages.processingFailed'));
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
                        {t('ragFlow.component.stepCounter')
                            .replace('{current}', details.currentStepIndex + 1)
                            .replace('{total}', details.totalSteps)}
                    </div>
                )}
                {details.elapsedTimeMs > 0 && (
                    <div className="elapsed-time">
                        {t('ragFlow.component.elapsedTimeLabel')
                            .replace('{time}', (details.elapsedTimeMs / 1000).toFixed(1))}
                    </div>
                )}
            </div>
        );
    }, [progress, t]);

    // 如果没有 documentId 且不是演示模式，显示提示 (Show message if no documentId and not demo mode)
    if (!documentId && !demoMode) {
        return (
            <Alert
                title={t('ragFlow.messages.noDocument')}
                description={t('ragFlow.messages.uploadTip')}
                type="info"
                showIcon
            />
        );
    }

    // 如果有错误，显示错误信息 (Show error if exists)
    if (error) {
        return (
            <Alert
                title={t('ragFlow.messages.processingFailed')}
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
                    <LoadingOutlined spin={progress && (progress.status === 'RUNNING' || progress.status === 'PROCESSING')} />
                    {t('ragFlow.component.title')}
                    {demoMode && <Tag color="blue">演示模式</Tag>}
                </Space>
            }
            className="document-processing-flow"
        >
            {/* 步骤展示 (Steps display) */}
            <Steps
                current={getCurrentStep()}
                status={progress?.status === 'FAILED' ? 'error' : progress?.status === 'COMPLETED' ? 'finish' : 'process'}
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
            {progress && (progress.status === 'RUNNING' || progress.status === 'PROCESSING') && (
                <div className="progress-section">
                    <div className="progress-header">
                        <span className="progress-label">
                            {t('ragFlow.component.currentProgressLabel')}: {STAGE_CONFIG[progress.stage]?.title[language]}
                        </span>
                        <span className="progress-percent">{progress.percentage || progress.progress || 0}%</span>
                    </div>
                    <Progress
                        percent={progress.percentage || progress.progress || 0}
                        status="active"
                        strokeColor={{
                            '0%': STAGE_CONFIG[progress.stage]?.color || '#1890ff',
                            '100%': '#52c41a',
                        }}
                    />
                    {progress.message && (
                        <div className="progress-message">{progress.message}</div>
                    )}
                </div>
            )}

            {/* 文档信息 (Document info) */}
            {progress && (
                <div className="document-info">
                    <Space size="large" wrap>
                        <div>
                            <strong>{t('ragFlow.info.documentName')}:</strong> {progress.documentName || '示例文档.pdf'}
                        </div>
                        {progress.documentId && (
                            <div>
                                <strong>{t('ragFlow.info.documentId')}:</strong> {progress.documentId}
                            </div>
                        )}
                        <div>
                            <strong>{t('ragFlow.info.status')}:</strong>{' '}
                            <Tag color={
                                (progress.status === 'RUNNING' || progress.status === 'PROCESSING') ? 'processing' :
                                progress.status === 'COMPLETED' ? 'success' :
                                'error'
                            }>
                                {t(`ragFlow.status.${(progress.status || 'processing').toLowerCase()}`)}
                            </Tag>
                        </div>
                        {progress.chunks > 0 && (
                            <div>
                                <strong>分块数量:</strong> {progress.chunks}
                            </div>
                        )}
                        {progress.vectors > 0 && (
                            <div>
                                <strong>向量数量:</strong> {progress.vectors}
                            </div>
                        )}
                    </Space>
                </div>
            )}

            {/* 预览内容 (Preview content) */}
            {progress && progress.preview && (
                <div className="preview-section">
                    <h4>{t('ragFlow.info.preview')}</h4>
                    <pre className="preview-content">{progress.preview}</pre>
                </div>
            )}

            {/* 操作按钮 (Action buttons) */}
            <div className="action-buttons">
                {progress && progress.status === 'COMPLETED' && (
                    <Space>
                        <Button type="primary" icon={<CheckCircleOutlined />}>
                            {t('ragFlow.actions.viewResult')}
                        </Button>
                        {demoMode && (
                            <Button icon={<ReloadOutlined />} onClick={() => {
                                setProgress(null);
                                setDemoStep(0);
                                setTimeout(() => simulateProcessing(), 100);
                            }}>
                                重新播放
                            </Button>
                        )}
                    </Space>
                )}
                {demoMode && !progress && (
                    <Button type="primary" icon={<PlayCircleOutlined />} onClick={simulateProcessing}>
                        开始演示
                    </Button>
                )}
            </div>
        </Card>
    );
}

// 导出到全局 (Export to global)
window.DocumentProcessingFlow = DocumentProcessingFlow;

export default DocumentProcessingFlow;

