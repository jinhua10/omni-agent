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
import { Steps, Card, Progress, Alert, Button, Tag, Space, Divider, Dropdown, Spin, Select, App, Modal, Input } from 'antd';
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
    PlayCircleOutlined,
    SettingOutlined,
    DownOutlined,
    SyncOutlined,
    LeftOutlined,
    RightOutlined,
    ThunderboltOutlined,
    SaveOutlined,
    PlusOutlined,
    DeleteOutlined
} from '@ant-design/icons';
import WebSocketClient from '../../utils/WebSocketClient';
import { useLanguage } from '../../contexts/LanguageContext';
import ragStrategyApi from '../../api/modules/ragStrategy';
import '../../assets/css/rag-flow/DocumentProcessingFlow.css';

const { Option } = Select;
const { TextArea } = Input;

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
    const { message } = App.useApp();

    // 状态管理 (State management)
    const [progress, setProgress] = useState(null);
    const [wsClient, setWsClient] = useState(null);
    const [error, setError] = useState(null);
    const [demoMode, setDemoMode] = useState(showDemo);
    const [demoStep, setDemoStep] = useState(0);
    const [demoExpanded, setDemoExpanded] = useState(false);
    const [documentsList, setDocumentsList] = useState([]);
    const [loading, setLoading] = useState(false);
    const [selectedDocId, setSelectedDocId] = useState(documentId);
    
    // 策略模板管理 (Strategy Template Management - 从后端加载)
    const [strategyTemplates, setStrategyTemplates] = useState([]);
    const [templateModalVisible, setTemplateModalVisible] = useState(false);
    const [newTemplateName, setNewTemplateName] = useState('');
    const [newTemplateDesc, setNewTemplateDesc] = useState('');
    const [templatesLoading, setTemplatesLoading] = useState(false);

    // 加载策略模板列表
    const loadTemplates = useCallback(async () => {
        setTemplatesLoading(true);
        try {
            const result = await ragStrategyApi.getTemplates();
            if (result.success) {
                setStrategyTemplates(result.data || []);
            } else {
                console.error('加载策略模板失败:', result.message);
            }
        } catch (error) {
            console.error('加载策略模板失败:', error);
        } finally {
            setTemplatesLoading(false);
        }
    }, []);

    // 删除策略模板
    const deleteTemplate = useCallback(async (templateId) => {
        try {
            const result = await ragStrategyApi.deleteTemplate(templateId);
            if (result.success) {
                message.success('模板已删除');
                loadTemplates(); // 重新加载列表
            } else {
                message.error(result.message || '删除失败');
            }
        } catch (error) {
            console.error('删除模板失败:', error);
            message.error('删除失败: ' + error.message);
        }
    }, [message, loadTemplates]);

    // 应用策略模板到文档
    const applyTemplateToDocument = useCallback(async (docId, templateId) => {
        try {
            const result = await ragStrategyApi.applyTemplateToDocument(docId, templateId);
            if (result.success) {
                message.success('策略模板已应用');
                loadDocumentsList(); // 刷新文档列表
            } else {
                message.error(result.message || '应用失败');
            }
        } catch (error) {
            console.error('应用模板失败:', error);
            message.error('应用失败: ' + error.message);
        }
    }, [message]);

    // 保存当前配置为模板
    const saveCurrentAsTemplate = useCallback(async () => {
        if (!selectedDocId) {
            message.warning('请先选择文档');
            return;
        }
        if (!newTemplateName.trim()) {
            message.warning('请输入模板名称');
            return;
        }

        try {
            const result = await ragStrategyApi.saveCurrentAsTemplate(selectedDocId, {
                name: newTemplateName.trim(),
                description: newTemplateDesc.trim()
            });
            
            if (result.success) {
                message.success('策略模板已保存');
                setTemplateModalVisible(false);
                setNewTemplateName('');
                setNewTemplateDesc('');
                loadTemplates(); // 重新加载模板列表
            } else {
                message.error(result.message || '保存失败');
            }
        } catch (error) {
            console.error('保存模板失败:', error);
            message.error('保存失败: ' + error.message);
        }
    }, [selectedDocId, newTemplateName, newTemplateDesc, message, loadTemplates]);

    // 开始处理文档
    const startProcessDocument = useCallback(async (docId) => {
        try {
            const result = await ragStrategyApi.startProcessing(docId);
            if (result.success) {
                message.success('开始处理文档：' + docId);
                loadDocumentsList(); // 刷新文档列表
            } else {
                message.error(result.message || '处理失败');
            }
        } catch (error) {
            console.error('开始处理失败:', error);
            message.error('处理失败: ' + error.message);
        }
    }, [message]);

    // 加载文档列表
    const loadDocumentsList = useCallback(async () => {
        setLoading(true);
        try {
            const response = await fetch('/api/system/rag-config/documents-status');
            const result = await response.json();
            if (result.success) {
                const docs = Object.values(result.data);
                setDocumentsList(docs);
                console.log('📋 加载文档列表:', docs.length, '个', docs);
            } else {
                console.error('加载文档列表失败:', result.message);
            }
        } catch (error) {
            console.error('加载文档列表失败:', error);
        } finally {
            setLoading(false);
        }
    }, []);

    // 初始加载
    useEffect(() => {
        loadDocumentsList();
        loadTemplates(); // 加载策略模板列表
    }, [loadDocumentsList, loadTemplates]);

    // 当选择文档时，根据文档状态初始化progress
    useEffect(() => {
        if (selectedDocId && documentsList.length > 0) {
            const doc = documentsList.find(d => d.documentId === selectedDocId);
            if (doc) {
                // 根据文档状态设置对应的处理阶段
                let stage = 'UPLOAD';
                let percentage = 0;
                
                if (doc.status === 'COMPLETED') {
                    stage = 'COMPLETED';
                    percentage = 100;
                } else if (doc.status === 'PENDING') {
                    // PENDING状态表示已上传但未处理，应该在UPLOAD之后
                    stage = 'EXTRACT';
                    percentage = 20;
                } else if (doc.status === 'PROCESSING') {
                    // 如果有currentStage信息，使用它
                    stage = doc.currentStage || 'CHUNK';
                    percentage = 50;
                }
                
                setProgress({
                    documentId: doc.documentId,
                    documentName: doc.documentId,
                    stage: stage,
                    status: doc.status,
                    percentage: percentage,
                    message: `当前阶段: ${STAGE_CONFIG[stage]?.title[language] || stage}`,
                    chunks: doc.chunks || 0,
                    vectors: doc.vectors || 0,
                    startTime: doc.createdAt
                });
            }
        }
    }, [selectedDocId, documentsList, language]);

    // 演示模式：模拟处理流程 (Demo mode: simulate processing flow)
    useEffect(() => {
        if (demoMode && autoStart && demoExpanded) {
            simulateProcessing();
        }
    }, [demoMode, autoStart, demoExpanded]);

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

        // 暂时禁用WebSocket，因为后端还未实现
        // TODO: 当后端WebSocket服务实现后再启用
        console.log('📡 WebSocket功能暂时禁用，等待后端实现');
        return;

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
    }, [documentId, demoMode]);

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
        <div className="document-processing-flow-container">
            {/* 顶部操作栏 - 始终可见 */}
            <div style={{ marginBottom: 16, display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                <Space>
                    <LoadingOutlined spin={progress && (progress.status === 'RUNNING' || progress.status === 'PROCESSING')} />
                    <span style={{ fontSize: 16, fontWeight: 500 }}>{t('ragFlow.component.title')}</span>
                    {demoMode && <Tag color="blue">{t('ragFlow.component.demoMode')}</Tag>}
                </Space>
                <Space>
                    <Button
                        icon={<SyncOutlined spin={loading} />}
                        onClick={loadDocumentsList}
                        loading={loading}
                    >
                        {t('ragFlow.component.refresh')}
                    </Button>
                    {demoMode && !demoExpanded && (
                        <Button
                            type="primary"
                            icon={<PlayCircleOutlined />}
                            onClick={() => setDemoExpanded(true)}
                        >
                            {t('ragFlow.component.viewDemo')}
                        </Button>
                    )}
                </Space>
            </div>

            {/* 文档列表 - 独立显示，不在Card里 */}
            {documentsList && documentsList.length > 0 ? (
                <Card
                    title={t('ragFlow.component.pendingDocuments')}
                    size="small"
                    style={{ marginBottom: 16 }}
                >
                    <div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
                        {documentsList.map((doc) => (
                            <div
                                key={doc.documentId}
                                style={{
                                    background: selectedDocId === doc.documentId 
                                        ? 'linear-gradient(135deg, #e6f7ff 0%, #bae7ff 100%)' 
                                        : '#fafafa',
                                    border: selectedDocId === doc.documentId ? '2px solid #1890ff' : '1px solid #e8e8e8',
                                    padding: '16px',
                                    borderRadius: '8px',
                                    transition: 'all 0.3s ease',
                                    boxShadow: selectedDocId === doc.documentId ? '0 2px 8px rgba(24, 144, 255, 0.15)' : 'none'
                                }}
                            >
                                <div
                                    onClick={() => {
                                        setSelectedDocId(doc.documentId);
                                    }}
                                    style={{
                                        cursor: 'pointer',
                                        marginBottom: '12px'
                                    }}
                                >
                                    <Space>
                                        <FileTextOutlined />
                                        <span style={{ fontWeight: 500, color: '#262626' }}>{doc.documentId}</span>
                                        <Tag color={
                                            doc.status === 'PENDING' ? 'orange' :
                                            doc.status === 'COMPLETED' ? 'green' :
                                            doc.status === 'FAILED' ? 'red' :
                                            'blue'
                                        }>
                                            {t(`ragFlow.status.${(doc.status || 'pending').toLowerCase()}`)}
                                        </Tag>
                                        {selectedDocId === doc.documentId && (
                                            <Tag color="blue" icon={<CheckCircleOutlined />}>已选中</Tag>
                                        )}
                                    </Space>
                                    <div style={{ color: '#8c8c8c', fontSize: '12px', marginTop: '4px' }}>
                                        {t('ragFlow.component.createdAt')}: {new Date(doc.createdAt).toLocaleString()}
                                    </div>
                                </div>

                                {/* 快速处理操作栏 */}
                                {doc.status === 'PENDING' && (
                                    <div style={{
                                        borderTop: '1px solid #f0f0f0',
                                        paddingTop: '12px',
                                        display: 'flex',
                                        gap: '8px',
                                        alignItems: 'center'
                                    }}>
                                        <Select
                                            placeholder="选择策略模板"
                                            style={{ flex: 1 }}
                                            size="small"
                                            popupRender={(menu) => (
                                                <>
                                                    {menu}
                                                    <Divider style={{ margin: '8px 0' }} />
                                                    <Space style={{ padding: '0 8px 4px' }}>
                                                        <Button 
                                                            type="text" 
                                                            icon={<PlusOutlined />} 
                                                            onClick={() => {
                                                                setSelectedDocId(doc.documentId);
                                                                setTemplateModalVisible(true);
                                                            }}
                                                            size="small"
                                                        >
                                                            新建模板
                                                        </Button>
                                                    </Space>
                                                </>
                                            )}
                                            onChange={(templateId) => {
                                                applyTemplateToDocument(doc.documentId, templateId);
                                            }}
                                        >
                                            {strategyTemplates.map(template => (
                                                <Option key={template.id} value={template.id}>
                                                    <Space>
                                                        {template.name}
                                                        {template.description && (
                                                            <span style={{ fontSize: '12px', color: '#999' }}>
                                                                ({template.description})
                                                            </span>
                                                        )}
                                                        {!template.builtin && (
                                                            <DeleteOutlined 
                                                                style={{ color: '#ff4d4f', fontSize: '12px' }}
                                                                onClick={(e) => {
                                                                    e.stopPropagation();
                                                                    deleteTemplate(template.id);
                                                                }}
                                                            />
                                                        )}
                                                    </Space>
                                                </Option>
                                            ))}
                                        </Select>
                                        <Button
                                            icon={<SaveOutlined />}
                                            size="small"
                                            onClick={() => {
                                                setSelectedDocId(doc.documentId);
                                                setTemplateModalVisible(true);
                                            }}
                                            title="将当前配置保存为模板"
                                        >
                                            保存为模板
                                        </Button>
                                        <Button
                                            type="primary"
                                            size="small"
                                            onClick={() => {
                                                startProcessDocument(doc.documentId);
                                            }}
                                        >
                                            开始处理
                                        </Button>
                                    </div>
                                )}
                            </div>
                        ))}
                    </div>
                </Card>
            ) : null}

            {/* 无文档提示 */}
            {!loading && documentsList.length === 0 && (
                <Alert
                    title={t('ragFlow.component.noDocuments')}
                    description={t('ragFlow.component.noDocumentsDesc')}
                    type="info"
                    showIcon
                    style={{ marginBottom: 16 }}
                    action={
                        <Button type="primary" icon={<PlayCircleOutlined />} onClick={() => {
                            setDemoMode(true);
                            setDemoExpanded(true);
                        }}>
                            {t('ragFlow.component.viewDemo')}
                        </Button>
                    }
                />
            )}

            {/* 演示模式提示 */}
            {demoMode && !demoExpanded && (
                <Alert
                    title={t('ragFlow.component.demoMode')}
                    description={
                        <Space vertical style={{ width: '100%' }}>
                            <span>{t('ragFlow.component.demoModeDesc')}</span>
                            <Button
                                type="primary"
                                icon={<PlayCircleOutlined />}
                                onClick={() => setDemoExpanded(true)}
                            >
                                {t('ragFlow.component.viewDemoFlow')}
                            </Button>
                        </Space>
                    }
                    type="info"
                    showIcon
                    style={{ marginBottom: 16 }}
                />
            )}

            {/* 处理流程Card - 只在有选中文档或进度时显示 */}
            {(selectedDocId || progress || (demoMode && demoExpanded)) && (
            <Card
                className="document-processing-flow"
                title={
                    selectedDocId ? (
                        <Space>
                            <FileTextOutlined />
                            <span>文档处理流程：{selectedDocId}</span>
                        </Space>
                    ) : demoMode ? (
                        <Space>
                            <PlayCircleOutlined />
                            <span>演示模式</span>
                        </Space>
                    ) : null
                }
            >
                {/* 步骤展示 (Steps display) - 可点击跳转到对应配置 */}
                <Steps
                    current={getCurrentStep()}
                    status={progress?.status === 'FAILED' ? 'error' : progress?.status === 'COMPLETED' ? 'finish' : 'process'}
                    size="default"
                    style={{
                        marginBottom: '32px',
                        padding: '24px'
                    }}
                    items={[
                    {
                        title: STAGE_CONFIG.UPLOAD.title[language],
                        icon: STAGE_CONFIG.UPLOAD.icon,
                        status: getStepStatus(0),
                        content: renderStepDescription('UPLOAD')
                    },
                    {
                        title: (
                            <a onClick={() => {
                                if (selectedDocId) {
                                    window.location.hash = `#/documents?view=textExtraction&docId=${selectedDocId}`;
                                }
                            }} style={{ cursor: selectedDocId ? 'pointer' : 'default' }}>
                                {STAGE_CONFIG.EXTRACT.title[language]}
                            </a>
                        ),
                        icon: STAGE_CONFIG.EXTRACT.icon,
                        status: getStepStatus(1),
                        content: (
                            <div>
                                {renderStepDescription('EXTRACT')}
                                {selectedDocId && (
                                    <div style={{ marginTop: 4, fontSize: 12, color: '#1890ff' }}>
                                        <SettingOutlined /> {t('ragFlow.component.clickToConfigExtract')}
                                    </div>
                                )}
                            </div>
                        )
                    },
                    {
                        title: (
                            <a onClick={() => {
                                if (selectedDocId) {
                                    window.location.hash = `#/documents?view=chunking&docId=${selectedDocId}`;
                                }
                            }} style={{ cursor: selectedDocId ? 'pointer' : 'default' }}>
                                {STAGE_CONFIG.CHUNK.title[language]}
                            </a>
                        ),
                        icon: STAGE_CONFIG.CHUNK.icon,
                        status: getStepStatus(2),
                        content: (
                            <div>
                                {renderStepDescription('CHUNK')}
                                {selectedDocId && (
                                    <div style={{ marginTop: 4, fontSize: 12, color: '#1890ff' }}>
                                        <SettingOutlined /> {t('ragFlow.component.clickToConfigChunk')}
                                    </div>
                                )}
                            </div>
                        )
                    },
                    {
                        title: STAGE_CONFIG.VECTORIZE.title[language],
                        icon: STAGE_CONFIG.VECTORIZE.icon,
                        status: getStepStatus(3),
                        content: renderStepDescription('VECTORIZE')
                    },
                    {
                        title: STAGE_CONFIG.INDEX.title[language],
                        icon: STAGE_CONFIG.INDEX.icon,
                        status: getStepStatus(4),
                        content: renderStepDescription('INDEX')
                    }
                ]}
            />

            {/* 流程控制按钮 */}
            {selectedDocId && (
                <div style={{
                    marginTop: '24px',
                    padding: '20px',
                    background: 'linear-gradient(135deg, #f0f5ff 0%, #e6f7ff 100%)',
                    borderRadius: '8px',
                    border: '1px solid #d6e4ff',
                    display: 'flex',
                    justifyContent: 'space-between',
                    alignItems: 'center',
                    boxShadow: '0 2px 4px rgba(0, 0, 0, 0.02)'
                }}>
                    <Space>
                        <Button
                            icon={<LeftOutlined />}
                            onClick={() => {
                                const currentStep = getCurrentStep();
                                if (currentStep === 1) {
                                    // 从文本提取回到上传
                                    window.location.hash = '#/documents?view=flow';
                                } else if (currentStep === 2) {
                                    // 从分块回到文本提取
                                    window.location.hash = `#/documents?view=textExtraction&docId=${selectedDocId}`;
                                }
                            }}
                            disabled={getCurrentStep() === 0}
                        >
                            上一步
                        </Button>
                        <Button
                            icon={<RightOutlined />}
                            onClick={() => {
                                const currentStep = getCurrentStep();
                                if (currentStep === 0) {
                                    // 从上传到文本提取
                                    window.location.hash = `#/documents?view=textExtraction&docId=${selectedDocId}`;
                                } else if (currentStep === 1) {
                                    // 从文本提取到分块
                                    window.location.hash = `#/documents?view=chunking&docId=${selectedDocId}`;
                                }
                            }}
                            disabled={getCurrentStep() >= 2}
                        >
                            下一步
                        </Button>
                    </Space>

                    <Button
                        type="primary"
                        size="large"
                        icon={<ThunderboltOutlined />}
                        onClick={async () => {
                            try {
                                // TODO: 触发完整的处理流程
                                message.success('开始处理文档：' + selectedDocId);
                            } catch (error) {
                                message.error('处理失败：' + error.message);
                            }
                        }}
                    >
                        开始完整处理
                    </Button>
                </div>
            )}
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
                                <strong>{t('ragFlow.component.chunkCount')}:</strong> {progress.chunks}
                            </div>
                        )}
                        {progress.vectors > 0 && (
                            <div>
                                <strong>{t('ragFlow.component.vectorCount')}:</strong> {progress.vectors}
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
                        <Dropdown
                            menu={{
                                items: [
                                    {
                                        key: 'textExtraction',
                                        icon: <FileTextOutlined />,
                                        label: t('ragFlow.actions.configureExtraction'),
                                        onClick: () => {
                                            // 跳转到文本提取配置
                                            window.location.hash = '#/documents?view=textExtraction&docId=' + progress.documentId;
                                        }
                                    },
                                    {
                                        key: 'chunking',
                                        icon: <ScissorOutlined />,
                                        label: t('ragFlow.actions.configureChunking'),
                                        onClick: () => {
                                            // 跳转到分块配置
                                            window.location.hash = '#/documents?view=chunking&docId=' + progress.documentId;
                                        }
                                    },
                                    {
                                        key: 'rebuild',
                                        icon: <ReloadOutlined />,
                                        label: t('ragFlow.actions.rebuildDocument'),
                                        onClick: () => {
                                            // 触发重建
                                            if (confirm(t('ragFlow.actions.confirmRebuild'))) {
                                                // TODO: 调用重建API
                                                console.log('重建文档:', progress.documentId);
                                            }
                                        }
                                    },
                                    {
                                        type: 'divider'
                                    },
                                    {
                                        key: 'viewChunks',
                                        icon: <DatabaseOutlined />,
                                        label: t('ragFlow.actions.viewChunks'),
                                        onClick: () => {
                                            // 跳转到浏览器视图查看分块
                                            window.location.hash = '#/documents?view=browser&docId=' + progress.documentId;
                                        }
                                    }
                                ]
                            }}
                            placement="bottomLeft"
                        >
                            <Button type="primary" icon={<SettingOutlined />}>
                                {t('ragFlow.actions.processingOptions')} <DownOutlined />
                            </Button>
                        </Dropdown>
                        {demoMode && (
                            <Button icon={<ReloadOutlined />} onClick={() => {
                                setProgress(null);
                                setDemoStep(0);
                                setDemoExpanded(false);
                            }}>
                                {t('ragFlow.actions.collapseDemo')}
                            </Button>
                        )}
                    </Space>
                )}
                {demoMode && !progress && demoExpanded && (
                    <Button type="primary" icon={<PlayCircleOutlined />} onClick={simulateProcessing}>
                        {t('ragFlow.actions.startDemo')}
                    </Button>
                )}
            </div>
            </Card>
            )}

            {/* 保存策略模板Modal */}
            <Modal
                title="保存为策略模板"
                open={templateModalVisible}
                onOk={saveCurrentAsTemplate}
                onCancel={() => {
                    setTemplateModalVisible(false);
                    setNewTemplateName('');
                    setNewTemplateDesc('');
                }}
                okText="保存"
                cancelText="取消"
            >
                <Space direction="vertical" style={{ width: '100%' }} size="middle">
                    <div>
                        <div style={{ marginBottom: 8 }}>模板名称</div>
                        <Input
                            value={newTemplateName}
                            onChange={(e) => setNewTemplateName(e.target.value)}
                            placeholder="请输入模板名称"
                            maxLength={50}
                        />
                    </div>
                    <div>
                        <div style={{ marginBottom: 8 }}>模板描述（可选）</div>
                        <TextArea
                            value={newTemplateDesc}
                            onChange={(e) => setNewTemplateDesc(e.target.value)}
                            placeholder="请简要描述该模板的用途和适用场景"
                            rows={4}
                            maxLength={200}
                        />
                    </div>
                </Space>
            </Modal>
        </div>
    );
}

// 导出到全局 (Export to global)
window.DocumentProcessingFlow = DocumentProcessingFlow;

export default DocumentProcessingFlow;

