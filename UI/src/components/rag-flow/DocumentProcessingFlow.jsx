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
    DeleteOutlined,
    EyeOutlined,
    ScanOutlined
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
    
    // ⭐ 新增：存储所有文档的实时进度（键为 documentId）
    const [documentsProgress, setDocumentsProgress] = useState({});

    // 策略模板管理 (Strategy Template Management - 从后端加载)
    const [strategyTemplates, setStrategyTemplates] = useState([]);
    const [templateModalVisible, setTemplateModalVisible] = useState(false);
    const [newTemplateName, setNewTemplateName] = useState('');
    const [newTemplateDesc, setNewTemplateDesc] = useState('');
    const [templatesLoading, setTemplatesLoading] = useState(false);
    const [documentConfigForTemplate, setDocumentConfigForTemplate] = useState(null);
    
    // 每个文档的配置信息 (key: documentId, value: config)
    const [documentConfigs, setDocumentConfigs] = useState({});
    // 分块策略列表
    const [chunkingStrategies, setChunkingStrategies] = useState([]);

    // 加载分块策略列表
    const loadChunkingStrategies = useCallback(async () => {
        try {
            const response = await fetch('/api/chunking/strategies');
            const result = await response.json();
            if (result.success && result.data) {
                setChunkingStrategies(result.data);
            }
        } catch (error) {
            console.error('加载分块策略失败:', error);
        }
    }, []);

    // 加载单个文档配置
    const loadDocumentConfig = useCallback(async (docId) => {
        try {
            const result = await ragStrategyApi.getDocumentConfig(docId);
            if (result.success && result.data) {
                setDocumentConfigs(prev => ({
                    ...prev,
                    [docId]: result.data
                }));
            }
        } catch (error) {
            console.error('加载文档配置失败:', error);
        }
    }, []);

    // 更新文档配置
    const updateDocumentConfig = useCallback(async (docId, configUpdates) => {
        try {
            // ⭐ 确保配置已加载
            let currentConfig = documentConfigs[docId];
            if (!currentConfig) {
                console.warn('⚠️ 文档配置不存在，正在加载配置...');
                await loadDocumentConfig(docId);

                // 等待状态更新后再获取
                await new Promise(resolve => setTimeout(resolve, 100));
                currentConfig = documentConfigs[docId];

                if (!currentConfig) {
                    console.error('❌ 无法获取文档配置，创建默认配置');
                    // 创建默认配置
                    currentConfig = {
                        documentId: docId,
                        status: 'PENDING',
                        createdAt: Date.now(),
                        chunkingParams: {}
                    };
                }
            }

            // ⭐ 深度合并配置更新（特别处理嵌套对象）
            const fullConfig = {
                ...currentConfig,
                ...configUpdates,
                documentId: docId,
                updatedAt: Date.now(),
                // 合并chunkingParams
                chunkingParams: {
                    ...(currentConfig.chunkingParams || {}),
                    ...(configUpdates.chunkingParams || {})
                }
            };

            console.log('📝 准备更新配置:', {
                docId,
                updates: configUpdates,
                fullConfig
            });

            // ⭐ 对URL中的documentId进行编码，避免中文字符问题
            const encodedDocId = encodeURIComponent(docId);
            const response = await fetch(`/api/system/rag-config/document/${encodedDocId}`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(fullConfig)
            });

            if (!response.ok) {
                const errorText = await response.text();
                console.error('❌ 服务器响应错误:', response.status, errorText);
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            const result = await response.json();
            if (result.success) {
                // 更新本地状态
                setDocumentConfigs(prev => ({
                    ...prev,
                    [docId]: fullConfig
                }));
                // 重新加载配置确保同步
                loadDocumentConfig(docId);
                message.success('配置已保存');
            } else {
                message.error(result.message || '保存失败');
            }
        } catch (error) {
            console.error('❌ 更新配置失败:', error);
            message.error('保存失败: ' + error.message);
        }
    }, [documentConfigs, loadDocumentConfig, message]);

    // 加载策略模板列表
    const loadTemplates = useCallback(async () => {
        setTemplatesLoading(true);
        try {
            const result = await ragStrategyApi.getTemplates();
            if (result.success) {
                // ⭐ 映射后端字段到前端期望的格式
                const mappedTemplates = (result.data || []).map(template => ({
                    id: template.templateId,              // 后端：templateId → 前端：id
                    name: template.templateName,          // 后端：templateName → 前端：name
                    description: template.description,
                    textExtractionModel: template.textExtractionModel,
                    chunkingStrategy: template.chunkingStrategy,
                    chunkingParams: template.chunkingParams,
                    createdAt: template.createdAt,
                    updatedAt: template.updatedAt,
                    useCount: template.useCount,
                    builtin: template.default,            // 后端：default → 前端：builtin
                    default: template.default
                }));
                setStrategyTemplates(mappedTemplates);
                console.log('✅ 加载策略模板成功:', mappedTemplates.length, '个');
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
    const deleteTemplate = useCallback((templateId, templateName) => {
        Modal.confirm({
            title: '确认删除',
            content: `确定要删除策略模板 "${templateName}" 吗？此操作不可恢复。`,
            okText: '确认',
            cancelText: '取消',
            okType: 'danger',
            onOk: async () => {
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
            }
        });
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

    // 打开保存模板Modal并加载配置
    const openSaveTemplateModal = useCallback(async (docId) => {
        try {
            // 加载文档配置
            const result = await ragStrategyApi.getDocumentConfig(docId);
            if (result.success && result.data) {
                const config = result.data;
                // 验证配置完整性
                if (!config.textExtractionModel) {
                    message.warning('请先选择文本提取方式');
                    return;
                }
                // ⭐ 修复：chunkingStrategy是字符串
                if (!config.chunkingStrategy) {
                    message.warning('请先选择分块策略');
                    return;
                }
                
                setDocumentConfigForTemplate(config);
                setSelectedDocId(docId);
                setTemplateModalVisible(true);
            } else {
                message.warning('无法加载文档配置');
            }
        } catch (error) {
            console.error('加载文档配置失败:', error);
            message.error('加载配置失败: ' + error.message);
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

    // 加载文档列表
    const loadDocumentsList = useCallback(async () => {
        setLoading(true);
        try {
            const response = await fetch('/api/system/rag-config/documents-status');
            const result = await response.json();
            if (result.success) {
                // ⭐ 只显示未完成的文档（排除COMPLETED状态）
                const docs = Object.values(result.data).filter(doc => doc.status !== 'COMPLETED');
                setDocumentsList(docs);
                console.log('📋 加载文档列表:', docs.length, '个待处理文档');
            } else {
                console.error('加载文档列表失败:', result.message);
            }
        } catch (error) {
            console.error('加载文档列表失败:', error);
        } finally {
            setLoading(false);
        }
    }, []);

    // 开始处理文档
    const startProcessDocument = useCallback(async (docId) => {
        try {
            const result = await ragStrategyApi.startProcessing(docId);
            if (result.success) {
                message.success('开始处理文档：' + docId);

                // ⭐ 自动选中该文档，显示处理进度
                setSelectedDocId(docId);

                // ⭐ 初始化进度状态
                setProgress({
                    documentId: docId,
                    documentName: docId,
                    stage: 'UPLOAD',
                    status: 'PROCESSING',
                    percentage: 0,
                    message: '开始处理...',
                    startTime: Date.now()
                });

                // 刷新文档列表
                loadDocumentsList();
            } else {
                message.error(result.message || '处理失败');
            }
        } catch (error) {
            console.error('开始处理失败:', error);
            message.error('处理失败: ' + error.message);
        }
    }, [message, loadDocumentsList]);


    // 初始加载
    useEffect(() => {
        loadDocumentsList();
        loadTemplates(); // 加载策略模板列表
        loadChunkingStrategies(); // 加载分块策略列表
    }, [loadDocumentsList, loadTemplates, loadChunkingStrategies]);

    // 当文档列表加载后，加载每个文档的配置
    useEffect(() => {
        if (documentsList && documentsList.length > 0) {
            documentsList.forEach(doc => {
                if (doc.status === 'PENDING' && !documentConfigs[doc.documentId]) {
                    loadDocumentConfig(doc.documentId);
                }
            });
        }
    }, [documentsList, documentConfigs, loadDocumentConfig]);

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

    /**
     * 处理 WebSocket 消息
     * (Handle WebSocket message)
     */
    const handleMessage = useCallback((message) => {
        if (message.type === 'progress') {
            const progressData = message.data;
            const docId = progressData.documentId;

            setProgress(progressData);

            // ⭐ 更新文档列表中该文档的进度
            setDocumentsProgress(prev => ({
                ...prev,
                [docId]: {
                    stage: progressData.stage,
                    percentage: progressData.percentage,
                    message: progressData.message,
                    status: progressData.status
                }
            }));

            // ⭐ 如果完成，刷新文档列表（移除已完成文档）
            if (progressData.status === 'COMPLETED') {
                console.log('✅ 文档处理完成，刷新列表移除该文档:', docId);
                // 延迟刷新，确保后端状态已更新
                setTimeout(() => {
                    loadDocumentsList();
                    // 清除该文档的进度信息
                    setDocumentsProgress(prev => {
                        const newProgress = { ...prev };
                        delete newProgress[docId];
                        return newProgress;
                    });
                }, 1000);

                if (onComplete) {
                    onComplete(progressData);
                }
            }

            // 如果失败，通知父组件 (Notify parent if failed)
            if (progressData.status === 'FAILED') {
                setError(progressData.errorMessage || t('ragFlow.messages.processingFailed'));
                if (onError) onError(progressData);
            }
        } else if (message.type === 'error') {
            setError(message.message);
            if (onError) onError(message);
        }
    }, [onComplete, onError, loadDocumentsList, t]);

    // 初始化 WebSocket 连接 (Initialize WebSocket connection)
    useEffect(() => {
        // ⭐ 当有文档列表且不是演示模式时，建立 WebSocket 连接
        if (documentsList.length === 0 || demoMode) return;

        console.log('📡 建立 WebSocket 连接，监听所有文档进度');

        let client = null;
        let pollInterval = null;
        let connectionFailed = false;

        try {
            // ⭐ 动态构建 WebSocket URL，支持开发和生产环境
            const protocol = window.location.protocol === 'https:' ? 'wss:' : 'ws:';
            const host = window.location.host; // 包含 hostname 和 port
            const wsUrl = `${protocol}//${host}/ws/progress`;

            console.log('🔗 WebSocket URL:', wsUrl);

            // 创建 WebSocket 客户端 (Create WebSocket client)
            client = new WebSocketClient(wsUrl);

            // 监听连接建立 (Listen for connection established)
            client.on('open', () => {
                console.log('✅ WebSocket 连接已建立');
                connectionFailed = false;
                // ⭐ 订阅所有文档的进度
                documentsList.forEach(doc => {
                    try {
                        client.subscribe(doc.documentId);
                        console.log('📝 订阅文档进度:', doc.documentId);
                    } catch (err) {
                        console.warn('⚠️ 订阅失败:', doc.documentId, err);
                    }
                });
            });

            // 监听进度更新 (Listen for progress updates)
            client.on('message', handleMessage);

            // 监听错误 (Listen for errors)
            client.on('error', (error) => {
                console.warn('⚠️ WebSocket 连接错误:', error);
                connectionFailed = true;
                // ⭐ 不显示错误提示，静默使用轮询作为备用方案
                console.log('💡 将使用轮询机制作为备用方案');
            });

            // 监听连接关闭
            client.on('close', (event) => {
                console.log('🔌 WebSocket 连接已关闭:', event?.code, event?.reason);
                connectionFailed = true;
            });

            // 连接 WebSocket (Connect WebSocket)
            client.connect();

            setWsClient(client);

        } catch (error) {
            console.warn('⚠️ WebSocket 初始化失败，将使用轮询机制:', error);
            connectionFailed = true;
        }

        // ⭐ 备用方案：轮询检查所有文档状态
        pollInterval = setInterval(async () => {
            // 如果 WebSocket 连接失败，轮询更频繁
            const shouldPoll = connectionFailed || !client || client.ws?.readyState !== WebSocket.OPEN;

            if (shouldPoll) {
                documentsList.forEach(async (doc) => {
                    try {
                        const response = await fetch(`/api/system/rag-config/document/${doc.documentId}`);
                        const result = await response.json();
                        if (result.success && result.data) {
                            const docData = result.data;
                            if (docData.status === 'PROCESSING' && docData.currentStage) {
                                // 模拟 WebSocket 消息格式
                                const progressData = {
                                    documentId: doc.documentId,
                                    stage: docData.currentStage || 'UPLOAD',
                                    percentage: docData.percentage || 0,
                                    message: docData.message || '处理中...',
                                    status: docData.status
                                };

                                // 更新进度
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
        }, connectionFailed ? 2000 : 5000); // WebSocket 失败时每 2 秒轮询，否则 5 秒

        // 清理函数 (Cleanup function)
        return () => {
            if (pollInterval) {
                clearInterval(pollInterval);
            }

            if (client) {
                try {
                    console.log('🔌 正在关闭 WebSocket 连接');
                    // 安全地取消订阅
                    if (client.ws && client.ws.readyState === WebSocket.OPEN) {
                        client.unsubscribe();
                    }
                    client.close();
                } catch (error) {
                    console.debug('清理 WebSocket 时出错（可忽略）:', error.message);
                }
            }
        };
    }, [documentsList, demoMode, handleMessage, t, onError]);


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
                    style={{ marginBottom: 16, maxHeight: '400px', overflow: 'auto' }}
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

                                {/* ⭐ 进度条 - 显示实时处理进度 */}
                                {documentsProgress[doc.documentId] && (
                                    <div style={{ marginTop: '12px', marginBottom: '12px' }}>
                                        <div style={{
                                            display: 'flex',
                                            justifyContent: 'space-between',
                                            marginBottom: '8px',
                                            fontSize: '12px'
                                        }}>
                                            <span style={{ color: '#666' }}>
                                                {STAGE_CONFIG[documentsProgress[doc.documentId].stage]?.title?.zh || documentsProgress[doc.documentId].stage}
                                            </span>
                                            <span style={{ fontWeight: 'bold', color: '#1890ff' }}>
                                                {documentsProgress[doc.documentId].percentage || 0}%
                                            </span>
                                        </div>
                                        <Progress
                                            percent={documentsProgress[doc.documentId].percentage || 0}
                                            status="active"
                                            strokeColor={{
                                                '0%': STAGE_CONFIG[documentsProgress[doc.documentId].stage]?.color || '#1890ff',
                                                '100%': '#52c41a',
                                            }}
                                            showInfo={false}
                                        />
                                        {documentsProgress[doc.documentId].message && (
                                            <div style={{
                                                fontSize: '11px',
                                                color: '#999',
                                                marginTop: '4px',
                                                fontStyle: 'italic'
                                            }}>
                                                {documentsProgress[doc.documentId].message}
                                            </div>
                                        )}
                                    </div>
                                )}

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
                                            onChange={(templateId) => {
                                                applyTemplateToDocument(doc.documentId, templateId);
                                            }}
                                            optionRender={(option) => {
                                                const template = strategyTemplates.find(t => t.id === option.value);
                                                if (!template) return option.label;
                                                return (
                                                    <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center', width: '100%' }}>
                                                        <div>
                                                            {template.name}
                                                            {template.description && (
                                                                <span style={{ fontSize: '12px', color: '#999', marginLeft: '8px' }}>
                                                                    ({template.description})
                                                                </span>
                                                            )}
                                                        </div>
                                                        {!template.builtin && (
                                                            <DeleteOutlined 
                                                                style={{ color: '#ff4d4f', fontSize: '12px', marginLeft: 'auto' }}
                                                                onClick={(e) => {
                                                                    e.stopPropagation();
                                                                    deleteTemplate(template.id, template.name);
                                                                }}
                                                            />
                                                        )}
                                                    </div>
                                                );
                                            }}
                                        >
                                            {strategyTemplates
                                                .filter(template => template && template.id) // 过滤掉null或无效数据
                                                .map(template => (
                                                <Option key={template.id} value={template.id}>
                                                    {template.name}
                                                </Option>
                                            ))}
                                        </Select>
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
                        <Space orientation="vertical" style={{ width: '100%' }}>
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
                            <span 
                                onClick={(e) => {
                                    e.stopPropagation();
                                    const docId = progress?.documentId || selectedDocId;
                                    console.log('点击文本提取标题, docId:', docId, 'progress:', progress, 'selectedDocId:', selectedDocId);
                                    if (docId) {
                                        const newHash = `#/documents?view=textExtraction&docId=${docId}`;
                                        // 如果 hash 相同，先改成别的，再改回来，强制触发 hashchange
                                        if (window.location.hash === newHash) {
                                            console.log('⚡ Hash相同，强制刷新视图');
                                            window.location.hash = '#/documents?view=flow';
                                            setTimeout(() => {
                                                window.location.hash = newHash;
                                            }, 0);
                                        } else {
                                            window.location.hash = newHash;
                                        }
                                    }
                                }}
                                style={{
                                    cursor: (progress?.documentId || selectedDocId) ? 'pointer' : 'default',
                                    color: (progress?.documentId || selectedDocId) ? '#1890ff' : 'inherit',
                                    textDecoration: (progress?.documentId || selectedDocId) ? 'underline' : 'none'
                                }}
                            >
                                {STAGE_CONFIG.EXTRACT.title[language]}
                            </span>
                        ),
                        icon: STAGE_CONFIG.EXTRACT.icon,
                        status: getStepStatus(1),
                        subTitle: (progress?.documentId || selectedDocId) && (
                            <div style={{ marginTop: '8px' }}>
                                <Select
                                    style={{ width: '200px' }}
                                    size="small"
                                    placeholder="选择文本提取方式"
                                    value={documentConfigs[progress?.documentId || selectedDocId]?.textExtractionModel || undefined}
                                    onChange={(value) => {
                                        const docId = progress?.documentId || selectedDocId;
                                        if (docId) {
                                            updateDocumentConfig(docId, { textExtractionModel: value });
                                        }
                                    }}
                                    popupRender={(menu) => (
                                        <>
                                            {menu}
                                            <Divider style={{ margin: '8px 0' }} />
                                            <div style={{ padding: '4px 8px', fontSize: '12px', color: '#999' }}>
                                                <SettingOutlined /> <a 
                                                    onClick={() => {
                                                        const docId = progress?.documentId || selectedDocId;
                                                        window.location.hash = `#/documents?view=textExtraction&docId=${docId}`;
                                                    }}
                                                    style={{ color: '#1890ff' }}
                                                >
                                                    高级配置
                                                </a>
                                            </div>
                                        </>
                                    )}
                                >
                                    <Option key="standard" value="standard">
                                        <Space>
                                            <FileTextOutlined style={{ color: '#1890ff' }} />
                                            标准提取
                                        </Space>
                                    </Option>
                                    <Option key="vision-llm" value="vision-llm">
                                        <Space>
                                            <EyeOutlined style={{ color: '#722ed1' }} />
                                            Vision LLM
                                        </Space>
                                    </Option>
                                    <Option key="ocr" value="ocr">
                                        <Space>
                                            <ScanOutlined style={{ color: '#52c41a' }} />
                                            OCR识别
                                        </Space>
                                    </Option>
                                </Select>
                            </div>
                        ),
                        content: renderStepDescription('EXTRACT')
                    },
                    {
                        title: (
                            <span 
                                onClick={(e) => {
                                    e.stopPropagation();
                                    const docId = progress?.documentId || selectedDocId;
                                    console.log('点击智能分块标题, docId:', docId, 'progress:', progress, 'selectedDocId:', selectedDocId);
                                    if (docId) {
                                        const newHash = `#/documents?view=chunking&docId=${docId}`;
                                        // 如果 hash 相同，先改成别的，再改回来，强制触发 hashchange
                                        if (window.location.hash === newHash) {
                                            console.log('⚡ Hash相同，强制刷新视图');
                                            window.location.hash = '#/documents?view=flow';
                                            setTimeout(() => {
                                                window.location.hash = newHash;
                                            }, 0);
                                        } else {
                                            window.location.hash = newHash;
                                        }
                                    }
                                }}
                                style={{
                                    cursor: (progress?.documentId || selectedDocId) ? 'pointer' : 'default',
                                    color: (progress?.documentId || selectedDocId) ? '#1890ff' : 'inherit',
                                    textDecoration: (progress?.documentId || selectedDocId) ? 'underline' : 'none'
                                }}
                            >
                                {STAGE_CONFIG.CHUNK.title[language]}
                            </span>
                        ),
                        icon: STAGE_CONFIG.CHUNK.icon,
                        status: getStepStatus(2),
                        subTitle: (progress?.documentId || selectedDocId) && (
                            <div style={{ marginTop: '8px' }}>
                                <Select
                                    style={{ width: '300px', maxWidth: '300px' }}
                                    size="small"
                                    placeholder="选择分块策略"
                                    value={documentConfigs[progress?.documentId || selectedDocId]?.chunkingStrategy || undefined}
                                    onChange={(value) => {
                                        const docId = progress?.documentId || selectedDocId;
                                        if (docId) {
                                            const strategy = chunkingStrategies.find(s => s.name === value);
                                            if (strategy) {
                                                // ⭐ 修复：chunkingStrategy应该是字符串，chunkingParams是对象
                                                updateDocumentConfig(docId, {
                                                    chunkingStrategy: strategy.name,  // 字符串
                                                    chunkingParams: strategy.defaultParams || {}  // 对象
                                                });
                                            }
                                        }
                                    }}
                                    popupRender={(menu) => (
                                        <>
                                            {menu}
                                            <Divider style={{ margin: '8px 0' }} />
                                            <div style={{ padding: '4px 8px', fontSize: '12px', color: '#999' }}>
                                                <SettingOutlined /> <a 
                                                    onClick={() => {
                                                        const docId = progress?.documentId || selectedDocId;
                                                        window.location.hash = `#/documents?view=chunking&docId=${docId}`;
                                                    }}
                                                    style={{ color: '#1890ff' }}
                                                >
                                                    高级配置
                                                </a>
                                            </div>
                                        </>
                                    )}
                                >
                                    {chunkingStrategies
                                        .filter(strategy => strategy && strategy.name) // 过滤掉null或无效数据
                                        .map(strategy => (
                                        <Option key={strategy.name} value={strategy.name}>
                                            <Space>
                                                <span>{strategy.displayName || strategy.name}</span>
                                                {strategy.description && (
                                                    <span style={{ fontSize: '11px', color: '#999' }}>({strategy.description})</span>
                                                )}
                                            </Space>
                                        </Option>
                                    ))}
                                </Select>
                            </div>
                        ),
                        content: renderStepDescription('CHUNK')
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
            {(progress?.documentId || selectedDocId) && (
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
                                const docId = progress?.documentId || selectedDocId;
                                const currentStep = getCurrentStep();
                                if (currentStep === 1) {
                                    // 从文本提取回到上传
                                    window.location.hash = '#/documents?view=flow';
                                } else if (currentStep === 2) {
                                    // 从分块回到文本提取
                                    window.location.hash = `#/documents?view=textExtraction&docId=${docId}`;
                                }
                            }}
                            disabled={getCurrentStep() === 0}
                        >
                            上一步
                        </Button>
                        <Button
                            icon={<RightOutlined />}
                            onClick={() => {
                                const docId = progress?.documentId || selectedDocId;
                                const currentStep = getCurrentStep();
                                if (currentStep === 0) {
                                    // 从上传到文本提取
                                    window.location.hash = `#/documents?view=textExtraction&docId=${docId}`;
                                } else if (currentStep === 1) {
                                    // 从文本提取到分块
                                    window.location.hash = `#/documents?view=chunking&docId=${docId}`;
                                }
                            }}
                            disabled={getCurrentStep() >= 2}
                        >
                            下一步
                        </Button>
                    </Space>

                    <Space>
                        <Button
                            icon={<SaveOutlined />}
                            onClick={() => {
                                const docId = progress?.documentId || selectedDocId;
                                if (docId) {
                                    openSaveTemplateModal(docId);
                                }
                            }}
                        >
                            保存为模板
                        </Button>
                        <Button
                            type="primary"
                            size="large"
                            icon={<ThunderboltOutlined />}
                            onClick={async () => {
                                try {
                                    const docId = progress?.documentId || selectedDocId;
                                    // TODO: 触发完整的处理流程
                                    message.success('开始处理文档：' + docId);
                                } catch (error) {
                                    message.error('处理失败：' + error.message);
                                }
                            }}
                        >
                            开始完整处理
                        </Button>
                    </Space>
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
                    setDocumentConfigForTemplate(null);
                }}
                okText="保存"
                cancelText="取消"
            >
                <Space orientation="vertical" style={{ width: '100%' }} size="middle">
                    {/* 显示当前配置摘要 */}
                    {documentConfigForTemplate && (
                        <Alert
                            title="当前配置"
                            description={
                                <Space orientation="vertical" size="small" style={{ width: '100%' }}>
                                    <div>
                                        <strong>📄 文本提取方式：</strong>
                                        <Tag color="blue" style={{ marginLeft: 8 }}>
                                            {documentConfigForTemplate.textExtractionModel === 'standard' ? '标准提取' :
                                             documentConfigForTemplate.textExtractionModel === 'vision-llm' ? 'Vision LLM' :
                                             documentConfigForTemplate.textExtractionModel === 'ocr' ? 'OCR识别' : '未配置'}
                                        </Tag>
                                    </div>
                                    <div>
                                        <strong>✂️ 分块策略：</strong>
                                        <Tag color="green" style={{ marginLeft: 8 }}>
                                            {documentConfigForTemplate.chunkingStrategy || '未配置'}
                                        </Tag>
                                    </div>
                                    {documentConfigForTemplate.chunkingParams?.chunkSize && (
                                        <div style={{ fontSize: '12px', color: '#666' }}>
                                            块大小: {documentConfigForTemplate.chunkingParams.chunkSize},
                                            重叠: {documentConfigForTemplate.chunkingParams.overlap || 0}
                                        </div>
                                    )}
                                    <div style={{ fontSize: '12px', color: '#999', marginTop: 4 }}>
                                        💡 保存后，此配置可快速应用到其他文档
                                    </div>
                                </Space>
                            }
                            type="info"
                            showIcon
                            style={{ marginBottom: 16 }}
                        />
                    )}
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

