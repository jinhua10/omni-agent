/**
 * 文档处理流程组件（重构版）
 * (Document Processing Flow Component - Refactored)
 *
 * 实时展示文档处理的完整流程
 * (Real-time display of complete document processing flow)
 *
 * @author OmniAgent Team
 * @since 2025-12-26 (Refactored)
 */

import React, { useState, useEffect, useCallback } from 'react';
import { Card, Progress, Alert, Button, Tag, Space, Divider, Dropdown, App, Modal } from 'antd';
import {
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
    FileTextOutlined,
    ScissorOutlined,
    DatabaseOutlined
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import ragStrategyApi from '../../api/modules/ragStrategy';

// 导入子组件
import PendingDocumentsList from './PendingDocumentsList';
import ProcessingStepsView from './ProcessingStepsView';
import SaveTemplateModal from './SaveTemplateModal';

// 导入自定义 Hooks
import useWebSocketProgress from '../../hooks/useWebSocketProgress';
import useDocumentConfig from '../../hooks/useDocumentConfig';

import '../../assets/css/rag-flow/DocumentProcessingFlow.css';

// 处理阶段配置
const STAGE_CONFIG = {
    UPLOAD: { title: { zh: '文档上传', en: 'Document Upload' }, color: '#1890ff' },
    EXTRACT: { title: { zh: '文本提取', en: 'Text Extraction' }, color: '#52c41a' },
    CHUNK: { title: { zh: '智能分块', en: 'Smart Chunking' }, color: '#faad14' },
    VECTORIZE: { title: { zh: '向量化', en: 'Vectorization' }, color: '#722ed1' },
    INDEX: { title: { zh: '索引存储', en: 'Index Storage' }, color: '#eb2f96' },
    COMPLETED: { title: { zh: '处理完成', en: 'Completed' }, color: '#52c41a' }
};

function DocumentProcessingFlow({ documentId, onComplete, onError, autoStart = false, showDemo = false }) {
    const { t, language } = useLanguage();
    const { message } = App.useApp();

    // 状态管理
    const [progress, setProgress] = useState(null);
    const [error, setError] = useState(null);
    const [demoMode, setDemoMode] = useState(showDemo);
    const [demoStep, setDemoStep] = useState(0);
    const [demoExpanded, setDemoExpanded] = useState(false);
    const [documentsList, setDocumentsList] = useState([]);
    const [loading, setLoading] = useState(false);
    const [selectedDocId, setSelectedDocId] = useState(documentId);

    // 策略模板管理
    const [strategyTemplates, setStrategyTemplates] = useState([]);
    const [templateModalVisible, setTemplateModalVisible] = useState(false);
    const [newTemplateName, setNewTemplateName] = useState('');
    const [newTemplateDesc, setNewTemplateDesc] = useState('');
    const [documentConfigForTemplate, setDocumentConfigForTemplate] = useState(null);

    // 分块策略列表
    const [chunkingStrategies, setChunkingStrategies] = useState([]);

    // 使用自定义 Hooks
    const { documentConfigs, loadDocumentConfig, updateDocumentConfig } = useDocumentConfig();
    const { documentsProgress } = useWebSocketProgress(documentsList, demoMode, (progressData) => {
        setProgress(progressData);

        if (progressData.status === 'COMPLETED') {
            console.log('✅ 文档处理完成，刷新列表移除该文档:', progressData.documentId);
            setTimeout(() => {
                loadDocumentsList();
            }, 1000);

            if (onComplete) {
                onComplete(progressData);
            }
        }

        if (progressData.status === 'FAILED') {
            setError(progressData.errorMessage || t('ragFlow.messages.processingFailed'));
            if (onError) onError(progressData);
        }
    });

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

    // 加载策略模板列表
    const loadTemplates = useCallback(async () => {
        try {
            const result = await ragStrategyApi.getTemplates();
            if (result.success) {
                const mappedTemplates = (result.data || []).map(template => ({
                    id: template.templateId,
                    name: template.templateName,
                    description: template.description,
                    textExtractionModel: template.textExtractionModel,
                    chunkingStrategy: template.chunkingStrategy,
                    chunkingParams: template.chunkingParams,
                    createdAt: template.createdAt,
                    updatedAt: template.updatedAt,
                    useCount: template.useCount,
                    builtin: template.default,
                    default: template.default
                }));
                setStrategyTemplates(mappedTemplates);
            }
        } catch (error) {
            console.error('加载策略模板失败:', error);
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
                        loadTemplates();
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
                loadDocumentsList();
            } else {
                message.error(result.message || '应用失败');
            }
        } catch (error) {
            console.error('应用模板失败:', error);
            message.error('应用失败: ' + error.message);
        }
    }, [message]);

    // 打开保存模板Modal
    const openSaveTemplateModal = useCallback(async (docId) => {
        try {
            const result = await ragStrategyApi.getDocumentConfig(docId);
            if (result.success && result.data) {
                const config = result.data;
                if (!config.textExtractionModel) {
                    message.warning('请先选择文本提取方式');
                    return;
                }
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
                loadTemplates();
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
                setSelectedDocId(docId);
                setProgress({
                    documentId: docId,
                    documentName: docId,
                    stage: 'UPLOAD',
                    status: 'PROCESSING',
                    percentage: 0,
                    message: '开始处理...',
                    startTime: Date.now()
                });
                loadDocumentsList();
            } else {
                message.error(result.message || '处理失败');
            }
        } catch (error) {
            console.error('开始处理失败:', error);
            message.error('处理失败: ' + error.message);
        }
    }, [message, loadDocumentsList]);

    // 导航到配置页面
    const navigateToConfig = useCallback((configType, docId) => {
        const newHash = `#/documents?view=${configType}&docId=${docId}`;
        if (window.location.hash === newHash) {
            window.location.hash = '#/documents?view=flow';
            setTimeout(() => {
                window.location.hash = newHash;
            }, 0);
        } else {
            window.location.hash = newHash;
        }
    }, []);

    // 获取当前步骤索引
    const getCurrentStep = useCallback(() => {
        if (!progress) return 0;
        const stages = ['UPLOAD', 'EXTRACT', 'CHUNK', 'VECTORIZE', 'INDEX', 'COMPLETED'];
        const index = stages.indexOf(progress.stage);
        return index >= 0 ? index : 0;
    }, [progress]);

    // 获取步骤状态
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

    // 渲染步骤描述
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

    // 初始加载
    useEffect(() => {
        loadDocumentsList();
        loadTemplates();
        loadChunkingStrategies();
    }, [loadDocumentsList, loadTemplates, loadChunkingStrategies]);

    // 加载文档配置
    useEffect(() => {
        if (documentsList && documentsList.length > 0) {
            documentsList.forEach(doc => {
                if (doc.status === 'PENDING' && !documentConfigs[doc.documentId]) {
                    loadDocumentConfig(doc.documentId);
                }
            });
        }
    }, [documentsList, documentConfigs, loadDocumentConfig]);

    // 如果有错误，显示错误信息
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
            {/* 顶部操作栏 */}
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
                </Space>
            </div>

            {/* 待处理文档列表 */}
            <PendingDocumentsList
                documentsList={documentsList}
                selectedDocId={selectedDocId}
                documentsProgress={documentsProgress}
                strategyTemplates={strategyTemplates}
                onSelectDocument={setSelectedDocId}
                onApplyTemplate={applyTemplateToDocument}
                onDeleteTemplate={deleteTemplate}
                onStartProcess={startProcessDocument}
            />

            {/* 无文档提示 */}
            {!loading && documentsList.length === 0 && (
                <Alert
                    title={t('ragFlow.component.noDocuments')}
                    description={t('ragFlow.component.noDocumentsDesc')}
                    type="info"
                    showIcon
                    style={{ marginBottom: 16 }}
                />
            )}

            {/* 处理流程Card */}
            {(selectedDocId || progress) && (
                <Card
                    className="document-processing-flow"
                    title={
                        <Space>
                            <FileTextOutlined />
                            <span>文档处理流程：{selectedDocId}</span>
                        </Space>
                    }
                >
                    {/* 步骤展示 */}
                    <ProcessingStepsView
                        progress={progress}
                        selectedDocId={selectedDocId}
                        documentConfigs={documentConfigs}
                        chunkingStrategies={chunkingStrategies}
                        onUpdateConfig={updateDocumentConfig}
                        onNavigateToConfig={navigateToConfig}
                        getCurrentStep={getCurrentStep}
                        getStepStatus={getStepStatus}
                        renderStepDescription={renderStepDescription}
                    />

                    {/* 流程控制按钮 */}
                    <div style={{
                        marginTop: '24px',
                        padding: '20px',
                        background: 'linear-gradient(135deg, #f0f5ff 0%, #e6f7ff 100%)',
                        borderRadius: '8px',
                        border: '1px solid #d6e4ff',
                        display: 'flex',
                        justifyContent: 'space-between',
                        alignItems: 'center'
                    }}>
                        <Space>
                            <Button
                                icon={<LeftOutlined />}
                                onClick={() => {
                                    const docId = progress?.documentId || selectedDocId;
                                    const currentStep = getCurrentStep();
                                    if (currentStep === 1) {
                                        window.location.hash = '#/documents?view=flow';
                                    } else if (currentStep === 2) {
                                        navigateToConfig('textExtraction', docId);
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
                                        navigateToConfig('textExtraction', docId);
                                    } else if (currentStep === 1) {
                                        navigateToConfig('chunking', docId);
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
                                onClick={() => {
                                    const docId = progress?.documentId || selectedDocId;
                                    message.success('开始处理文档：' + docId);
                                }}
                            >
                                开始完整处理
                            </Button>
                        </Space>
                    </div>
                </Card>
            )}

            {/* 保存策略模板Modal */}
            <SaveTemplateModal
                visible={templateModalVisible}
                templateName={newTemplateName}
                templateDesc={newTemplateDesc}
                documentConfig={documentConfigForTemplate}
                onNameChange={setNewTemplateName}
                onDescChange={setNewTemplateDesc}
                onSave={saveCurrentAsTemplate}
                onCancel={() => {
                    setTemplateModalVisible(false);
                    setNewTemplateName('');
                    setNewTemplateDesc('');
                    setDocumentConfigForTemplate(null);
                }}
            />
        </div>
    );
}

export default DocumentProcessingFlow;

