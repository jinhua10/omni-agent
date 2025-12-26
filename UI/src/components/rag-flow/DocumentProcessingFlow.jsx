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

    // ⭐ 批量处理相关状态
    const [selectedDocIds, setSelectedDocIds] = useState([]); // 选中的文档ID列表
    const [filterKeyword, setFilterKeyword] = useState(''); // 过滤关键词
    const [batchTemplateId, setBatchTemplateId] = useState(null); // 批量处理选择的模板ID

    // ⭐ 左右布局比例（从 localStorage 读取，默认左侧 45%）
    const [leftWidth, setLeftWidth] = useState(() => {
        const saved = localStorage.getItem('documentFlow.leftWidth');
        return saved ? parseInt(saved) : 45;
    });
    const [isDragging, setIsDragging] = useState(false);

    // ⭐ 保存布局比例到 localStorage
    useEffect(() => {
        localStorage.setItem('documentFlow.leftWidth', leftWidth.toString());
    }, [leftWidth]);

    // ⭐ 处理拖拽调整比例
    const handleMouseDown = useCallback((e) => {
        e.preventDefault();
        setIsDragging(true);
    }, []);

    const handleMouseMove = useCallback((e) => {
        if (!isDragging) return;

        const container = document.querySelector('.document-processing-flow-container');
        if (!container) return;

        const containerRect = container.getBoundingClientRect();
        const newLeftWidth = ((e.clientX - containerRect.left) / containerRect.width) * 100;

        // 限制在 20% 到 60% 之间
        if (newLeftWidth >= 20 && newLeftWidth <= 60) {
            setLeftWidth(Math.round(newLeftWidth));
        }
    }, [isDragging]);

    const handleMouseUp = useCallback(() => {
        setIsDragging(false);
    }, []);

    // ⭐ 添加和移除鼠标事件监听
    useEffect(() => {
        if (isDragging) {
            document.addEventListener('mousemove', handleMouseMove);
            document.addEventListener('mouseup', handleMouseUp);
            document.body.style.cursor = 'col-resize';
            document.body.style.userSelect = 'none';
        } else {
            document.removeEventListener('mousemove', handleMouseMove);
            document.removeEventListener('mouseup', handleMouseUp);
            document.body.style.cursor = '';
            document.body.style.userSelect = '';
        }

        return () => {
            document.removeEventListener('mousemove', handleMouseMove);
            document.removeEventListener('mouseup', handleMouseUp);
            document.body.style.cursor = '';
            document.body.style.userSelect = '';
        };
    }, [isDragging, handleMouseMove, handleMouseUp]);

    // 使用自定义 Hooks
    const { documentConfigs, loadDocumentConfig, updateDocumentConfig } = useDocumentConfig();

    // 调试：监控 documentsList 变化（仅在长度或 demoMode 变化时输出）
    const prevDocListLengthRef = React.useRef(0);
    const prevDemoModeRef = React.useRef(demoMode);

    React.useEffect(() => {
        if (documentsList.length !== prevDocListLengthRef.current || demoMode !== prevDemoModeRef.current) {
            console.log('📄 DocumentProcessingFlow - documentsList 变化:', {
                length: documentsList.length,
                demoMode,
                firstDoc: documentsList[0]?.documentId
            });
            prevDocListLengthRef.current = documentsList.length;
            prevDemoModeRef.current = demoMode;
        }
    }, [documentsList.length, demoMode]);

    const { documentsProgress } = useWebSocketProgress(documentsList, demoMode, (progressData) => {
        console.log('🔄 收到进度更新:', progressData);
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

    // 调试：监控 documentsProgress 变化（仅在对象键数量变化时输出）
    const prevProgressKeysCountRef = React.useRef(0);

    React.useEffect(() => {
        const currentKeysCount = Object.keys(documentsProgress).length;
        if (currentKeysCount !== prevProgressKeysCountRef.current) {
            console.log('📊 DocumentProcessingFlow - documentsProgress 更新:', {
                count: currentKeysCount,
                docIds: Object.keys(documentsProgress)
            });
            prevProgressKeysCountRef.current = currentKeysCount;
        }
    }, [documentsProgress]);

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
            title: t('ragFlow.component.confirmDelete'),
            content: t('ragFlow.component.confirmDeleteContent').replace('{name}', templateName),
            okText: t('common.confirm'),
            cancelText: t('common.cancel'),
            okType: 'danger',
            onOk: async () => {
                try {
                    const result = await ragStrategyApi.deleteTemplate(templateId);
                    if (result.success) {
                        message.success(t('ragFlow.component.templateDeleted'));
                        loadTemplates();
                    } else {
                        message.error(result.message || t('ragFlow.component.deleteFailed'));
                    }
                } catch (error) {
                    console.error('删除模板失败:', error);
                    message.error(t('ragFlow.component.deleteFailed') + ': ' + error.message);
                }
            }
        });
    }, [message, loadTemplates, t]);

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

    // 应用策略模板到文档
    const applyTemplateToDocument = useCallback(async (docId, templateId) => {
        try {
            const result = await ragStrategyApi.applyTemplateToDocument(docId, templateId);
            if (result.success) {
                message.success(t('ragFlow.component.templateApplied'));
                loadDocumentsList();
            } else {
                message.error(result.message || t('ragFlow.component.applyFailed'));
            }
        } catch (error) {
            console.error('应用模板失败:', error);
            message.error(t('ragFlow.component.applyFailed') + ': ' + error.message);
        }
    }, [message, t, loadDocumentsList]);

    // 打开保存模板Modal
    const openSaveTemplateModal = useCallback(async (docId) => {
        try {
            const result = await ragStrategyApi.getDocumentConfig(docId);
            if (result.success && result.data) {
                const config = result.data;
                if (!config.textExtractionModel) {
                    message.warning(t('ragFlow.component.pleaseSelectTextExtraction'));
                    return;
                }
                if (!config.chunkingStrategy) {
                    message.warning(t('ragFlow.component.pleaseSelectChunkingStrategy'));
                    return;
                }

                setDocumentConfigForTemplate(config);
                setSelectedDocId(docId);
                setTemplateModalVisible(true);
            } else {
                message.warning(t('ragFlow.component.cannotLoadConfig'));
            }
        } catch (error) {
            console.error('加载文档配置失败:', error);
            message.error(t('ragFlow.component.loadConfigFailed') + ': ' + error.message);
        }
    }, [message, t]);

    // 保存当前配置为模板
    const saveCurrentAsTemplate = useCallback(async () => {
        if (!selectedDocId) {
            message.warning(t('ragFlow.component.pleaseSelectDocument'));
            return;
        }
        if (!newTemplateName.trim()) {
            message.warning(t('ragFlow.component.pleaseEnterTemplateName'));
            return;
        }

        try {
            const result = await ragStrategyApi.saveCurrentAsTemplate(selectedDocId, {
                name: newTemplateName.trim(),
                description: newTemplateDesc.trim()
            });

            if (result.success) {
                message.success(t('ragFlow.component.templateSaved'));
                setTemplateModalVisible(false);
                setNewTemplateName('');
                setNewTemplateDesc('');
                loadTemplates();
            } else {
                message.error(result.message || t('ragFlow.component.saveFailed'));
            }
        } catch (error) {
            console.error('保存模板失败:', error);
            message.error(t('ragFlow.component.saveFailed') + ': ' + error.message);
        }
    }, [selectedDocId, newTemplateName, newTemplateDesc, message, loadTemplates, t]);


    // 开始处理文档
    const startProcessDocument = useCallback(async (docId) => {
        try {
            const result = await ragStrategyApi.startProcessing(docId);
            if (result.success) {
                message.success(t('ragFlow.component.startProcessingDoc') + docId);
                setSelectedDocId(docId);
                setProgress({
                    documentId: docId,
                    documentName: docId,
                    stage: 'UPLOAD',
                    status: 'PROCESSING',
                    percentage: 0,
                    message: t('ragFlow.component.processingStarted'),
                    startTime: Date.now()
                });
                loadDocumentsList();
            } else {
                message.error(result.message || t('ragFlow.component.applyFailed'));
            }
        } catch (error) {
            console.error('开始处理失败:', error);
            message.error(t('ragFlow.component.applyFailed') + ': ' + error.message);
        }
    }, [message, loadDocumentsList, t]);

    // ⭐ 批量处理相关函数

    // 过滤文档列表
    const filteredDocumentsList = React.useMemo(() => {
        if (!filterKeyword.trim()) return documentsList;
        return documentsList.filter(doc =>
            doc.documentId.toLowerCase().includes(filterKeyword.toLowerCase())
        );
    }, [documentsList, filterKeyword]);

    // ⭐ 统计文件类型
    const fileTypeStats = React.useMemo(() => {
        const stats = {};
        filteredDocumentsList.forEach(doc => {
            const ext = doc.documentId.split('.').pop()?.toLowerCase() || 'unknown';
            if (!stats[ext]) {
                stats[ext] = {
                    count: 0,
                    docIds: []
                };
            }
            stats[ext].count++;
            stats[ext].docIds.push(doc.documentId);
        });
        return stats;
    }, [filteredDocumentsList]);

    // 全选/取消全选
    const handleSelectAll = useCallback(() => {
        if (selectedDocIds.length === filteredDocumentsList.length) {
            setSelectedDocIds([]);
        } else {
            setSelectedDocIds(filteredDocumentsList.map(doc => doc.documentId));
        }
    }, [selectedDocIds, filteredDocumentsList]);

    // 切换单个文档的选择
    const handleToggleDocSelect = useCallback((docId) => {
        setSelectedDocIds(prev => {
            if (prev.includes(docId)) {
                return prev.filter(id => id !== docId);
            } else {
                return [...prev, docId];
            }
        });
    }, []);

    // ⭐ 按文件类型选择/取消选择
    const handleToggleFileTypeSelect = useCallback((fileType) => {
        const typeDocIds = fileTypeStats[fileType]?.docIds || [];
        setSelectedDocIds(prev => {
            // 检查该类型的文档是否全部被选中
            const allSelected = typeDocIds.every(id => prev.includes(id));

            if (allSelected) {
                // 如果全部选中，则取消选择该类型的所有文档
                return prev.filter(id => !typeDocIds.includes(id));
            } else {
                // 否则，添加该类型的所有文档
                const newIds = [...prev];
                typeDocIds.forEach(id => {
                    if (!newIds.includes(id)) {
                        newIds.push(id);
                    }
                });
                return newIds;
            }
        });
    }, [fileTypeStats]);

    // 检查某个文件类型是否全部被选中
    const isFileTypeSelected = useCallback((fileType) => {
        const typeDocIds = fileTypeStats[fileType]?.docIds || [];
        if (typeDocIds.length === 0) return false;
        return typeDocIds.every(id => selectedDocIds.includes(id));
    }, [fileTypeStats, selectedDocIds]);

    // 检查某个文件类型是否部分被选中
    const isFileTypeIndeterminate = useCallback((fileType) => {
        const typeDocIds = fileTypeStats[fileType]?.docIds || [];
        if (typeDocIds.length === 0) return false;
        const selectedCount = typeDocIds.filter(id => selectedDocIds.includes(id)).length;
        return selectedCount > 0 && selectedCount < typeDocIds.length;
    }, [fileTypeStats, selectedDocIds]);

    // 批量应用模板
    const handleBatchApplyTemplate = useCallback(async () => {
        if (selectedDocIds.length === 0) {
            message.warning(t('ragFlow.component.pleaseSelectDocuments'));
            return;
        }
        if (!batchTemplateId) {
            message.warning(t('ragFlow.component.pleaseSelectTemplate'));
            return;
        }

        Modal.confirm({
            title: t('ragFlow.component.batchProcessConfirm'),
            content: t('ragFlow.component.batchProcessContent')
                .replace('{count}', selectedDocIds.length)
                .replace('{template}', strategyTemplates.find(t => t.id === batchTemplateId)?.name || ''),
            okText: t('common.confirm'),
            cancelText: t('common.cancel'),
            onOk: async () => {
                const successCount = 0;
                const failCount = 0;

                for (const docId of selectedDocIds) {
                    try {
                        await ragStrategyApi.applyTemplateToDocument(docId, batchTemplateId);
                        await ragStrategyApi.startProcessing(docId);
                    } catch (error) {
                        console.error('批量处理失败:', docId, error);
                    }
                }

                message.success(t('ragFlow.component.batchProcessSuccess').replace('{count}', selectedDocIds.length));
                setSelectedDocIds([]);
                setBatchTemplateId(null);
                loadDocumentsList();
            }
        });
    }, [selectedDocIds, batchTemplateId, strategyTemplates, message, t, loadDocumentsList]);

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
            <div className="document-processing-flow-container__header">
                <Space>
                    <LoadingOutlined spin={progress && (progress.status === 'RUNNING' || progress.status === 'PROCESSING')} />
                    <span className="document-processing-flow-container__title">{t('ragFlow.component.title')}</span>
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

            {/* 无文档提示 */}
            {!loading && documentsList.length === 0 && (
                <Alert
                    title={t('ragFlow.component.noDocuments')}
                    description={t('ragFlow.component.noDocumentsDesc')}
                    type="info"
                    showIcon
                    className="document-processing-flow-container__no-docs-alert"
                />
            )}

            {/* ⭐ 左右布局容器 */}
            {documentsList.length > 0 && (
                <div className="document-processing-flow-container__layout">
                    {/* 左侧：待处理文档列表 */}
                    <div
                        className="document-processing-flow-container__left"
                        style={{ width: `${leftWidth}%` }}
                    >
                        <PendingDocumentsList
                            documentsList={filteredDocumentsList}
                            selectedDocId={selectedDocId}
                            documentsProgress={documentsProgress}
                            strategyTemplates={strategyTemplates}
                            onSelectDocument={setSelectedDocId}
                            onApplyTemplate={applyTemplateToDocument}
                            onDeleteTemplate={deleteTemplate}
                            onStartProcess={startProcessDocument}
                            selectedDocIds={selectedDocIds}
                            filterKeyword={filterKeyword}
                            batchTemplateId={batchTemplateId}
                            onFilterChange={setFilterKeyword}
                            onSelectAll={handleSelectAll}
                            onToggleDocSelect={handleToggleDocSelect}
                            onBatchTemplateChange={setBatchTemplateId}
                            onBatchProcess={handleBatchApplyTemplate}
                            fileTypeStats={fileTypeStats}
                            onToggleFileTypeSelect={handleToggleFileTypeSelect}
                            isFileTypeSelected={isFileTypeSelected}
                            isFileTypeIndeterminate={isFileTypeIndeterminate}
                        />
                    </div>

                    {/* 可拖拽分隔条 */}
                    <div
                        className="document-processing-flow-container__resizer"
                        onMouseDown={handleMouseDown}
                    >
                        <div className="document-processing-flow-container__resizer-line" />
                    </div>

                    {/* 右侧：文档处理流程 */}
                    <div
                        className="document-processing-flow-container__right"
                        style={{ width: `${100 - leftWidth}%` }}
                    >
                        {/* 处理流程Card - 始终显示 */}
                        <Card
                            className="document-processing-flow"
                            title={
                                <Space>
                                    <FileTextOutlined />
                                    {selectedDocId ? (
                                        <span>{t('ragFlow.component.documentFlowTitle')}{selectedDocId}</span>
                                    ) : (
                                        <span>{t('ragFlow.component.processingFlowOverview')}</span>
                                    )}
                                </Space>
                            }
                        >
                            {(selectedDocId || progress) ? (
                                <>
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
                                    <div className="document-processing-flow__controls">
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
                                                {t('ragFlow.component.previousStep')}
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
                                                {t('ragFlow.component.nextStep')}
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
                                                {t('ragFlow.component.saveAsTemplate')}
                                            </Button>
                                            <Button
                                                type="primary"
                                                size="large"
                                                icon={<ThunderboltOutlined />}
                                                onClick={() => {
                                                    const docId = progress?.documentId || selectedDocId;
                                                    message.success(t('ragFlow.component.startProcessingDoc') + docId);
                                                }}
                                            >
                                                {t('ragFlow.component.startFullProcess')}
                                            </Button>
                                        </Space>
                                    </div>
                                </>
                            ) : (
                                /* 未选中文档时的提示 */
                                <div className="document-processing-flow__placeholder">
                                    <div className="document-processing-flow__placeholder-content">
                                        <FileTextOutlined style={{ fontSize: 48, color: '#d9d9d9', marginBottom: 16 }} />
                                        <h3>{t('ragFlow.component.selectDocumentHint')}</h3>
                                        <p>{t('ragFlow.component.selectDocumentDesc')}</p>
                                        <div className="document-processing-flow__steps-preview">
                                            <ProcessingStepsView
                                                progress={null}
                                                selectedDocId={null}
                                                documentConfigs={{}}
                                                chunkingStrategies={chunkingStrategies}
                                                onUpdateConfig={() => {}}
                                                onNavigateToConfig={() => {}}
                                                getCurrentStep={() => 0}
                                                getStepStatus={() => 'wait'}
                                                renderStepDescription={() => null}
                                            />
                                        </div>
                                    </div>
                                </div>
                            )}
                        </Card>
                    </div>
                </div>
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

