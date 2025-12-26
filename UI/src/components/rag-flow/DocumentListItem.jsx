/**
 * 文档列表项组件
 * (Document List Item Component)
 *
 * 显示单个文档的信息、进度条和操作按钮
 *
 * @author OmniAgent Team
 * @since 2025-12-26
 */

import React from 'react';
import { Button, Tag, Space, Select, Progress } from 'antd';
import {
    FileTextOutlined,
    CheckCircleOutlined,
    DeleteOutlined
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import '../../assets/css/rag-flow/document-list-item.css';

const { Option } = Select;

// 处理阶段配置（仅包含颜色，文本由国际化提供）
const STAGE_CONFIG = {
    UPLOAD: { color: '#1890ff' },
    EXTRACT: { color: '#52c41a' },
    CHUNK: { color: '#faad14' },
    VECTORIZE: { color: '#722ed1' },
    INDEX: { color: '#eb2f96' },
    COMPLETED: { color: '#52c41a' }
};

// 阶段名称映射到国际化键
const STAGE_I18N_MAP = {
    UPLOAD: 'stageUpload',
    EXTRACT: 'stageExtract',
    CHUNK: 'stageChunk',
    VECTORIZE: 'stageVectorize',
    INDEX: 'stageIndex',
    COMPLETED: 'stageCompleted'
};

function DocumentListItem({
    doc,
    isSelected,
    progress,
    strategyTemplates,
    onSelect,
    onApplyTemplate,
    onDeleteTemplate,
    onStartProcess
}) {
    const { t } = useLanguage();

    // 调试：仅在进度首次出现时输出
    const hasLoggedProgressRef = React.useRef(false);

    React.useEffect(() => {
        if (progress && !hasLoggedProgressRef.current) {
            console.log('📊 文档进度开始:', {
                docId: doc.documentId,
                stage: progress.stage,
                percentage: progress.percentage
            });
            hasLoggedProgressRef.current = true;
        }

        // 当进度完成或失败时重置标志
        if (progress?.status === 'COMPLETED' || progress?.status === 'FAILED') {
            hasLoggedProgressRef.current = false;
        }
    }, [progress, doc.documentId]);

    // ⭐ 如果没有 progress，显示初始状态
    const displayProgress = progress || {
        stage: 'UPLOAD',
        percentage: 0,
        message: t('ragFlow.component.waitingToStart'),
        status: 'WAITING'
    };

    return (
        <div className={`document-list-item ${isSelected ? 'selected' : ''}`}>
            {/* 文档信息 */}
            <div
                onClick={onSelect}
                className="document-list-item__info"
            >
                <Space>
                    <FileTextOutlined />
                    <span className="document-list-item__filename">{doc.documentId}</span>
                    <Tag color={
                        doc.status === 'PENDING' ? 'orange' :
                        doc.status === 'COMPLETED' ? 'green' :
                        doc.status === 'FAILED' ? 'red' :
                        'blue'
                    }>
                        {t(`ragFlow.status.${(doc.status || 'pending').toLowerCase()}`)}
                    </Tag>
                    {isSelected && (
                        <Tag color="blue" icon={<CheckCircleOutlined />}>
                            {t('ragFlow.component.selected')}
                        </Tag>
                    )}
                </Space>
                <div className="document-list-item__created-at">
                    {t('ragFlow.component.createdAt')}: {new Date(doc.createdAt).toLocaleString()}
                </div>
            </div>

            {/* 进度条 - 始终显示 */}
            <div className="document-list-item__progress">
                <div className="document-list-item__progress-header">
                    <div className="document-list-item__progress-stage">
                        <div
                            className="document-list-item__progress-indicator"
                            style={{
                                background: STAGE_CONFIG[displayProgress.stage]?.color || '#1890ff'
                            }}
                        />
                        <span className="document-list-item__progress-stage-text">
                            {displayProgress.stage && STAGE_I18N_MAP[displayProgress.stage]
                                ? t(`ragFlow.component.${STAGE_I18N_MAP[displayProgress.stage]}`)
                                : displayProgress.stage}
                        </span>
                    </div>
                    <span
                        className="document-list-item__progress-percentage"
                        style={{
                            color: STAGE_CONFIG[displayProgress.stage]?.color || '#1890ff'
                        }}
                    >
                        {displayProgress.percentage || 0}%
                    </span>
                </div>
                <Progress
                    percent={displayProgress.percentage || 0}
                    status={displayProgress.status === 'FAILED' ? 'exception' : displayProgress.status === 'WAITING' ? 'normal' : 'active'}
                    strokeColor={{
                        '0%': STAGE_CONFIG[displayProgress.stage]?.color || '#1890ff',
                        '100%': '#52c41a',
                    }}
                    size={8}
                    showInfo={false}
                    className={`document-list-item__progress-bar ${displayProgress.message ? 'with-message' : ''}`}
                />
                {displayProgress.message && (
                    <div
                        className="document-list-item__progress-message"
                        style={{
                            borderLeft: `3px solid ${STAGE_CONFIG[displayProgress.stage]?.color || '#1890ff'}`
                        }}
                    >
                        💬 {displayProgress.message}
                    </div>
                )}
            </div>

            {/* 操作栏 */}
            {doc.status === 'PENDING' && (
                <div className="document-list-item__actions">
                    <Select
                        placeholder={t('ragFlow.component.selectTemplate')}
                        className="document-list-item__template-select"
                        size="small"
                        onChange={onApplyTemplate}
                        optionRender={(option) => {
                            const template = strategyTemplates.find(t => t.id === option.value);
                            if (!template) return option.label;
                            return (
                                <div className="document-list-item__template-option">
                                    <div>
                                        {template.name}
                                        {template.description && (
                                            <span className="document-list-item__template-option-desc">
                                                ({template.description})
                                            </span>
                                        )}
                                    </div>
                                    {!template.builtin && (
                                        <DeleteOutlined
                                            className="document-list-item__template-option-delete"
                                            onClick={(e) => {
                                                e.stopPropagation();
                                                onDeleteTemplate(template.id, template.name);
                                            }}
                                        />
                                    )}
                                </div>
                            );
                        }}
                    >
                        {strategyTemplates
                            .filter(template => template && template.id)
                            .map(template => (
                                <Option key={template.id} value={template.id}>
                                    {template.name}
                                </Option>
                            ))}
                    </Select>
                    <Button
                        type="primary"
                        size="small"
                        onClick={onStartProcess}
                    >
                        {t('ragFlow.component.startProcess')}
                    </Button>
                </div>
            )}
        </div>
    );
}

export default DocumentListItem;

