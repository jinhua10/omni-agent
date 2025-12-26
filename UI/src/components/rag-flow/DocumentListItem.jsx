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


// 处理阶段配置
const STAGE_CONFIG = {
    UPLOAD: { title: { zh: '文档上传', en: 'Document Upload' }, color: '#1890ff' },
    EXTRACT: { title: { zh: '文本提取', en: 'Text Extraction' }, color: '#52c41a' },
    CHUNK: { title: { zh: '智能分块', en: 'Smart Chunking' }, color: '#faad14' },
    VECTORIZE: { title: { zh: '向量化', en: 'Vectorization' }, color: '#722ed1' },
    INDEX: { title: { zh: '索引存储', en: 'Index Storage' }, color: '#eb2f96' },
    COMPLETED: { title: { zh: '处理完成', en: 'Completed' }, color: '#52c41a' }
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
    const { t, language } = useLanguage();

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

            {/* 进度条 */}
            {progress && (
                <div className="document-list-item__progress">
                    <div className="document-list-item__progress-header">
                        <div className="document-list-item__progress-stage">
                            <div
                                className="document-list-item__progress-indicator"
                                style={{
                                    background: STAGE_CONFIG[progress.stage]?.color || '#1890ff'
                                }}
                            />
                            <span className="document-list-item__progress-stage-text">
                                {STAGE_CONFIG[progress.stage]?.title?.[language] || progress.stage}
                            </span>
                        </div>
                        <span
                            className="document-list-item__progress-percentage"
                            style={{
                                color: STAGE_CONFIG[progress.stage]?.color || '#1890ff'
                            }}
                        >
                            {progress.percentage || 0}%
                        </span>
                    </div>
                    <Progress
                        percent={progress.percentage || 0}
                        status={progress.status === 'FAILED' ? 'exception' : 'active'}
                        strokeColor={{
                            '0%': STAGE_CONFIG[progress.stage]?.color || '#1890ff',
                            '100%': '#52c41a',
                        }}
                        strokeWidth={8}
                        showInfo={false}
                        className={`document-list-item__progress-bar ${progress.message ? 'with-message' : ''}`}
                    />
                    {progress.message && (
                        <div
                            className="document-list-item__progress-message"
                            style={{
                                borderLeft: `3px solid ${STAGE_CONFIG[progress.stage]?.color || '#1890ff'}`
                            }}
                        >
                            💬 {progress.message}
                        </div>
                    )}
                </div>
            )}

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

