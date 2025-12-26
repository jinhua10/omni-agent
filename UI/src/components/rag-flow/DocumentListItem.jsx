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
    const { t } = useLanguage();

    return (
        <div
            style={{
                background: isSelected
                    ? 'linear-gradient(135deg, #e6f7ff 0%, #bae7ff 100%)'
                    : '#fafafa',
                border: isSelected ? '2px solid #1890ff' : '1px solid #e8e8e8',
                padding: '16px',
                borderRadius: '8px',
                transition: 'all 0.3s ease',
                boxShadow: isSelected ? '0 2px 8px rgba(24, 144, 255, 0.15)' : 'none'
            }}
        >
            {/* 文档信息 */}
            <div
                onClick={onSelect}
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
                    {isSelected && (
                        <Tag color="blue" icon={<CheckCircleOutlined />}>已选中</Tag>
                    )}
                </Space>
                <div style={{ color: '#8c8c8c', fontSize: '12px', marginTop: '4px' }}>
                    {t('ragFlow.component.createdAt')}: {new Date(doc.createdAt).toLocaleString()}
                </div>
            </div>

            {/* 进度条 */}
            {progress && (
                <div style={{ marginTop: '12px', marginBottom: '12px' }}>
                    <div style={{
                        display: 'flex',
                        justifyContent: 'space-between',
                        marginBottom: '8px',
                        fontSize: '12px'
                    }}>
                        <span style={{ color: '#666' }}>
                            {STAGE_CONFIG[progress.stage]?.title?.zh || progress.stage}
                        </span>
                        <span style={{ fontWeight: 'bold', color: '#1890ff' }}>
                            {progress.percentage || 0}%
                        </span>
                    </div>
                    <Progress
                        percent={progress.percentage || 0}
                        status="active"
                        strokeColor={{
                            '0%': STAGE_CONFIG[progress.stage]?.color || '#1890ff',
                            '100%': '#52c41a',
                        }}
                        showInfo={false}
                    />
                    {progress.message && (
                        <div style={{
                            fontSize: '11px',
                            color: '#999',
                            marginTop: '4px',
                            fontStyle: 'italic'
                        }}>
                            {progress.message}
                        </div>
                    )}
                </div>
            )}

            {/* 操作栏 */}
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
                        onChange={onApplyTemplate}
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
                        开始处理
                    </Button>
                </div>
            )}
        </div>
    );
}

export default DocumentListItem;

