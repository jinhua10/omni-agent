/**
 * 保存策略模板对话框组件
 * (Save Template Modal Component)
 *
 * 用于保存当前文档配置为策略模板
 *
 * @author OmniAgent Team
 * @since 2025-12-26
 */

import React from 'react';
import { Modal, Input, Space, Alert, Tag } from 'antd';

const { TextArea } = Input;

function SaveTemplateModal({
    visible,
    templateName,
    templateDesc,
    documentConfig,
    onNameChange,
    onDescChange,
    onSave,
    onCancel
}) {
    return (
        <Modal
            title="保存为策略模板"
            open={visible}
            onOk={onSave}
            onCancel={onCancel}
            okText="保存"
            cancelText="取消"
        >
            <Space orientation="vertical" style={{ width: '100%' }} size="middle">
                {/* 显示当前配置摘要 */}
                {documentConfig && (
                    <Alert
                        title="当前配置"
                        description={
                            <Space orientation="vertical" size="small" style={{ width: '100%' }}>
                                <div>
                                    <strong>📄 文本提取方式：</strong>
                                    <Tag color="blue" style={{ marginLeft: 8 }}>
                                        {documentConfig.textExtractionModel === 'standard' ? '标准提取' :
                                         documentConfig.textExtractionModel === 'vision-llm' ? 'Vision LLM' :
                                         documentConfig.textExtractionModel === 'ocr' ? 'OCR识别' : '未配置'}
                                    </Tag>
                                </div>
                                <div>
                                    <strong>✂️ 分块策略：</strong>
                                    <Tag color="green" style={{ marginLeft: 8 }}>
                                        {documentConfig.chunkingStrategy || '未配置'}
                                    </Tag>
                                </div>
                                {documentConfig.chunkingParams?.chunkSize && (
                                    <div style={{ fontSize: '12px', color: '#666' }}>
                                        块大小: {documentConfig.chunkingParams.chunkSize},
                                        重叠: {documentConfig.chunkingParams.overlap || 0}
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
                        value={templateName}
                        onChange={(e) => onNameChange(e.target.value)}
                        placeholder="请输入模板名称"
                        maxLength={50}
                    />
                </div>
                <div>
                    <div style={{ marginBottom: 8 }}>模板描述（可选）</div>
                    <TextArea
                        value={templateDesc}
                        onChange={(e) => onDescChange(e.target.value)}
                        placeholder="请简要描述该模板的用途和适用场景"
                        rows={4}
                        maxLength={200}
                    />
                </div>
            </Space>
        </Modal>
    );
}

export default SaveTemplateModal;

