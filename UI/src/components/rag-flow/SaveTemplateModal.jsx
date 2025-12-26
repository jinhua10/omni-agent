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
import { useLanguage } from '../../contexts/LanguageContext';
import '../../assets/css/rag-flow/SaveTemplateModal.css';

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
    const { t } = useLanguage();

    return (
        <Modal
            title={t('ragFlow.component.saveTemplateTitle')}
            open={visible}
            onOk={onSave}
            onCancel={onCancel}
            okText={t('common.save')}
            cancelText={t('common.cancel')}
        >
            <Space direction="vertical" className="save-template-modal__content" size="middle">
                {/* 显示当前配置摘要 */}
                {documentConfig && (
                    <Alert
                        title={t('ragFlow.component.currentConfig')}
                        description={
                            <Space direction="vertical" size="small" className="save-template-modal__config-space">
                                <div>
                                    <strong>{t('ragFlow.component.textExtractionMethod')}</strong>
                                    <Tag color="blue" className="save-template-modal__extraction-tag">
                                        {documentConfig.textExtractionModel === 'standard' ? t('ragFlow.component.standardExtraction') :
                                         documentConfig.textExtractionModel === 'vision-llm' ? t('ragFlow.component.visionLLM') :
                                         documentConfig.textExtractionModel === 'ocr' ? t('ragFlow.component.ocrRecognition') :
                                         t('ragFlow.component.notConfigured')}
                                    </Tag>
                                </div>
                                <div>
                                    <strong>{t('ragFlow.component.chunkingStrategy')}</strong>
                                    <Tag color="green" className="save-template-modal__chunking-tag">
                                        {documentConfig.chunkingStrategy || t('ragFlow.component.notConfigured')}
                                    </Tag>
                                </div>
                                {documentConfig.chunkingParams?.chunkSize && (
                                    <div className="save-template-modal__params">
                                        {t('ragFlow.component.chunkSize')}: {documentConfig.chunkingParams.chunkSize},
                                        {t('ragFlow.component.overlap')}: {documentConfig.chunkingParams.overlap || 0}
                                    </div>
                                )}
                                <div className="save-template-modal__tip">
                                    {t('ragFlow.component.saveTemplateTip')}
                                </div>
                            </Space>
                        }
                        type="info"
                        showIcon
                        className="save-template-modal__alert"
                    />
                )}
                <div>
                    <div className="save-template-modal__field-label">
                        {t('ragFlow.component.templateName')}
                    </div>
                    <Input
                        value={templateName}
                        onChange={(e) => onNameChange(e.target.value)}
                        placeholder={t('ragFlow.component.templateNamePlaceholder')}
                        maxLength={50}
                    />
                </div>
                <div>
                    <div className="save-template-modal__field-label">
                        {t('ragFlow.component.templateDescription')}
                    </div>
                    <TextArea
                        value={templateDesc}
                        onChange={(e) => onDescChange(e.target.value)}
                        placeholder={t('ragFlow.component.templateDescPlaceholder')}
                        rows={4}
                        maxLength={200}
                    />
                </div>
            </Space>
        </Modal>
    );
}

export default SaveTemplateModal;

