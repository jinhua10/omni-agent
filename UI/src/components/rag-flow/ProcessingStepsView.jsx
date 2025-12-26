/**
 * 处理步骤视图组件
 * (Processing Steps View Component)
 *
 * 显示文档处理的各个阶段步骤
 *
 * @author OmniAgent Team
 * @since 2025-12-26
 */

import React from 'react';
import { Steps, Select, Space, Divider } from 'antd';
import {
    FileAddOutlined,
    FileTextOutlined,
    ScissorOutlined,
    FunctionOutlined,
    DatabaseOutlined,
    CheckCircleOutlined,
    SettingOutlined,
    EyeOutlined,
    ScanOutlined
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import '../../assets/css/rag-flow/ProcessingStepsView.css';

const { Option } = Select;

// 处理阶段配置（仅包含图标和颜色，文本由国际化提供）
const STAGE_CONFIG = {
    UPLOAD: {
        icon: <FileAddOutlined />,
        color: '#1890ff'
    },
    EXTRACT: {
        icon: <FileTextOutlined />,
        color: '#52c41a'
    },
    CHUNK: {
        icon: <ScissorOutlined />,
        color: '#faad14'
    },
    VECTORIZE: {
        icon: <FunctionOutlined />,
        color: '#722ed1'
    },
    INDEX: {
        icon: <DatabaseOutlined />,
        color: '#eb2f96'
    },
    COMPLETED: {
        icon: <CheckCircleOutlined />,
        color: '#52c41a'
    }
};

function ProcessingStepsView({
    progress,
    selectedDocId,
    documentConfigs,
    chunkingStrategies,
    onUpdateConfig,
    onNavigateToConfig,
    getCurrentStep,
    getStepStatus,
    renderStepDescription
}) {
    const { t, language } = useLanguage();

    const docId = progress?.documentId || selectedDocId;

    return (
        <Steps
            orientation="vertical"
            current={getCurrentStep()}
            status={progress?.status === 'FAILED' ? 'error' : progress?.status === 'COMPLETED' ? 'finish' : 'process'}
            size="default"
            className="processing-steps-view"
            items={[
                {
                    title: t('ragFlow.component.stageUpload'),
                    icon: STAGE_CONFIG.UPLOAD.icon,
                    status: getStepStatus(0),
                    content: renderStepDescription('UPLOAD')
                },
                {
                    title: (
                        <span
                            onClick={(e) => {
                                e.stopPropagation();
                                if (docId) {
                                    onNavigateToConfig('textExtraction', docId);
                                }
                            }}
                            className={docId ? 'processing-steps-view__step-title' : 'processing-steps-view__step-title--disabled'}
                        >
                            {t('ragFlow.component.stageExtract')}
                        </span>
                    ),
                    icon: STAGE_CONFIG.EXTRACT.icon,
                    status: getStepStatus(1),
                    subTitle: docId && (
                        <div className="processing-steps-view__selector-container">
                            <Select
                                className="processing-steps-view__text-extraction-select"
                                size="small"
                                placeholder={t('ragFlow.component.selectTextExtraction')}
                                value={documentConfigs[docId]?.textExtractionModel || undefined}
                                onChange={(value) => {
                                    if (docId) {
                                        onUpdateConfig(docId, { textExtractionModel: value });
                                    }
                                }}
                                popupRender={(menu) => (
                                    <>
                                        {menu}
                                        <Divider className="processing-steps-view__popup-divider" />
                                        <div className="processing-steps-view__popup-config">
                                            <SettingOutlined /> <a
                                                onClick={() => onNavigateToConfig('textExtraction', docId)}
                                                className="processing-steps-view__popup-config-link"
                                            >
                                                {t('ragFlow.component.advancedConfig')}
                                            </a>
                                        </div>
                                    </>
                                )}
                            >
                                <Option key="standard" value="standard">
                                    <Space>
                                        <FileTextOutlined className="processing-steps-view__option-icon--blue" />
                                        {t('ragFlow.component.standardExtraction')}
                                    </Space>
                                </Option>
                                <Option key="vision-llm" value="vision-llm">
                                    <Space>
                                        <EyeOutlined className="processing-steps-view__option-icon--purple" />
                                        {t('ragFlow.component.visionLLM')}
                                    </Space>
                                </Option>
                                <Option key="ocr" value="ocr">
                                    <Space>
                                        <ScanOutlined className="processing-steps-view__option-icon--green" />
                                        {t('ragFlow.component.ocrRecognition')}
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
                                if (docId) {
                                    onNavigateToConfig('chunking', docId);
                                }
                            }}
                            className={docId ? 'processing-steps-view__step-title' : 'processing-steps-view__step-title--disabled'}
                        >
                            {t('ragFlow.component.stageChunk')}
                        </span>
                    ),
                    icon: STAGE_CONFIG.CHUNK.icon,
                    status: getStepStatus(2),
                    subTitle: docId && (
                        <div className="processing-steps-view__selector-container">
                            <Select
                                className="processing-steps-view__chunking-select"
                                size="small"
                                placeholder={t('ragFlow.component.selectChunkingStrategy')}
                                value={documentConfigs[docId]?.chunkingStrategy || undefined}
                                onChange={(value) => {
                                    if (docId) {
                                        const strategy = chunkingStrategies.find(s => s.name === value);
                                        if (strategy) {
                                            onUpdateConfig(docId, {
                                                chunkingStrategy: strategy.name,
                                                chunkingParams: strategy.defaultParams || {}
                                            });
                                        }
                                    }
                                }}
                                popupRender={(menu) => (
                                    <>
                                        {menu}
                                        <Divider className="processing-steps-view__popup-divider" />
                                        <div className="processing-steps-view__popup-config">
                                            <SettingOutlined /> <a
                                                onClick={() => onNavigateToConfig('chunking', docId)}
                                                className="processing-steps-view__popup-config-link"
                                            >
                                                {t('ragFlow.component.advancedConfig')}
                                            </a>
                                        </div>
                                    </>
                                )}
                            >
                                {chunkingStrategies
                                    .filter(strategy => strategy && strategy.name)
                                    .map(strategy => (
                                        <Option key={strategy.name} value={strategy.name}>
                                            <Space>
                                                <span>{strategy.displayName || strategy.name}</span>
                                                {strategy.description && (
                                                    <span className="processing-steps-view__option-desc">
                                                        ({strategy.description})
                                                    </span>
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
                    title: t('ragFlow.component.stageVectorize'),
                    icon: STAGE_CONFIG.VECTORIZE.icon,
                    status: getStepStatus(3),
                    content: renderStepDescription('VECTORIZE')
                },
                {
                    title: t('ragFlow.component.stageIndex'),
                    icon: STAGE_CONFIG.INDEX.icon,
                    status: getStepStatus(4),
                    content: renderStepDescription('INDEX')
                }
            ]}
        />
    );
}

export default ProcessingStepsView;

