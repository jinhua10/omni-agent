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

const { Option } = Select;

// 处理阶段配置
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
                                if (docId) {
                                    onNavigateToConfig('textExtraction', docId);
                                }
                            }}
                            style={{
                                cursor: docId ? 'pointer' : 'default',
                                color: docId ? '#1890ff' : 'inherit',
                                textDecoration: docId ? 'underline' : 'none'
                            }}
                        >
                            {STAGE_CONFIG.EXTRACT.title[language]}
                        </span>
                    ),
                    icon: STAGE_CONFIG.EXTRACT.icon,
                    status: getStepStatus(1),
                    subTitle: docId && (
                        <div style={{ marginTop: '8px' }}>
                            <Select
                                style={{ width: '200px' }}
                                size="small"
                                placeholder="选择文本提取方式"
                                value={documentConfigs[docId]?.textExtractionModel || undefined}
                                onChange={(value) => {
                                    if (docId) {
                                        onUpdateConfig(docId, { textExtractionModel: value });
                                    }
                                }}
                                popupRender={(menu) => (
                                    <>
                                        {menu}
                                        <Divider style={{ margin: '8px 0' }} />
                                        <div style={{ padding: '4px 8px', fontSize: '12px', color: '#999' }}>
                                            <SettingOutlined /> <a
                                                onClick={() => onNavigateToConfig('textExtraction', docId)}
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
                                if (docId) {
                                    onNavigateToConfig('chunking', docId);
                                }
                            }}
                            style={{
                                cursor: docId ? 'pointer' : 'default',
                                color: docId ? '#1890ff' : 'inherit',
                                textDecoration: docId ? 'underline' : 'none'
                            }}
                        >
                            {STAGE_CONFIG.CHUNK.title[language]}
                        </span>
                    ),
                    icon: STAGE_CONFIG.CHUNK.icon,
                    status: getStepStatus(2),
                    subTitle: docId && (
                        <div style={{ marginTop: '8px' }}>
                            <Select
                                style={{ width: '300px', maxWidth: '300px' }}
                                size="small"
                                placeholder="选择分块策略"
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
                                        <Divider style={{ margin: '8px 0' }} />
                                        <div style={{ padding: '4px 8px', fontSize: '12px', color: '#999' }}>
                                            <SettingOutlined /> <a
                                                onClick={() => onNavigateToConfig('chunking', docId)}
                                                style={{ color: '#1890ff' }}
                                            >
                                                高级配置
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
    );
}

export default ProcessingStepsView;

