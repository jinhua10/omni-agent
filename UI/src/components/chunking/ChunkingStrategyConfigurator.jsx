/**
 * 分块策略配置器 (Chunking Strategy Configurator)
 *
 * Phase 4.2.1 - 交互式分块策略配置和实时预览
 *
 * 功能特性:
 * - 策略选择和参数配置
 * - 实时预览分块效果
 * - 多策略对比
 * - 分块统计和可视化
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */

import React, { useState, useEffect } from 'react';
import {
    Card,
    Select,
    Form,
    InputNumber,
    Button,
    Space,
    Divider,
    Alert,
    Tabs,
    Collapse,
    Statistic,
    Row,
    Col,
    message,
    Spin,
    Empty,
    Tag,
    Tooltip
} from 'antd';
import {
    SettingOutlined,
    EyeOutlined,
    CompareOutlined,
    InfoCircleOutlined,
    ThunderboltOutlined,
    CheckCircleOutlined
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import ChunkPreviewList from './ChunkPreviewList';
import StrategyComparisonChart from './StrategyComparisonChart';
import './ChunkingStrategyConfigurator.css';

const { Option } = Select;
const { TabPane } = Tabs;
const { Panel } = Collapse;

/**
 * 分块策略配置器主组件
 */
function ChunkingStrategyConfigurator({ defaultContent = '' }) {
    const { t } = useLanguage();

    // ========== 状态管理 ==========
    const [loading, setLoading] = useState(false);
    const [strategies, setStrategies] = useState([]);  // 可用策略列表
    const [selectedStrategy, setSelectedStrategy] = useState('fixed_size');  // 当前选择的策略
    const [strategyParams, setStrategyParams] = useState({});  // 策略参数
    const [content, setContent] = useState(defaultContent);  // 要分块的内容
    const [previewResult, setPreviewResult] = useState(null);  // 预览结果
    const [comparisonResults, setComparisonResults] = useState([]);  // 对比结果
    const [activeTab, setActiveTab] = useState('single');  // 当前激活的标签页

    // ========== 生命周期 ==========

    /**
     * 组件挂载时加载可用策略
     */
    useEffect(() => {
        loadAvailableStrategies();
    }, []);

    /**
     * 策略变更时加载默认参数
     */
    useEffect(() => {
        if (selectedStrategy && strategies.length > 0) {
            loadStrategyDefaultParams(selectedStrategy);
        }
    }, [selectedStrategy, strategies]);

    // ========== API 调用 ==========

    /**
     * 加载可用的分块策略
     */
    const loadAvailableStrategies = async () => {
        try {
            setLoading(true);
            const response = await fetch('/api/chunking/strategies');
            const result = await response.json();

            if (result.success) {
                setStrategies(result.data);
                console.log('✅ 加载策略:', result.data.length, '个');
            } else {
                message.error('加载策略失败: ' + result.error);
            }
        } catch (error) {
            console.error('❌ 加载策略失败:', error);
            message.error('加载策略失败');
        } finally {
            setLoading(false);
        }
    };

    /**
     * 加载策略的默认参数
     */
    const loadStrategyDefaultParams = async (strategyName) => {
        try {
            const response = await fetch(`/api/chunking/strategies/${strategyName}`);
            const result = await response.json();

            if (result.success) {
                setStrategyParams(result.data.defaultParams || {});
                console.log('✅ 加载默认参数:', result.data.defaultParams);
            }
        } catch (error) {
            console.error('❌ 加载默认参数失败:', error);
        }
    };

    /**
     * 预览分块效果
     */
    const handlePreview = async () => {
        if (!content || content.trim().length === 0) {
            message.warning('请输入要分块的内容');
            return;
        }

        try {
            setLoading(true);
            const response = await fetch('/api/chunking/preview', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    content: content,
                    strategy: selectedStrategy,
                    params: strategyParams
                })
            });

            const result = await response.json();

            if (result.success) {
                setPreviewResult(result.data);
                message.success(`✅ 分块完成: ${result.data.totalChunks} 个块 (${result.data.elapsedTimeMs}ms)`);
                console.log('✅ 分块预览:', result.data);
            } else {
                message.error('分块预览失败: ' + result.error);
            }
        } catch (error) {
            console.error('❌ 分块预览失败:', error);
            message.error('分块预览失败');
        } finally {
            setLoading(false);
        }
    };

    /**
     * 对比多个策略
     */
    const handleCompare = async () => {
        if (!content || content.trim().length === 0) {
            message.warning('请输入要分块的内容');
            return;
        }

        // 选择要对比的策略（当前+其他常用策略）
        const strategiesToCompare = [
            { strategy: selectedStrategy, params: strategyParams },
            { strategy: 'fixed_size', params: { chunkSize: 500, overlapSize: 50 } },
            { strategy: 'paragraph', params: { maxParagraphsPerChunk: 3 } }
        ].filter((item, index, self) =>
            index === self.findIndex((t) => t.strategy === item.strategy)
        );

        try {
            setLoading(true);
            const response = await fetch('/api/chunking/compare', {
                method: 'POST',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify({
                    content: content,
                    strategies: strategiesToCompare
                })
            });

            const result = await response.json();

            if (result.success) {
                setComparisonResults(result.data);
                setActiveTab('compare');
                message.success(`✅ 对比完成: ${result.data.length} 个策略`);
                console.log('✅ 策略对比:', result.data);
            } else {
                message.error('策略对比失败: ' + result.error);
            }
        } catch (error) {
            console.error('❌ 策略对比失败:', error);
            message.error('策略对比失败');
        } finally {
            setLoading(false);
        }
    };

    // ========== 渲染方法 ==========

    /**
     * 渲染策略选择器
     */
    const renderStrategySelector = () => {
        const currentStrategy = strategies.find(s => s.id === selectedStrategy);

        return (
            <Card
                title={
                    <Space>
                        <SettingOutlined />
                        <span>策略配置</span>
                    </Space>
                }
                className="strategy-selector-card"
            >
                <Form layout="vertical">
                    {/* 策略选择 */}
                    <Form.Item
                        label={
                            <Space>
                                <span>分块策略</span>
                                <Tooltip title="选择不同的分块策略会影响文档的切分方式">
                                    <InfoCircleOutlined style={{ color: '#1890ff' }} />
                                </Tooltip>
                            </Space>
                        }
                    >
                        <Select
                            value={selectedStrategy}
                            onChange={setSelectedStrategy}
                            style={{ width: '100%' }}
                            loading={loading}
                        >
                            {strategies.map(strategy => (
                                <Option key={strategy.id} value={strategy.id}>
                                    <Space>
                                        <span>{strategy.name}</span>
                                        <span style={{ color: '#999', fontSize: '12px' }}>
                                            {strategy.description}
                                        </span>
                                    </Space>
                                </Option>
                            ))}
                        </Select>
                    </Form.Item>

                    {/* 策略描述 */}
                    {currentStrategy && (
                        <Alert
                            message={currentStrategy.description}
                            type="info"
                            showIcon
                            style={{ marginBottom: 16 }}
                        />
                    )}

                    {/* 动态参数配置 */}
                    {renderStrategyParams()}

                    <Divider />

                    {/* 操作按钮 */}
                    <Space size="middle" style={{ width: '100%', justifyContent: 'center' }}>
                        <Button
                            type="primary"
                            icon={<EyeOutlined />}
                            onClick={handlePreview}
                            loading={loading}
                            size="large"
                        >
                            预览分块
                        </Button>
                        <Button
                            icon={<CompareOutlined />}
                            onClick={handleCompare}
                            loading={loading}
                            size="large"
                        >
                            对比策略
                        </Button>
                    </Space>
                </Form>
            </Card>
        );
    };

    /**
     * 渲染策略参数配置
     */
    const renderStrategyParams = () => {
        if (!strategyParams || Object.keys(strategyParams).length === 0) {
            return null;
        }

        return (
            <Collapse
                defaultActiveKey={['1']}
                style={{ marginTop: 16 }}
            >
                <Panel header="高级参数配置" key="1">
                    <Space direction="vertical" style={{ width: '100%' }} size="middle">
                        {Object.entries(strategyParams).map(([key, value]) => (
                            <Form.Item
                                key={key}
                                label={getParamLabel(key)}
                                style={{ marginBottom: 0 }}
                            >
                                <InputNumber
                                    value={value}
                                    onChange={(newValue) => {
                                        setStrategyParams({
                                            ...strategyParams,
                                            [key]: newValue
                                        });
                                    }}
                                    style={{ width: '100%' }}
                                    min={0}
                                />
                            </Form.Item>
                        ))}
                    </Space>
                </Panel>
            </Collapse>
        );
    };

    /**
     * 获取参数的中文标签
     */
    const getParamLabel = (key) => {
        const labels = {
            'chunkSize': '分块大小',
            'overlapSize': '重叠大小',
            'maxParagraphsPerChunk': '每块最大段落数',
            'minChunkSize': '最小分块大小',
            'maxChunkSize': '最大分块大小',
            'threshold': '阈值'
        };
        return labels[key] || key;
    };

    /**
     * 渲染内容输入区域
     */
    const renderContentInput = () => {
        return (
            <Card
                title="输入内容"
                style={{ marginBottom: 20 }}
            >
                <textarea
                    className="content-input"
                    value={content}
                    onChange={(e) => setContent(e.target.value)}
                    placeholder="请输入要分块的文本内容..."
                    rows={10}
                    style={{
                        width: '100%',
                        padding: '12px',
                        fontSize: '14px',
                        lineHeight: '1.6',
                        border: '1px solid #d9d9d9',
                        borderRadius: '4px',
                        resize: 'vertical'
                    }}
                />
                <div style={{ marginTop: 8, color: '#999', fontSize: '12px' }}>
                    字符数: {content.length}
                </div>
            </Card>
        );
    };

    /**
     * 渲染统计信息
     */
    const renderStatistics = (stats) => {
        if (!stats) return null;

        return (
            <Row gutter={16} style={{ marginBottom: 20 }}>
                <Col span={6}>
                    <Statistic
                        title="总分块数"
                        value={stats.totalChunks}
                        prefix={<ThunderboltOutlined />}
                    />
                </Col>
                <Col span={6}>
                    <Statistic
                        title="平均长度"
                        value={stats.avgLength}
                        suffix="字符"
                    />
                </Col>
                <Col span={6}>
                    <Statistic
                        title="最小长度"
                        value={stats.minLength}
                        suffix="字符"
                    />
                </Col>
                <Col span={6}>
                    <Statistic
                        title="最大长度"
                        value={stats.maxLength}
                        suffix="字符"
                    />
                </Col>
            </Row>
        );
    };

    // ========== 主渲染 ==========

    return (
        <div className="chunking-strategy-configurator">
            <Card
                title={
                    <Space>
                        <SettingOutlined />
                        <span>分块策略配置器</span>
                        <Tag color="blue">Phase 4.2.1</Tag>
                    </Space>
                }
                extra={
                    <Space>
                        <Tag color="green">
                            <CheckCircleOutlined /> 可用策略: {strategies.length}
                        </Tag>
                    </Space>
                }
            >
                <Row gutter={[20, 20]}>
                    {/* 左侧：配置区域 */}
                    <Col span={10}>
                        {renderContentInput()}
                        {renderStrategySelector()}
                    </Col>

                    {/* 右侧：预览区域 */}
                    <Col span={14}>
                        <Spin spinning={loading}>
                            <Tabs activeKey={activeTab} onChange={setActiveTab}>
                                {/* 单策略预览 */}
                                <TabPane tab="预览结果" key="single">
                                    {previewResult ? (
                                        <>
                                            {renderStatistics(previewResult.statistics)}
                                            <Alert
                                                message={`处理耗时: ${previewResult.elapsedTimeMs}ms`}
                                                type="success"
                                                showIcon
                                                style={{ marginBottom: 16 }}
                                            />
                                            <ChunkPreviewList chunks={previewResult.chunks} />
                                        </>
                                    ) : (
                                        <Empty
                                            description="请配置参数后点击"预览分块"查看效果"
                                            style={{ padding: '60px 0' }}
                                        />
                                    )}
                                </TabPane>

                                {/* 多策略对比 */}
                                <TabPane tab="策略对比" key="compare">
                                    {comparisonResults.length > 0 ? (
                                        <StrategyComparisonChart results={comparisonResults} />
                                    ) : (
                                        <Empty
                                            description="点击"对比策略"查看不同策略的效果对比"
                                            style={{ padding: '60px 0' }}
                                        />
                                    )}
                                </TabPane>
                            </Tabs>
                        </Spin>
                    </Col>
                </Row>
            </Card>
        </div>
    );
}

// 导出到全局
window.ChunkingStrategyConfigurator = ChunkingStrategyConfigurator;

export default ChunkingStrategyConfigurator;

