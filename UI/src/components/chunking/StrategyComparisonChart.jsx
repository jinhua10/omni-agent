/**
 * 策略对比图表组件 (Strategy Comparison Chart)
 *
 * 可视化展示不同分块策略的对比结果
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */

import React from 'react';
import { Card, Row, Col, Statistic, Table, Tag, Space, Divider } from 'antd';
import {
    ThunderboltOutlined,
    ClockCircleOutlined,
    FileTextOutlined,
    RiseOutlined
} from '@ant-design/icons';
import ChunkPreviewList from './ChunkPreviewList';

/**
 * 策略对比图表组件
 */
function StrategyComparisonChart({ results = [] }) {

    /**
     * 构建对比表格数据
     */
    const getTableData = () => {
        return results.map((result, index) => ({
            key: index,
            strategy: result.strategy,
            chunkCount: result.chunkCount,
            avgLength: result.statistics?.avgLength || 0,
            minLength: result.statistics?.minLength || 0,
            maxLength: result.statistics?.maxLength || 0,
            elapsedTime: result.elapsedTimeMs
        }));
    };

    /**
     * 表格列定义
     */
    const columns = [
        {
            title: '策略',
            dataIndex: 'strategy',
            key: 'strategy',
            render: (text) => <Tag color="blue">{text}</Tag>
        },
        {
            title: '分块数量',
            dataIndex: 'chunkCount',
            key: 'chunkCount',
            sorter: (a, b) => a.chunkCount - b.chunkCount,
            render: (value) => (
                <Space>
                    <FileTextOutlined />
                    <span>{value}</span>
                </Space>
            )
        },
        {
            title: '平均长度',
            dataIndex: 'avgLength',
            key: 'avgLength',
            sorter: (a, b) => a.avgLength - b.avgLength,
            render: (value) => `${value} 字符`
        },
        {
            title: '最小/最大长度',
            key: 'range',
            render: (_, record) => `${record.minLength} ~ ${record.maxLength}`
        },
        {
            title: '处理耗时',
            dataIndex: 'elapsedTime',
            key: 'elapsedTime',
            sorter: (a, b) => a.elapsedTime - b.elapsedTime,
            render: (value) => (
                <Space>
                    <ClockCircleOutlined />
                    <span>{value}ms</span>
                </Space>
            )
        }
    ];

    /**
     * 找出最佳策略
     */
    const getBestStrategy = () => {
        if (results.length === 0) return null;

        // 综合评分：考虑分块数量和处理速度
        const scores = results.map(result => ({
            strategy: result.strategy,
            score: (1 / result.chunkCount) * 1000 + (1 / result.elapsedTimeMs) * 10000
        }));

        return scores.reduce((best, current) =>
            current.score > best.score ? current : best
        );
    };

    const bestStrategy = getBestStrategy();

    if (results.length === 0) {
        return <div style={{ textAlign: 'center', padding: '40px 0', color: '#999' }}>
            暂无对比数据
        </div>;
    }

    return (
        <div className="strategy-comparison-chart">
            {/* 最佳推荐 */}
            {bestStrategy && (
                <Card
                    size="small"
                    style={{ marginBottom: 20, background: '#f6ffed', borderColor: '#b7eb8f' }}
                >
                    <Space>
                        <RiseOutlined style={{ color: '#52c41a', fontSize: '18px' }} />
                        <span style={{ color: '#52c41a', fontWeight: 'bold' }}>
                            推荐策略:
                        </span>
                        <Tag color="success">{bestStrategy.strategy}</Tag>
                        <span style={{ color: '#666', fontSize: '12px' }}>
                            (综合考虑分块质量和处理速度)
                        </span>
                    </Space>
                </Card>
            )}

            {/* 对比表格 */}
            <Card title="策略对比" size="small" style={{ marginBottom: 20 }}>
                <Table
                    dataSource={getTableData()}
                    columns={columns}
                    pagination={false}
                    size="small"
                />
            </Card>

            {/* 详细统计 */}
            <Row gutter={[16, 16]}>
                {results.map((result, index) => (
                    <Col span={24} key={index}>
                        <Card
                            title={
                                <Space>
                                    <Tag color="blue">{result.strategy}</Tag>
                                    {bestStrategy && bestStrategy.strategy === result.strategy && (
                                        <Tag color="success">推荐</Tag>
                                    )}
                                </Space>
                            }
                            size="small"
                        >
                            {/* 统计卡片 */}
                            <Row gutter={16} style={{ marginBottom: 16 }}>
                                <Col span={6}>
                                    <Statistic
                                        title="分块数量"
                                        value={result.chunkCount}
                                        prefix={<FileTextOutlined />}
                                        valueStyle={{ fontSize: '24px' }}
                                    />
                                </Col>
                                <Col span={6}>
                                    <Statistic
                                        title="平均长度"
                                        value={result.statistics?.avgLength || 0}
                                        suffix="字符"
                                        valueStyle={{ fontSize: '24px' }}
                                    />
                                </Col>
                                <Col span={6}>
                                    <Statistic
                                        title="长度范围"
                                        value={`${result.statistics?.minLength || 0} ~ ${result.statistics?.maxLength || 0}`}
                                        valueStyle={{ fontSize: '16px' }}
                                    />
                                </Col>
                                <Col span={6}>
                                    <Statistic
                                        title="处理耗时"
                                        value={result.elapsedTimeMs}
                                        suffix="ms"
                                        prefix={<ClockCircleOutlined />}
                                        valueStyle={{ fontSize: '24px' }}
                                    />
                                </Col>
                            </Row>

                            <Divider />

                            {/* 分块预览（前3个） */}
                            {result.chunkPreviews && result.chunkPreviews.length > 0 && (
                                <>
                                    <h4 style={{ marginBottom: 12 }}>分块预览（前3个）</h4>
                                    <ChunkPreviewList chunks={result.chunkPreviews} />
                                </>
                            )}
                        </Card>
                    </Col>
                ))}
            </Row>
        </div>
    );
}

export default StrategyComparisonChart;

