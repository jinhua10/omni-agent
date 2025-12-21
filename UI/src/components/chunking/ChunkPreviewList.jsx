/**
 * 分块预览列表组件 (Chunk Preview List)
 *
 * 展示分块结果的详细信息
 *
 * @author OmniAgent Team
 * @since 2.0.0 (Phase 4)
 */

import React, { useState } from 'react';
import { List, Card, Tag, Space, Collapse, Typography, Badge } from 'antd';
import {
    FileTextOutlined,
    InfoCircleOutlined,
    EyeOutlined
} from '@ant-design/icons';

const { Panel } = Collapse;
const { Paragraph, Text } = Typography;

/**
 * 分块预览列表组件
 */
function ChunkPreviewList({ chunks = [] }) {
    const [expandedChunks, setExpandedChunks] = useState([]);

    /**
     * 渲染分块卡片
     */
    const renderChunkCard = (chunk) => {
        const isExpanded = expandedChunks.includes(chunk.id);

        return (
            <Card
                size="small"
                hoverable
                className="chunk-preview-card"
                title={
                    <Space>
                        <FileTextOutlined />
                        <span>分块 #{chunk.sequence + 1}</span>
                        <Tag color="blue">{chunk.contentLength} 字符</Tag>
                    </Space>
                }
                extra={
                    <Space size="small">
                        {chunk.metadata && chunk.metadata.strategy && (
                            <Tag color="green">{chunk.metadata.strategy}</Tag>
                        )}
                        <EyeOutlined />
                    </Space>
                }
            >
                {/* 内容预览 */}
                <Paragraph
                    ellipsis={{
                        rows: isExpanded ? 999 : 3,
                        expandable: true,
                        symbol: isExpanded ? '收起' : '展开',
                        onExpand: (_, info) => {
                            if (info.expanded) {
                                setExpandedChunks([...expandedChunks, chunk.id]);
                            } else {
                                setExpandedChunks(expandedChunks.filter(id => id !== chunk.id));
                            }
                        }
                    }}
                    style={{
                        marginBottom: 8,
                        whiteSpace: 'pre-wrap',
                        wordBreak: 'break-word'
                    }}
                >
                    {chunk.content}
                </Paragraph>

                {/* 元数据 */}
                {chunk.metadata && Object.keys(chunk.metadata).length > 0 && (
                    <Collapse
                        ghost
                        size="small"
                        style={{ marginTop: 8 }}
                    >
                        <Panel
                            header={
                                <Space size="small">
                                    <InfoCircleOutlined />
                                    <Text type="secondary" style={{ fontSize: '12px' }}>
                                        元数据
                                    </Text>
                                </Space>
                            }
                            key="1"
                        >
                            <Space direction="vertical" size="small">
                                {Object.entries(chunk.metadata).map(([key, value]) => (
                                    <Text key={key} type="secondary" style={{ fontSize: '12px' }}>
                                        <strong>{key}:</strong> {JSON.stringify(value)}
                                    </Text>
                                ))}
                            </Space>
                        </Panel>
                    </Collapse>
                )}
            </Card>
        );
    };

    if (chunks.length === 0) {
        return <div style={{ textAlign: 'center', padding: '40px 0', color: '#999' }}>
            暂无分块数据
        </div>;
    }

    return (
        <div className="chunk-preview-list">
            <List
                dataSource={chunks}
                renderItem={(chunk) => (
                    <List.Item key={chunk.id}>
                        {renderChunkCard(chunk)}
                    </List.Item>
                )}
                pagination={{
                    pageSize: 5,
                    showSizeChanger: false,
                    showTotal: (total) => `共 ${total} 个分块`
                }}
            />
        </div>
    );
}

export default ChunkPreviewList;

