/**
 * Agent 选择器组件 (Agent Selector Component)
 * 
 * 用于选择可用的 Agent 来创建工作流步骤
 * (Select available agents to create workflow steps)
 * 
 * @author AI Reviewer Team
 * @since 2025-12-21
 */

import React, { useState } from 'react';
import { Input, List, Card, Tag, Empty } from 'antd';
import { SearchOutlined, RobotOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';

const { Search } = Input;

/**
 * Agent 选择器组件 (Agent Selector Component)
 */
const AgentSelector = ({ agents, onSelect }) => {
  const { t } = useLanguage();
  const [searchKeyword, setSearchKeyword] = useState('');

  /**
   * 过滤 Agents (Filter agents)
   */
  const filteredAgents = agents.filter(agent => {
    if (!searchKeyword) return true;
    const keyword = searchKeyword.toLowerCase();
    return (
      agent.name?.toLowerCase().includes(keyword) ||
      agent.description?.toLowerCase().includes(keyword) ||
      agent.category?.toLowerCase().includes(keyword)
    );
  });

  /**
   * 获取 Agent 类别颜色 (Get agent category color)
   */
  const getCategoryColor = (category) => {
    const colors = {
      'data': 'blue',
      'transform': 'green',
      'validate': 'orange',
      'utility': 'purple',
      'integration': 'cyan',
      'ai': 'magenta',
    };
    return colors[category?.toLowerCase()] || 'default';
  };

  return (
    <div className="agent-selector">
      {/* 搜索框 (Search box) */}
      <Search
        placeholder={t('workflowBuilder.agentSelector.searchPlaceholder')}
        prefix={<SearchOutlined />}
        value={searchKeyword}
        onChange={(e) => setSearchKeyword(e.target.value)}
        style={{ marginBottom: 16 }}
        allowClear
      />

      {/* Agent 列表 (Agent list) */}
      {filteredAgents.length === 0 ? (
        <Empty description={t('workflowBuilder.agentSelector.noAgents')} />
      ) : (
        <List
          grid={{ gutter: 16, column: 2 }}
          dataSource={filteredAgents}
          renderItem={(agent) => (
            <List.Item>
              <Card
                hoverable
                onClick={() => onSelect(agent)}
                className="agent-card"
              >
                <div className="agent-card-header">
                  <RobotOutlined className="agent-icon" />
                  <h3>{agent.name}</h3>
                </div>
                
                {agent.description && (
                  <p className="agent-description">{agent.description}</p>
                )}
                
                <div className="agent-meta">
                  {agent.category && (
                    <Tag color={getCategoryColor(agent.category)}>
                      {agent.category}
                    </Tag>
                  )}
                  
                  {agent.tags && agent.tags.map(tag => (
                    <Tag key={tag}>{tag}</Tag>
                  ))}
                </div>
              </Card>
            </List.Item>
          )}
        />
      )}
    </div>
  );
};

export default AgentSelector;
