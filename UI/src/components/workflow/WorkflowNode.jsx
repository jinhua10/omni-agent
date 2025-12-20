/**
 * 工作流节点组件 (Workflow Node Component)
 * 
 * 表示工作流中的单个步骤节点
 * (Represents a single step node in the workflow)
 * 
 * @author AI Reviewer Team
 * @since 2025-12-21
 */

import React from 'react';
import { Tooltip, Badge } from 'antd';
import {
  EditOutlined,
  DeleteOutlined,
  LinkOutlined,
  ClockCircleOutlined,
  WarningOutlined,
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';

/**
 * 工作流节点组件 (Workflow Node Component)
 */
const WorkflowNode = ({
  step,
  isConnecting,
  isConnectingFrom,
  isDragging,
  onMouseDown,
  onStartConnect,
  onCompleteConnect,
  onEdit,
  onDelete,
  onMouseEnter,
  onMouseLeave,
}) => {
  const { t } = useLanguage();
  
  // 节点样式 (Node style)
  const nodeStyle = {
    position: 'absolute',
    left: `${step.position?.x || 0}px`,
    top: `${step.position?.y || 0}px`,
    cursor: isDragging ? 'grabbing' : 'grab',
    opacity: isDragging ? 0.7 : 1,
    transform: isDragging ? 'scale(1.05)' : 'scale(1)',
    transition: isDragging ? 'none' : 'all 0.2s ease',
  };

  // 节点类名 (Node className)
  const nodeClassName = [
    'workflow-node',
    isConnectingFrom && 'connecting-from',
    isConnecting && !isConnectingFrom && 'connectable',
  ].filter(Boolean).join(' ');

  /**
   * 获取步骤状态图标 (Get step status icon)
   */
  const getStepIcon = () => {
    if (step.allowFailure) {
      return (
        <Tooltip title={t('workflowBuilder.node.allowFailure')}>
          <WarningOutlined style={{ color: '#faad14' }} />
        </Tooltip>
      );
    }
    if (step.timeout && step.timeout !== 60000) {
      return (
        <Tooltip title={t('workflowBuilder.node.timeout', step.timeout)}>
          <ClockCircleOutlined style={{ color: '#1890ff' }} />
        </Tooltip>
      );
    }
    return null;
  };

  return (
    <div
      className={nodeClassName}
      style={nodeStyle}
      onMouseDown={onMouseDown}
      onMouseEnter={onMouseEnter}
      onMouseLeave={onMouseLeave}
    >
      {/* 节点头部 (Node header) */}
      <div className="node-header">
        <div className="node-title">
          <span className="node-icon">⚙️</span>
          <span className="node-name">{step.name || step.id}</span>
        </div>
        
        <div className="node-actions">
          {getStepIcon()}
          
          <Tooltip title={t('workflowBuilder.node.connect')}>
            <LinkOutlined
              className="node-action-icon"
              onClick={(e) => {
                e.stopPropagation();
                if (isConnecting && !isConnectingFrom) {
                  onCompleteConnect(e);
                } else {
                  onStartConnect(e);
                }
              }}
            />
          </Tooltip>
          
          <Tooltip title={t('common.edit')}>
            <EditOutlined
              className="node-action-icon"
              onClick={(e) => {
                e.stopPropagation();
                onEdit();
              }}
            />
          </Tooltip>
          
          <Tooltip title={t('common.delete')}>
            <DeleteOutlined
              className="node-action-icon delete-icon"
              onClick={(e) => {
                e.stopPropagation();
                onDelete();
              }}
            />
          </Tooltip>
        </div>
      </div>

      {/* 节点内容 (Node content) */}
      <div className="node-content">
        <div className="node-agent">
          <span className="node-label">{t('workflowBuilder.node.agent')}:</span>
          <Badge status="processing" text={step.agent} />
        </div>
        
        {step.description && (
          <div className="node-description">
            {step.description}
          </div>
        )}
        
        {step.dependencies && step.dependencies.length > 0 && (
          <div className="node-dependencies">
            <span className="node-label">{t('workflowBuilder.node.dependencies')}:</span>
            <span className="dependency-count">{step.dependencies.length}</span>
          </div>
        )}
      </div>

      {/* 连接点 (Connection points) */}
      <div className="node-connectors">
        <div className="connector connector-in" title={t('workflowBuilder.node.input')} />
        <div className="connector connector-out" title={t('workflowBuilder.node.output')} />
      </div>
    </div>
  );
};

export default WorkflowNode;
