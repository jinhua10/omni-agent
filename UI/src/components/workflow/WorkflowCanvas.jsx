/**
 * 工作流画布组件 (Workflow Canvas Component)
 * 
 * 可视化展示和编辑工作流步骤
 * (Visualize and edit workflow steps)
 * 
 * @author AI Reviewer Team
 * @since 2025-12-21
 */

import React, { useRef, useState, useCallback, useEffect } from 'react';
import { Button, Tooltip } from 'antd';
import { 
  EditOutlined, 
  DeleteOutlined,
  LinkOutlined,
  DisconnectOutlined 
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import WorkflowNode from './WorkflowNode';

/**
 * 工作流画布组件 (Workflow Canvas Component)
 */
const WorkflowCanvas = ({
  workflow,
  onStepClick,
  onStepDelete,
  onStepMove,
  onAddDependency,
  onRemoveDependency,
}) => {
  const { t } = useLanguage();
  const canvasRef = useRef(null);
  const [draggingNode, setDraggingNode] = useState(null);
  const [dragOffset, setDragOffset] = useState({ x: 0, y: 0 });
  const [connectingFrom, setConnectingFrom] = useState(null);
  const [hoveredNode, setHoveredNode] = useState(null);
  const [canvasOffset, setCanvasOffset] = useState({ x: 0, y: 0 });
  const [isPanning, setIsPanning] = useState(false);
  const [panStart, setPanStart] = useState({ x: 0, y: 0 });

  /**
   * 开始拖拽节点 (Start dragging node)
   */
  const handleNodeMouseDown = useCallback((e, step) => {
    if (e.button !== 0) return; // 只处理左键 (Only handle left click)
    
    e.stopPropagation();
    const rect = canvasRef.current.getBoundingClientRect();
    setDraggingNode(step.id);
    setDragOffset({
      x: e.clientX - rect.left - (step.position?.x || 0) - canvasOffset.x,
      y: e.clientY - rect.top - (step.position?.y || 0) - canvasOffset.y,
    });
  }, [canvasOffset]);

  /**
   * 开始连接节点 (Start connecting nodes)
   */
  const handleStartConnect = useCallback((e, stepId) => {
    e.stopPropagation();
    setConnectingFrom(stepId);
  }, []);

  /**
   * 完成连接 (Complete connection)
   */
  const handleCompleteConnect = useCallback((e, toStepId) => {
    e.stopPropagation();
    if (connectingFrom && connectingFrom !== toStepId) {
      onAddDependency(connectingFrom, toStepId);
    }
    setConnectingFrom(null);
  }, [connectingFrom, onAddDependency]);

  /**
   * 处理鼠标移动 (Handle mouse move)
   */
  const handleMouseMove = useCallback((e) => {
    if (draggingNode) {
      const rect = canvasRef.current.getBoundingClientRect();
      const newX = e.clientX - rect.left - dragOffset.x - canvasOffset.x;
      const newY = e.clientY - rect.top - dragOffset.y - canvasOffset.y;
      onStepMove(draggingNode, { x: newX, y: newY });
    } else if (isPanning) {
      setCanvasOffset({
        x: canvasOffset.x + (e.clientX - panStart.x),
        y: canvasOffset.y + (e.clientY - panStart.y),
      });
      setPanStart({ x: e.clientX, y: e.clientY });
    }
  }, [draggingNode, dragOffset, onStepMove, isPanning, canvasOffset, panStart]);

  /**
   * 处理鼠标释放 (Handle mouse up)
   */
  const handleMouseUp = useCallback(() => {
    setDraggingNode(null);
    setIsPanning(false);
  }, []);

  /**
   * 开始拖动画布 (Start panning canvas)
   */
  const handleCanvasMouseDown = useCallback((e) => {
    if (e.button === 0) { // 左键 (Left click)
      setIsPanning(true);
      setPanStart({ x: e.clientX, y: e.clientY });
    }
  }, []);

  /**
   * 绘制连接线 (Draw connection lines)
   */
  const renderConnections = useCallback(() => {
    const connections = [];
    
    workflow.steps.forEach(step => {
      const dependencies = step.dependencies || [];
      const toPos = step.position || { x: 0, y: 0 };
      const toX = toPos.x + 120; // 节点宽度的一半 (Half of node width)
      const toY = toPos.y + 40;  // 节点高度的一半 (Half of node height)
      
      dependencies.forEach(depId => {
        const fromStep = workflow.steps.find(s => s.id === depId);
        if (fromStep) {
          const fromPos = fromStep.position || { x: 0, y: 0 };
          const fromX = fromPos.x + 120;
          const fromY = fromPos.y + 40;
          
          connections.push({
            key: `${depId}-${step.id}`,
            from: { x: fromX, y: fromY },
            to: { x: toX, y: toY },
            fromStepId: depId,
            toStepId: step.id,
          });
        }
      });
    });
    
    return connections;
  }, [workflow.steps]);

  /**
   * 绘制箭头 (Draw arrow)
   */
  const renderArrow = useCallback((from, to, fromStepId, toStepId) => {
    const dx = to.x - from.x;
    const dy = to.y - from.y;
    const angle = Math.atan2(dy, dx);
    const arrowSize = 10;
    
    // 箭头终点调整 (Adjust arrow end point)
    const endX = to.x - Math.cos(angle) * 40;
    const endY = to.y - Math.sin(angle) * 40;
    
    return (
      <g key={`${fromStepId}-${toStepId}`}>
        {/* 连接线 (Connection line) */}
        <line
          x1={from.x + canvasOffset.x}
          y1={from.y + canvasOffset.y}
          x2={endX + canvasOffset.x}
          y2={endY + canvasOffset.y}
          stroke="#667eea"
          strokeWidth="2"
          markerEnd="url(#arrowhead)"
        />
        
        {/* 删除连接按钮 (Delete connection button) */}
        <g
          transform={`translate(${(from.x + endX) / 2 + canvasOffset.x}, ${(from.y + endY) / 2 + canvasOffset.y})`}
          onClick={(e) => {
            e.stopPropagation();
            onRemoveDependency(fromStepId, toStepId);
          }}
          className="connection-delete"
        >
          <circle r="12" fill="white" stroke="#ff4d4f" strokeWidth="2" />
          <line x1="-6" y1="-6" x2="6" y2="6" stroke="#ff4d4f" strokeWidth="2" />
          <line x1="-6" y1="6" x2="6" y2="-6" stroke="#ff4d4f" strokeWidth="2" />
        </g>
      </g>
    );
  }, [canvasOffset, onRemoveDependency]);

  useEffect(() => {
    if (draggingNode || isPanning) {
      document.addEventListener('mousemove', handleMouseMove);
      document.addEventListener('mouseup', handleMouseUp);
      return () => {
        document.removeEventListener('mousemove', handleMouseMove);
        document.removeEventListener('mouseup', handleMouseUp);
      };
    }
  }, [draggingNode, isPanning, handleMouseMove, handleMouseUp]);

  const connections = renderConnections();

  return (
    <div 
      className="workflow-canvas" 
      ref={canvasRef}
      onMouseDown={handleCanvasMouseDown}
    >
      {/* SVG 层用于绘制连接线 (SVG layer for drawing connections) */}
      <svg className="canvas-svg">
        <defs>
          <marker
            id="arrowhead"
            markerWidth="10"
            markerHeight="10"
            refX="8"
            refY="3"
            orient="auto"
          >
            <polygon points="0 0, 10 3, 0 6" fill="#667eea" />
          </marker>
        </defs>
        
        {connections.map(conn => 
          renderArrow(conn.from, conn.to, conn.fromStepId, conn.toStepId)
        )}
        
        {/* 正在连接的临时线 (Temporary line while connecting) */}
        {connectingFrom && hoveredNode && (
          <line
            x1={(workflow.steps.find(s => s.id === connectingFrom)?.position?.x || 0) + 120 + canvasOffset.x}
            y1={(workflow.steps.find(s => s.id === connectingFrom)?.position?.y || 0) + 40 + canvasOffset.y}
            x2={(workflow.steps.find(s => s.id === hoveredNode)?.position?.x || 0) + 120 + canvasOffset.x}
            y2={(workflow.steps.find(s => s.id === hoveredNode)?.position?.y || 0) + 40 + canvasOffset.y}
            stroke="#667eea"
            strokeWidth="2"
            strokeDasharray="5,5"
          />
        )}
      </svg>

      {/* 工作流节点层 (Workflow nodes layer) */}
      <div 
        className="canvas-nodes"
        style={{
          transform: `translate(${canvasOffset.x}px, ${canvasOffset.y}px)`,
        }}
      >
        {workflow.steps.map((step) => (
          <WorkflowNode
            key={step.id}
            step={step}
            isConnecting={connectingFrom !== null}
            isConnectingFrom={connectingFrom === step.id}
            isDragging={draggingNode === step.id}
            onMouseDown={(e) => handleNodeMouseDown(e, step)}
            onStartConnect={(e) => handleStartConnect(e, step.id)}
            onCompleteConnect={(e) => handleCompleteConnect(e, step.id)}
            onEdit={() => onStepClick(step)}
            onDelete={() => onStepDelete(step.id)}
            onMouseEnter={() => setHoveredNode(step.id)}
            onMouseLeave={() => setHoveredNode(null)}
          />
        ))}
      </div>

      {/* 空状态提示 (Empty state hint) */}
      {workflow.steps.length === 0 && (
        <div className="canvas-empty">
          <p>🎨 {t('workflowBuilder.canvas.emptyHint')}</p>
          <p>{t('workflowBuilder.canvas.addFirstStep')}</p>
        </div>
      )}
    </div>
  );
};

export default WorkflowCanvas;
