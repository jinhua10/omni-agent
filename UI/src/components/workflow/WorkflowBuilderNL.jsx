/**
 * 自然语言工作流构建器 (Natural Language Workflow Builder)
 * 
 * 用户通过自然语言描述构建工作流，无需关心底层Agent实现
 * (Users build workflows through natural language, no need to know Agent implementation)
 * 
 * @author AI Reviewer Team
 * @since 2025-12-21
 */

import React, { useState, useCallback, useRef, useEffect } from 'react';
import { Button, Input, Card, Empty, Space, Tag, Tooltip, App, Switch } from 'antd';
import {
  SaveOutlined,
  PlayCircleOutlined,
  ArrowLeftOutlined,
  PlusOutlined,
  DeleteOutlined,
  EditOutlined,
  ThunderboltOutlined,
  BulbOutlined,
  CheckCircleOutlined,
  CloseCircleOutlined,
  ArrowRightOutlined,
  RobotOutlined,
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import workflowApi from '../../api/modules/workflow';
import '../../assets/css/workflow/workflow-builder-nl.css';

const { TextArea } = Input;
const { createWorkflow, updateWorkflow, executeWorkflow, generateWorkflowFromDescription } = workflowApi;

/**
 * 自然语言工作流构建器
 */
const WorkflowBuilderNL = ({ workflowId, onBack }) => {
  const { t } = useLanguage();
  const { message } = App.useApp();
  
  // 工作流名称和描述
  const [workflowName, setWorkflowName] = useState('');
  const [workflowDescription, setWorkflowDescription] = useState('');
  
  // 步骤列表 (每个步骤就是一个自然语言描述)
  const [steps, setSteps] = useState([]);
  
  // 当前编辑的步骤
  const [editingStepId, setEditingStepId] = useState(null);
  const [editingStepText, setEditingStepText] = useState('');
  
  // 新步骤输入
  const [newStepText, setNewStepText] = useState('');
  
  // AI 辅助模式
  const [aiAssistEnabled, setAiAssistEnabled] = useState(true);
  const [aiSuggestions, setAiSuggestions] = useState([]);
  
  // 保存/测试状态
  const [saving, setSaving] = useState(false);
  const [testing, setTesting] = useState(false);
  
  // 引用
  const newStepInputRef = useRef(null);

  /**
   * 添加新步骤
   */
  const handleAddStep = useCallback(() => {
    if (!newStepText.trim()) {
      message.warning('请输入步骤描述');
      return;
    }

    const newStep = {
      id: `step_${Date.now()}`,
      description: newStepText.trim(),
      createdAt: new Date().toISOString(),
    };

    setSteps(prev => [...prev, newStep]);
    setNewStepText('');
    message.success('步骤已添加');
    
    // 聚焦回输入框
    setTimeout(() => {
      newStepInputRef.current?.focus();
    }, 100);
  }, [newStepText, message]);

  /**
   * 删除步骤
   */
  const handleDeleteStep = useCallback((stepId) => {
    setSteps(prev => prev.filter(s => s.id !== stepId));
    message.success('步骤已删除');
  }, [message]);

  /**
   * 开始编辑步骤
   */
  const handleStartEdit = useCallback((step) => {
    setEditingStepId(step.id);
    setEditingStepText(step.description);
  }, []);

  /**
   * 保存编辑
   */
  const handleSaveEdit = useCallback(() => {
    if (!editingStepText.trim()) {
      message.warning('步骤描述不能为空');
      return;
    }

    setSteps(prev => prev.map(s => 
      s.id === editingStepId 
        ? { ...s, description: editingStepText.trim() }
        : s
    ));
    
    setEditingStepId(null);
    setEditingStepText('');
    message.success('步骤已更新');
  }, [editingStepId, editingStepText, message]);

  /**
   * 取消编辑
   */
  const handleCancelEdit = useCallback(() => {
    setEditingStepId(null);
    setEditingStepText('');
  }, []);

  /**
   * AI 辅助：根据当前步骤推荐下一步
   */
  const handleGetAISuggestions = useCallback(async () => {
    if (steps.length === 0) {
      setAiSuggestions([
        '验证输入数据的格式和完整性',
        '从数据库中查询相关信息',
        '调用外部API获取数据',
        '对数据进行清洗和转换',
      ]);
      return;
    }

    // 基于已有步骤的简单推荐逻辑
    const lastStep = steps[steps.length - 1].description.toLowerCase();
    const suggestions = [];

    if (lastStep.includes('验证') || lastStep.includes('检查')) {
      suggestions.push('处理验证失败的情况', '记录验证结果', '继续处理验证通过的数据');
    } else if (lastStep.includes('查询') || lastStep.includes('获取')) {
      suggestions.push('解析查询结果', '对结果进行过滤和排序', '缓存查询结果');
    } else if (lastStep.includes('转换') || lastStep.includes('处理')) {
      suggestions.push('验证处理结果', '存储处理后的数据', '通知相关系统');
    } else {
      suggestions.push('记录执行日志', '发送通知', '清理临时数据', '返回执行结果');
    }

    setAiSuggestions(suggestions);
  }, [steps]);

  /**
   * 使用 AI 建议
   */
  const handleUseSuggestion = useCallback((suggestion) => {
    setNewStepText(suggestion);
    setAiSuggestions([]);
    newStepInputRef.current?.focus();
  }, []);

  /**
   * 保存工作流
   */
  const handleSaveWorkflow = useCallback(async () => {
    if (!workflowName.trim()) {
      message.error('请输入工作流名称');
      return;
    }

    if (steps.length === 0) {
      message.error('至少需要一个步骤');
      return;
    }

    setSaving(true);
    try {
      const workflowData = {
        name: workflowName.trim(),
        description: workflowDescription.trim(),
        version: '1.0.0',
        category: 'general',
        status: 'draft',
        // 将自然语言步骤转换为后端格式
        steps: steps.map((step, index) => ({
          id: step.id,
          name: `步骤 ${index + 1}`,
          description: step.description,
          // 后端会根据description智能匹配Agent
          agent: 'auto', 
          input: {},
          dependencies: index > 0 ? [steps[index - 1].id] : [],
        })),
      };

      if (workflowId) {
        await updateWorkflow(workflowId, workflowData);
        message.success('工作流更新成功');
      } else {
        await createWorkflow(workflowData);
        message.success('工作流创建成功');
      }
    } catch (error) {
      console.error('Save workflow failed:', error);
      message.error('保存失败: ' + error.message);
    } finally {
      setSaving(false);
    }
  }, [workflowName, workflowDescription, steps, workflowId, message]);

  /**
   * 测试运行工作流
   */
  const handleTestWorkflow = useCallback(async () => {
    if (steps.length === 0) {
      message.warning('请先添加步骤');
      return;
    }

    setTesting(true);
    try {
      message.info('开始测试执行...');
      
      const workflowData = {
        name: workflowName || '测试工作流',
        steps: steps.map((step, index) => ({
          id: step.id,
          name: `步骤 ${index + 1}`,
          description: step.description,
          agent: 'auto',
          input: {},
          dependencies: index > 0 ? [steps[index - 1].id] : [],
        })),
      };

      const result = await executeWorkflow(workflowData);
      message.success('测试执行成功');
      console.log('Workflow execution result:', result);
    } catch (error) {
      console.error('Test workflow failed:', error);
      message.error('测试执行失败: ' + error.message);
    } finally {
      setTesting(false);
    }
  }, [steps, workflowName, message]);

  /**
   * AI 一键生成工作流
   */
  const handleAIGenerate = useCallback(async () => {
    if (!workflowDescription.trim()) {
      message.warning('请先输入工作流描述');
      return;
    }

    setSaving(true);
    try {
      const result = await generateWorkflowFromDescription(workflowDescription);
      
      if (result.success && result.workflow) {
        setWorkflowName(result.workflow.name || '');
        setSteps(result.workflow.steps.map(step => ({
          id: step.id || `step_${Date.now()}_${Math.random()}`,
          description: step.description || step.name,
          createdAt: new Date().toISOString(),
        })));
        message.success('AI 生成成功！');
      } else {
        message.error('AI 生成失败');
      }
    } catch (error) {
      console.error('AI generate failed:', error);
      message.error('AI 生成失败: ' + error.message);
    } finally {
      setSaving(false);
    }
  }, [workflowDescription, message]);

  // 自动获取AI建议
  useEffect(() => {
    if (aiAssistEnabled && steps.length > 0) {
      handleGetAISuggestions();
    }
  }, [steps.length, aiAssistEnabled, handleGetAISuggestions]);

  return (
    <div className="workflow-builder-nl">
      {/* 顶部工具栏 */}
      <div className="workflow-builder-header">
        <Button 
          icon={<ArrowLeftOutlined />} 
          onClick={onBack}
          type="text"
        >
          返回
        </Button>
        
        <div className="header-actions">
          <Space>
            <Button
              icon={<PlayCircleOutlined />}
              onClick={handleTestWorkflow}
              loading={testing}
            >
              测试运行
            </Button>
            <Button
              type="primary"
              icon={<SaveOutlined />}
              onClick={handleSaveWorkflow}
              loading={saving}
            >
              保存工作流
            </Button>
          </Space>
        </div>
      </div>

      {/* 主内容区 */}
      <div className="workflow-builder-content">
        {/* 左侧：工作流信息和步骤列表 */}
        <div className="workflow-left-panel">
          {/* 工作流基本信息 */}
          <Card 
            title="工作流信息" 
            size="small"
            extra={
              <Space>
                <Tooltip title="AI 辅助模式会根据上下文推荐下一步操作">
                  <Switch
                    checked={aiAssistEnabled}
                    onChange={setAiAssistEnabled}
                    checkedChildren={<RobotOutlined />}
                    unCheckedChildren={<RobotOutlined />}
                    size="small"
                  />
                </Tooltip>
                <span style={{ fontSize: '12px', color: '#999' }}>AI 辅助</span>
              </Space>
            }
          >
            <Space orientation="vertical" style={{ width: '100%' }} size="middle">
              <div>
                <label className="input-label">工作流名称</label>
                <Input
                  placeholder="例如：用户注册审核流程"
                  value={workflowName}
                  onChange={e => setWorkflowName(e.target.value)}
                  size="large"
                />
              </div>
              
              <div>
                <label className="input-label">
                  工作流描述
                  <Button
                    type="link"
                    size="small"
                    icon={<ThunderboltOutlined />}
                    onClick={handleAIGenerate}
                    loading={saving}
                    style={{ marginLeft: 8 }}
                  >
                    AI 一键生成
                  </Button>
                </label>
                <TextArea
                  placeholder="用一段话描述这个工作流要做什么，AI 可以帮你自动生成步骤"
                  value={workflowDescription}
                  onChange={e => setWorkflowDescription(e.target.value)}
                  rows={3}
                />
              </div>
            </Space>
          </Card>

          {/* 步骤列表 */}
          <Card 
            title={
              <Space>
                <span>工作流步骤</span>
                <Tag color="blue">{steps.length} 个步骤</Tag>
              </Space>
            }
            size="small"
            className="steps-card"
          >
            <div className="steps-list">
              {steps.length === 0 ? (
                <Empty
                  image={Empty.PRESENTED_IMAGE_SIMPLE}
                  description="还没有步骤，从下方添加第一个步骤吧"
                />
              ) : (
                steps.map((step, index) => (
                  <div key={step.id} className="step-item">
                    <div className="step-number">{index + 1}</div>
                    
                    {editingStepId === step.id ? (
                      <div className="step-edit-area">
                        <TextArea
                          value={editingStepText}
                          onChange={e => setEditingStepText(e.target.value)}
                          autoSize={{ minRows: 2, maxRows: 6 }}
                          autoFocus
                        />
                        <div className="step-edit-actions">
                          <Button
                            type="primary"
                            size="small"
                            icon={<CheckCircleOutlined />}
                            onClick={handleSaveEdit}
                          >
                            保存
                          </Button>
                          <Button
                            size="small"
                            icon={<CloseCircleOutlined />}
                            onClick={handleCancelEdit}
                          >
                            取消
                          </Button>
                        </div>
                      </div>
                    ) : (
                      <>
                        <div className="step-content">
                          <div className="step-description">{step.description}</div>
                        </div>
                        
                        <div className="step-actions">
                          <Tooltip title="编辑">
                            <Button
                              type="text"
                              size="small"
                              icon={<EditOutlined />}
                              onClick={() => handleStartEdit(step)}
                            />
                          </Tooltip>
                          <Tooltip title="删除">
                            <Button
                              type="text"
                              size="small"
                              danger
                              icon={<DeleteOutlined />}
                              onClick={() => handleDeleteStep(step.id)}
                            />
                          </Tooltip>
                        </div>
                      </>
                    )}
                    
                    {index < steps.length - 1 && (
                      <div className="step-connector">
                        <ArrowRightOutlined />
                      </div>
                    )}
                  </div>
                ))
              )}
            </div>

            {/* 添加新步骤 */}
            <div className="add-step-area">
              <TextArea
                ref={newStepInputRef}
                placeholder="描述这一步要做什么，例如：验证用户输入的邮箱格式是否正确"
                value={newStepText}
                onChange={e => setNewStepText(e.target.value)}
                onPressEnter={(e) => {
                  if (e.ctrlKey || e.metaKey) {
                    handleAddStep();
                  }
                }}
                autoSize={{ minRows: 2, maxRows: 6 }}
              />
              <Button
                type="dashed"
                icon={<PlusOutlined />}
                onClick={handleAddStep}
                block
                style={{ marginTop: 8 }}
              >
                添加步骤 (Ctrl+Enter)
              </Button>
            </div>
          </Card>
        </div>

        {/* 右侧：AI 建议面板 */}
        {aiAssistEnabled && aiSuggestions.length > 0 && (
          <div className="workflow-right-panel">
            <Card 
              title={
                <Space>
                  <BulbOutlined style={{ color: '#faad14' }} />
                  <span>AI 推荐下一步</span>
                </Space>
              }
              size="small"
              className="suggestions-card"
            >
              <div className="suggestions-list">
                {aiSuggestions.map((suggestion, index) => (
                  <div 
                    key={index} 
                    className="suggestion-item"
                    onClick={() => handleUseSuggestion(suggestion)}
                  >
                    <div className="suggestion-icon">💡</div>
                    <div className="suggestion-text">{suggestion}</div>
                  </div>
                ))}
              </div>
              
              <div className="suggestions-tip">
                💡 点击建议可快速添加，或自己输入任何想法
              </div>
            </Card>
          </div>
        )}
      </div>
    </div>
  );
};

export default WorkflowBuilderNL;
