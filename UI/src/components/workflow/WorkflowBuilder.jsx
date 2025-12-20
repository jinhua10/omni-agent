/**
 * 工作流构建器组件 (Workflow Builder Component)
 * 
 * 提供可视化的工作流设计界面
 * (Provides visual workflow design interface)
 * 
 * @author AI Reviewer Team
 * @since 2025-12-21
 */

import React, { useState, useCallback } from 'react';
import { Button, Input, Select, Modal, Drawer, Tabs, App } from 'antd';
import {
  SaveOutlined,
  PlayCircleOutlined,
  PlusOutlined,
  DeleteOutlined,
  SettingOutlined,
  ArrowLeftOutlined,
  ExportOutlined,
  ImportOutlined,
  RobotOutlined,
  ThunderboltOutlined,
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import WorkflowCanvas from './WorkflowCanvas';
import StepEditor from './StepEditor';
import AgentSelector from './AgentSelector';
import workflowApi from '../../api/modules/workflow';
import '../../assets/css/workflow/workflow-builder.css';

const { 
  createWorkflow, 
  updateWorkflow, 
  executeWorkflow,
  getAgentList 
} = workflowApi;

const { TabPane } = Tabs;

/**
 * 工作流构建器组件 (Workflow Builder Component)
 */
const WorkflowBuilder = ({ workflowId, onBack }) => {
  const { t } = useLanguage();
  const { message } = App.useApp();
  
  // 工作流基本信息 (Workflow basic info)
  const [workflow, setWorkflow] = useState({
    name: '',
    version: '1.0.0',
    description: '',
    author: '',
    category: 'general',
    tags: [],
    status: 'draft',
    steps: [],
  });
  
  // 选中的步骤 (Selected step)
  const [selectedStep, setSelectedStep] = useState(null);
  
  // 编辑器抽屉 (Editor drawer)
  const [editorVisible, setEditorVisible] = useState(false);
  
  // Agent 选择器 (Agent selector)
  const [agentSelectorVisible, setAgentSelectorVisible] = useState(false);
  
  // 可用的 Agents (Available agents)
  const [agents, setAgents] = useState([]);
  
  // 加载状态 (Loading state)
  const [loading, setLoading] = useState(false);
  
  // 测试执行抽屉 (Test execution drawer)
  const [testDrawerVisible, setTestDrawerVisible] = useState(false);
  const [testInput, setTestInput] = useState('{}');
  const [testResult, setTestResult] = useState(null);
  
  // AI 生成工作流 (AI workflow generation)
  const [aiDescription, setAiDescription] = useState('');
  const [aiGenerating, setAiGenerating] = useState(false);

  /**
   * 加载可用的 Agents (Load available agents)
   */
  const loadAgents = useCallback(async () => {
    try {
      const result = await getAgentList();
      setAgents(result.agents || []);
    } catch (error) {
      console.error('Failed to load agents:', error);
      message.error(t('workflowBuilder.agents.loadFailed'));
    }
  }, [t]);

  /**
   * 添加新步骤 (Add new step)
   */
  const handleAddStep = useCallback(() => {
    setAgentSelectorVisible(true);
    loadAgents();
  }, [loadAgents]);

  /**
   * 选择 Agent 后创建步骤 (Create step after agent selected)
   */
  const handleAgentSelected = useCallback((agent) => {
    const newStep = {
      id: `step_${Date.now()}`,
      name: agent.name || 'New Step',
      description: '',
      agent: agent.name,
      input: '${workflow.input}',
      config: {},
      dependencies: [],
      allowFailure: false,
      timeout: 60000,
      retries: 0,
      condition: '',
      position: {
        x: 100,
        y: 100 + workflow.steps.length * 120,
      },
    };
    
    setWorkflow(prev => ({
      ...prev,
      steps: [...prev.steps, newStep],
    }));
    
    setAgentSelectorVisible(false);
    message.success(t('workflowBuilder.step.added'));
  }, [workflow.steps.length, t]);

  /**
   * 删除步骤 (Delete step)
   */
  const handleDeleteStep = useCallback((stepId) => {
    Modal.confirm({
      title: t('workflowBuilder.step.deleteConfirm'),
      content: t('workflowBuilder.step.deleteWarning'),
      onOk: () => {
        setWorkflow(prev => ({
          ...prev,
          steps: prev.steps.filter(s => s.id !== stepId),
        }));
        message.success(t('workflowBuilder.step.deleted'));
      },
    });
  }, [t]);

  /**
   * 编辑步骤 (Edit step)
   */
  const handleEditStep = useCallback((step) => {
    setSelectedStep(step);
    setEditorVisible(true);
  }, []);

  /**
   * 更新步骤 (Update step)
   */
  const handleUpdateStep = useCallback((updatedStep) => {
    setWorkflow(prev => ({
      ...prev,
      steps: prev.steps.map(s => 
        s.id === updatedStep.id ? updatedStep : s
      ),
    }));
    setEditorVisible(false);
    message.success(t('workflowBuilder.step.updated'));
  }, [t]);

  /**
   * 添加依赖关系 (Add dependency)
   */
  const handleAddDependency = useCallback((fromStepId, toStepId) => {
    setWorkflow(prev => ({
      ...prev,
      steps: prev.steps.map(step => {
        if (step.id === toStepId) {
          const dependencies = step.dependencies || [];
          if (!dependencies.includes(fromStepId)) {
            return {
              ...step,
              dependencies: [...dependencies, fromStepId],
            };
          }
        }
        return step;
      }),
    }));
  }, []);

  /**
   * 移除依赖关系 (Remove dependency)
   */
  const handleRemoveDependency = useCallback((fromStepId, toStepId) => {
    setWorkflow(prev => ({
      ...prev,
      steps: prev.steps.map(step => {
        if (step.id === toStepId) {
          return {
            ...step,
            dependencies: (step.dependencies || []).filter(id => id !== fromStepId),
          };
        }
        return step;
      }),
    }));
  }, []);

  /**
   * 保存工作流 (Save workflow)
   */
  const handleSave = useCallback(async () => {
    // 验证工作流 (Validate workflow)
    if (!workflow.name) {
      message.error(t('workflowBuilder.validation.nameRequired'));
      return;
    }
    
    if (workflow.steps.length === 0) {
      message.error(t('workflowBuilder.validation.stepsRequired'));
      return;
    }

    setLoading(true);
    try {
      if (workflowId) {
        await updateWorkflow(workflowId, workflow);
        message.success(t('workflowBuilder.save.updated'));
      } else {
        await createWorkflow(workflow);
        message.success(t('workflowBuilder.save.created'));
      }
    } catch (error) {
      console.error('Failed to save workflow:', error);
      message.error(t('workflowBuilder.save.failed') + ': ' + error.message);
    } finally {
      setLoading(false);
    }
  }, [workflow, workflowId, t]);

  /**
   * 测试执行工作流 (Test execute workflow)
   */
  const handleTest = useCallback(async () => {
    try {
      const input = JSON.parse(testInput);
      setLoading(true);
      
      const result = await executeWorkflow(workflow.name, input);
      setTestResult(result);
      message.success(t('workflowBuilder.test.success'));
    } catch (error) {
      console.error('Failed to execute workflow:', error);
      message.error(t('workflowBuilder.test.failed') + ': ' + error.message);
      setTestResult({ error: error.message });
    } finally {
      setLoading(false);
    }
  }, [workflow.name, testInput, t]);

  /**
   * 导出工作流 (Export workflow)
   */
  const handleExport = useCallback(() => {
    const dataStr = JSON.stringify(workflow, null, 2);
    const dataBlob = new Blob([dataStr], { type: 'application/json' });
    const url = URL.createObjectURL(dataBlob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `${workflow.name || 'workflow'}.json`;
    link.click();
    URL.revokeObjectURL(url);
    message.success(t('workflowBuilder.export.success'));
  }, [workflow, t]);

  /**
   * 导入工作流 (Import workflow)
   */
  const handleImport = useCallback((e) => {
    const file = e.target.files?.[0];
    if (!file) return;

    const reader = new FileReader();
    reader.onload = (event) => {
      try {
        const imported = JSON.parse(event.target.result);
        setWorkflow(imported);
        message.success(t('workflowBuilder.import.success'));
      } catch (error) {
        message.error(t('workflowBuilder.import.failed'));
      }
    };
    reader.readAsText(file);
  }, [t]);

  /**
   * AI 生成工作流 (AI generate workflow)
   */
  const handleAiGenerate = useCallback(async () => {
    if (!aiDescription.trim()) {
      message.warning(t('workflowBuilder.ai.descriptionRequired'));
      return;
    }

    setAiGenerating(true);
    try {
      const result = await workflowApi.generateWorkflowFromDescription(aiDescription);
      
      if (result.success && result.workflow) {
        // 为每个步骤添加位置信息
        const workflowWithPositions = {
          ...result.workflow,
          steps: result.workflow.steps.map((step, index) => ({
            ...step,
            id: step.id || `step_${Date.now()}_${index}`,
            position: {
              x: 100,
              y: 100 + index * 150,
            },
          })),
        };
        
        setWorkflow(workflowWithPositions);
        message.success(t('workflowBuilder.ai.generateSuccess'));
        setAiDescription(''); // 清空描述
      } else {
        message.error(result.message || t('workflowBuilder.ai.generateFailed'));
      }
    } catch (error) {
      console.error('AI generation failed:', error);
      message.error(t('workflowBuilder.ai.generateFailed') + ': ' + error.message);
    } finally {
      setAiGenerating(false);
    }
  }, [aiDescription, t]);

  return (
    <div className="workflow-builder">
      {/* AI 生成区域 (AI Generation Area) */}
      <div className="workflow-ai-generator">
        <div className="ai-generator-icon">
          <RobotOutlined style={{ fontSize: 24 }} />
        </div>
        <Input.TextArea
          placeholder={t('workflowBuilder.ai.placeholder')}
          value={aiDescription}
          onChange={(e) => setAiDescription(e.target.value)}
          autoSize={{ minRows: 2, maxRows: 4 }}
          style={{ flex: 1, marginLeft: 12, marginRight: 12 }}
        />
        <Button
          type="primary"
          size="large"
          icon={<ThunderboltOutlined />}
          onClick={handleAiGenerate}
          loading={aiGenerating}
          disabled={!aiDescription.trim()}
        >
          {aiGenerating ? t('workflowBuilder.ai.generating') : t('workflowBuilder.ai.generate')}
        </Button>
      </div>

      {/* 顶部工具栏 (Top toolbar) */}
      <div className="workflow-toolbar">
        <div className="toolbar-left">
          <Button
            icon={<ArrowLeftOutlined />}
            onClick={onBack}
          >
            {t('common.back')}
          </Button>
          
          <Input
            placeholder={t('workflowBuilder.namePlaceholder')}
            value={workflow.name}
            onChange={(e) => setWorkflow(prev => ({ ...prev, name: e.target.value }))}
            style={{ width: 300, marginLeft: 12 }}
          />
          
          <Select
            value={workflow.status}
            onChange={(value) => setWorkflow(prev => ({ ...prev, status: value }))}
            style={{ width: 120, marginLeft: 12 }}
          >
            <Select.Option value="draft">{t('workflowBuilder.status.draft')}</Select.Option>
            <Select.Option value="active">{t('workflowBuilder.status.active')}</Select.Option>
            <Select.Option value="deprecated">{t('workflowBuilder.status.deprecated')}</Select.Option>
          </Select>
        </div>

        <div className="toolbar-right">
          <Button
            icon={<PlusOutlined />}
            onClick={handleAddStep}
            type="primary"
          >
            {t('workflowBuilder.addStep')}
          </Button>
          
          <Button
            icon={<PlayCircleOutlined />}
            onClick={() => setTestDrawerVisible(true)}
            disabled={!workflow.name || workflow.steps.length === 0}
          >
            {t('workflowBuilder.testButton')}
          </Button>
          
          <Button
            icon={<ExportOutlined />}
            onClick={handleExport}
          >
            {t('workflowBuilder.exportButton')}
          </Button>
          
          <Button
            icon={<ImportOutlined />}
            onClick={() => document.getElementById('workflow-import-input').click()}
          >
            {t('workflowBuilder.importButton')}
          </Button>
          <input
            id="workflow-import-input"
            type="file"
            accept=".json"
            style={{ display: 'none' }}
            onChange={handleImport}
          />
          
          <Button
            icon={<SaveOutlined />}
            onClick={handleSave}
            type="primary"
            loading={loading}
          >
            {t('common.save')}
          </Button>
        </div>
      </div>

      {/* 工作流画布 (Workflow canvas) */}
      <WorkflowCanvas
        workflow={workflow}
        onStepClick={handleEditStep}
        onStepDelete={handleDeleteStep}
        onAddDependency={handleAddDependency}
        onRemoveDependency={handleRemoveDependency}
        onStepMove={(stepId, position) => {
          setWorkflow(prev => ({
            ...prev,
            steps: prev.steps.map(s =>
              s.id === stepId ? { ...s, position } : s
            ),
          }));
        }}
      />

      {/* 步骤编辑器 (Step editor) */}
      <Drawer
        title={t('workflowBuilder.stepEditor.title')}
        size="large"
        open={editorVisible}
        onClose={() => setEditorVisible(false)}
        destroyOnClose
      >
        {selectedStep && (
          <StepEditor
            step={selectedStep}
            allSteps={workflow.steps}
            onUpdate={handleUpdateStep}
            onCancel={() => setEditorVisible(false)}
          />
        )}
      </Drawer>

      {/* Agent 选择器 (Agent selector) */}
      <Modal
        title={t('workflowBuilder.agentSelector.title')}
        open={agentSelectorVisible}
        onCancel={() => setAgentSelectorVisible(false)}
        footer={null}
        width={800}
      >
        <AgentSelector
          agents={agents}
          onSelect={handleAgentSelected}
        />
      </Modal>

      {/* 测试执行抽屉 (Test execution drawer) */}
      <Drawer
        title={t('workflowBuilder.test.title')}
        size="large"
        open={testDrawerVisible}
        onClose={() => setTestDrawerVisible(false)}
      >
        <div className="test-panel">
          <h3>{t('workflowBuilder.test.input')}</h3>
          <Input.TextArea
            value={testInput}
            onChange={(e) => setTestInput(e.target.value)}
            rows={8}
            placeholder='{"key": "value"}'
          />
          
          <Button
            type="primary"
            icon={<PlayCircleOutlined />}
            onClick={handleTest}
            loading={loading}
            style={{ marginTop: 12 }}
          >
            {t('workflowBuilder.test.execute')}
          </Button>

          {testResult && (
            <>
              <h3 style={{ marginTop: 24 }}>{t('workflowBuilder.test.result')}</h3>
              <pre className="test-result">
                {JSON.stringify(testResult, null, 2)}
              </pre>
            </>
          )}
        </div>
      </Drawer>
    </div>
  );
};

export default WorkflowBuilder;
