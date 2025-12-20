/**
 * 步骤编辑器组件 (Step Editor Component)
 * 
 * 用于编辑工作流步骤的详细属性
 * (Edit detailed properties of workflow steps)
 * 
 * @author AI Reviewer Team
 * @since 2025-12-21
 */

import React, { useState, useEffect } from 'react';
import { Form, Input, InputNumber, Select, Switch, Button, Space, Tag } from 'antd';
import { PlusOutlined, MinusCircleOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';

const { TextArea } = Input;
const { Option } = Select;

/**
 * 步骤编辑器组件 (Step Editor Component)
 */
const StepEditor = ({ step, allSteps, onUpdate, onCancel }) => {
  const { t } = useLanguage();
  const [form] = Form.useForm();
  const [inputMode, setInputMode] = useState('expression');

  useEffect(() => {
    form.setFieldsValue({
      ...step,
      dependencies: step.dependencies || [],
    });
    
    // 检测输入模式 (Detect input mode)
    if (typeof step.input === 'string') {
      setInputMode('expression');
    } else {
      setInputMode('json');
    }
  }, [step, form]);

  /**
   * 处理表单提交 (Handle form submit)
   */
  const handleSubmit = () => {
    form.validateFields().then(values => {
      // 处理输入格式 (Process input format)
      let processedInput = values.input;
      if (inputMode === 'json' && typeof values.input === 'string') {
        try {
          processedInput = JSON.parse(values.input);
        } catch (error) {
          form.setFields([{
            name: 'input',
            errors: [t('workflowBuilder.stepEditor.invalidJson')],
          }]);
          return;
        }
      }
      
      onUpdate({
        ...step,
        ...values,
        input: processedInput,
      });
    });
  };

  /**
   * 获取可用的依赖步骤 (Get available dependency steps)
   */
  const getAvailableDependencies = () => {
    return allSteps.filter(s => s.id !== step.id);
  };

  return (
    <Form
      form={form}
      layout="vertical"
      onFinish={handleSubmit}
    >
      {/* 步骤名称 (Step name) */}
      <Form.Item
        label={t('workflowBuilder.stepEditor.name')}
        name="name"
        rules={[{ required: true, message: t('workflowBuilder.stepEditor.nameRequired') }]}
      >
        <Input placeholder={t('workflowBuilder.stepEditor.namePlaceholder')} />
      </Form.Item>

      {/* 步骤描述 (Step description) */}
      <Form.Item
        label={t('workflowBuilder.stepEditor.description')}
        name="description"
      >
        <TextArea
          rows={3}
          placeholder={t('workflowBuilder.stepEditor.descriptionPlaceholder')}
        />
      </Form.Item>

      {/* Agent 名称 (Agent name) */}
      <Form.Item
        label={t('workflowBuilder.stepEditor.agent')}
        name="agent"
        rules={[{ required: true, message: t('workflowBuilder.stepEditor.agentRequired') }]}
      >
        <Input
          placeholder="EchoAgent, DataTransformer, etc."
          disabled
        />
      </Form.Item>

      {/* 输入配置 (Input configuration) */}
      <Form.Item label={t('workflowBuilder.stepEditor.input')}>
        <Space direction="vertical" style={{ width: '100%' }}>
          <Select
            value={inputMode}
            onChange={setInputMode}
            style={{ width: 150 }}
          >
            <Option value="expression">{t('workflowBuilder.stepEditor.expression')}</Option>
            <Option value="json">JSON</Option>
          </Select>
          
          <Form.Item
            name="input"
            noStyle
            rules={[{ required: true, message: t('workflowBuilder.stepEditor.inputRequired') }]}
          >
            <TextArea
              rows={inputMode === 'json' ? 6 : 3}
              placeholder={
                inputMode === 'expression'
                  ? '${workflow.input} or ${step_id.output}'
                  : '{"key": "value", "data": "${step1.output}"}'
              }
            />
          </Form.Item>
          
          <div className="input-hints">
            <Tag color="blue">${'workflow.input'}</Tag>
            <Tag color="green">${'{step_id.output}'}</Tag>
            <Tag color="orange">${'{step_id.output.field}'}</Tag>
          </div>
        </Space>
      </Form.Item>

      {/* 依赖步骤 (Dependencies) */}
      <Form.Item
        label={t('workflowBuilder.stepEditor.dependencies')}
        name="dependencies"
      >
        <Select
          mode="multiple"
          placeholder={t('workflowBuilder.stepEditor.dependenciesPlaceholder')}
          options={getAvailableDependencies().map(s => ({
            label: s.name || s.id,
            value: s.id,
          }))}
        />
      </Form.Item>

      {/* 高级配置 (Advanced configuration) */}
      <div className="advanced-config">
        <h4>{t('workflowBuilder.stepEditor.advancedConfig')}</h4>
        
        {/* 允许失败 (Allow failure) */}
        <Form.Item
          label={t('workflowBuilder.stepEditor.allowFailure')}
          name="allowFailure"
          valuePropName="checked"
          tooltip={t('workflowBuilder.stepEditor.allowFailureTooltip')}
        >
          <Switch />
        </Form.Item>

        {/* 超时时间 (Timeout) */}
        <Form.Item
          label={t('workflowBuilder.stepEditor.timeout')}
          name="timeout"
          tooltip={t('workflowBuilder.stepEditor.timeoutTooltip')}
        >
          <InputNumber
            min={1000}
            max={600000}
            step={1000}
            addonAfter="ms"
            style={{ width: '100%' }}
          />
        </Form.Item>

        {/* 重试次数 (Retries) */}
        <Form.Item
          label={t('workflowBuilder.stepEditor.retries')}
          name="retries"
          tooltip={t('workflowBuilder.stepEditor.retriesTooltip')}
        >
          <InputNumber
            min={0}
            max={10}
            style={{ width: '100%' }}
          />
        </Form.Item>

        {/* 条件执行 (Condition) */}
        <Form.Item
          label={t('workflowBuilder.stepEditor.condition')}
          name="condition"
          tooltip={t('workflowBuilder.stepEditor.conditionTooltip')}
        >
          <Input
            placeholder="${step1.output.status} == 'success'"
          />
        </Form.Item>
      </div>

      {/* 操作按钮 (Action buttons) */}
      <Form.Item>
        <Space>
          <Button type="primary" htmlType="submit">
            {t('common.save')}
          </Button>
          <Button onClick={onCancel}>
            {t('common.cancel')}
          </Button>
        </Space>
      </Form.Item>
    </Form>
  );
};

export default StepEditor;
