import React, { useState, useEffect } from 'react';
import { Modal, Form, Input, Select, InputNumber, Button, List, Tag, Space, Popconfirm, App } from 'antd';
import { SaveOutlined, DeleteOutlined, CheckCircleOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';

const { Option } = Select;
const { TextArea } = Input;

/**
 * 策略模板管理组件
 *
 * 功能：
 * 1. 保存当前配置为策略模板
 * 2. 列出所有已保存的模板
 * 3. 应用模板到当前文档
 * 4. 删除模板
 */
function StrategyTemplateManager({ visible, onClose, currentConfig, onApplyTemplate }) {
  const { t } = useLanguage();
  const [form] = Form.useForm();
  const { message } = App.useApp();

  const [templates, setTemplates] = useState([]);
  const [loading, setLoading] = useState(false);
  const [saveMode, setSaveMode] = useState(false);

  // 加载所有模板
  const loadTemplates = async () => {
    try {
      const response = await fetch('/api/system/rag-config/templates');
      const result = await response.json();
      if (result.success) {
        setTemplates(result.data);
      }
    } catch (error) {
      console.error('加载策略模板失败:', error);
    }
  };

  useEffect(() => {
    if (visible) {
      loadTemplates();
    }
  }, [visible]);

  // 保存为新模板
  const handleSaveAsTemplate = async (values) => {
    setLoading(true);
    try {
      const template = {
        templateName: values.templateName,
        description: values.description,
        textExtractionModel: currentConfig.textExtractionModel,
        chunkingStrategy: currentConfig.chunkingStrategy,
        chunkingParams: currentConfig.chunkingParams || {},
        isDefault: values.isDefault || false
      };

      const response = await fetch('/api/system/rag-config/templates', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(template)
      });

      const result = await response.json();
      if (result.success) {
        message.success('策略模板保存成功！');
        form.resetFields();
        setSaveMode(false);
        loadTemplates();
      } else {
        message.error(result.message || '保存失败');
      }
    } catch (error) {
      message.error('保存失败：' + error.message);
    } finally {
      setLoading(false);
    }
  };

  // 删除模板
  const handleDeleteTemplate = async (templateId) => {
    try {
      const response = await fetch(`/api/system/rag-config/templates/${templateId}`, {
        method: 'DELETE'
      });

      const result = await response.json();
      if (result.success) {
        message.success('模板删除成功！');
        loadTemplates();
      } else {
        message.error(result.message || '删除失败');
      }
    } catch (error) {
      message.error('删除失败：' + error.message);
    }
  };

  // 应用模板
  const handleApplyTemplate = (template) => {
    if (onApplyTemplate) {
      onApplyTemplate(template);
    }
    message.success(`已应用策略模板：${template.templateName}`);
    onClose();
  };

  return (
    <Modal
      title="策略模板管理"
      open={visible}
      onCancel={onClose}
      width={800}
      footer={[
        <Button key="close" onClick={onClose}>
          关闭
        </Button>
      ]}
    >
      <div style={{ marginBottom: 16 }}>
        <Button
          type="primary"
          icon={<SaveOutlined />}
          onClick={() => setSaveMode(!saveMode)}
        >
          {saveMode ? '取消保存' : '保存当前配置为模板'}
        </Button>
      </div>

      {/* 保存模板表单 */}
      {saveMode && (
        <Form
          form={form}
          layout="vertical"
          onFinish={handleSaveAsTemplate}
          style={{
            marginBottom: 24,
            padding: 16,
            background: '#f5f5f5',
            borderRadius: 8
          }}
        >
          <Form.Item
            name="templateName"
            label="模板名称"
            rules={[{ required: true, message: '请输入模板名称' }]}
          >
            <Input placeholder="例如：PDF标准处理" />
          </Form.Item>

          <Form.Item
            name="description"
            label="模板描述"
          >
            <TextArea rows={3} placeholder="描述这个模板的用途和特点" />
          </Form.Item>

          <Form.Item>
            <Space>
              <Button type="primary" htmlType="submit" loading={loading}>
                保存模板
              </Button>
              <Button onClick={() => setSaveMode(false)}>
                取消
              </Button>
            </Space>
          </Form.Item>
        </Form>
      )}

      {/* 模板列表 */}
      <div>
        <h4>已保存的策略模板</h4>
        {templates.length === 0 ? (
          <div style={{ textAlign: 'center', padding: 40, color: '#999' }}>
            暂无保存的策略模板
          </div>
        ) : (
          <List
            dataSource={templates}
            renderItem={(template) => (
              <List.Item
                key={template.templateId}
                actions={[
                  <Button
                    type="link"
                    icon={<CheckCircleOutlined />}
                    onClick={() => handleApplyTemplate(template)}
                  >
                    应用
                  </Button>,
                  <Popconfirm
                    title="确定要删除这个模板吗？"
                    onConfirm={() => handleDeleteTemplate(template.templateId)}
                    okText="确定"
                    cancelText="取消"
                  >
                    <Button
                      type="link"
                      danger
                      icon={<DeleteOutlined />}
                    >
                      删除
                    </Button>
                  </Popconfirm>
                ]}
              >
                <List.Item.Meta
                  title={
                    <Space>
                      <span>{template.templateName}</span>
                      {template.isDefault && <Tag color="green">默认</Tag>}
                      {template.useCount > 0 && (
                        <Tag color="blue">已使用 {template.useCount} 次</Tag>
                      )}
                    </Space>
                  }
                  description={
                    <div>
                      <div>{template.description || '无描述'}</div>
                      <div style={{ marginTop: 8, fontSize: 12, color: '#666' }}>
                        文本提取: <Tag>{template.textExtractionModel}</Tag>
                        分块策略: <Tag>{template.chunkingStrategy}</Tag>
                      </div>
                    </div>
                  }
                />
              </List.Item>
            )}
          />
        )}
      </div>
    </Modal>
  );
}

export default StrategyTemplateManager;

