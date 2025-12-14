import React from 'react';
import { Form, Select, Button, Card, message } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import { adminApi } from '../../api/modules/admin';

const { Option } = Select;

const ModelConfig = () => {
  const { t } = useLanguage();
  const [form] = Form.useForm();

  const handleSave = async () => {
    try {
      const values = await form.validateFields();
      await adminApi.updateModelConfig(values);
      message.success(t('admin.model.saveSuccess'));
    } catch (error) {
      console.error('Failed to save model config:', error);
      message.error(t('admin.model.saveFailed'));
    }
  };

  return (
    <div className="model-config">
      <Card title={t('admin.model.llmModel')}>
        <Form form={form} layout="vertical">
          <Form.Item name="llmModel" label={t('admin.model.selectModel')}>
            <Select>
              <Option value="gpt-3.5">{t('admin.model.gpt35')}</Option>
              <Option value="gpt-4">{t('admin.model.gpt4')}</Option>
              <Option value="local">{t('admin.model.local')}</Option>
            </Select>
          </Form.Item>
          <Form.Item name="vectorDB" label={t('admin.model.vectorDB')}>
            <Select>
              <Option value="faiss">FAISS</Option>
              <Option value="milvus">Milvus</Option>
              <Option value="chroma">Chroma</Option>
            </Select>
          </Form.Item>
          <Form.Item>
            <Button type="primary" onClick={handleSave}>{t('common.save')}</Button>
          </Form.Item>
        </Form>
      </Card>
    </div>
  );
};

export default ModelConfig;

