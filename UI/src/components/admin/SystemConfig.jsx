import React from 'react';
import { Form, Input, Select, Switch, Button, Space, App } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import { adminApi } from '../../api/modules/admin';

const { Option } = Select;

const SystemConfig = () => {
  const { t } = useLanguage();
  const { message } = App.useApp();
  const [form] = Form.useForm();

  const handleSave = async () => {
    try {
      const values = await form.validateFields();
      await adminApi.updateSystemConfig(values);
      message.success(t('admin.config.saveSuccess'));
    } catch (error) {
      console.error('Failed to save config:', error);
      message.error(t('admin.config.saveFailed'));
    }
  };

  return (
    <div className="system-config">
      <Form form={form} layout="vertical" initialValues={{ cache: true, maxFileSize: 100 }}>
        <Form.Item name="systemName" label={t('admin.config.systemName')}>
          <Input />
        </Form.Item>
        <Form.Item name="maxFileSize" label={t('admin.config.maxFileSize')}>
          <Space.Compact style={{ width: '100%' }}>
            <Input type="number" style={{ width: 'calc(100% - 50px)' }} />
            <Input value="MB" disabled style={{ width: '50px' }} />
          </Space.Compact>
        </Form.Item>
        <Form.Item name="cache" label={t('admin.config.enableCache')} valuePropName="checked">
          <Switch />
        </Form.Item>
        <Form.Item>
          <Button type="primary" onClick={handleSave}>{t('common.save')}</Button>
        </Form.Item>
      </Form>
    </div>
  );
};

export default SystemConfig;

