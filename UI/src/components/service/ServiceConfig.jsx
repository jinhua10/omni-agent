import React, { useState } from 'react';
import { Form, Input, Switch, Select, Button, message } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import { serviceApi } from '../../api/modules/service';

const { Option } = Select;

const ServiceConfig = ({ service, onUpdate }) => {
  const { t } = useLanguage();
  const [form] = Form.useForm();
  const [saving, setSaving] = useState(false);

  const handleSave = async () => {
    try {
      const values = await form.validateFields();
      setSaving(true);
      await serviceApi.updateServiceConfig(service.id, values);
      message.success(t('aiService.config.saveSuccess'));
      onUpdate();
    } catch (error) {
      console.error('Failed to save config:', error);
      message.error(t('aiService.config.saveFailed'));
    } finally {
      setSaving(false);
    }
  };

  return (
    <div className="service-config">
      <Form
        form={form}
        layout="vertical"
        initialValues={service.config || {}}
      >
        <Form.Item
          name="enabled"
          label={t('aiService.config.enabled')}
          valuePropName="checked"
        >
          <Switch />
        </Form.Item>

        <Form.Item
          name="model"
          label={t('aiService.config.model')}
        >
          <Select placeholder={t('aiService.config.selectModel')}>
            <Option value="local">{t('aiService.localModel')}</Option>
            <Option value="online">{t('aiService.onlineModel')}</Option>
          </Select>
        </Form.Item>

        <Form.Item
          name="apiKey"
          label={t('aiService.config.apiKey')}
        >
          <Input.Password placeholder={t('aiService.config.apiKeyPlaceholder')} />
        </Form.Item>

        <Form.Item>
          <Button type="primary" onClick={handleSave} loading={saving}>
            {t('common.save')}
          </Button>
        </Form.Item>
      </Form>
    </div>
  );
};

export default ServiceConfig;

