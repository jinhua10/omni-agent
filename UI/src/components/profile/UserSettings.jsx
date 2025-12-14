import React from 'react';
import { Form, Switch, Select, Button, message } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import { profileApi } from '../../api/modules/profile';

const { Option } = Select;

const UserSettings = () => {
  const { t, language, toggleLanguage } = useLanguage();
  const [form] = Form.useForm();

  const handleSave = async () => {
    try {
      const values = await form.validateFields();
      await profileApi.updateSettings(values);
      message.success(t('profile.settingsSaved'));
    } catch (error) {
      console.error('Failed to save settings:', error);
      message.error(t('profile.settingsFailed'));
    }
  };

  return (
    <div className="user-settings">
      <Form
        form={form}
        layout="vertical"
        initialValues={{
          language: language,
          theme: 'light',
          notifications: true,
        }}
      >
        <Form.Item
          name="language"
          label={t('profile.language')}
        >
          <Select onChange={toggleLanguage}>
            <Option value="zh">中文</Option>
            <Option value="en">English</Option>
          </Select>
        </Form.Item>

        <Form.Item
          name="theme"
          label={t('profile.theme')}
        >
          <Select>
            <Option value="light">{t('profile.lightTheme')}</Option>
            <Option value="dark">{t('profile.darkTheme')}</Option>
            <Option value="auto">{t('profile.autoTheme')}</Option>
          </Select>
        </Form.Item>

        <Form.Item
          name="notifications"
          label={t('profile.notifications')}
          valuePropName="checked"
        >
          <Switch />
        </Form.Item>

        <Form.Item>
          <Button type="primary" onClick={handleSave}>
            {t('common.save')}
          </Button>
        </Form.Item>
      </Form>
    </div>
  );
};

export default UserSettings;

