import React, { useState } from 'react';
import { Modal, Form, Input, Upload, Button, message } from 'antd';
import { UploadOutlined, UserOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { profileApi } from '../../api/modules/profile';

const { TextArea } = Input;

const ProfileEditor = ({ visible, userInfo, onClose, onSuccess }) => {
  const { t } = useLanguage();
  const [form] = Form.useForm();
  const [saving, setSaving] = useState(false);

  const handleSubmit = async () => {
    try {
      const values = await form.validateFields();
      setSaving(true);
      await profileApi.updateUserInfo(values);
      message.success(t('profile.updateSuccess'));
      onSuccess();
    } catch (error) {
      console.error('Failed to update profile:', error);
      message.error(t('profile.updateFailed'));
    } finally {
      setSaving(false);
    }
  };

  return (
    <Modal
      title={t('profile.editInfo')}
      open={visible}
      onCancel={onClose}
      onOk={handleSubmit}
      confirmLoading={saving}
      width={600}
    >
      <Form
        form={form}
        layout="vertical"
        initialValues={userInfo}
      >
        <Form.Item
          name="nickname"
          label={t('profile.nickname')}
          rules={[{ required: true, message: t('profile.nicknameRequired') }]}
        >
          <Input prefix={<UserOutlined />} />
        </Form.Item>

        <Form.Item
          name="email"
          label={t('profile.email')}
          rules={[
            { required: true, message: t('profile.emailRequired') },
            { type: 'email', message: t('profile.emailInvalid') }
          ]}
        >
          <Input />
        </Form.Item>

        <Form.Item
          name="bio"
          label={t('profile.bio')}
        >
          <TextArea rows={4} maxLength={200} showCount />
        </Form.Item>

        <Form.Item
          name="avatar"
          label={t('profile.avatar')}
        >
          <Upload>
            <Button icon={<UploadOutlined />}>{t('profile.uploadAvatar')}</Button>
          </Upload>
        </Form.Item>
      </Form>
    </Modal>
  );
};

export default ProfileEditor;

