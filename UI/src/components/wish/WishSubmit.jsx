import React, { useState } from 'react';
import { Modal, Input, Select, Form, message } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import { wishApi } from '../../api/modules/wish';
import '../../assets/css/wish/wish-submit.css';

const { TextArea } = Input;
const { Option } = Select;

const WishSubmit = ({ visible, onClose, onSuccess }) => {
  const { t } = useLanguage();
  const [form] = Form.useForm();
  const [submitting, setSubmitting] = useState(false);

  const handleSubmit = async () => {
    try {
      const values = await form.validateFields();
      setSubmitting(true);

      await wishApi.submitWish(values);

      message.success(t('wish.submitSuccess'));
      form.resetFields();
      onSuccess();
    } catch (error) {
      if (error.errorFields) {
        // 表单验证错误
        return;
      }
      console.error('Failed to submit wish:', error);
      message.error(t('wish.submitFailed'));
    } finally {
      setSubmitting(false);
    }
  };

  const handleCancel = () => {
    form.resetFields();
    onClose();
  };

  return (
    <Modal
      title={t('wish.submitTitle')}
      open={visible}
      onOk={handleSubmit}
      onCancel={handleCancel}
      okText={t('wish.form.submit')}
      cancelText={t('wish.form.cancel')}
      confirmLoading={submitting}
      width={600}
      className="wish-submit-modal"
    >
      <Form
        form={form}
        layout="vertical"
        className="wish-submit-form"
      >
        <Form.Item
          name="title"
          label={t('wish.form.title')}
          rules={[
            { required: true, message: t('wish.form.titleRequired') },
            { max: 50, message: t('wish.form.titleTooLong') },
          ]}
        >
          <Input
            placeholder={t('wish.form.titlePlaceholder')}
            maxLength={50}
            showCount
          />
        </Form.Item>

        <Form.Item
          name="category"
          label={t('wish.form.category')}
          rules={[
            { required: true, message: t('wish.form.categoryRequired') },
          ]}
        >
          <Select placeholder={t('wish.form.categoryPlaceholder')}>
            <Option value="feature">{t('wish.category.feature')}</Option>
            <Option value="bug">{t('wish.category.bug')}</Option>
            <Option value="interface">{t('wish.category.interface')}</Option>
          </Select>
        </Form.Item>

        <Form.Item
          name="description"
          label={t('wish.form.description')}
          rules={[
            { required: true, message: t('wish.form.descriptionRequired') },
            { max: 500, message: t('wish.form.descriptionTooLong') },
          ]}
        >
          <TextArea
            placeholder={t('wish.form.descriptionPlaceholder')}
            rows={6}
            maxLength={500}
            showCount
          />
        </Form.Item>

        <div className="wish-submit-form__tips">
          <h4>{t('wish.form.tipsTitle')}</h4>
          <ul>
            <li>{t('wish.form.tip1')}</li>
            <li>{t('wish.form.tip2')}</li>
            <li>{t('wish.form.tip3')}</li>
          </ul>
        </div>
      </Form>
    </Modal>
  );
};

export default WishSubmit;

