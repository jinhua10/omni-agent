import React, { useState } from 'react';
import { Form, Input, Select, Button, message, Steps } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import { serviceApi } from '../../api/modules/service';
import '../../assets/css/service/ppt-generator.css';

const { TextArea } = Input;
const { Option } = Select;

const PPTGenerator = () => {
  const { t } = useLanguage();
  const [form] = Form.useForm();
  const [generating, setGenerating] = useState(false);
  const [currentStep, setCurrentStep] = useState(0);

  const handleGenerate = async () => {
    try {
      const values = await form.validateFields();
      setGenerating(true);
      await serviceApi.generatePPT(values);
      message.success(t('aiService.ppt.generateSuccess'));
      setCurrentStep(2);
    } catch (error) {
      console.error('Failed to generate PPT:', error);
      message.error(t('aiService.ppt.generateFailed'));
    } finally {
      setGenerating(false);
    }
  };

  return (
    <div className="ppt-generator">
      <Steps current={currentStep} className="ppt-generator__steps">
        <Steps.Step title={t('aiService.ppt.step1')} />
        <Steps.Step title={t('aiService.ppt.step2')} />
        <Steps.Step title={t('aiService.ppt.step3')} />
      </Steps>

      <Form form={form} layout="vertical" className="ppt-generator__form">
        <Form.Item
          name="topic"
          label={t('aiService.ppt.topic')}
          rules={[{ required: true }]}
        >
          <Input placeholder={t('aiService.ppt.topicPlaceholder')} />
        </Form.Item>

        <Form.Item
          name="outline"
          label={t('aiService.ppt.outline')}
        >
          <TextArea rows={6} placeholder={t('aiService.ppt.outlinePlaceholder')} />
        </Form.Item>

        <Form.Item
          name="style"
          label={t('aiService.ppt.style')}
        >
          <Select>
            <Option value="business">{t('aiService.ppt.styleBusiness')}</Option>
            <Option value="academic">{t('aiService.ppt.styleAcademic')}</Option>
            <Option value="creative">{t('aiService.ppt.styleCreative')}</Option>
          </Select>
        </Form.Item>

        <Form.Item>
          <Button type="primary" onClick={handleGenerate} loading={generating} size="large">
            {t('aiService.ppt.generate')}
          </Button>
        </Form.Item>
      </Form>
    </div>
  );
};

export default PPTGenerator;

