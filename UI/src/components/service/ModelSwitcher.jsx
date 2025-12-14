import React, { useState } from 'react';
import { Radio, Card, Button, Tag, message } from 'antd';
import { CloudOutlined, DatabaseOutlined, CheckCircleOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { serviceApi } from '../../api/modules/service';
import '../../assets/css/service/model-switcher.css';

const ModelSwitcher = () => {
  const { t } = useLanguage();
  const [selectedModel, setSelectedModel] = useState('local');
  const [switching, setSwitching] = useState(false);

  const handleSwitch = async () => {
    try {
      setSwitching(true);
      await serviceApi.switchModel(selectedModel);
      message.success(t('aiService.model.switchSuccess'));
    } catch (error) {
      console.error('Failed to switch model:', error);
      message.error(t('aiService.model.switchFailed'));
    } finally {
      setSwitching(false);
    }
  };

  return (
    <div className="model-switcher">
      <h3 className="model-switcher__title">{t('aiService.model.title')}</h3>

      <Radio.Group
        value={selectedModel}
        onChange={(e) => setSelectedModel(e.target.value)}
        className="model-switcher__options"
      >
        <Card
          className={`model-switcher__card ${selectedModel === 'local' ? 'model-switcher__card--active' : ''}`}
          onClick={() => setSelectedModel('local')}
        >
          <Radio value="local">
            <div className="model-switcher__option">
              <DatabaseOutlined className="model-switcher__icon" />
              <div className="model-switcher__info">
                <h4>{t('aiService.localModel')}</h4>
                <p>{t('aiService.model.localDesc')}</p>
                <div className="model-switcher__tags">
                  <Tag color="green">{t('aiService.model.fast')}</Tag>
                  <Tag color="blue">{t('aiService.model.offline')}</Tag>
                </div>
              </div>
              {selectedModel === 'local' && <CheckCircleOutlined className="model-switcher__check" />}
            </div>
          </Radio>
        </Card>

        <Card
          className={`model-switcher__card ${selectedModel === 'online' ? 'model-switcher__card--active' : ''}`}
          onClick={() => setSelectedModel('online')}
        >
          <Radio value="online">
            <div className="model-switcher__option">
              <CloudOutlined className="model-switcher__icon" />
              <div className="model-switcher__info">
                <h4>{t('aiService.onlineModel')}</h4>
                <p>{t('aiService.model.onlineDesc')}</p>
                <div className="model-switcher__tags">
                  <Tag color="purple">{t('aiService.model.powerful')}</Tag>
                  <Tag color="orange">{t('aiService.model.latest')}</Tag>
                </div>
              </div>
              {selectedModel === 'online' && <CheckCircleOutlined className="model-switcher__check" />}
            </div>
          </Radio>
        </Card>
      </Radio.Group>

      <div className="model-switcher__actions">
        <Button type="primary" size="large" onClick={handleSwitch} loading={switching}>
          {t('aiService.model.apply')}
        </Button>
      </div>
    </div>
  );
};

export default ModelSwitcher;

