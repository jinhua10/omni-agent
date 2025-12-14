import React, { useState } from 'react';
import { Modal, Tabs, Button, Descriptions, Tag, Rate } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import ServiceConfig from './ServiceConfig';
import '../../assets/css/service/service-detail.css';

const { TabPane } = Tabs;

const ServiceDetail = ({ visible, service, onClose, onInstall, onUninstall, onUpdate }) => {
  const { t } = useLanguage();
  const [activeTab, setActiveTab] = useState('overview');

  if (!service) return null;

  const handleInstall = async () => {
    await onInstall(service.id);
    onUpdate();
  };

  const handleUninstall = async () => {
    await onUninstall(service.id);
    onClose();
  };

  return (
    <Modal
      title={service.name}
      open={visible}
      onCancel={onClose}
      width={800}
      footer={[
        <Button key="close" onClick={onClose}>
          {t('common.close')}
        </Button>,
        service.installed ? (
          <Button key="uninstall" danger onClick={handleUninstall}>
            {t('aiService.uninstall')}
          </Button>
        ) : (
          <Button key="install" type="primary" onClick={handleInstall}>
            {t('aiService.install')}
          </Button>
        ),
      ]}
      className="service-detail-modal"
    >
      <div className="service-detail">
        <Tabs activeKey={activeTab} onChange={setActiveTab}>
          <TabPane tab={t('aiService.detail.overview')} key="overview">
            <div className="service-detail__overview">
              {/* 基本信息 */}
              <div className="service-detail__info">
                <div className="service-detail__icon">{service.icon || '🤖'}</div>
                <div className="service-detail__meta">
                  <h3>{service.name}</h3>
                  <div className="service-detail__rating">
                    <Rate disabled value={service.rating || 0} allowHalf />
                    <span>({service.rating || 0})</span>
                  </div>
                  <div className="service-detail__tags">
                    <Tag color="blue">{t(`aiService.category.${service.category}`)}</Tag>
                    {service.installed && <Tag color="green">{t('aiService.installed')}</Tag>}
                  </div>
                </div>
              </div>

              {/* 描述 */}
              <Descriptions title={t('aiService.detail.description')} column={1}>
                <Descriptions.Item>{service.description}</Descriptions.Item>
              </Descriptions>

              {/* 详细信息 */}
              <Descriptions title={t('aiService.detail.info')} column={2}>
                <Descriptions.Item label={t('aiService.detail.version')}>
                  {service.version || '1.0.0'}
                </Descriptions.Item>
                <Descriptions.Item label={t('aiService.detail.author')}>
                  {service.author || t('aiService.official')}
                </Descriptions.Item>
                <Descriptions.Item label={t('aiService.detail.usageCount')}>
                  {service.usageCount || 0}
                </Descriptions.Item>
                <Descriptions.Item label={t('aiService.detail.size')}>
                  {service.size || 'N/A'}
                </Descriptions.Item>
              </Descriptions>

              {/* 功能列表 */}
              {service.features && service.features.length > 0 && (
                <div className="service-detail__features">
                  <h4>{t('aiService.detail.features')}</h4>
                  <ul>
                    {service.features.map((feature, index) => (
                      <li key={index}>{feature}</li>
                    ))}
                  </ul>
                </div>
              )}
            </div>
          </TabPane>

          {service.installed && (
            <TabPane tab={t('aiService.detail.configuration')} key="config">
              <ServiceConfig service={service} onUpdate={onUpdate} />
            </TabPane>
          )}

          <TabPane tab={t('aiService.detail.changelog')} key="changelog">
            <div className="service-detail__changelog">
              {service.changelog && service.changelog.length > 0 ? (
                <ul>
                  {service.changelog.map((log, index) => (
                    <li key={index}>
                      <strong>{log.version}</strong> - {log.date}
                      <p>{log.changes}</p>
                    </li>
                  ))}
                </ul>
              ) : (
                <p>{t('aiService.detail.noChangelog')}</p>
              )}
            </div>
          </TabPane>
        </Tabs>
      </div>
    </Modal>
  );
};

export default ServiceDetail;

