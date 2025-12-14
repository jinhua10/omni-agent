import React from 'react';
import { Tag, Button, Rate, Tooltip } from 'antd';
import {
  CloudDownloadOutlined,
  DeleteOutlined,
  SettingOutlined,
  CheckCircleOutlined,
  RocketOutlined
} from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import '../../assets/css/service/service-card.css';

const ServiceCard = ({ service, viewMode = 'grid', onClick, onInstall, onUninstall, onConfigure }) => {
  const { t } = useLanguage();

  // 获取分类颜色
  const getCategoryColor = (category) => {
    const colors = {
      generation: 'blue',
      analysis: 'green',
      conversion: 'orange',
      optimization: 'purple',
    };
    return colors[category] || 'default';
  };

  // 获取服务图标
  const getServiceIcon = (icon) => {
    if (icon) return icon;
    return '🤖';
  };

  return (
    <div
      className={`service-card service-card--${viewMode} ${service.installed ? 'service-card--installed' : ''}`}
      onClick={onClick}
    >
      {/* 已安装标识 */}
      {service.installed && (
        <div className="service-card__installed-badge">
          <CheckCircleOutlined />
          <span>{t('aiService.installed')}</span>
        </div>
      )}

      {/* 图标 */}
      <div className="service-card__icon">
        {getServiceIcon(service.icon)}
      </div>

      {/* 内容 */}
      <div className="service-card__content">
        {/* 标题和标签 */}
        <div className="service-card__header">
          <h3 className="service-card__title">{service.name}</h3>
          <div className="service-card__tags">
            <Tag color={getCategoryColor(service.category)}>
              {t(`aiService.category.${service.category}`)}
            </Tag>
            {service.isNew && (
              <Tag color="red">{t('aiService.new')}</Tag>
            )}
            {service.isPopular && (
              <Tag color="gold">
                <RocketOutlined /> {t('aiService.popular')}
              </Tag>
            )}
          </div>
        </div>

        {/* 描述 */}
        <p className="service-card__description">{service.description}</p>

        {/* 评分和统计 */}
        <div className="service-card__stats">
          <div className="service-card__rating">
            <Rate disabled value={service.rating || 0} allowHalf />
            <span className="service-card__rating-score">{service.rating || 0}</span>
          </div>
          <div className="service-card__usage">
            <span>{service.usageCount || 0} {t('aiService.usages')}</span>
          </div>
        </div>

        {/* 作者 */}
        <div className="service-card__author">
          <span>{t('aiService.author')}: {service.author || t('aiService.official')}</span>
        </div>

        {/* 操作按钮 */}
        <div className="service-card__actions">
          {service.installed ? (
            <>
              <Tooltip title={t('aiService.configure')}>
                <Button
                  type="primary"
                  icon={<SettingOutlined />}
                  onClick={(e) => {
                    e.stopPropagation();
                    onConfigure();
                  }}
                >
                  {t('aiService.configure')}
                </Button>
              </Tooltip>
              <Tooltip title={t('aiService.uninstall')}>
                <Button
                  danger
                  icon={<DeleteOutlined />}
                  onClick={(e) => {
                    e.stopPropagation();
                    onUninstall();
                  }}
                >
                  {t('aiService.uninstall')}
                </Button>
              </Tooltip>
            </>
          ) : (
            <Button
              type="primary"
              icon={<CloudDownloadOutlined />}
              onClick={(e) => {
                e.stopPropagation();
                onInstall();
              }}
            >
              {t('aiService.install')}
            </Button>
          )}
        </div>
      </div>
    </div>
  );
};

export default ServiceCard;

