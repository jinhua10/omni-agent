import React, { useState, useEffect } from 'react';
import { Input, Select, Tabs, Empty, Spin, App } from 'antd';
import { SearchOutlined, AppstoreOutlined, UnorderedListOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { serviceApi } from '../../api/modules/service';
import ServiceCard from './ServiceCard';
import ServiceDetail from './ServiceDetail';
import '../../assets/css/service/service-market.css';

const { Search } = Input;
const { Option } = Select;

const ServiceMarket = () => {
  const { t } = useLanguage();
  const { message } = App.useApp();
  const [services, setServices] = useState([]);
  const [loading, setLoading] = useState(false);
  const [activeTab, setActiveTab] = useState('all'); // all | installed | available
  const [viewMode, setViewMode] = useState('grid'); // grid | list
  const [searchKeyword, setSearchKeyword] = useState('');
  const [categoryFilter, setCategoryFilter] = useState('all');
  const [detailModalVisible, setDetailModalVisible] = useState(false);
  const [selectedService, setSelectedService] = useState(null);

  // 加载服务列表
  const loadServices = async () => {
    setLoading(true);
    try {
      const params = {
        tab: activeTab,
        category: categoryFilter === 'all' ? undefined : categoryFilter,
        keyword: searchKeyword || undefined,
      };
      const response = await serviceApi.getServices(params);
      // Backend returns List<ServiceDTO> directly
      const serviceData = Array.isArray(response) ? response : (response.data || []);
      setServices(Array.isArray(serviceData) ? serviceData : []);
    } catch (error) {
      console.error('Failed to load services:', error);
      message.error(t('aiService.loadFailed'));
      setServices([]);
    } finally {
      setLoading(false);
    }
  };

  useEffect(() => {
    loadServices();
  }, [activeTab, categoryFilter, searchKeyword]);

  // 处理服务点击
  const handleServiceClick = (service) => {
    setSelectedService(service);
    setDetailModalVisible(true);
  };

  // 处理安装
  const handleInstall = async (serviceId) => {
    try {
      await serviceApi.installService(serviceId);
      message.success(t('aiService.installSuccess'));
      loadServices();
    } catch (error) {
      console.error('Failed to install service:', error);
      message.error(t('aiService.installFailed'));
    }
  };

  // 处理卸载
  const handleUninstall = async (serviceId) => {
    try {
      await serviceApi.uninstallService(serviceId);
      message.success(t('aiService.uninstallSuccess'));
      loadServices();
    } catch (error) {
      console.error('Failed to uninstall service:', error);
      message.error(t('aiService.uninstallFailed'));
    }
  };

  // 处理配置
  const handleConfigure = (service) => {
    setSelectedService(service);
    setDetailModalVisible(true);
  };

  // 过滤后的服务列表
  const filteredServices = services.filter(service => {
    if (activeTab === 'installed') return service.installed;
    if (activeTab === 'available') return !service.installed;
    return true;
  });

  return (
    <div className="service-market">
      {/* 头部 */}
      <div className="service-market__header">
        <div className="service-market__header-left">
          <h2 className="service-market__title">{t('aiService.title')}</h2>
          <span className="service-market__count">
            {filteredServices.length} {t('aiService.services')}
          </span>
        </div>
      </div>

      {/* 标签页 */}
      <Tabs
        activeKey={activeTab}
        onChange={setActiveTab}
        className="service-market__tabs"
        items={[
          { key: 'all', label: t('aiService.all') },
          { key: 'installed', label: t('aiService.installed') },
          { key: 'available', label: t('aiService.available') }
        ]}
      />

      {/* 工具栏 */}
      <div className="service-market__toolbar">
        <div className="service-market__toolbar-left">
          <Search
            className="service-market__search"
            placeholder={t('aiService.searchPlaceholder')}
            allowClear
            onSearch={setSearchKeyword}
            prefix={<SearchOutlined />}
            style={{ width: 300 }}
          />

          <Select
            className="service-market__filter"
            value={categoryFilter}
            onChange={setCategoryFilter}
            style={{ width: 150 }}
          >
            <Option value="all">{t('aiService.category.all')}</Option>
            <Option value="generation">{t('aiService.category.generation')}</Option>
            <Option value="analysis">{t('aiService.category.analysis')}</Option>
            <Option value="conversion">{t('aiService.category.conversion')}</Option>
            <Option value="optimization">{t('aiService.category.optimization')}</Option>
          </Select>
        </div>

        <div className="service-market__toolbar-right">
          <div className="service-market__view-switch">
            <button
              className={`service-market__view-btn ${viewMode === 'grid' ? 'service-market__view-btn--active' : ''}`}
              onClick={() => setViewMode('grid')}
            >
              <AppstoreOutlined />
            </button>
            <button
              className={`service-market__view-btn ${viewMode === 'list' ? 'service-market__view-btn--active' : ''}`}
              onClick={() => setViewMode('list')}
            >
              <UnorderedListOutlined />
            </button>
          </div>
        </div>
      </div>

      {/* 服务列表 */}
      <div className="service-market__content">
        {loading ? (
          <div className="service-market__loading">
            <Spin size="large" tip={t('aiService.loading')}>
              <div style={{ padding: 50 }} />
            </Spin>
          </div>
        ) : filteredServices.length === 0 ? (
          <Empty
            className="service-market__empty"
            description={t('aiService.empty')}
            image={Empty.PRESENTED_IMAGE_SIMPLE}
          />
        ) : (
          <div className={`service-market__list service-market__list--${viewMode}`}>
            {filteredServices.map((service) => (
              <ServiceCard
                key={service.id}
                service={service}
                viewMode={viewMode}
                onClick={() => handleServiceClick(service)}
                onInstall={() => handleInstall(service.id)}
                onUninstall={() => handleUninstall(service.id)}
                onConfigure={() => handleConfigure(service)}
              />
            ))}
          </div>
        )}
      </div>

      {/* 服务详情模态框 */}
      {selectedService && (
        <ServiceDetail
          visible={detailModalVisible}
          service={selectedService}
          onClose={() => {
            setDetailModalVisible(false);
            setSelectedService(null);
          }}
          onInstall={handleInstall}
          onUninstall={handleUninstall}
          onUpdate={loadServices}
        />
      )}
    </div>
  );
};

export default ServiceMarket;

