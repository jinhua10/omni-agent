import React, { useState } from 'react';
import { Card, Tabs } from 'antd';
import { SettingOutlined, DatabaseOutlined, FileTextOutlined, DashboardOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import SystemConfig from './SystemConfig';
import ModelConfig from './ModelConfig';
import LogViewer from './LogViewer';
import MonitorDashboard from './MonitorDashboard';
import '../../assets/css/admin/admin-panel.css';

const AdminPanel = () => {
  const { t } = useLanguage();
  const [activeTab, setActiveTab] = useState('system');

  return (
    <div className="admin-panel">
      <Card className="admin-panel__card">
        <h2 className="admin-panel__title">{t('admin.title')}</h2>

        <Tabs 
          activeKey={activeTab} 
          onChange={setActiveTab} 
          className="admin-panel__tabs"
          items={[
            {
              key: 'system',
              label: (
                <span>
                  <SettingOutlined />
                  {t('admin.systemConfig')}
                </span>
              ),
              children: <SystemConfig />
            },
            {
              key: 'model',
              label: (
                <span>
                  <DatabaseOutlined />
                  {t('admin.modelConfig')}
                </span>
              ),
              children: <ModelConfig />
            },
            {
              key: 'logs',
              label: (
                <span>
                  <FileTextOutlined />
                  {t('admin.logViewer')}
                </span>
              ),
              children: <LogViewer />
            },
            {
              key: 'monitor',
              label: (
                <span>
                  <DashboardOutlined />
                  {t('admin.monitor')}
                </span>
              ),
              children: <MonitorDashboard />
            }
          ]}
        />
      </Card>
    </div>
  );
};

export default AdminPanel;

