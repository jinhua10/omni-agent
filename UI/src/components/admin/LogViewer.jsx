import React, { useState, useEffect } from 'react';
import { Input, Select, Button, Space, Divider, Tag } from 'antd';
import { SearchOutlined, DownloadOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { adminApi } from '../../api/modules/admin';
import '../../assets/css/admin/log-viewer.css';

const { Search } = Input;
const { Option } = Select;

const LogViewer = () => {
  const { t } = useLanguage();
  const [logs, setLogs] = useState([]);
  const [level, setLevel] = useState('all');
  const [keyword, setKeyword] = useState('');

  useEffect(() => {
    loadLogs();
  }, [level]);

  const loadLogs = async () => {
    try {
      const response = await adminApi.getLogs({ level: level === 'all' ? undefined : level, keyword });
      // Backend returns { content: [...], totalElements, totalPages, currentPage }
      const logData = response?.data?.content || response?.content || response?.data || [];
      setLogs(Array.isArray(logData) ? logData : []);
    } catch (error) {
      console.error('Failed to load logs:', error);
      setLogs([]);
    }
  };

  const getLevelColor = (level) => {
    const colors = { ERROR: 'red', WARN: 'orange', INFO: 'blue', DEBUG: 'green' };
    return colors[level] || 'default';
  };

  return (
    <div className="log-viewer">
      <div className="log-viewer__toolbar">
        <Search
          placeholder={t('admin.log.searchPlaceholder')}
          onSearch={(value) => { setKeyword(value); loadLogs(); }}
          style={{ width: 300 }}
        />
        <Select value={level} onChange={setLevel} style={{ width: 150 }}>
          <Option value="all">{t('admin.log.all')}</Option>
          <Option value="ERROR">{t('admin.log.error')}</Option>
          <Option value="WARN">{t('admin.log.warn')}</Option>
          <Option value="INFO">{t('admin.log.info')}</Option>
        </Select>
        <Button icon={<DownloadOutlined />}>{t('admin.log.download')}</Button>
      </div>
      <div className="log-viewer__list">
        <Space orientation="vertical" style={{ width: '100%' }} size="small">
          {logs.length === 0 ? (
            <div style={{ padding: '20px', textAlign: 'center', color: '#999' }}>
              {t('admin.log.noLogs') || 'No logs found'}
            </div>
          ) : (
            logs.map((log, index) => (
              <div key={index}>
                <div className="log-viewer__item" style={{ padding: '12px', display: 'flex', alignItems: 'center', gap: '12px' }}>
                  <Tag color={getLevelColor(log.level)}>{log.level}</Tag>
                  <span className="log-viewer__time" style={{ color: '#666', fontSize: '12px' }}>
                    {log.timestamp?.toString() || log.timestamp}
                  </span>
                  <span className="log-viewer__message" style={{ flex: 1 }}>
                    {log.message}
                  </span>
                </div>
                {index < logs.length - 1 && <Divider style={{ margin: 0 }} />}
              </div>
            ))
          )}
        </Space>
      </div>
    </div>
  );
};

export default LogViewer;

