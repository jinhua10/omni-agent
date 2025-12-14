import React, { useState, useEffect } from 'react';
import { Card, Row, Col, Progress } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import { profileApi } from '../../api/modules/profile';
import StatisticsChart from './StatisticsChart';

const UsageStatistics = ({ userId }) => {
  const { t } = useLanguage();
  const [stats, setStats] = useState(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    loadStatistics();
  }, [userId]);

  const loadStatistics = async () => {
    setLoading(true);
    try {
      const response = await profileApi.getUsageStatistics(userId);
      setStats(response.data);
    } catch (error) {
      console.error('Failed to load statistics:', error);
    } finally {
      setLoading(false);
    }
  };

  if (!stats) return null;

  return (
    <div className="usage-statistics">
      <Row gutter={[16, 16]}>
        <Col xs={24} sm={12} md={6}>
          <Card>
            <div className="stat-card">
              <div className="stat-card__value">{stats.qaCount || 0}</div>
              <div className="stat-card__label">{t('profile.qaCount')}</div>
            </div>
          </Card>
        </Col>
        <Col xs={24} sm={12} md={6}>
          <Card>
            <div className="stat-card">
              <div className="stat-card__value">{stats.documentCount || 0}</div>
              <div className="stat-card__label">{t('profile.documentCount')}</div>
            </div>
          </Card>
        </Col>
        <Col xs={24} sm={12} md={6}>
          <Card>
            <div className="stat-card">
              <div className="stat-card__value">{stats.feedbackCount || 0}</div>
              <div className="stat-card__label">{t('profile.feedbackCount')}</div>
            </div>
          </Card>
        </Col>
        <Col xs={24} sm={12} md={6}>
          <Card>
            <div className="stat-card">
              <div className="stat-card__value">{stats.activeHours || 0}h</div>
              <div className="stat-card__label">{t('profile.activeHours')}</div>
            </div>
          </Card>
        </Col>
      </Row>

      <Row gutter={[16, 16]} style={{ marginTop: 16 }}>
        <Col span={24}>
          <Card title={t('profile.usageTrend')}>
            <StatisticsChart data={stats.trendData} />
          </Card>
        </Col>
      </Row>
    </div>
  );
};

export default UsageStatistics;

