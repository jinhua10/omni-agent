import React, { useState, useEffect } from 'react';
import { Card, List, Progress, Tag } from 'antd';
import { TrophyOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { profileApi } from '../../api/modules/profile';

const ContributionStats = ({ userId }) => {
  const { t } = useLanguage();
  const [contributions, setContributions] = useState([]);

  useEffect(() => {
    loadContributions();
  }, [userId]);

  const loadContributions = async () => {
    try {
      const response = await profileApi.getContributions(userId);
      setContributions(response.data || []);
    } catch (error) {
      console.error('Failed to load contributions:', error);
    }
  };

  return (
    <div className="contribution-stats">
      <Card title={t('profile.contributionRanking')}>
        <List
          dataSource={contributions}
          renderItem={(item, index) => (
            <List.Item>
              <div className="contribution-item">
                <span className="contribution-item__rank">
                  {index < 3 ? <TrophyOutlined style={{ color: '#ffa940' }} /> : `#${index + 1}`}
                </span>
                <span className="contribution-item__name">{item.name}</span>
                <Progress percent={item.score} style={{ flex: 1, marginLeft: 16 }} />
              </div>
            </List.Item>
          )}
        />
      </Card>
    </div>
  );
};

export default ContributionStats;

