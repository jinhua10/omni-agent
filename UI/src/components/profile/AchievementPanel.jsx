import React, { useState, useEffect } from 'react';
import { Card, Row, Col, Badge, Progress } from 'antd';
import { TrophyOutlined, StarOutlined, CrownOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { profileApi } from '../../api/modules/profile';
import '../../assets/css/profile/achievement-panel.css';

const AchievementPanel = ({ userId }) => {
  const { t } = useLanguage();
  const [achievements, setAchievements] = useState([]);

  useEffect(() => {
    loadAchievements();
  }, [userId]);

  const loadAchievements = async () => {
    try {
      const response = await profileApi.getAchievements(userId);
      setAchievements(response.data || []);
    } catch (error) {
      console.error('Failed to load achievements:', error);
    }
  };

  const getAchievementIcon = (type) => {
    const icons = {
      bronze: <StarOutlined style={{ color: '#cd7f32' }} />,
      silver: <StarOutlined style={{ color: '#c0c0c0' }} />,
      gold: <TrophyOutlined style={{ color: '#ffd700' }} />,
      diamond: <CrownOutlined style={{ color: '#b9f2ff' }} />,
    };
    return icons[type] || <StarOutlined />;
  };

  return (
    <div className="achievement-panel">
      <Row gutter={[16, 16]}>
        {achievements.map((achievement) => (
          <Col xs={24} sm={12} md={8} lg={6} key={achievement.id}>
            <Badge.Ribbon text={achievement.unlocked ? t('profile.unlocked') : t('profile.locked')} color={achievement.unlocked ? 'green' : 'gray'}>
              <Card className={`achievement-card ${achievement.unlocked ? 'achievement-card--unlocked' : 'achievement-card--locked'}`}>
                <div className="achievement-card__icon">
                  {getAchievementIcon(achievement.type)}
                </div>
                <h4 className="achievement-card__title">{achievement.title}</h4>
                <p className="achievement-card__description">{achievement.description}</p>
                <Progress percent={achievement.progress || 0} size="small" />
              </Card>
            </Badge.Ribbon>
          </Col>
        ))}
      </Row>
    </div>
  );
};

export default AchievementPanel;

