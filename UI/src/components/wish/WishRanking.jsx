import React, { useState, useEffect } from 'react';
import { Card, Spin, Empty } from 'antd';
import { TrophyOutlined, FireOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { wishApi } from '../../api/modules/wish';
import '../../assets/css/wish/wish-ranking.css';

const WishRanking = ({ onWishClick }) => {
  const { t } = useLanguage();
  const [loading, setLoading] = useState(false);
  const [rankings, setRankings] = useState([]);

  useEffect(() => {
    loadRanking();
  }, []);

  const loadRanking = async () => {
    setLoading(true);
    try {
      const response = await wishApi.getRanking();
      // 后端直接返回 List<WishDTO>，不是包装在data中 (Backend returns List directly, not wrapped in data)
      const rankingData = Array.isArray(response) ? response : response.data || [];
      setRankings(rankingData);
    } catch (error) {
      console.error('Failed to load ranking:', error);
      setRankings([]); // 设置为空数组防止崩溃 (Set to empty array to prevent crash)
    } finally {
      setLoading(false);
    }
  };

  const getRankIcon = (rank) => {
    if (rank === 1) return '🥇';
    if (rank === 2) return '🥈';
    if (rank === 3) return '🥉';
    return `${rank}`;
  };

  const getRankClass = (rank) => {
    if (rank <= 3) return `wish-ranking__item--top${rank}`;
    return '';
  };

  return (
    <Card
      title={
        <div className="wish-ranking__title">
          <TrophyOutlined />
          <span>{t('wish.ranking.title')}</span>
          <FireOutlined className="wish-ranking__fire-icon" />
        </div>
      }
      className="wish-ranking"
      variant="borderless"
    >
      {loading ? (
        <div className="wish-ranking__loading">
          <Spin />
        </div>
      ) : rankings.length === 0 ? (
        <Empty
          image={Empty.PRESENTED_IMAGE_SIMPLE}
          description={t('wish.ranking.empty')}
        />
      ) : (
        <div className="wish-ranking__list">
          {rankings.map((wish, index) => {
            const rank = index + 1;
            return (
              <div
                key={wish.id}
                className={`wish-ranking__item ${getRankClass(rank)}`}
                onClick={() => onWishClick(wish)}
              >
                <div className="wish-ranking__rank">
                  {getRankIcon(rank)}
                </div>
                <div className="wish-ranking__content">
                  <div className="wish-ranking__wish-title">
                    {wish.title}
                  </div>
                  <div className="wish-ranking__votes">
                    <FireOutlined />
                    <span>{wish.votes} {t('wish.votes')}</span>
                  </div>
                </div>
              </div>
            );
          })}
        </div>
      )}
    </Card>
  );
};

export default WishRanking;

