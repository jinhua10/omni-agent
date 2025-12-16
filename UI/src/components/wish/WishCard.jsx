import React from 'react';
import { Tag, Tooltip } from 'antd';
import { LikeOutlined, CommentOutlined, ClockCircleOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import WishVote from './WishVote';
import '../../assets/css/wish/wish-card.css';

const WishCard = ({ wish, viewMode = 'grid', onClick, onVote }) => {
  const { t } = useLanguage();

  // 获取状态颜色
  const getStatusColor = (status) => {
    const colors = {
      pending: 'orange',
      in_progress: 'blue',
      completed: 'green',
      rejected: 'red',
    };
    return colors[status] || 'default';
  };

  // 获取分类颜色
  const getCategoryColor = (category) => {
    const colors = {
      feature: 'purple',
      bug: 'red',
      interface: 'cyan',
      improvement: 'blue',
    };
    return colors[category] || 'default';
  };

  // 格式化时间
  const formatTime = (timestamp) => {
    const date = new Date(timestamp);
    const now = new Date();
    const diff = now - date;
    const days = Math.floor(diff / (1000 * 60 * 60 * 24));

    if (days === 0) {
      const hours = Math.floor(diff / (1000 * 60 * 60));
      if (hours === 0) {
        const minutes = Math.floor(diff / (1000 * 60));
        return `${minutes} ${t('wish.minutesAgo')}`;
      }
      return `${hours} ${t('wish.hoursAgo')}`;
    }
    if (days < 7) {
      return `${days} ${t('wish.daysAgo')}`;
    }
    return date.toLocaleDateString();
  };

  return (
    <div
      className={`wish-card wish-card--${viewMode}`}
      onClick={onClick}
    >
      {/* 头部 */}
      <div className="wish-card__header">
        <div className="wish-card__tags">
          <Tag color={getStatusColor(wish.status)}>
            {t(`wish.status.${wish.status}`)}
          </Tag>
          <Tag color={getCategoryColor(wish.category)}>
            {t(`wish.category.${wish.category}`)}
          </Tag>
        </div>
        <div className="wish-card__time">
          <ClockCircleOutlined />
          <span>{formatTime(wish.createdAt)}</span>
        </div>
      </div>

      {/* 标题 */}
      <h3 className="wish-card__title">{wish.title}</h3>

      {/* 描述 */}
      <p className="wish-card__description">
        {wish.description.length > 150
          ? `${wish.description.substring(0, 150)}...`
          : wish.description}
      </p>

      {/* 作者信息 */}
      <div className="wish-card__author">
        {wish.author?.avatar ? (
          <img
            src={wish.author.avatar}
            alt={wish.author.name}
            className="wish-card__avatar"
          />
        ) : (
          <div className="wish-card__avatar wish-card__avatar--default">
            {wish.author?.name?.[0]?.toUpperCase() || 'U'}
          </div>
        )}
        <span className="wish-card__author-name">{wish.author?.name || t('wish.anonymous')}</span>
      </div>

      {/* 底部操作栏 */}
      <div className="wish-card__footer">
        <div className="wish-card__stats">
          <WishVote
            wishId={wish.id}
            votes={wish.votes}
            userVoted={wish.userVoted}
            onVote={onVote}
            compact
          />

          <Tooltip title={t('wish.comments')}>
            <div className="wish-card__stat">
              <CommentOutlined />
              <span>{wish.commentsCount || 0}</span>
            </div>
          </Tooltip>
        </div>

        <button
          className="wish-card__detail-btn"
          onClick={(e) => {
            e.stopPropagation();
            onClick();
          }}
        >
          {t('wish.viewDetail')}
        </button>
      </div>
    </div>
  );
};

export default WishCard;

