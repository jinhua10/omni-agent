import React, { useState, useEffect } from 'react';
import { Modal, Tag, Timeline, Divider, Spin } from 'antd';
import { ClockCircleOutlined, UserOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { wishApi } from '../../api/modules/wish';
import WishVote from './WishVote';
import WishComments from './WishComments';
import '../../assets/css/wish/wish-detail.css';

const WishDetail = ({ visible, wish, onClose, onVote, onUpdate }) => {
  const { t } = useLanguage();
  const [loading, setLoading] = useState(false);
  const [detailData, setDetailData] = useState(null);

  useEffect(() => {
    if (visible && wish) {
      loadDetail();
    }
  }, [visible, wish]);

  const loadDetail = async () => {
    setLoading(true);
    try {
      const response = await wishApi.getWishDetail(wish.id);
      setDetailData(response.data);
    } catch (error) {
      console.error('Failed to load wish detail:', error);
    } finally {
      setLoading(false);
    }
  };

  const getStatusColor = (status) => {
    const colors = {
      pending: 'orange',
      in_progress: 'blue',
      completed: 'green',
      rejected: 'red',
    };
    return colors[status] || 'default';
  };

  const getCategoryColor = (category) => {
    const colors = {
      feature: 'purple',
      bug: 'red',
      interface: 'cyan',
      improvement: 'blue',
    };
    return colors[category] || 'default';
  };

  const formatDateTime = (timestamp) => {
    const date = new Date(timestamp);
    return date.toLocaleString();
  };

  if (!wish) return null;

  return (
    <Modal
      title={null}
      open={visible}
      onCancel={onClose}
      footer={null}
      width={800}
      className="wish-detail-modal"
    >
      {loading ? (
        <div className="wish-detail__loading">
          <Spin size="large" />
        </div>
      ) : (
        <div className="wish-detail">
          {/* 头部 */}
          <div className="wish-detail__header">
            <div className="wish-detail__tags">
              <Tag color={getStatusColor(wish.status)} className="wish-detail__tag">
                {t(`wish.status.${wish.status}`)}
              </Tag>
              <Tag color={getCategoryColor(wish.category)} className="wish-detail__tag">
                {t(`wish.category.${wish.category}`)}
              </Tag>
            </div>
          </div>

          {/* 标题 */}
          <h2 className="wish-detail__title">{wish.title}</h2>

          {/* 元信息 */}
          <div className="wish-detail__meta">
            <div className="wish-detail__author">
              {wish.author?.avatar ? (
                <img src={wish.author.avatar} alt={wish.author.name} className="wish-detail__avatar" />
              ) : (
                <div className="wish-detail__avatar wish-detail__avatar--default">
                  <UserOutlined />
                </div>
              )}
              <span className="wish-detail__author-name">{wish.author?.name || t('wish.anonymous')}</span>
            </div>
            <div className="wish-detail__time">
              <ClockCircleOutlined />
              <span>{formatDateTime(wish.createdAt)}</span>
            </div>
          </div>

          {/* 投票 */}
          <div className="wish-detail__vote">
            <WishVote
              wishId={wish.id}
              votes={wish.votes}
              userVoted={wish.userVoted}
              onVote={(voteType) => {
                onVote(wish.id, voteType);
                onUpdate();
              }}
            />
          </div>

          <Divider />

          {/* 描述 */}
          <div className="wish-detail__description">
            <h3>{t('wish.detail.description')}</h3>
            <p>{wish.description}</p>
          </div>

          {/* 状态历史 */}
          {detailData?.statusHistory && detailData.statusHistory.length > 0 && (
            <>
              <Divider />
              <div className="wish-detail__history">
                <h3>{t('wish.detail.statusHistory')}</h3>
                <Timeline>
                  {detailData.statusHistory.map((item, index) => (
                    <Timeline.Item
                      key={index}
                      color={getStatusColor(item.status)}
                    >
                      <div className="wish-detail__history-item">
                        <div className="wish-detail__history-status">
                          {t(`wish.status.${item.status}`)}
                        </div>
                        <div className="wish-detail__history-time">
                          {formatDateTime(item.timestamp)}
                        </div>
                        {item.comment && (
                          <div className="wish-detail__history-comment">
                            {item.comment}
                          </div>
                        )}
                      </div>
                    </Timeline.Item>
                  ))}
                </Timeline>
              </div>
            </>
          )}

          <Divider />

          {/* 评论区 */}
          <div className="wish-detail__comments">
            <h3>{t('wish.detail.comments')} ({wish.commentsCount || 0})</h3>
            <WishComments wishId={wish.id} />
          </div>
        </div>
      )}
    </Modal>
  );
};

export default WishDetail;

