import React, { useState, useEffect } from 'react';
import { List, Input, Button, Avatar, message, Empty, Spin } from 'antd';
import { UserOutlined, LikeOutlined, LikeFilled } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { wishApi } from '../../api/modules/wish';
import '../../assets/css/wish/wish-comments.css';

const { TextArea } = Input;

const WishComments = ({ wishId }) => {
  const { t } = useLanguage();
  const [comments, setComments] = useState([]);
  const [loading, setLoading] = useState(false);
  const [submitting, setSubmitting] = useState(false);
  const [commentText, setCommentText] = useState('');
  const [replyTo, setReplyTo] = useState(null);

  useEffect(() => {
    loadComments();
  }, [wishId]);

  const loadComments = async () => {
    setLoading(true);
    try {
      const response = await wishApi.getComments(wishId);
      setComments(response.data || []);
    } catch (error) {
      console.error('Failed to load comments:', error);
    } finally {
      setLoading(false);
    }
  };

  const handleSubmitComment = async () => {
    if (!commentText.trim()) {
      message.warning(t('wish.comment.emptyWarning'));
      return;
    }

    setSubmitting(true);
    try {
      await wishApi.addComment(wishId, {
        content: commentText,
        parentId: replyTo?.id,
      });
      message.success(t('wish.comment.submitSuccess'));
      setCommentText('');
      setReplyTo(null);
      loadComments();
    } catch (error) {
      console.error('Failed to submit comment:', error);
      message.error(t('wish.comment.submitFailed'));
    } finally {
      setSubmitting(false);
    }
  };

  const handleLikeComment = async (commentId) => {
    try {
      await wishApi.likeComment(commentId);
      loadComments();
    } catch (error) {
      console.error('Failed to like comment:', error);
    }
  };

  const formatTime = (timestamp) => {
    const date = new Date(timestamp);
    const now = new Date();
    const diff = now - date;
    const minutes = Math.floor(diff / (1000 * 60));
    const hours = Math.floor(diff / (1000 * 60 * 60));
    const days = Math.floor(diff / (1000 * 60 * 60 * 24));

    if (minutes < 60) return `${minutes} ${t('wish.minutesAgo')}`;
    if (hours < 24) return `${hours} ${t('wish.hoursAgo')}`;
    if (days < 7) return `${days} ${t('wish.daysAgo')}`;
    return date.toLocaleDateString();
  };

  const renderComment = (comment, isReply = false) => (
    <div key={comment.id} className={`wish-comment ${isReply ? 'wish-comment--reply' : ''}`}>
      <div className="wish-comment__avatar">
        {comment.author?.avatar ? (
          <Avatar src={comment.author.avatar} size={isReply ? 32 : 40} />
        ) : (
          <Avatar icon={<UserOutlined />} size={isReply ? 32 : 40} />
        )}
      </div>
      <div className="wish-comment__content">
        <div className="wish-comment__header">
          <span className="wish-comment__author">
            {comment.author?.name || t('wish.anonymous')}
          </span>
          <span className="wish-comment__time">{formatTime(comment.createdAt)}</span>
        </div>
        <div className="wish-comment__text">{comment.content}</div>
        <div className="wish-comment__actions">
          <button
            className={`wish-comment__action ${comment.userLiked ? 'wish-comment__action--liked' : ''}`}
            onClick={() => handleLikeComment(comment.id)}
          >
            {comment.userLiked ? <LikeFilled /> : <LikeOutlined />}
            <span>{comment.likes || 0}</span>
          </button>
          {!isReply && (
            <button
              className="wish-comment__action"
              onClick={() => setReplyTo(comment)}
            >
              {t('wish.comment.reply')}
            </button>
          )}
        </div>
        {/* 回复列表 */}
        {comment.replies && comment.replies.length > 0 && (
          <div className="wish-comment__replies">
            {comment.replies.map((reply) => renderComment(reply, true))}
          </div>
        )}
      </div>
    </div>
  );

  return (
    <div className="wish-comments">
      {/* 评论输入框 */}
      <div className="wish-comments__input-wrapper">
        {replyTo && (
          <div className="wish-comments__reply-hint">
            {t('wish.comment.replyTo')} @{replyTo.author?.name}
            <button
              className="wish-comments__cancel-reply"
              onClick={() => setReplyTo(null)}
            >
              {t('wish.comment.cancel')}
            </button>
          </div>
        )}
        <TextArea
          className="wish-comments__input"
          value={commentText}
          onChange={(e) => setCommentText(e.target.value)}
          placeholder={replyTo ? t('wish.comment.replyPlaceholder') : t('wish.comment.placeholder')}
          rows={3}
          maxLength={500}
          showCount
        />
        <div className="wish-comments__submit-wrapper">
          <Button
            type="primary"
            loading={submitting}
            onClick={handleSubmitComment}
            disabled={!commentText.trim()}
          >
            {replyTo ? t('wish.comment.submitReply') : t('wish.comment.submit')}
          </Button>
        </div>
      </div>

      {/* 评论列表 */}
      <div className="wish-comments__list">
        {loading ? (
          <div className="wish-comments__loading">
            <Spin />
          </div>
        ) : comments.length === 0 ? (
          <Empty
            image={Empty.PRESENTED_IMAGE_SIMPLE}
            description={t('wish.comment.empty')}
          />
        ) : (
          <div className="wish-comments__items">
            {comments.map((comment) => renderComment(comment))}
          </div>
        )}
      </div>
    </div>
  );
};

export default WishComments;

