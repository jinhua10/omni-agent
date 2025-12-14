import React, { useState } from 'react';
import { LikeOutlined, LikeFilled, DislikeOutlined, DislikeFilled } from '@ant-design/icons';
import { Tooltip } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';

const WishVote = ({ wishId, votes = 0, userVoted, onVote, compact = false }) => {
  const { t } = useLanguage();
  const [localVoted, setLocalVoted] = useState(userVoted);
  const [localVotes, setLocalVotes] = useState(votes);
  const [voting, setVoting] = useState(false);

  const handleVote = async (e, voteType) => {
    e.stopPropagation();

    if (voting) return;

    // 乐观更新
    const newVoted = localVoted === voteType ? null : voteType;
    const voteDiff = newVoted === 'up' ? (localVoted === 'up' ? 0 : localVoted === 'down' ? 2 : 1)
                   : newVoted === 'down' ? (localVoted === 'down' ? 0 : localVoted === 'up' ? -2 : -1)
                   : localVoted === 'up' ? -1 : 1;

    setLocalVoted(newVoted);
    setLocalVotes(localVotes + voteDiff);
    setVoting(true);

    try {
      await onVote(voteType);
    } catch (error) {
      // 回滚
      setLocalVoted(localVoted);
      setLocalVotes(localVotes);
    } finally {
      setVoting(false);
    }
  };

  if (compact) {
    return (
      <div className="wish-vote wish-vote--compact">
        <Tooltip title={localVoted === 'up' ? t('wish.voted') : t('wish.vote')}>
          <button
            className={`wish-vote__btn ${localVoted === 'up' ? 'wish-vote__btn--active' : ''}`}
            onClick={(e) => handleVote(e, 'up')}
            disabled={voting}
          >
            {localVoted === 'up' ? <LikeFilled /> : <LikeOutlined />}
            <span className="wish-vote__count">{localVotes}</span>
          </button>
        </Tooltip>
      </div>
    );
  }

  return (
    <div className="wish-vote">
      <Tooltip title={localVoted === 'up' ? t('wish.cancelVote') : t('wish.voteUp')}>
        <button
          className={`wish-vote__btn ${localVoted === 'up' ? 'wish-vote__btn--active-up' : ''}`}
          onClick={(e) => handleVote(e, 'up')}
          disabled={voting}
        >
          {localVoted === 'up' ? <LikeFilled /> : <LikeOutlined />}
        </button>
      </Tooltip>

      <span className="wish-vote__count">{localVotes}</span>

      <Tooltip title={localVoted === 'down' ? t('wish.cancelVote') : t('wish.voteDown')}>
        <button
          className={`wish-vote__btn ${localVoted === 'down' ? 'wish-vote__btn--active-down' : ''}`}
          onClick={(e) => handleVote(e, 'down')}
          disabled={voting}
        >
          {localVoted === 'down' ? <DislikeFilled /> : <DislikeOutlined />}
        </button>
      </Tooltip>
    </div>
  );
};

export default WishVote;

