/**
 * 工作流详情组件 (Workflow Detail Component)
 *
 * 显示单个工作流的详细信息
 *
 * @author OmniAgent Team
 * @since 2025-12-20
 */

import React, { useState, useEffect } from 'react';
import workflowApi from '../../api/modules/workflow';

const { getWorkflowDetail, downloadWorkflow, installWorkflow, getWorkflowRatings, rateWorkflow } = workflowApi;
import { useLanguage } from '../../contexts/LanguageContext';
import RatingStars from './RatingStars';
import '../../assets/css/workflow/workflow-detail.css';

const WorkflowDetail = ({ workflowId, onBack }) => {
  const { t } = useLanguage();
  const [workflow, setWorkflow] = useState(null);
  const [ratings, setRatings] = useState([]);
  const [loading, setLoading] = useState(true);
  const [activeTab, setActiveTab] = useState('overview');
  const [userRating, setUserRating] = useState(0);
  const [userComment, setUserComment] = useState('');

  useEffect(() => {
    if (workflowId) {
      loadWorkflowDetail();
      loadRatings();
    }
  }, [workflowId]);

  const loadWorkflowDetail = async () => {
    try {
      const data = await getWorkflowDetail(workflowId);
      setWorkflow(data);
    } catch (error) {
      console.error('Failed to load workflow detail:', error);
    } finally {
      setLoading(false);
    }
  };

  const loadRatings = async () => {
    try {
      const data = await getWorkflowRatings(workflowId);
      setRatings(data);
    } catch (error) {
      console.error('Failed to load ratings:', error);
    }
  };

  const handleDownload = async () => {
    try {
      const data = await downloadWorkflow(workflowId);
      // 创建下载链接
      const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
      const url = window.URL.createObjectURL(blob);
      const a = document.createElement('a');
      a.href = url;
      a.download = `${workflow.name}.json`;
      a.click();
      window.URL.revokeObjectURL(url);
    } catch (error) {
      console.error('Failed to download workflow:', error);
      alert(t('workflowMarket.detail.downloadFailed') + ': ' + error.message);
    }
  };

  const handleInstall = async () => {
    try {
      await installWorkflow(workflowId);
      alert(t('workflowMarket.detail.installSuccess'));
    } catch (error) {
      console.error('Failed to install workflow:', error);
      alert(t('workflowMarket.detail.installFailed') + ': ' + error.message);
    }
  };

  const handleRate = async () => {
    if (userRating === 0) {
      alert(t('workflowMarket.rating.pleaseRate'));
      return;
    }
    try {
      await rateWorkflow(workflowId, userRating, userComment);
      alert(t('workflowMarket.rating.rateSuccess'));
      setUserRating(0);
      setUserComment('');
      loadRatings();
    } catch (error) {
      console.error('Failed to rate workflow:', error);
      alert(t('workflowMarket.rating.rateFailed') + ': ' + error.message);
    }
  };

  if (loading) {
    return (
      <div className="workflow-detail loading">
        <div className="spinner"></div>
        <p>{t('workflowMarket.loading')}</p>
      </div>
    );
  }

  if (!workflow) {
    return (
      <div className="workflow-detail error">
        <h2>😔 {t('workflowMarket.detail.notFound')}</h2>
        <button onClick={onBack}>{t('workflowMarket.detail.backToMarket')}</button>
      </div>
    );
  }

  return (
    <div className="workflow-detail">
      {/* 返回按钮 */}
      <button className="back-btn" onClick={onBack}>
        ← {t('workflowMarket.detail.backToMarket')}
      </button>

      {/* 头部信息 */}
      <div className="detail-header">
        <div className="header-left">
          <h1 className="workflow-title">{workflow.name}</h1>
          <p className="workflow-subtitle">{workflow.description}</p>

          <div className="workflow-meta">
            <span className="meta-item">
              👤 {workflow.author || 'Unknown'}
            </span>
            <span className="meta-item">
              📦 v{workflow.version || '1.0.0'}
            </span>
            <span className="meta-item">
              📁 {workflow.category || 'General'}
            </span>
            <span className="meta-item">
              ⬇️ {workflow.downloadCount || 0} {t('workflowMarket.card.downloads')}
            </span>
          </div>

          {workflow.tags && workflow.tags.length > 0 && (
            <div className="workflow-tags">
              {workflow.tags.map((tag, index) => (
                <span key={index} className="tag">{tag}</span>
              ))}
            </div>
          )}
        </div>

        <div className="header-right">
          <div className="rating-box">
            <RatingStars rating={workflow.averageRating || 0} size="large" />
            <p className="rating-count">({ratings.length} {t('workflowMarket.rating.ratingsCount')})</p>
          </div>

          <div className="action-buttons">
            <button className="btn-primary" onClick={handleDownload}>
              ⬇️ {t('workflowMarket.detail.download')}
            </button>
            <button className="btn-secondary" onClick={handleInstall}>
              ⚙️ {t('workflowMarket.detail.install')}
            </button>
          </div>
        </div>
      </div>

      {/* 标签页 */}
      <div className="detail-tabs">
        <button
          className={`tab ${activeTab === 'overview' ? 'active' : ''}`}
          onClick={() => setActiveTab('overview')}
        >
          {t('workflowMarket.detail.overview')}
        </button>
        <button
          className={`tab ${activeTab === 'steps' ? 'active' : ''}`}
          onClick={() => setActiveTab('steps')}
        >
          {t('workflowMarket.detail.steps')}
        </button>
        <button
          className={`tab ${activeTab === 'ratings' ? 'active' : ''}`}
          onClick={() => setActiveTab('ratings')}
        >
          {t('workflowMarket.detail.ratings')} ({ratings.length})
        </button>
      </div>

      {/* 标签页内容 */}
      <div className="detail-content">
        {activeTab === 'overview' && (
          <div className="tab-content">
            <h2>📝 {t('workflowMarket.detail.description')}</h2>
            <p>{workflow.description || t('workflowMarket.detail.noDescription')}</p>

            {workflow.steps && (
              <>
                <h2>🔢 {t('workflowMarket.detail.stepsCount')}</h2>
                <p>{workflow.steps.length} {t('workflowMarket.detail.stepUnit')}</p>
              </>
            )}
          </div>
        )}

        {activeTab === 'steps' && (
          <div className="tab-content">
            <h2>📋 {t('workflowMarket.detail.steps')}</h2>
            {workflow.steps && workflow.steps.length > 0 ? (
              <div className="steps-list">
                {workflow.steps.map((step, index) => (
                  <div key={index} className="step-item">
                    <div className="step-number">{index + 1}</div>
                    <div className="step-info">
                      <h3>{step.name || step.id}</h3>
                      <p className="step-agent">{t('workflowMarket.detail.agent')}: {step.agent}</p>
                      {step.description && (
                        <p className="step-description">{step.description}</p>
                      )}
                      {step.dependencies && step.dependencies.length > 0 && (
                        <p className="step-dependencies">
                          {t('workflowMarket.detail.dependencies')}: {step.dependencies.join(', ')}
                        </p>
                      )}
                    </div>
                  </div>
                ))}
              </div>
            ) : (
              <p>{t('workflowMarket.detail.noSteps')}</p>
            )}
          </div>
        )}

        {activeTab === 'ratings' && (
          <div className="tab-content">
            <h2>⭐ {t('workflowMarket.rating.title')}</h2>

            {/* 评分表单 */}
            <div className="rating-form">
              <h3>{t('workflowMarket.rating.giveRating')}</h3>
              <RatingStars
                rating={userRating}
                size="large"
                interactive
                onRate={setUserRating}
              />
              <textarea
                className="comment-input"
                placeholder={t('workflowMarket.rating.commentPlaceholder')}
                value={userComment}
                onChange={(e) => setUserComment(e.target.value)}
                rows={4}
              />
              <button className="btn-primary" onClick={handleRate}>
                {t('workflowMarket.rating.submit')}
              </button>
            </div>

            {/* 评分列表 */}
            <div className="ratings-list">
              {ratings.length > 0 ? (
                ratings.map((rating, index) => (
                  <div key={index} className="rating-item">
                    <div className="rating-header">
                      <span className="rating-user">👤 {rating.userId}</span>
                      <RatingStars rating={rating.rating} size="small" />
                    </div>
                    {rating.comment && (
                      <p className="rating-comment">{rating.comment}</p>
                    )}
                    <span className="rating-date">
                      {new Date(rating.createdAt).toLocaleDateString()}
                    </span>
                  </div>
                ))
              ) : (
                <p>{t('workflowMarket.rating.noRatings')}</p>
              )}
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default WorkflowDetail;

