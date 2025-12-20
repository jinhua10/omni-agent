/**
 * 工作流详情组件 (Workflow Detail Component)
 *
 * 显示单个工作流的详细信息
 *
 * @author OmniAgent Team
 * @since 2025-12-20
 */

import React, { useState, useEffect } from 'react';
import { getWorkflowDetail, downloadWorkflow, installWorkflow, getWorkflowRatings, rateWorkflow } from '../../api/workflowApi';
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
      alert('下载失败：' + error.message);
    }
  };

  const handleInstall = async () => {
    try {
      const userId = 'user-001'; // TODO: 从认证系统获取
      await installWorkflow(workflowId, userId);
      alert('工作流安装成功！');
    } catch (error) {
      console.error('Failed to install workflow:', error);
      alert('安装失败：' + error.message);
    }
  };

  const handleRate = async () => {
    if (userRating === 0) {
      alert('请选择评分');
      return;
    }
    try {
      const userId = 'user-001'; // TODO: 从认证系统获取
      await rateWorkflow(workflowId, userRating, userId, userComment);
      alert('评分成功！');
      setUserRating(0);
      setUserComment('');
      loadRatings();
    } catch (error) {
      console.error('Failed to rate workflow:', error);
      alert('评分失败：' + error.message);
    }
  };

  if (loading) {
    return (
      <div className="workflow-detail loading">
        <div className="spinner"></div>
        <p>加载中...</p>
      </div>
    );
  }

  if (!workflow) {
    return (
      <div className="workflow-detail error">
        <h2>😔 工作流不存在</h2>
        <button onClick={onBack}>返回市场</button>
      </div>
    );
  }

  return (
    <div className="workflow-detail">
      {/* 返回按钮 */}
      <button className="back-btn" onClick={onBack}>
        ← 返回市场
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
              ⬇️ {workflow.downloadCount || 0} 次下载
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
            <p className="rating-count">({ratings.length} 个评分)</p>
          </div>

          <div className="action-buttons">
            <button className="btn-primary" onClick={handleDownload}>
              ⬇️ 下载
            </button>
            <button className="btn-secondary" onClick={handleInstall}>
              ⚙️ 安装
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
          概览
        </button>
        <button
          className={`tab ${activeTab === 'steps' ? 'active' : ''}`}
          onClick={() => setActiveTab('steps')}
        >
          步骤
        </button>
        <button
          className={`tab ${activeTab === 'ratings' ? 'active' : ''}`}
          onClick={() => setActiveTab('ratings')}
        >
          评分 ({ratings.length})
        </button>
      </div>

      {/* 标签页内容 */}
      <div className="detail-content">
        {activeTab === 'overview' && (
          <div className="tab-content">
            <h2>📝 描述</h2>
            <p>{workflow.description || '暂无详细描述'}</p>

            {workflow.steps && (
              <>
                <h2>🔢 步骤数量</h2>
                <p>{workflow.steps.length} 个步骤</p>
              </>
            )}
          </div>
        )}

        {activeTab === 'steps' && (
          <div className="tab-content">
            <h2>📋 工作流步骤</h2>
            {workflow.steps && workflow.steps.length > 0 ? (
              <div className="steps-list">
                {workflow.steps.map((step, index) => (
                  <div key={index} className="step-item">
                    <div className="step-number">{index + 1}</div>
                    <div className="step-info">
                      <h3>{step.name || step.id}</h3>
                      <p className="step-agent">Agent: {step.agent}</p>
                      {step.description && (
                        <p className="step-description">{step.description}</p>
                      )}
                      {step.dependencies && step.dependencies.length > 0 && (
                        <p className="step-dependencies">
                          依赖: {step.dependencies.join(', ')}
                        </p>
                      )}
                    </div>
                  </div>
                ))}
              </div>
            ) : (
              <p>暂无步骤信息</p>
            )}
          </div>
        )}

        {activeTab === 'ratings' && (
          <div className="tab-content">
            <h2>⭐ 评分和评论</h2>

            {/* 评分表单 */}
            <div className="rating-form">
              <h3>给这个工作流评分</h3>
              <RatingStars
                rating={userRating}
                size="large"
                interactive
                onRate={setUserRating}
              />
              <textarea
                className="comment-input"
                placeholder="写下你的评论（可选）..."
                value={userComment}
                onChange={(e) => setUserComment(e.target.value)}
                rows={4}
              />
              <button className="btn-primary" onClick={handleRate}>
                提交评分
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
                <p>还没有评分，成为第一个评分的人吧！</p>
              )}
            </div>
          </div>
        )}
      </div>
    </div>
  );
};

export default WorkflowDetail;

