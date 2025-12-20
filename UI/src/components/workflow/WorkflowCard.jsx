import React from 'react';
import { useLanguage } from '../../contexts/LanguageContext';
import RatingStars from './RatingStars';
import '../../assets/css/workflow/workflow-card.css';

const WorkflowCard = ({ workflow, onViewDetail }) => {
  const { t } = useLanguage();
  const handleClick = () => {
    if (onViewDetail) {
      onViewDetail(workflow.id);
    }
  };

  const getCategoryIcon = (category) => {
    const icons = {
      'data-processing': '📊',
      'api-integration': '🔌',
      'automation': '🤖',
      'transformation': '🔄',
      'analysis': '📈',
      'example': '📝',
    };
    return icons[category] || '📦';
  };

  return (
    <div className="workflow-card" onClick={handleClick}>
      {/* 卡片头部 */}
      <div className="card-header">
        <div className="category-badge">
          <span className="category-icon">{getCategoryIcon(workflow.category)}</span>
          <span className="category-name">{workflow.category || 'General'}</span>
        </div>
        {workflow.status === 'featured' && (
          <div className="featured-badge">⭐ {t('workflowMarket.card.featured')}</div>
        )}
      </div>

      {/* 工作流名称和描述 */}
      <div className="card-body">
        <h3 className="workflow-name">{workflow.name}</h3>
        <p className="workflow-description">
          {workflow.description || t('workflowMarket.detail.noDescription')}
        </p>

        {/* 标签 */}
        {workflow.tags && workflow.tags.length > 0 && (
          <div className="tags">
            {workflow.tags.slice(0, 3).map((tag, index) => (
              <span key={index} className="tag">
                {tag}
              </span>
            ))}
            {workflow.tags.length > 3 && (
              <span className="tag">+{workflow.tags.length - 3}</span>
            )}
          </div>
        )}
      </div>

      {/* 卡片底部 */}
      <div className="card-footer">
        <div className="author-info">
          <span className="author-icon">👤</span>
          <span className="author-name">{workflow.author || 'Unknown'}</span>
        </div>

        <div className="workflow-stats">
          <RatingStars rating={workflow.averageRating || 0} size="small" />
          <span className="download-count">
            ⬇️ {workflow.downloadCount || 0}
          </span>
        </div>
      </div>

      {/* 版本信息 */}
      <div className="card-version">
        <span className="version">v{workflow.version || '1.0.0'}</span>
      </div>
    </div>
  );
};

export default WorkflowCard;

