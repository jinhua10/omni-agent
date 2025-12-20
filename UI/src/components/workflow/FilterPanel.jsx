import React from 'react';
import { useLanguage } from '../../contexts/LanguageContext';
import '../../assets/css/workflow/filter-panel.css';

const FilterPanel = ({ selectedCategory, onCategoryChange, sortBy, onSortChange }) => {
  const { t } = useLanguage();

  const categories = [
    { id: 'all', name: t('workflowMarket.category.all'), icon: '📦' },
    { id: 'data-processing', name: t('workflowMarket.category.dataProcessing'), icon: '📊' },
    { id: 'api-integration', name: t('workflowMarket.category.apiIntegration'), icon: '🔌' },
    { id: 'automation', name: t('workflowMarket.category.automation'), icon: '🤖' },
    { id: 'transformation', name: t('workflowMarket.category.transformation'), icon: '🔄' },
    { id: 'analysis', name: t('workflowMarket.category.analysis'), icon: '📈' },
    { id: 'example', name: t('workflowMarket.category.example'), icon: '📝' },
  ];

  const sortOptions = [
    { id: 'popular', name: t('workflowMarket.sort.popular') },
    { id: 'recent', name: t('workflowMarket.sort.recent') },
    { id: 'top-rated', name: t('workflowMarket.sort.topRated') },
    { id: 'name', name: t('workflowMarket.sort.name') },
  ];

  return (
    <div className="filter-panel">
      {/* 分类筛选 */}
      <div className="filter-section">
        <h3 className="filter-title">{t('workflowMarket.category.title')}</h3>
        <ul className="category-list">
          {categories.map((category) => (
            <li
              key={category.id}
              className={`category-item ${selectedCategory === category.id ? 'active' : ''}`}
              onClick={() => onCategoryChange(category.id)}
            >
              <span className="category-icon">{category.icon}</span>
              <span className="category-name">{category.name}</span>
            </li>
          ))}
        </ul>
      </div>

      {/* 排序选项 */}
      <div className="filter-section">
        <h3 className="filter-title">{t('workflowMarket.sort.title')}</h3>
        <ul className="sort-list">
          {sortOptions.map((option) => (
            <li
              key={option.id}
              className={`sort-item ${sortBy === option.id ? 'active' : ''}`}
              onClick={() => onSortChange(option.id)}
            >
              <input
                type="radio"
                name="sort"
                checked={sortBy === option.id}
                onChange={() => onSortChange(option.id)}
              />
              <span className="sort-name">{option.name}</span>
            </li>
          ))}
        </ul>
      </div>

      {/* 重置按钮 */}
      <div className="filter-actions">
        <button
          className="reset-btn"
          onClick={() => {
            onCategoryChange('all');
            onSortChange('popular');
          }}
        >
          🔄 {t('workflowMarket.reset')}
        </button>
      </div>
    </div>
  );
};

export default FilterPanel;

