/**
 * 工作流市场浏览器组件 (Market Browser Component)
 *
 * 显示工作流市场的主界面
 *
 * @author OmniAgent Team
 * @since 2025-12-20
 */

import React, { useState, useEffect } from 'react';
import workflowApi from '../../api/modules/workflow';

const { searchWorkflows, getPopularWorkflows } = workflowApi;
import { useLanguage } from '../../contexts/LanguageContext';
import WorkflowCard from './WorkflowCard';
import SearchBar from './SearchBar';
import FilterPanel from './FilterPanel';
import '../../assets/css/workflow/market-browser.css';

const MarketBrowser = ({ onViewDetail }) => {
  const { t } = useLanguage();
  const [workflows, setWorkflows] = useState([]);
  const [loading, setLoading] = useState(false);
  const [searchKeyword, setSearchKeyword] = useState('');
  const [selectedCategory, setSelectedCategory] = useState('all');
  const [sortBy, setSortBy] = useState('popular');
  const [page, setPage] = useState(0);
  const [hasMore, setHasMore] = useState(true);

  // 加载工作流
  const loadWorkflows = async (reset = false) => {
    if (loading) return;

    setLoading(true);
    try {
      const currentPage = reset ? 0 : page;
      let result;

      if (sortBy === 'popular') {
        result = await getPopularWorkflows(20);
      } else if (searchKeyword) {
        result = await searchWorkflows(searchKeyword, currentPage, 20);
      } else {
        result = await searchWorkflows('', currentPage, 20);
      }

      const newWorkflows = result.workflows || result.content || result || [];
      
      // ⭐ 确保 newWorkflows 是数组
      const workflowsArray = Array.isArray(newWorkflows) ? newWorkflows : [];

      if (reset) {
        setWorkflows(workflowsArray);
        setPage(0);
      } else {
        setWorkflows([...workflows, ...workflowsArray]);
      }

      setHasMore(workflowsArray.length === 20);
    } catch (error) {
      console.error('Failed to load workflows:', error);
    } finally {
      setLoading(false);
    }
  };

  // 初始加载
  useEffect(() => {
    loadWorkflows(true);
  }, [sortBy]);

  // 搜索处理
  const handleSearch = (keyword) => {
    setSearchKeyword(keyword);
    setPage(0);
    loadWorkflows(true);
  };

  // 分类筛选
  const handleCategoryChange = (category) => {
    setSelectedCategory(category);
    setPage(0);
    loadWorkflows(true);
  };

  // 排序变更
  const handleSortChange = (sort) => {
    setSortBy(sort);
  };

  // 加载更多
  const handleLoadMore = () => {
    setPage(page + 1);
    loadWorkflows(false);
  };

  return (
    <div className="market-browser">
      {/* 顶部搜索栏 */}
      <div className="market-header">
        <h1>🏪 {t('workflowMarket.title')}</h1>
        <p>{t('workflowMarket.subtitle')}</p>
        <SearchBar onSearch={handleSearch} />
      </div>

      <div className="market-content">
        {/* 左侧筛选面板 */}
        <FilterPanel
          selectedCategory={selectedCategory}
          onCategoryChange={handleCategoryChange}
          sortBy={sortBy}
          onSortChange={handleSortChange}
        />

        {/* 右侧工作流列表 */}
        <div className="workflows-grid">
          {loading && workflows.length === 0 ? (
            <div className="loading">
              <div className="spinner"></div>
              <p>{t('workflowMarket.loading')}</p>
            </div>
          ) : workflows.length === 0 ? (
            <div className="empty-state">
              <p>😔 {t('workflowMarket.search.noResults')}</p>
              <p>{t('workflowMarket.search.tryOtherKeywords')}</p>
            </div>
          ) : (
            <>
              {workflows.map((workflow) => (
                <WorkflowCard
                  key={workflow.id}
                  workflow={workflow}
                  onViewDetail={onViewDetail}
                />
              ))}

              {hasMore && (
                <div className="load-more">
                  <button
                    onClick={handleLoadMore}
                    disabled={loading}
                    className="load-more-btn"
                  >
                    {loading ? t('workflowMarket.loading') : t('workflowMarket.loadMore')}
                  </button>
                </div>
              )}
            </>
          )}
        </div>
      </div>
    </div>
  );
};

export default MarketBrowser;

