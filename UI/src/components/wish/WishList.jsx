import React, { useState, useEffect, useCallback } from 'react';
import { Input, Select, Radio, Empty, Spin, message } from 'antd';
import { SearchOutlined, PlusOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import { wishApi } from '../../api/modules/wish';
import WishCard from './WishCard';
import WishSubmit from './WishSubmit';
import WishDetail from './WishDetail';
import WishRanking from './WishRanking';
import '../../assets/css/wish/wish-list.css';

const { Search } = Input;
const { Option } = Select;

const WishList = () => {
  const { t } = useLanguage();
  const [wishes, setWishes] = useState([]);
  const [loading, setLoading] = useState(false);
  const [viewMode, setViewMode] = useState('grid'); // grid | list
  const [filterStatus, setFilterStatus] = useState('all');
  const [filterCategory, setFilterCategory] = useState('all');
  const [sortBy, setSortBy] = useState('latest'); // latest | hottest | most_voted
  const [searchKeyword, setSearchKeyword] = useState('');
  const [submitModalVisible, setSubmitModalVisible] = useState(false);
  const [detailModalVisible, setDetailModalVisible] = useState(false);
  const [selectedWish, setSelectedWish] = useState(null);
  const [showRanking, setShowRanking] = useState(true);

  // 加载愿望列表
  const loadWishes = useCallback(async () => {
    setLoading(true);
    try {
      const params = {
        status: filterStatus === 'all' ? undefined : filterStatus,
        category: filterCategory === 'all' ? undefined : filterCategory,
        sortBy,
        keyword: searchKeyword || undefined,
      };
      const response = await wishApi.getWishes(params);
      // 后端返回分页结构：{ content: [], totalElements: 0, ... } (Backend returns paginated structure)
      const wishData = response.content || response.data || [];
      setWishes(Array.isArray(wishData) ? wishData : []);
    } catch (error) {
      console.error('Failed to load wishes:', error);
      message.error(t('wish.loadFailed'));
      setWishes([]); // 设置为空数组防止崩溃 (Set to empty array to prevent crash)
    } finally {
      setLoading(false);
    }
  }, [filterStatus, filterCategory, sortBy, searchKeyword, t]);

  useEffect(() => {
    loadWishes();
  }, [loadWishes]);

  // 处理搜索
  const handleSearch = (value) => {
    setSearchKeyword(value);
  };

  // 处理愿望点击
  const handleWishClick = (wish) => {
    setSelectedWish(wish);
    setDetailModalVisible(true);
  };

  // 处理投票
  const handleVote = async (wishId, voteType) => {
    try {
      await wishApi.voteWish(wishId, voteType);
      message.success(t('wish.voteSuccess'));
      loadWishes(); // 重新加载列表
    } catch (error) {
      console.error('Failed to vote:', error);
      message.error(t('wish.voteFailed'));
    }
  };

  // 处理提交成功
  const handleSubmitSuccess = () => {
    setSubmitModalVisible(false);
    message.success(t('wish.submitSuccess'));
    loadWishes(); // 重新加载列表
  };

  return (
    <div className="wish-list">
      {/* 头部 */}
      <div className="wish-list__header">
        <div className="wish-list__header-left">
          <h2 className="wish-list__title">{t('wish.title')}</h2>
          <span className="wish-list__count">
            {wishes.length} {t('wish.totalWishes')}
          </span>
        </div>
        <div className="wish-list__header-right">
          <button
            className="wish-list__submit-btn"
            onClick={() => setSubmitModalVisible(true)}
          >
            <PlusOutlined /> {t('wish.submit')}
          </button>
        </div>
      </div>

      {/* 过滤和排序工具栏 */}
      <div className="wish-list__toolbar">
        <div className="wish-list__toolbar-left">
          <Search
            className="wish-list__search"
            placeholder={t('wish.searchPlaceholder')}
            allowClear
            onSearch={handleSearch}
            prefix={<SearchOutlined />}
            style={{ width: 300 }}
          />

          <Select
            className="wish-list__filter"
            value={filterStatus}
            onChange={setFilterStatus}
            style={{ width: 150 }}
          >
            <Option value="all">{t('wish.filter.all')}</Option>
            <Option value="pending">{t('wish.status.pending')}</Option>
            <Option value="in_progress">{t('wish.status.in_progress')}</Option>
            <Option value="completed">{t('wish.status.completed')}</Option>
            <Option value="rejected">{t('wish.status.rejected')}</Option>
          </Select>

          <Select
            className="wish-list__filter"
            value={filterCategory}
            onChange={setFilterCategory}
            style={{ width: 150 }}
          >
            <Option value="all">{t('wish.filter.all')}</Option>
            <Option value="feature">{t('wish.category.feature')}</Option>
            <Option value="bug">{t('wish.category.bug')}</Option>
            <Option value="interface">{t('wish.category.interface')}</Option>
            <Option value="improvement">{t('wish.category.improvement')}</Option>
          </Select>

          <Select
            className="wish-list__sort"
            value={sortBy}
            onChange={setSortBy}
            style={{ width: 150 }}
          >
            <Option value="latest">{t('wish.sort.latest')}</Option>
            <Option value="hottest">{t('wish.sort.hottest')}</Option>
            <Option value="most_voted">{t('wish.sort.most_voted')}</Option>
          </Select>
        </div>

        <div className="wish-list__toolbar-right">
          <Radio.Group value={viewMode} onChange={(e) => setViewMode(e.target.value)}>
            <Radio.Button value="grid">{t('wish.view.grid')}</Radio.Button>
            <Radio.Button value="list">{t('wish.view.list')}</Radio.Button>
          </Radio.Group>
        </div>
      </div>

      {/* 主内容区域 */}
      <div className="wish-list__content">
        {/* 左侧：愿望列表 */}
        <div className="wish-list__main">
          {loading ? (
            <div className="wish-list__loading">
              <Spin size="large" tip={t('wish.loading')}>
                <div style={{ padding: 50 }} />
              </Spin>
            </div>
          ) : wishes.length === 0 ? (
            <Empty
              className="wish-list__empty"
              description={t('wish.empty')}
              image={Empty.PRESENTED_IMAGE_SIMPLE}
            />
          ) : (
            <div className={`wish-list__items wish-list__items--${viewMode}`}>
              {wishes.map((wish) => (
                <WishCard
                  key={wish.id}
                  wish={wish}
                  viewMode={viewMode}
                  onClick={() => handleWishClick(wish)}
                  onVote={(voteType) => handleVote(wish.id, voteType)}
                />
              ))}
            </div>
          )}
        </div>

        {/* 右侧：排行榜 */}
        {showRanking && (
          <div className="wish-list__sidebar">
            <WishRanking onWishClick={handleWishClick} />
          </div>
        )}
      </div>

      {/* 提交愿望模态框 */}
      <WishSubmit
        visible={submitModalVisible}
        onClose={() => setSubmitModalVisible(false)}
        onSuccess={handleSubmitSuccess}
      />

      {/* 愿望详情模态框 */}
      {selectedWish && (
        <WishDetail
          visible={detailModalVisible}
          wish={selectedWish}
          onClose={() => {
            setDetailModalVisible(false);
            setSelectedWish(null);
          }}
          onVote={handleVote}
          onUpdate={loadWishes}
        />
      )}
    </div>
  );
};

export default WishList;

