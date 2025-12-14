// Wish API Module
import api from '../index';

export const wishApi = {
  /**
   * 获取愿望列表
   * @param {Object} params - 查询参数
   * @param {string} params.status - 状态筛选
   * @param {string} params.category - 分类筛选
   * @param {string} params.sortBy - 排序方式
   * @param {string} params.keyword - 搜索关键词
   * @returns {Promise}
   */
  getWishes: (params) => {
    return api.get('/wishes', { params });
  },

  /**
   * 获取愿望详情
   * @param {number|string} id - 愿望ID
   * @returns {Promise}
   */
  getWishDetail: (id) => {
    return api.get(`/wishes/${id}`);
  },

  /**
   * 提交新愿望
   * @param {Object} data - 愿望数据
   * @param {string} data.title - 标题
   * @param {string} data.description - 描述
   * @param {string} data.category - 分类
   * @returns {Promise}
   */
  submitWish: (data) => {
    return api.post('/wishes', data);
  },

  /**
   * 投票
   * @param {number|string} id - 愿望ID
   * @param {string} voteType - 投票类型 (up/down)
   * @param {string} userId - 用户ID (可选，默认使用匿名用户)
   * @returns {Promise}
   */
  voteWish: (id, voteType, userId = 'anonymous') => {
    return api.post(`/wishes/${id}/vote`, { voteType, userId });
  },

  /**
   * 获取评论列表
   * @param {number|string} wishId - 愿望ID
   * @param {Object} params - 查询参数
   * @returns {Promise}
   */
  getComments: (wishId, params = {}) => {
    return api.get(`/wishes/${wishId}/comments`, { params });
  },

  /**
   * 添加评论
   * @param {number|string} wishId - 愿望ID
   * @param {Object} data - 评论数据
   * @param {string} data.content - 评论内容
   * @param {number} data.parentId - 父评论ID（回复时）
   * @returns {Promise}
   */
  addComment: (wishId, data) => {
    return api.post(`/wishes/${wishId}/comments`, data);
  },

  /**
   * 点赞评论
   * @param {number|string} commentId - 评论ID
   * @param {string} userId - 用户ID (可选，默认使用匿名用户)
   * @returns {Promise}
   */
  likeComment: (commentId, userId = 'anonymous') => {
    return api.post(`/wishes/comments/${commentId}/like`, { userId });
  },

  /**
   * 获取愿望排行榜
   * @param {number} limit - 数量限制
   * @returns {Promise}
   */
  getRanking: (limit = 10) => {
    return api.get('/wishes/ranking', { params: { limit } });
  },
};

