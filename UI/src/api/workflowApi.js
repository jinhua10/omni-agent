import axios from 'axios';

// 获取 API 基础 URL
const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:8080';

// 创建 axios 实例
const apiClient = axios.create({
  baseURL: API_BASE_URL,
  timeout: 10000,
  headers: {
    'Content-Type': 'application/json',
  },
});

// 请求拦截器
apiClient.interceptors.request.use(
  (config) => {
    // 可以在这里添加 token
    const token = localStorage.getItem('token');
    if (token) {
      config.headers.Authorization = `Bearer ${token}`;
    }
    return config;
  },
  (error) => {
    return Promise.reject(error);
  }
);

// 响应拦截器
apiClient.interceptors.response.use(
  (response) => {
    return response.data;
  },
  (error) => {
    console.error('API Error:', error);
    return Promise.reject(error);
  }
);

// ========== 工作流市场 API ==========

/**
 * 搜索工作流
 * @param {string} keyword - 搜索关键词
 * @param {number} page - 页码
 * @param {number} size - 每页数量
 */
export const searchWorkflows = (keyword = '', page = 0, size = 20) => {
  return apiClient.get('/api/workflows/market/search', {
    params: { keyword, page, size },
  });
};

/**
 * 获取热门工作流
 * @param {number} limit - 数量限制
 */
export const getPopularWorkflows = (limit = 10) => {
  return apiClient.get('/api/workflows/market/popular', {
    params: { limit },
  });
};

/**
 * 获取最新工作流
 * @param {number} limit - 数量限制
 */
export const getRecentWorkflows = (limit = 10) => {
  return apiClient.get('/api/workflows/market/recent', {
    params: { limit },
  });
};

/**
 * 获取高评分工作流
 * @param {number} limit - 数量限制
 */
export const getTopRatedWorkflows = (limit = 10) => {
  return apiClient.get('/api/workflows/market/top-rated', {
    params: { limit },
  });
};

/**
 * 获取工作流详情
 * @param {string} id - 工作流ID
 */
export const getWorkflowDetail = (id) => {
  return apiClient.get(`/api/workflows/market/${id}`);
};

/**
 * 按分类获取工作流
 * @param {string} category - 分类
 * @param {number} page - 页码
 * @param {number} size - 每页数量
 */
export const getWorkflowsByCategory = (category, page = 0, size = 20) => {
  return apiClient.get(`/api/workflows/market/category/${category}`, {
    params: { page, size },
  });
};

/**
 * 按作者获取工作流
 * @param {string} authorId - 作者ID
 * @param {number} page - 页码
 * @param {number} size - 每页数量
 */
export const getWorkflowsByAuthor = (authorId, page = 0, size = 20) => {
  return apiClient.get(`/api/workflows/market/author/${authorId}`, {
    params: { page, size },
  });
};

/**
 * 下载工作流
 * @param {string} id - 工作流ID
 */
export const downloadWorkflow = (id) => {
  return apiClient.get(`/api/workflows/market/${id}/download`);
};

/**
 * 安装工作流
 * @param {string} id - 工作流ID
 * @param {string} userId - 用户ID
 */
export const installWorkflow = (id, userId) => {
  return apiClient.post(`/api/workflows/market/${id}/install`, null, {
    headers: { 'X-User-Id': userId },
  });
};

/**
 * 发布工作流
 * @param {Object} workflow - 工作流数据
 */
export const publishWorkflow = (workflow) => {
  return apiClient.post('/api/workflows/market/publish', workflow);
};

/**
 * 评分工作流
 * @param {string} id - 工作流ID
 * @param {number} rating - 评分 (1-5)
 * @param {string} userId - 用户ID
 * @param {string} comment - 评论（可选）
 */
export const rateWorkflow = (id, rating, userId, comment = '') => {
  return apiClient.post(`/api/workflows/market/${id}/rate`, {
    rating,
    userId,
    comment,
  });
};

/**
 * 获取工作流评分
 * @param {string} id - 工作流ID
 */
export const getWorkflowRatings = (id) => {
  return apiClient.get(`/api/workflows/market/${id}/ratings`);
};

export default apiClient;

