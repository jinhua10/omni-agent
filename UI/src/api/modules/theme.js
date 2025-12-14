// 主题管理 API 模块 / Theme Management API Module
import api from '../index';

/**
 * 主题API / Theme API
 */
export const themeApi = {
  /**
   * 上传主题到服务器 / Upload theme to server
   * @param {FormData} formData - 包含主题配置和文件的表单数据 / Form data with theme config and files
   * @returns {Promise} 上传结果 / Upload result
   */
  uploadTheme: (formData) => {
    return api.post('/themes/upload', formData, {
      headers: {
        'Content-Type': 'multipart/form-data',
      },
    });
  },

  /**
   * 获取服务器上的主题列表 / Get theme list from server
   * @returns {Promise} 主题列表 / Theme list
   */
  getServerThemes: () => {
    return api.get('/themes/list');
  },

  /**
   * 获取指定主题详情 / Get specific theme details
   * @param {string} themeId - 主题ID / Theme ID
   * @returns {Promise} 主题详情 / Theme details
   */
  getThemeById: (themeId) => {
    return api.get(`/themes/${themeId}`);
  },

  /**
   * 删除服务器上的主题 / Delete theme from server
   * @param {string} themeId - 主题ID / Theme ID
   * @returns {Promise} 删除结果 / Delete result
   */
  deleteTheme: (themeId) => {
    return api.delete(`/themes/${themeId}`);
  },

  /**
   * 下载主题文件 / Download theme files
   * @param {string} themeId - 主题ID / Theme ID
   * @returns {Promise} 主题文件 / Theme files
   */
  downloadTheme: (themeId) => {
    return api.get(`/themes/${themeId}/download`, {
      responseType: 'blob',
    });
  },

  /**
   * 同步主题到服务器 / Sync theme to server
   * @param {Object} themeData - 主题数据 / Theme data
   * @returns {Promise} 同步结果 / Sync result
   */
  syncTheme: (themeData) => {
    return api.put('/themes/sync', themeData);
  },
};

export default themeApi;

