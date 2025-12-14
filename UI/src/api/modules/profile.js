// Profile API Module
import api from '../index';

export const profileApi = {
  /**
   * 获取用户信息
   */
  getUserInfo: () => {
    return api.get('/profile/info');
  },

  /**
   * 更新用户信息
   */
  updateUserInfo: (data) => {
    return api.put('/profile/info', data);
  },

  /**
   * 获取使用统计
   */
  getUsageStatistics: (userId) => {
    return api.get(`/profile/${userId}/statistics`);
  },

  /**
   * 获取贡献统计
   */
  getContributions: (userId) => {
    return api.get(`/profile/${userId}/contributions`);
  },

  /**
   * 获取成就列表
   */
  getAchievements: (userId) => {
    return api.get(`/profile/${userId}/achievements`);
  },

  /**
   * 更新用户设置
   */
  updateSettings: (settings) => {
    return api.put('/profile/settings', settings);
  },
};

