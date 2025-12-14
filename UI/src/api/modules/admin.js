// Admin API Module
import api from '../index';

export const adminApi = {
  /**
   * 更新系统配置
   */
  updateSystemConfig: (config) => {
    return api.put('/admin/system-config', config);
  },

  /**
   * 更新模型配置
   */
  updateModelConfig: (config) => {
    return api.put('/admin/model-config', config);
  },

  /**
   * 获取日志
   */
  getLogs: (params) => {
    return api.get('/admin/logs', { params });
  },

  /**
   * 获取监控指标
   */
  getMetrics: () => {
    return api.get('/admin/metrics');
  },

  /**
   * 健康检查
   */
  healthCheck: () => {
    return api.get('/admin/health');
  },
};

