// Service API Module
import api from '../index';

export const serviceApi = {
  /**
   * 获取服务列表
   * @param {Object} params - 查询参数
   */
  getServices: (params) => {
    return api.get('/services', { params });
  },

  /**
   * 获取服务详情
   * @param {string} id - 服务ID
   */
  getServiceDetail: (id) => {
    return api.get(`/services/${id}`);
  },

  /**
   * 安装服务
   * @param {string} id - 服务ID
   */
  installService: (id) => {
    return api.post(`/services/${id}/install`);
  },

  /**
   * 卸载服务
   * @param {string} id - 服务ID
   */
  uninstallService: (id) => {
    return api.post(`/services/${id}/uninstall`);
  },

  /**
   * 更新服务配置
   * @param {string} id - 服务ID
   * @param {Object} config - 配置数据
   */
  updateServiceConfig: (id, config) => {
    return api.put(`/services/${id}/config`, config);
  },

  /**
   * 生成PPT
   * @param {Object} data - PPT数据
   */
  generatePPT: (data) => {
    return api.post('/services/ppt/generate', data);
  },

  /**
   * 切换模型
   * @param {string} modelType - 模型类型 (local/online)
   */
  switchModel: (modelType) => {
    return api.post('/services/model/switch', { modelType });
  },
};

