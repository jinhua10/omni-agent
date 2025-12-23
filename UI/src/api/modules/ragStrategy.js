/**
 * RAG策略模板管理API
 * (RAG Strategy Template Management API)
 * 
 * @author AI Reviewer Team
 * @since 2025-12-23
 */

import axios from 'axios';

const BASE_URL = '/api/system/rag-config';

/**
 * 策略模板管理API
 */
const ragStrategyApi = {
  /**
   * 获取所有策略模板
   * @returns {Promise} 策略模板列表
   */
  getTemplates: async () => {
    const response = await axios.get(`${BASE_URL}/templates`);
    return response.data;
  },

  /**
   * 获取单个策略模板详情
   * @param {string} templateId - 模板ID
   * @returns {Promise} 策略模板详情
   */
  getTemplate: async (templateId) => {
    const response = await axios.get(`${BASE_URL}/templates/${templateId}`);
    return response.data;
  },

  /**
   * 创建策略模板
   * @param {Object} template - 模板配置
   * @param {string} template.name - 模板名称
   * @param {string} template.description - 模板描述
   * @param {Object} template.extractionConfig - 文本提取配置
   * @param {Object} template.chunkingConfig - 分块配置
   * @param {Object} template.vectorizationConfig - 向量化配置
   * @returns {Promise} 创建结果
   */
  createTemplate: async (template) => {
    const response = await axios.post(`${BASE_URL}/templates`, template);
    return response.data;
  },

  /**
   * 更新策略模板
   * @param {string} templateId - 模板ID
   * @param {Object} template - 模板配置
   * @returns {Promise} 更新结果
   */
  updateTemplate: async (templateId, template) => {
    const response = await axios.put(`${BASE_URL}/templates/${templateId}`, template);
    return response.data;
  },

  /**
   * 删除策略模板
   * @param {string} templateId - 模板ID
   * @returns {Promise} 删除结果
   */
  deleteTemplate: async (templateId) => {
    const response = await axios.delete(`${BASE_URL}/templates/${templateId}`);
    return response.data;
  },

  /**
   * 应用策略模板到文档
   * @param {string} documentId - 文档ID
   * @param {string} templateId - 模板ID
   * @returns {Promise} 应用结果
   */
  applyTemplateToDocument: async (documentId, templateId) => {
    // ⭐ 对URL中的documentId进行编码
    const encodedDocId = encodeURIComponent(documentId);
    const response = await axios.post(`${BASE_URL}/documents/${encodedDocId}/apply-template`, {
      templateId
    });
    return response.data;
  },

  /**
   * 从当前文档配置保存为模板
   * @param {string} documentId - 文档ID
   * @param {Object} templateInfo - 模板信息
   * @param {string} templateInfo.name - 模板名称
   * @param {string} templateInfo.description - 模板描述
   * @returns {Promise} 保存结果
   */
  saveCurrentAsTemplate: async (documentId, templateInfo) => {
    // ⭐ 对URL中的documentId进行编码
    const encodedDocId = encodeURIComponent(documentId);
    const response = await axios.post(`${BASE_URL}/documents/${encodedDocId}/save-as-template`, templateInfo);
    return response.data;
  },

  /**
   * 获取文档当前配置
   * @param {string} documentId - 文档ID
   * @returns {Promise} 文档配置
   */
  getDocumentConfig: async (documentId) => {
    // ⭐ 对URL中的documentId进行编码
    const encodedDocId = encodeURIComponent(documentId);
    const response = await axios.get(`${BASE_URL}/document/${encodedDocId}`);
    return response.data;
  },

  /**
   * 开始处理文档（使用当前配置）
   * @param {string} documentId - 文档ID
   * @returns {Promise} 处理结果
   */
  startProcessing: async (documentId) => {
    // ⭐ 对URL中的documentId进行编码
    const encodedDocId = encodeURIComponent(documentId);
    const response = await axios.post(`${BASE_URL}/documents/${encodedDocId}/process`);
    return response.data;
  }
};

export default ragStrategyApi;
