/**
 * 工作流 API 模块 (Workflow API Module)
 *
 * 提供工作流市场和构建器相关的 API 接口
 * (Provides workflow market and builder related API interfaces)
 *
 * @author AI Reviewer Team
 * @since 2025-12-21
 */

import { request } from '../index'

const workflowApi = {
  // ========== 工作流市场 API (Workflow Market API) ==========

  /**
   * 搜索工作流 (Search workflows)
   * @param {string} keyword - 搜索关键词 (Search keyword)
   * @param {number} page - 页码 (Page number)
   * @param {number} size - 每页数量 (Page size)
   * @returns {Promise} 工作流列表
   */
  searchWorkflows(keyword = '', page = 0, size = 20) {
    return request.get('/workflows/market/search', { keyword, page, size })
  },

  /**
   * 获取热门工作流 (Get popular workflows)
   * @param {number} limit - 数量限制 (Limit)
   * @returns {Promise} 热门工作流列表
   */
  getPopularWorkflows(limit = 10) {
    return request.get('/workflows/market/popular', { limit })
  },

  /**
   * 获取最新工作流 (Get recent workflows)
   * @param {number} limit - 数量限制 (Limit)
   * @returns {Promise} 最新工作流列表
   */
  getRecentWorkflows(limit = 10) {
    return request.get('/workflows/market/recent', { limit })
  },

  /**
   * 获取高评分工作流 (Get top rated workflows)
   * @param {number} limit - 数量限制 (Limit)
   * @returns {Promise} 高评分工作流列表
   */
  getTopRatedWorkflows(limit = 10) {
    return request.get('/workflows/market/top-rated', { limit })
  },

  /**
   * 获取工作流详情 (Get workflow detail)
   * @param {string} id - 工作流ID (Workflow ID)
   * @returns {Promise} 工作流详情
   */
  getWorkflowDetail(id) {
    return request.get(`/workflows/market/${id}`)
  },

  /**
   * 按分类获取工作流 (Get workflows by category)
   * @param {string} category - 分类 (Category)
   * @param {number} page - 页码 (Page number)
   * @param {number} size - 每页数量 (Page size)
   * @returns {Promise} 工作流列表
   */
  getWorkflowsByCategory(category, page = 0, size = 20) {
    return request.get(`/workflows/market/category/${category}`, { page, size })
  },

  /**
   * 按作者获取工作流 (Get workflows by author)
   * @param {string} authorId - 作者ID (Author ID)
   * @param {number} page - 页码 (Page number)
   * @param {number} size - 每页数量 (Page size)
   * @returns {Promise} 工作流列表
   */
  getWorkflowsByAuthor(authorId, page = 0, size = 20) {
    return request.get(`/workflows/market/author/${authorId}`, { page, size })
  },

  /**
   * 下载工作流 (Download workflow)
   * @param {string} id - 工作流ID (Workflow ID)
   * @returns {Promise} 下载结果
   */
  downloadWorkflow(id) {
    return request.get(`/workflows/market/${id}/download`)
  },

  /**
   * 安装工作流 (Install workflow)
   * @param {string} id - 工作流ID (Workflow ID)
   * @param {string} userId - 用户ID (User ID)
   * @returns {Promise} 安装结果
   */
  installWorkflow(id, userId) {
    return request.post(`/workflows/market/${id}/install`, null, {
      headers: { 'X-User-Id': userId },
    })
  },

  /**
   * 发布工作流 (Publish workflow)
   * @param {Object} workflow - 工作流数据 (Workflow data)
   * @param {string} workflow.name - 工作流名称
   * @param {string} workflow.description - 描述
   * @param {string} workflow.category - 分类
   * @param {Array} workflow.steps - 步骤列表
   * @returns {Promise} 发布结果
   */
  publishWorkflow(workflow) {
    return request.post('/workflows/market/publish', workflow)
  },

  /**
   * 评分工作流 (Rate workflow)
   * @param {string} id - 工作流ID (Workflow ID)
   * @param {number} rating - 评分 (1-5) (Rating 1-5)
   * @param {string} userId - 用户ID (User ID)
   * @param {string} comment - 评论（可选）(Comment, optional)
   * @returns {Promise} 评分结果
   */
  rateWorkflow(id, rating, userId, comment = '') {
    return request.post(`/workflows/market/${id}/rate`, {
      rating,
      userId,
      comment,
    })
  },

  /**
   * 获取工作流评分 (Get workflow ratings)
   * @param {string} id - 工作流ID (Workflow ID)
   * @returns {Promise} 评分列表
   */
  getWorkflowRatings(id) {
    return request.get(`/workflows/market/${id}/ratings`)
  },

  // ========== 工作流构建器 API (Workflow Builder API) ==========

  /**
   * 创建工作流 (Create workflow)
   * @param {Object} workflow - 工作流定义 (Workflow definition)
   * @param {string} workflow.name - 工作流名称
   * @param {string} workflow.description - 描述
   * @param {Array} workflow.steps - 步骤列表
   * @returns {Promise} 创建结果
   */
  createWorkflow(workflow) {
    return request.post('/example/workflow/create', workflow)
  },

  /**
   * 更新工作流 (Update workflow)
   * @param {string} workflowId - 工作流ID (Workflow ID)
   * @param {Object} workflow - 工作流定义 (Workflow definition)
   * @returns {Promise} 更新结果
   */
  updateWorkflow(workflowId, workflow) {
    return request.put(`/example/workflow/${workflowId}`, workflow)
  },

  /**
   * 获取工作流列表 (Get workflow list)
   * @returns {Promise} 工作流列表
   */
  getWorkflowList() {
    return request.get('/example/workflow/list')
  },

  /**
   * 获取工作流详情（本地）(Get workflow detail - local)
   * @param {string} workflowName - 工作流名称 (Workflow name)
   * @returns {Promise} 工作流详情
   */
  getLocalWorkflowDetail(workflowName) {
    return request.get(`/example/workflow/detail/${workflowName}`)
  },

  /**
   * 执行工作流 (Execute workflow)
   * @param {string} workflowName - 工作流名称 (Workflow name)
   * @param {Object} input - 输入数据 (Input data)
   * @returns {Promise} 执行结果
   */
  executeWorkflow(workflowName, input) {
    return request.post(`/example/workflow/execute/${workflowName}`, input)
  },

  /**
   * 异步执行工作流 (Execute workflow asynchronously)
   * @param {string} workflowName - 工作流名称 (Workflow name)
   * @param {Object} input - 输入数据 (Input data)
   * @returns {Promise} 执行任务ID
   */
  executeWorkflowAsync(workflowName, input) {
    return request.post(`/example/workflow/execute-async/${workflowName}`, input)
  },

  /**
   * 获取可用的 Agent 列表 (Get available agents list)
   * @returns {Promise} Agent 列表
   */
  async getAgentList() {
    try {
      return await request.get('/example/workflow/agents')
    } catch (error) {
      // 后端接口不可用时，返回默认的 Agent 列表
      // If backend is unavailable, return default agents list
      console.warn('⚠️ Agent list API not available, using default agents')
      return {
        success: true,
        agents: [
          {
            name: 'EchoAgent',
            description: '回显输入数据，用于测试和调试 (Echo input data for testing)',
            category: 'utility',
            tags: ['test', 'debug'],
          },
          {
            name: 'DataTransformer',
            description: '转换和处理数据 (Transform and process data)',
            category: 'transform',
            tags: ['data', 'transform'],
          },
          {
            name: 'DataValidator',
            description: '验证数据格式和内容 (Validate data format and content)',
            category: 'validate',
            tags: ['data', 'validate'],
          },
          {
            name: 'DataFilter',
            description: '过滤和筛选数据 (Filter and select data)',
            category: 'data',
            tags: ['data', 'filter'],
          },
          {
            name: 'WorkflowInvoker',
            description: '调用其他工作流，支持并行和批量处理 (Invoke other workflows with parallel and batch support)',
            category: 'integration',
            tags: ['workflow', 'invoke'],
          },
        ],
      }
    }
  },

  /**
   * 测试工作流 (Test workflow)
   * @returns {Promise} 测试结果
   */
  testWorkflow() {
    return request.get('/example/workflow/test')
  },
}

export default workflowApi
