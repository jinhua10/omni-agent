/**
 * 页面数据适配器 / Page Data Adapter
 * 
 * 统一的数据层，所有主题Shell都通过这个适配器获取数据
 * 后端联调时只需要修改这一个文件
 * 
 * Unified data layer, all theme Shells get data through this adapter
 * Only need to modify this file when integrating with backend
 * 
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { useState, useEffect } from 'react';

/**
 * QA页面数据适配器
 * 所有主题的QA Shell都使用这个Hook获取数据
 * 
 * 后端接口：
 * - GET  /api/qa/statistics - 获取统计信息
 * - GET  /api/qa/health - 健康检查
 * - POST /api/qa/ask - 提问接口
 */
export function useQAPageData() {
  const [data, setData] = useState({
    stats: {
      documentCount: 0,           // 文档总数
      indexedDocumentCount: 0,    // 已索引文档数
      unindexedCount: 0,          // 未索引文档数
      indexProgress: 0,           // 索引进度
      needsIndexing: false        // 是否需要索引
    },
    systemStatus: 'unknown',      // 系统状态
    recentQuestions: [],
    loading: true,
    error: null
  });

  useEffect(() => {
    const fetchData = async () => {
      try {
        // 并行获取统计信息和健康状态
        const [statsResponse, healthResponse] = await Promise.all([
          apiCall('/qa/statistics'),
          apiCall('/qa/health')
        ]);

        setData({
          stats: {
            documentCount: statsResponse.documentCount || 0,
            indexedDocumentCount: statsResponse.indexedDocumentCount || 0,
            unindexedCount: statsResponse.unindexedCount || 0,
            indexProgress: statsResponse.indexProgress || 0,
            needsIndexing: statsResponse.needsIndexing || false
          },
          systemStatus: healthResponse.status || 'unknown',
          recentQuestions: [], // 可以后续添加最近问题列表接口
          loading: false,
          error: null
        });
      } catch (error) {
        console.error('Failed to fetch QA page data:', error);
        setData(prev => ({ ...prev, loading: false, error: error.message }));
      }
    };

    fetchData();
  }, []);

  return data;
}

/**
 * 首页数据适配器
 */
export function useHomePageData() {
  const [data, setData] = useState({
    summary: {
      documents: 0,
      reviews: 0,
      users: 0,
      aiScore: 0
    },
    recentActivities: [],
    loading: true,
    error: null
  });

  useEffect(() => {
    // TODO: 替换为真实API
    setTimeout(() => {
      setData({
        summary: {
          documents: 856,
          reviews: 2341,
          users: 128,
          aiScore: 92
        },
        recentActivities: [],
        loading: false,
        error: null
      });
    }, 500);
  }, []);

  return data;
}

/**
 * 文档管理页面数据适配器 / Documents page data adapter
 */
export function useDocumentsPageData() {
  const [data, setData] = useState({
    documents: [],
    stats: {
      total: 0,
      indexed: 0,
      unindexed: 0,
      fileTypes: []
    },
    loading: true,
    error: null
  });

  useEffect(() => {
    const fetchData = async () => {
      try {
        // 并行获取文档列表和支持的文件类型 / Fetch documents list and file types in parallel
        const [listResponse, typesResponse] = await Promise.all([
          apiCall('/documents/list?page=1&pageSize=-1'), // Get all documents
          apiCall('/documents/supported-types')
        ]);

        const documents = listResponse.documents || [];
        const indexedCount = documents.filter(doc => doc.indexed).length;

        setData({
          documents: documents,
          stats: {
            total: listResponse.total || 0,
            indexed: indexedCount,
            unindexed: (listResponse.total || 0) - indexedCount,
            fileTypes: typesResponse.types || []
          },
          loading: false,
          error: null
        });
      } catch (error) {
        console.error('Failed to fetch documents page data:', error);
        setData(prev => ({ ...prev, loading: false, error: error.message }));
      }
    };

    fetchData();
  }, []);

  return data;
}

/**
 * 协作空间页面数据适配器
 */
export function useCollaborationPageData() {
  const [data, setData] = useState({
    members: [],
    projects: [],
    messages: [],
    loading: true,
    error: null
  });

  useEffect(() => {
    // TODO: 替换为真实API
    setTimeout(() => {
      setData({
        members: [],
        projects: [],
        messages: [],
        loading: false,
        error: null
      });
    }, 500);
  }, []);

  return data;
}

/**
 * 数据分析页面数据适配器
 */
export function useAnalyticsPageData() {
  const [data, setData] = useState({
    charts: {
      trends: [],
      distribution: [],
      performance: []
    },
    metrics: {
      accuracy: 0,
      speed: 0,
      satisfaction: 0
    },
    loading: true,
    error: null
  });

  useEffect(() => {
    // TODO: 替换为真实API
    setTimeout(() => {
      setData({
        charts: {
          trends: [],
          distribution: [],
          performance: []
        },
        metrics: {
          accuracy: 94,
          speed: 88,
          satisfaction: 92
        },
        loading: false,
        error: null
      });
    }, 500);
  }, []);

  return data;
}

/**
 * 系统设置页面数据适配器
 */
export function useSettingsPageData() {
  const [data, setData] = useState({
    user: {
      name: '',
      email: '',
      avatar: ''
    },
    preferences: {
      language: 'zh',
      theme: 'bubble',
      notifications: true
    },
    loading: true,
    error: null
  });

  useEffect(() => {
    // TODO: 替换为真实API
    setTimeout(() => {
      setData({
        user: {
          name: 'Admin',
          email: 'admin@example.com',
          avatar: ''
        },
        preferences: {
          language: 'zh',
          theme: 'bubble',
          notifications: true
        },
        loading: false,
        error: null
      });
    }, 500);
  }, []);

  return data;
}

/**
 * 通用的API调用函数
 * 已配置后端API基础URL
 */
export async function apiCall(endpoint, options = {}) {
  // 后端API基础URL - 使用Vite代理配置的相对路径 / Backend API base URL - use Vite proxy relative path
  const baseURL = process.env.REACT_APP_API_BASE_URL || '/api';
  
  const defaultOptions = {
    headers: {
      'Content-Type': 'application/json',
      'Accept': 'application/json',
      // 添加认证token等
      // 'Authorization': `Bearer ${getToken()}`
    },
    ...options
  };

  try {
    const url = `${baseURL}${endpoint}`;
    console.log(`API Call: ${options.method || 'GET'} ${url}`);
    
    const response = await fetch(url, defaultOptions);
    
    if (!response.ok) {
      const errorText = await response.text().catch(() => 'Unknown error');
      throw new Error(`API Error ${response.status}: ${errorText}`);
    }
    
    const result = await response.json();
    console.log(`API Response:`, result);
    
    return result;
  } catch (error) {
    console.error('API call failed:', error);
    throw error;
  }
}

/**
 * POST请求封装
 */
export async function apiPost(endpoint, data) {
  return apiCall(endpoint, {
    method: 'POST',
    body: JSON.stringify(data)
  });
}

/**
 * 提问接口
 * POST /api/qa/ask
 */
export async function askQuestion(question, hopeSessionId = null) {
  return apiPost('/qa/ask', {
    question,
    hopeSessionId
  });
}

/**
 * 搜索文档接口
 * GET /api/qa/search?query=xxx&limit=10
 */
export async function searchDocuments(query, limit = 10) {
  return apiCall(`/qa/search?query=${encodeURIComponent(query)}&limit=${limit}`);
}

/**
 * 批量上传文档 / Batch upload documents
 * @param {File[]} files - 文件数组
 * @param {Function} onProgress - 进度回调
 * @returns {Promise} 上传结果
 */
export async function batchUploadDocuments(files, onProgress) {
  const formData = new FormData();
  
  // 添加所有文件到FormData / Add all files to FormData
  files.forEach((file) => {
    formData.append('files', file);
  });
  
  // 添加语言参数 / Add language parameter
  formData.append('lang', localStorage.getItem('language') || 'zh');
  
  try {
    const baseURL = process.env.REACT_APP_API_BASE_URL || '/api';
    
    return new Promise((resolve, reject) => {
      const xhr = new XMLHttpRequest();
      
      // 监听上传进度 / Monitor upload progress
      if (onProgress) {
        xhr.upload.addEventListener('progress', (e) => {
          if (e.lengthComputable) {
            const percentComplete = Math.round((e.loaded / e.total) * 100);
            onProgress(percentComplete);
          }
        });
      }
      
      // 监听完成 / Monitor completion
      xhr.addEventListener('load', () => {
        if (xhr.status >= 200 && xhr.status < 300) {
          try {
            const result = JSON.parse(xhr.responseText);
            console.log('Batch upload success:', result);
            resolve(result);
          } catch (e) {
            reject(new Error('Failed to parse response'));
          }
        } else {
          reject(new Error(`Upload failed with status ${xhr.status}`));
        }
      });
      
      // 监听错误 / Monitor errors
      xhr.addEventListener('error', () => {
        reject(new Error('Network error during upload'));
      });
      
      xhr.open('POST', `${baseURL}/documents/upload-batch`);
      xhr.send(formData);
    });
  } catch (error) {
    console.error('Batch upload failed:', error);
    throw error;
  }
}

/**
 * 触发知识库重建
 * POST /api/qa/rebuild
 */
export async function rebuildKnowledgeBase() {
  return apiPost('/qa/rebuild', {});
}

/**
 * 触发增量索引
 * POST /api/qa/incremental-index
 */
export async function incrementalIndex() {
  return apiPost('/qa/incremental-index', {});
}

/**
 * 示例：真实API调用的实现方式
 * 
 * export function useQAPageData() {
 *   const [data, setData] = useState({ loading: true, error: null });
 *   
 *   useEffect(() => {
 *     async function fetchData() {
 *       try {
 *         const stats = await apiCall('/qa/stats');
 *         const questions = await apiCall('/qa/recent');
 *         
 *         setData({
 *           stats,
 *           recentQuestions: questions,
 *           loading: false,
 *           error: null
 *         });
 *       } catch (error) {
 *         setData(prev => ({ ...prev, loading: false, error: error.message }));
 *       }
 *     }
 *     
 *     fetchData();
 *   }, []);
 *   
 *   return data;
 * }
 */
