/**
 * 完整文档管理组件 / Complete Document Management Component
 * 
 * 提供文档管理的完整功能，包括：
 * - 文档列表展示（带分页）
 * - 简单搜索和高级搜索
 * - 批量文件上传（带进度）
 * - 文档删除操作
 * 
 * Provides complete document management features including:
 * - Document list display (with pagination)
 * - Simple search and advanced search
 * - Batch file upload (with progress)
 * - Document deletion
 * 
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect, useCallback } from 'react';
import { useLanguage } from '../../../../contexts/LanguageContext';
import { apiCall, batchUploadDocuments } from '../../../../adapters/PageDataAdapter';
import './bubble-common.css';
import './DocumentManagement.css';

function DocumentManagement() {
  // ============================================================================
  // Hooks / 钩子
  // ============================================================================
  const { t, language } = useLanguage();
  
  // ============================================================================
  // State / 状态管理
  // ============================================================================
  
  // 文档列表状态 (Document list state)
  const [documents, setDocuments] = useState([]); // 文档数组 (Documents array)
  const [loading, setLoading] = useState(false); // 加载状态 (Loading state)
  const [error, setError] = useState(null); // 错误信息 (Error message)
  
  // 分页状态 (Pagination state)
  const [pagination, setPagination] = useState({
    currentPage: 1,      // 当前页码 (Current page number)
    pageSize: 20,        // 每页数量 (Items per page)
    total: 0,            // 总文档数 (Total documents)
    totalPages: 0        // 总页数 (Total pages)
  });
  
  // 支持的文件类型 (Supported file types)
  const [supportedFileTypes, setSupportedFileTypes] = useState([]);
  
  // 搜索状态 (Search state)
  const [showAdvancedSearch, setShowAdvancedSearch] = useState(false); // 是否显示高级搜索 (Show advanced search)
  const [simpleSearch, setSimpleSearch] = useState(''); // 简单搜索关键词 (Simple search keyword)
  const [advancedFilters, setAdvancedFilters] = useState({
    search: '',          // 搜索关键词 (Search keyword)
    searchMode: 'contains', // 搜索模式 (Search mode)
    fileTypes: [],       // 文件类型过滤 (File type filter)
    minSize: '',         // 最小文件大小 (Min file size)
    maxSize: '',         // 最大文件大小 (Max file size)
    indexed: 'all',      // 索引状态 (Index status)
    startDate: '',       // 开始日期 (Start date)
    endDate: '',         // 结束日期 (End date)
    sortBy: 'date',      // 排序字段 (Sort field)
    sortOrder: 'desc'    // 排序方向 (Sort order)
  });
  
  // 上传状态 (Upload state)
  const [uploading, setUploading] = useState(false); // 是否正在上传 (Uploading)
  const [uploadProgress, setUploadProgress] = useState(null); // 上传进度 (Upload progress)
  
  // ============================================================================
  // API Functions / API 函数
  // ============================================================================
  
  /**
   * 加载支持的文件类型 (Load supported file types)
   * 
   * 从后端获取系统支持的所有文件类型，并更新全局文件图标映射
   * Fetch all supported file types from backend and update global file icon map
   * 
   * @returns {Promise<void>}
   */
  const loadSupportedFileTypes = useCallback(async () => {
    try {
      const response = await apiCall('/documents/supported-types');
      if (response.types) {
        setSupportedFileTypes(response.types);
        // 更新全局文件图标映射
        if (window.updateFileIconMap) {
          window.updateFileIconMap(response.types);
        }
      }
    } catch (error) {
      console.error('Failed to load supported file types:', error);
    }
  }, []);
  
  /**
   * 加载文档列表 (Load documents list)
   * 
   * 根据当前搜索条件和分页参数从后端获取文档列表
   * Fetch documents list from backend based on current search criteria and pagination
   * 
   * @param {number} [page] - 页码（可选），如果不提供则使用当前页码 (Page number (optional), use current page if not provided)
   * @returns {Promise<void>}
   */
  const loadDocuments = async (page) => {
    setLoading(true);
    setError(null);
    
    try {
      // 使用传入的页码或当前页码
      const currentPage = page !== undefined ? page : pagination.currentPage;
      
      // 构建查询参数
      const filters = showAdvancedSearch ? advancedFilters : {
        search: simpleSearch,
        searchMode: 'contains',
        sortBy: 'date',
        sortOrder: 'desc'
      };
      
      const params = new URLSearchParams({
        page: currentPage.toString(),
        pageSize: pagination.pageSize.toString(),
        search: filters.search || '',
        searchMode: filters.searchMode || 'contains',
        sortBy: filters.sortBy || 'date',
        sortOrder: filters.sortOrder || 'desc',
        fileTypes: (filters.fileTypes || []).join(','),
        minSize: filters.minSize || '0',
        maxSize: filters.maxSize || '9223372036854775807',
        indexed: filters.indexed || 'all',
        startDate: filters.startDate || '',
        endDate: filters.endDate || '',
        lang: language || 'zh'
      });
      
      console.log('Fetching documents with params:', params.toString());
      const response = await apiCall(`/documents/list?${params.toString()}`);
      
      console.log('Document list response:', response);
      
      if (response) {
        const docs = response.documents || [];
        console.log('Documents count:', docs.length);
        setDocuments(docs);
        setPagination(prev => ({
          ...prev,
          currentPage: response.page || currentPage,
          total: response.total || 0,
          totalPages: response.totalPages || 0
        }));
      }
    } catch (error) {
      console.error('Failed to load documents:', error);
      setError(error.message);
    } finally {
      setLoading(false);
    }
  };
  
  // ============================================================================
  // Effects / 副作用
  // ============================================================================
  
  /**
   * 初始化 - 加载支持的文件类型 (Initialize - load supported file types)
   */
  useEffect(() => {
    loadSupportedFileTypes();
  }, []);
  
  /**
   * 初始化 - 加载文档列表 (Initialize - load documents list)
   */
  useEffect(() => {
    loadDocuments();
  }, []);
  
  // ============================================================================
  // Event Handlers / 事件处理函数
  // ============================================================================
  
  /**
   * 处理文件选择事件 (Handle file selection event)
   * 
   * 当用户选择文件后，执行批量上传操作
   * When user selects files, perform batch upload operation
   * 
   * @param {Event} e - 文件选择事件 (File selection event)
   * @returns {Promise<void>}
   */
  const handleFileSelect = async (e) => {
    const files = Array.from(e.target.files);
    if (files.length === 0) return;
    
    setUploading(true);
    setUploadProgress({
      current: 0,
      total: files.length,
      success: 0,
      failed: 0
    });
    
    try {
      const result = await batchUploadDocuments(files, (percent) => {
        console.log(`Upload progress: ${percent}%`);
      });
      
      setUploadProgress({
        current: files.length,
        total: files.length,
        success: result.successCount || files.length,
        failed: result.failCount || 0
      });
      
      // 刷新文档列表
      setTimeout(() => {
        loadDocuments();
        setUploadProgress(null);
      }, 3000);
      
    } catch (error) {
      console.error('Upload failed:', error);
      setUploadProgress({
        current: files.length,
        total: files.length,
        success: 0,
        failed: files.length
      });
    } finally {
      setUploading(false);
      e.target.value = ''; // 重置文件选择
    }
  };
  
  /**
   * 处理文档删除 (Handle document deletion)
   * 
   * 删除指定的文档，删除前会弹出确认对话框
   * Delete specified document, will show confirmation dialog before deletion
   * 
   * @param {string} fileName - 要删除的文件名 (File name to delete)
   * @returns {Promise<void>}
   */
  const handleDelete = async (fileName) => {
    if (!confirm(t('document.deleteConfirm') + ': ' + fileName)) {
      return;
    }
    
    try {
      await apiCall(`/documents/${encodeURIComponent(fileName)}`, {
        method: 'DELETE'
      });
      loadDocuments();
    } catch (error) {
      console.error('Failed to delete document:', error);
      alert(t('document.deleteFailed'));
    }
  };
  
  /**
   * 处理简单搜索 (Handle simple search)
   * 
   * 执行简单搜索，搜索时会重置到第一页
   * Execute simple search, reset to first page when searching
   */
  const handleSimpleSearch = () => {
    loadDocuments(1); // 搜索时回到第一页 (Reset to first page when searching)
  };
  
  /**
   * 处理高级搜索 (Handle advanced search)
   * 
   * 执行高级搜索，搜索时会重置到第一页
   * Execute advanced search, reset to first page when searching
   */
  const handleAdvancedSearch = () => {
    loadDocuments(1); // 搜索时回到第一页 (Reset to first page when searching)
  };
  
  /**
   * 重置所有筛选条件 (Reset all filter conditions)
   * 
   * 将高级搜索的所有筛选条件恢复到默认值
   * Reset all advanced search filter conditions to default values
   */
  const resetFilters = () => {
    setAdvancedFilters({
      search: '',
      searchMode: 'contains',
      fileTypes: [],
      minSize: '',
      maxSize: '',
      indexed: 'all',
      startDate: '',
      endDate: '',
      sortBy: 'date',
      sortOrder: 'desc'
    });
  };
  
  /**
   * 更新单个筛选项 (Update single filter option)
   * 
   * @param {string} key - 筛选项的键名 (Filter key name)
   * @param {any} value - 筛选项的新值 (New filter value)
   */
  const updateFilter = (key, value) => {
    setAdvancedFilters(prev => ({
      ...prev,
      [key]: value
    }));
  };
  
  /**
   * 切换文件类型选择状态 (Toggle file type selection state)
   * 
   * @param {string} type - 文件类型 (File type)
   * @param {boolean} checked - 是否选中 (Whether checked)
   */
  const toggleFileType = (type, checked) => {
    setAdvancedFilters(prev => ({
      ...prev,
      fileTypes: checked
        ? [...prev.fileTypes, type]
        : prev.fileTypes.filter(t => t !== type)
    }));
  };
  
  /**
   * 跳转到指定页 (Jump to specified page)
   * 
   * @param {number} page - 目标页码 (Target page number)
   */
  const goToPage = (page) => {
    if (page < 1 || page > pagination.totalPages) return;
    loadDocuments(page);
  };
  
  // ============================================================================
  // Helper Functions / 工具函数
  // ============================================================================
  
  /**
   * 格式化文件大小 (Format file size)
   * 
   * 将字节数转换为可读的文件大小格式 (B, KB, MB, GB)
   * Convert bytes to readable file size format (B, KB, MB, GB)
   * 
   * @param {number} bytes - 字节数 (Bytes)
   * @returns {string} 格式化后的文件大小 (Formatted file size)
   */
  const formatFileSize = (bytes) => {
    if (bytes === 0) return '0 B';
    const k = 1024;
    const sizes = ['B', 'KB', 'MB', 'GB'];
    const i = Math.floor(Math.log(bytes) / Math.log(k));
    return Math.round(bytes / Math.pow(k, i) * 100) / 100 + ' ' + sizes[i];
  };
  
  /**
   * 获取文件图标 (Get file icon)
   * 
   * 根据文件类型返回对应的 emoji 图标
   * Return corresponding emoji icon based on file type
   * 
   * @param {string} fileType - 文件类型 (File type)
   * @returns {string} 文件图标 emoji (File icon emoji)
   */
  const getFileIcon = (fileType) => {
    if (!fileType) return '📄';
    const type = fileType.toLowerCase();
    
    // 使用全局文件图标映射或默认图标
    if (window.fileIconMap && window.fileIconMap[type]) {
      return window.fileIconMap[type];
    }
    
    // 简单的文件类型图标映射
    const iconMap = {
      'xls': '📊',
      'xlsx': '📊',
      'doc': '📝',
      'docx': '📝',
      'pdf': '📕',
      'txt': '📃',
      'ppt': '📽️',
      'pptx': '📽️',
    };
    
    return iconMap[type] || '📄';
  };
  
  return (
    <div className="bubble-documents-shell document-management-container">
      {/* 上传区域 */}
      <div className="document-upload-section">
        <h3>{t('document.upload')}</h3>
        <input
          type="file"
          id="fileInput"
          className="document-upload-input"
          multiple
          onChange={handleFileSelect}
          disabled={uploading}
        />
        <label
          htmlFor="fileInput"
          className={`document-upload-label ${uploading ? 'disabled' : ''}`}
        >
          {uploading ? t('document.uploading') : t('document.selectFiles')}
        </label>
        
        {uploadProgress && (
          <div className="upload-progress-container">
            <div>
              {t('document.uploadProgress')}: {uploadProgress.current}/{uploadProgress.total}
            </div>
            <div className="upload-progress-text">
              {t('document.uploadSuccess')}: {uploadProgress.success} | 
              {t('document.uploadFailed')}: {uploadProgress.failed}
            </div>
          </div>
        )}
      </div>
      
      {/* 搜索区域 */}
      <div className="document-search-section">
        <div className="search-mode-toggle">
          <button
            onClick={() => setShowAdvancedSearch(!showAdvancedSearch)}
            className="search-toggle-btn"
          >
            {showAdvancedSearch ? t('document.simpleSearch') : t('document.advancedSearch')}
          </button>
        </div>
        
        {/* 简单搜索 */}
        {!showAdvancedSearch && (
          <div className="simple-search-container">
            <input
              type="text"
              value={simpleSearch}
              onChange={(e) => setSimpleSearch(e.target.value)}
              onKeyPress={(e) => e.key === 'Enter' && handleSimpleSearch()}
              placeholder={t('document.searchPlaceholder')}
              className="simple-search-input"
            />
            <button
              onClick={handleSimpleSearch}
              className="search-btn"
            >
              {t('document.search')}
            </button>
          </div>
        )}
        
        {/* 高级搜索 */}
        {showAdvancedSearch && (
          <div className="advanced-search-panel">
            {/* 搜索关键词 */}
            <div className="search-field-group">
              <label className="search-field-label">
                {t('document.keyword')}
              </label>
              <div className="search-field-row">
                <input
                  type="text"
                  value={advancedFilters.search}
                  onChange={(e) => updateFilter('search', e.target.value)}
                  placeholder={t('document.searchPlaceholder')}
                  className="search-input"
                />
                <select
                  value={advancedFilters.searchMode}
                  onChange={(e) => updateFilter('searchMode', e.target.value)}
                  className="search-select"
                >
                  <option value="contains">{t('document.searchMode.contains')}</option>
                  <option value="exact">{t('document.searchMode.exact')}</option>
                  <option value="regex">{t('document.searchMode.regex')}</option>
                </select>
              </div>
            </div>
            
            {/* 文件类型 */}
            <div className="search-field-group">
              <label className="search-field-label">
                {t('document.fileType')}
              </label>
              <div className="file-type-grid">
                {supportedFileTypes.map(type => (
                  <label key={type} className="file-type-checkbox">
                    <input
                      type="checkbox"
                      checked={advancedFilters.fileTypes.includes(type)}
                      onChange={(e) => toggleFileType(type, e.target.checked)}
                    />
                    <span>{getFileIcon(type)} {type.toUpperCase()}</span>
                  </label>
                ))}
              </div>
            </div>
            
            {/* 文件大小 */}
            <div className="search-field-group">
              <div className="search-field-row file-size-inputs">
                <label className="search-field-label">{t('document.fileSize')}</label>
                <input
                  type="number"
                  value={advancedFilters.minSize}
                  onChange={(e) => updateFilter('minSize', e.target.value)}
                  placeholder={t('document.minSize')}
                  className="search-input size-input"
                />
                <span>-</span>
                <input
                  type="number"
                  value={advancedFilters.maxSize}
                  onChange={(e) => updateFilter('maxSize', e.target.value)}
                  placeholder={t('document.maxSize')}
                  className="search-input size-input"
                />
                <span>KB</span>
              </div>
            </div>
            
            {/* 索引状态 */}
            <div className="search-field-group">
              <div className="search-field-row">
                <label className="search-field-label">{t('document.indexStatus')}</label>
                <select
                  value={advancedFilters.indexed}
                  onChange={(e) => updateFilter('indexed', e.target.value)}
                  className="search-select"
                >
                  <option value="all">{t('document.indexStatus.all')}</option>
                  <option value="true">{t('document.indexStatus.indexed')}</option>
                  <option value="false">{t('document.indexStatus.unindexed')}</option>
                </select>
              </div>
            </div>
            
            {/* 日期范围 */}
            <div className="search-field-group">
              <div className="search-field-row">
                <label className="search-field-label">{t('document.dateRange')}</label>
                <input
                  type="date"
                  value={advancedFilters.startDate}
                  onChange={(e) => updateFilter('startDate', e.target.value)}
                  className="search-input date-input"
                />
                <span>-</span>
                <input
                  type="date"
                  value={advancedFilters.endDate}
                  onChange={(e) => updateFilter('endDate', e.target.value)}
                  className="search-input date-input"
                />
              </div>
            </div>
            
            {/* 排序 */}
            <div className="search-field-group">
              <div className="search-field-row">
                <label className="search-field-label">{t('document.sortBy')}</label>
                <select
                  value={advancedFilters.sortBy}
                  onChange={(e) => updateFilter('sortBy', e.target.value)}
                  className="search-select"
                >
                  <option value="date">{t('document.sortBy.date')}</option>
                  <option value="name">{t('document.sortBy.name')}</option>
                  <option value="size">{t('document.sortBy.size')}</option>
                </select>
                <select
                  value={advancedFilters.sortOrder}
                  onChange={(e) => updateFilter('sortOrder', e.target.value)}
                  className="search-select"
                >
                  <option value="asc">{t('document.sortOrder.asc')}</option>
                  <option value="desc">{t('document.sortOrder.desc')}</option>
                </select>
              </div>
            </div>
            
            {/* 操作按钮 */}
            <div className="search-actions">
              <button onClick={handleAdvancedSearch} className="btn-apply">
                {t('document.applyFilters')}
              </button>
              <button onClick={resetFilters} className="btn-reset">
                {t('document.resetFilters')}
              </button>
            </div>
          </div>
        )}
      </div>
      
      {/* 文档列表头部 */}
      <div className="document-list-header">
        <h3 className="document-list-title">
          {t('document.list')} ({documents.length}/{pagination.total})
        </h3>
        <button
          onClick={loadDocuments}
          disabled={loading}
          className="btn-refresh"
        >
          {loading ? t('document.refreshing') : t('document.refresh')}
        </button>
      </div>
      
      {/* 调试信息（开发模式）*/}
      {process.env.NODE_ENV === 'development' && (
        <div className="debug-panel">
          <div>📊 调试信息 / Debug Info:</div>
          <div>• Loading: {loading ? '是' : '否'} / {loading ? 'Yes' : 'No'}</div>
          <div>• Error: {error || '无 / None'}</div>
          <div>• Documents Length: {documents.length}</div>
          <div>• Total: {pagination.total}</div>
          <div>• Current Page: {pagination.currentPage}</div>
          <div>• Total Pages: {pagination.totalPages}</div>
        </div>
      )}
      
      {/* 加载状态 */}
      {loading && (
        <div className="loading-container">
          <div className="loading-text">{t('document.loading')}</div>
        </div>
      )}
      
      {/* 错误状态 */}
      {error && (
        <div className="error-container">
          {t('document.loadFailed')}: {error}
        </div>
      )}
      
      {/* 文档列表 */}
      {!loading && !error && (
        <>
          {documents.length === 0 ? (
            <div className="empty-state">
              <div className="empty-icon">📄</div>
              <div className="empty-text">{t('document.noDocuments')}</div>
            </div>
          ) : (
            <div className="document-list-container">
              {documents.map((doc) => (
                <div key={doc.fileName + doc.uploadTime} className="document-card">
                  <div className="document-info">
                    <div className="document-name">
                      {getFileIcon(doc.fileType)} {doc.fileName}
                    </div>
                    <div className="document-meta">
                      📦 {formatFileSize(doc.fileSize)} |
                      📅 {doc.uploadTime} |
                      🏷️ {doc.fileType.toUpperCase()}
                      {doc.indexed && <span>| ✅ {t('document.indexed')}</span>}
                    </div>
                  </div>
                  <button
                    onClick={() => handleDelete(doc.fileName)}
                    className="btn-delete"
                  >
                    {t('document.delete')}
                  </button>
                </div>
              ))}
            </div>
          )}
          
          {/* 分页 */}
          {pagination.totalPages > 1 && (
            <div className="pagination-container">
              <button
                onClick={() => goToPage(pagination.currentPage - 1)}
                disabled={pagination.currentPage === 1}
                className="pagination-btn"
              >
                {t('document.prevPage')}
              </button>
              
              <span className="pagination-info">
                {t('document.page')} {pagination.currentPage} / {pagination.totalPages}
              </span>
              
              <button
                onClick={() => goToPage(pagination.currentPage + 1)}
                disabled={pagination.currentPage === pagination.totalPages}
                className="pagination-btn"
              >
                {t('document.nextPage')}
              </button>
            </div>
          )}
        </>
      )}
    </div>
  );
}

export default DocumentManagement;
