/**
 * 完整文档管理组件 / Complete Document Management Component
 * 
 * 提供文档管理的完整功能，包括：
 * - FTP风格文件浏览器
 * - 传统列表视图
 * - 视图切换
 *
 * Provides complete document management features including:
 * - FTP-style file browser
 * - Traditional list view
 * - View switching
 *
 * @author OmniAgent Team
 * @since 2025-12-19
 */

import React from 'react';
import DocumentManagement from '../../../document/DocumentManagement';
import './bubble-common.css';

function DocumentManagementShell() {
  return (
    <div className="bubble-shell document-management-shell">
      <DocumentManagement />
    </div>
  );
}

export default DocumentManagementShell;
