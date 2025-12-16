/**
 * 批量上传示例组件 / Batch Upload Example Component
 * 
 * 展示如何使用批量上传功能
 * Demonstrates how to use batch upload functionality
 * 
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react';
import { useLanguage } from '../../contexts/LanguageContext';
import { useMessage } from '../../hooks/useMessage';
import { batchUploadDocuments } from '../../adapters/PageDataAdapter';
import UploadDropZone from './UploadDropZone';

function BatchUploadExample() {
  const { t } = useLanguage();
  const message = useMessage();
  const [uploading, setUploading] = useState(false);
  const [progress, setProgress] = useState(0);

  /**
   * 处理批量上传 / Handle batch upload
   * @param {File[]} fileList - 文件列表
   */
  const handleBatchUpload = async (fileList) => {
    if (!fileList || fileList.length === 0) {
      message.warning(t('document.uploadFirst'));
      return;
    }

    // 过滤有效文件（大小检查）/ Filter valid files (size check)
    const validFiles = fileList.filter(file => {
      const isValidSize = file.size / 1024 / 1024 < 100;
      if (!isValidSize) {
        message.error(`${file.name}: ${t('document.uploadLimit', { size: 100 })}`);
      }
      return isValidSize;
    });

    if (validFiles.length === 0) {
      return;
    }

    try {
      setUploading(true);
      setProgress(0);

      // 显示上传提示 / Show upload notification
      const uploadingMsg = t('document.batchUploading', { count: validFiles.length })
        .replace('{count}', validFiles.length);
      message.loading(uploadingMsg, 0);

      // 调用批量上传API / Call batch upload API
      const result = await batchUploadDocuments(validFiles, (percent) => {
        setProgress(percent);
      });

      message.destroy();

      // 处理上传结果 / Handle upload result
      if (result.successCount > 0 || result.failureCount > 0) {
        const successMsg = t('document.batchUploadSuccess', {
          success: result.successCount,
          failed: result.failureCount
        }).replace('{success}', result.successCount)
          .replace('{failed}', result.failureCount);

        if (result.failureCount === 0) {
          message.success(successMsg);
        } else {
          message.warning(successMsg);
        }

        // 显示失败的文件列表 / Show failed files list
        if (result.failedFiles && result.failedFiles.length > 0) {
          console.error('Failed files:', result.failedFiles);
        }
      } else {
        message.success(t('document.uploadSuccess'));
      }

    } catch (error) {
      console.error('Batch upload error:', error);
      message.destroy();
      message.error(t('document.uploadFailed') + ': ' + error.message);
    } finally {
      setUploading(false);
      setProgress(0);
    }
  };

  return (
    <div className="batch-upload-example">
      <UploadDropZone
        onUpload={handleBatchUpload}
        uploading={uploading}
        progress={progress}
        multiple={true}
      />
    </div>
  );
}

export default BatchUploadExample;
