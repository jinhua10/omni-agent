/**
 * 文档上传组件 (Document Upload Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { Modal } from 'antd'
import UploadDropZone from './UploadDropZone'
import { useLanguage } from '../../contexts/LanguageContext'
import { useMessage } from '../../hooks/useMessage'
import documentApi from '../../api/modules/document'

function DocumentUpload(props) {
  const { visible, onCancel, onSuccess } = props
  const { t } = useLanguage()
  const message = useMessage()
  const [uploading, setUploading] = useState(false)
  const [progress, setProgress] = useState(0)

  // 使用批量上传接口，支持单文件和多文件 / Use batch upload API, supports single and multiple files
  const handleUpload = async (fileList) => {
    // fileList 可能是单个 File 或 File 数组 / fileList can be a single File or File array
    const files = Array.isArray(fileList) ? fileList : [fileList];
    
    const formData = new FormData()
    // 批量上传使用 'files' 字段名 / Batch upload uses 'files' field name
    files.forEach(file => {
      formData.append('files', file)
    })
    // 添加语言参数 / Add language parameter
    formData.append('lang', localStorage.getItem('language') || 'zh')

    setUploading(true)
    setProgress(0)

    try {
      const result = await documentApi.batchUpload(formData, (percent) => {
        setProgress(percent)
      })

      // 显示上传结果 / Show upload result
      if (result.failureCount === 0) {
        message.success(t('document.uploadSuccess'))
      } else {
        const msg = t('document.batchUploadSuccess', {
          success: result.successCount,
          failed: result.failureCount
        }).replace('{success}', result.successCount)
          .replace('{failed}', result.failureCount)
        message.warning(msg)
      }

      if (onSuccess) {
        onSuccess()
      }
    } catch (error) {
      console.error('Upload failed:', error)
      message.error(t('document.uploadFailed') + ': ' + (error.message || error))
    } finally {
      setUploading(false)
      setProgress(0)
    }
  }

  return (
    <Modal
      title={t('document.upload')}
      open={visible}
      onCancel={onCancel}
      footer={null}
      width={600}
    >
      <UploadDropZone
        onUpload={handleUpload}
        uploading={uploading}
        progress={progress}
        multiple={true}
      />
    </Modal>
  )
}

export default DocumentUpload

