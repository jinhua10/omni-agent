/**
 * 拖拽上传区域组件 (Upload Drop Zone Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useCallback } from 'react'
import { Upload, Progress } from 'antd'
import { InboxOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/document/upload-dropzone.css'

const { Dragger } = Upload

function UploadDropZone(props) {
  const { onUpload, uploading, progress, multiple = true } = props
  const { t } = useLanguage()

  // 支持批量上传：收集所有文件后一次性上传 / Support batch upload: collect all files then upload once
  const handleBeforeUpload = useCallback((file, fileList) => {
    const isValidSize = file.size / 1024 / 1024 < 100
    if (!isValidSize) {
      return false
    }

    // 当是最后一个文件时，触发上传 / When it's the last file, trigger upload
    if (fileList[fileList.length - 1] === file && onUpload) {
      onUpload(fileList)
    }
    return false
  }, [onUpload])

  return (
    <div className="upload-dropzone">
      <Dragger
        name="files"
        multiple={multiple}
        beforeUpload={handleBeforeUpload}
        showUploadList={false}
        disabled={uploading}
        className="upload-dropzone__dragger"
      >
        <p className="upload-dropzone__icon">
          <InboxOutlined />
        </p>
        <p className="upload-dropzone__text">{t('document.uploadTip')}</p>
        <p className="upload-dropzone__hint">{t('document.uploadHint')}</p>
      </Dragger>

      {uploading && (
        <div className="upload-dropzone__progress">
          <Progress percent={progress} status="active" />
        </div>
      )}
    </div>
  )
}

export default UploadDropZone

