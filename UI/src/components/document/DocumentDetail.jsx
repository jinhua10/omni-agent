/**
 * 文档详情组件 (Document Detail Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Modal, Descriptions, Tag } from 'antd'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/document/document-detail.css'

function DocumentDetail(props) {
  const { visible, document, onClose } = props
  const { t } = useLanguage()

  if (!document) return null

  const formatFileSize = (bytes) => {
    if (bytes === 0) return '0 B'
    const k = 1024
    const sizes = ['B', 'KB', 'MB', 'GB']
    const i = Math.floor(Math.log(bytes) / Math.log(k))
    return Math.round(bytes / Math.pow(k, i) * 100) / 100 + ' ' + sizes[i]
  }

  return (
    <Modal
      title={t('document.detail')}
      open={visible}
      onCancel={onClose}
      footer={null}
      width={700}
      className="document-detail"
    >
      <Descriptions column={1} bordered>
        <Descriptions.Item label={t('document.name')}>
          {document.name}
        </Descriptions.Item>

        <Descriptions.Item label={t('document.size')}>
          {formatFileSize(document.size || 0)}
        </Descriptions.Item>

        <Descriptions.Item label={t('document.uploadTime')}>
          {new Date(document.uploadTime || document.createdAt).toLocaleString()}
        </Descriptions.Item>

        {document.category && (
          <Descriptions.Item label={t('document.category')}>
            {document.category}
          </Descriptions.Item>
        )}

        {document.tags && document.tags.length > 0 && (
          <Descriptions.Item label={t('document.tags')}>
            {document.tags.map((tag, index) => (
              <Tag key={index}>{tag}</Tag>
            ))}
          </Descriptions.Item>
        )}

        {document.description && (
          <Descriptions.Item label={t('document.description')}>
            {document.description}
          </Descriptions.Item>
        )}
      </Descriptions>
    </Modal>
  )
}

export default DocumentDetail

