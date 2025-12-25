/**
 * 文档详情组件 (Document Detail Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect } from 'react'
import { Modal, Descriptions, Tag, Spin, Tabs, Empty, Alert } from 'antd'
import { FileTextOutlined, InfoCircleOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import MarkdownRenderer from '../common/MarkdownRenderer'
import '../../assets/css/document/document-detail.css'

const { TabPane } = Tabs

function DocumentDetail(props) {
  const { visible, document, onClose } = props
  const { t } = useLanguage()

  const [extractedText, setExtractedText] = useState(null)
  const [loading, setLoading] = useState(false)
  const [error, setError] = useState(null)

  // 当对话框打开且有文档时，获取提取的文本内容
  useEffect(() => {
    if (visible && document && document.fileName) {
      fetchExtractedText()
    } else {
      // 对话框关闭时清空数据
      setExtractedText(null)
      setError(null)
    }
  }, [visible, document])

  const fetchExtractedText = async () => {
    setLoading(true)
    setError(null)

    try {
      const encodedDocId = encodeURIComponent(document.fileName)
      const response = await fetch(`/api/documents/processing/${encodedDocId}/extraction-status`)
      const result = await response.json()

      if (result.success && result.data.extracted) {
        setExtractedText(result.data.content)
      } else {
        setExtractedText(null)
      }
    } catch (err) {
      console.error('获取提取文本失败:', err)
      setError(err.message)
    } finally {
      setLoading(false)
    }
  }

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
      width={900}
      className="document-detail"
      style={{ top: 20 }}
    >
      <Tabs defaultActiveKey="info">
        <TabPane
          tab={
            <span>
              <InfoCircleOutlined />
              {t('document.basicInfo') || '基本信息'}
            </span>
          }
          key="info"
        >
          <Descriptions column={1} bordered>
            <Descriptions.Item label={t('document.name')}>
              {document.fileName || document.name}
            </Descriptions.Item>

            <Descriptions.Item label={t('document.size')}>
              {formatFileSize(document.fileSize || document.size || 0)}
            </Descriptions.Item>

            <Descriptions.Item label={t('document.uploadTime')}>
              {new Date(document.uploadTime || document.createdAt || Date.now()).toLocaleString()}
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
        </TabPane>

        <TabPane
          tab={
            <span>
              <FileTextOutlined />
              {t('document.extractedText') || '提取文本'}
            </span>
          }
          key="extracted"
        >
          {loading ? (
            <div style={{ textAlign: 'center', padding: '40px' }}>
              <Spin tip={t('document.loadingExtractedText') || '正在加载提取文本...'} />
            </div>
          ) : error ? (
            <Alert
              message={t('document.loadError') || '加载失败'}
              description={error}
              type="error"
              showIcon
            />
          ) : extractedText ? (
            <div style={{
              maxHeight: '60vh',
              overflowY: 'auto',
              padding: '16px',
              backgroundColor: '#fafafa',
              borderRadius: '4px'
            }}>
              <MarkdownRenderer content={extractedText} />
            </div>
          ) : (
            <Empty
              description={t('document.noExtractedText') || '该文档尚未提取文本内容'}
              style={{ padding: '40px' }}
            />
          )}
        </TabPane>
      </Tabs>
    </Modal>
  )
}

export default DocumentDetail

