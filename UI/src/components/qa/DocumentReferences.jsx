/**
 * 文档引用组件 (Document References Component)
 *
 * 显示问答回答中引用的文档列表，支持添加到AI分析
 * (Displays document references in Q&A answers, supports adding to AI analysis)
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React from 'react'
import { Button, Tag, Tooltip, Space } from 'antd'
import {
  FileTextOutlined,
  PlusOutlined,
  CheckOutlined,
  DownloadOutlined,
} from '@ant-design/icons'
import { useQA } from '../../contexts/QAContext'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/qa/document-references.css'

/**
 * 文档引用组件
 */
function DocumentReferences({ sources, chunks }) {
  const { t } = useLanguage()
  const { addDocToAIAnalysis, isDocInAIAnalysis } = useQA()

  if (!sources || sources.length === 0) {
    return null
  }

  /**
   * 处理添加文档到AI分析
   */
  const handleAddToAIAnalysis = (docName, index) => {
    // 查找对应的chunk信息（如果有）
    const chunk = chunks && chunks[index]
    
    const doc = {
      name: docName,
      title: docName,
      fileName: docName,
      source: 'qa-reference',
      chunk: chunk ? {
        chunkId: chunk.chunkId,
        content: chunk.content,
      } : null,
    }

    addDocToAIAnalysis(doc)
  }

  /**
   * 处理下载文档
   */
  const handleDownload = (docName) => {
    // TODO: 实现文档下载功能
    console.log('📥 Download document:', docName)
  }

  return (
    <div className="document-references">
      <div className="document-references__header">
        <FileTextOutlined className="document-references__icon" />
        <span className="document-references__title">
          {t('qa.references.title')} ({sources.length})
        </span>
      </div>

      <div className="document-references__list">
        {sources.map((source, index) => {
          const isInAnalysis = isDocInAIAnalysis(source)
          
          return (
            <div key={index} className="document-references__item">
              <div className="document-references__item-info">
                <Tag
                  color="blue"
                  className="document-references__item-tag"
                >
                  {index + 1}
                </Tag>
                <Tooltip title={source}>
                  <span className="document-references__item-name">{source}</span>
                </Tooltip>
              </div>

              <Space className="document-references__item-actions">
                <Tooltip
                  title={
                    isInAnalysis
                      ? t('qa.references.alreadyInAnalysis')
                      : t('qa.references.addToAnalysis')
                  }
                >
                  <Button
                    type={isInAnalysis ? 'primary' : 'default'}
                    size="small"
                    icon={isInAnalysis ? <CheckOutlined /> : <PlusOutlined />}
                    onClick={() => !isInAnalysis && handleAddToAIAnalysis(source, index)}
                    disabled={isInAnalysis}
                    className={`document-references__add-btn ${isInAnalysis ? 'document-references__add-btn--added' : ''}`}
                  >
                    {isInAnalysis ? t('qa.references.alreadyInAnalysis') : t('qa.references.addToAnalysis')}
                  </Button>
                </Tooltip>

                <Tooltip title={t('qa.references.download')}>
                  <Button
                    type="text"
                    size="small"
                    icon={<DownloadOutlined />}
                    onClick={() => handleDownload(source)}
                  />
                </Tooltip>
              </Space>
            </div>
          )
        })}
      </div>

      <div className="document-references__footer">
        <Button
          size="small"
          type="link"
          onClick={() => {
            sources.forEach((source, index) => {
              if (!isDocInAIAnalysis(source)) {
                handleAddToAIAnalysis(source, index)
              }
            })
          }}
        >
          📚 {t('qa.references.addAllToAnalysis')}
        </Button>
      </div>
    </div>
  )
}

export default DocumentReferences
