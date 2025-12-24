/**
 * 批次内容查看器组件
 * (Batch Content Viewer Component)
 *
 * 提供批次级别的内容展示，支持折叠/展开、合并等功能
 * (Provides batch-level content display with collapse/expand and merge features)
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */

import React, { useState, useEffect } from 'react'
import { Collapse, Space, Tag, Button, Tooltip, App } from 'antd'
import {
  LoadingOutlined,
  CheckCircleFilled,
  MergeCellsOutlined,
  ExpandOutlined,
  ShrinkOutlined,
} from '@ant-design/icons'
import MarkdownRenderer from './MarkdownRenderer'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/common/BatchContentViewer.css'

/**
 * 批次内容查看器
 * 
 * @param {Object} props
 * @param {Array} props.batches - 批次数据数组 [{index, number, content, status}]
 * @param {Function} props.onMerge - 合并批次回调
 * @param {boolean} props.autoExpand - 是否自动展开所有批次
 * @param {string} props.emptyText - 空内容提示文本
 * @param {boolean} props.showMergeButton - 是否显示合并按钮
 * @param {boolean} props.showExpandButton - 是否显示全部展开/收起按钮
 */
function BatchContentViewer({
  batches = [],
  onMerge,
  autoExpand = true,
  emptyText = '',
  showMergeButton = true,
  showExpandButton = true,
}) {
  const { t } = useLanguage()
  const { message } = App.useApp()
  const [expandedBatches, setExpandedBatches] = useState([])

  // 初始化展开状态
  useEffect(() => {
    if (autoExpand && batches.length > 0) {
      setExpandedBatches(batches.map(b => b.index))
    }
  }, [batches, autoExpand])

  // 检查是否所有批次都已完成
  const allCompleted = batches.length > 0 && batches.every(b => b.status === 'completed')

  // 合并批次
  const handleMerge = () => {
    if (onMerge) {
      onMerge(batches)
    }
  }

  // 全部展开/收起
  const handleToggleAll = () => {
    if (expandedBatches.length === batches.length) {
      setExpandedBatches([])
    } else {
      setExpandedBatches(batches.map(b => b.index))
    }
  }

  // 获取批次状态标签
  const getBatchStatusTag = (status) => {
    switch (status) {
      case 'pending':
        return <Tag color="default">{t('common.batch.waiting')}</Tag>
      case 'processing':
        return <Tag icon={<LoadingOutlined />} color="processing">{t('common.batch.processing')}</Tag>
      case 'completed':
        return <Tag icon={<CheckCircleFilled />} color="success">{t('common.batch.completed')}</Tag>
      default:
        return null
    }
  }

  if (batches.length === 0) {
    return (
      <div className="batch-content-viewer-empty">
        {emptyText || t('common.batch.noContent')}
      </div>
    )
  }

  return (
    <div className="batch-content-viewer">
      {/* 操作按钮栏 */}
      <div className="batch-viewer-toolbar">
        <Space>
          {allCompleted && showMergeButton && (
            <Button
              type="primary"
              icon={<MergeCellsOutlined />}
              onClick={handleMerge}
              size="small"
            >
              {t('common.batch.mergeBatches')}
            </Button>
          )}
          {showExpandButton && (
            <Tooltip title={expandedBatches.length === batches.length ? t('common.batch.collapseAll') : t('common.batch.expandAll')}>
              <Button
                icon={expandedBatches.length === batches.length ? <ShrinkOutlined /> : <ExpandOutlined />}
                onClick={handleToggleAll}
                size="small"
              >
                {expandedBatches.length === batches.length ? t('common.batch.collapseAll') : t('common.batch.expandAll')}
              </Button>
            </Tooltip>
          )}
          {/* 批次统计信息 */}
          <Tag color="blue">
            {t('common.batch.total')}: {batches.length}
          </Tag>
          <Tag color="green">
            {t('common.batch.completed')}: {batches.filter(b => b.status === 'completed').length}
          </Tag>
        </Space>
      </div>

      {/* 批次折叠面板 */}
      <Collapse
        className="batch-collapse-panel"
        activeKey={expandedBatches}
        onChange={(keys) => setExpandedBatches(keys)}
        items={batches.map(batch => ({
          key: batch.index,
          label: (
            <Space>
              <span>{t('common.batch.batch')} {batch.number}</span>
              {getBatchStatusTag(batch.status)}
              {batch.content && (
                <Tag color="cyan">{batch.content.length} {t('common.batch.characters')}</Tag>
              )}
            </Space>
          ),
          children: (
            <MarkdownRenderer
              content={batch.content || emptyText || t('common.batch.waiting')}
            />
          ),
        }))}
      />
    </div>
  )
}

export default BatchContentViewer
