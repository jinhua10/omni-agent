/**
 * 文档搜索组件 (Document Search Component)
 * 
 * 支持：
 * - 输入时前端实时过滤
 * - 回车时调用后端API搜索
 * - 排序功能（按名称/大小/时间）
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useCallback } from 'react'
import { Input, Select, Space, Button } from 'antd'
import { SearchOutlined, SortAscendingOutlined, SortDescendingOutlined, FilterOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/document/document-search.css'

const { Search } = Input

function DocumentSearch(props) {
  const { onSearch, onFilterChange, sortBy, sortOrder, onSortChange, onAdvancedSearch } = props
  const { t } = useLanguage()
  const [value, setValue] = useState('')

  // 处理输入变化 - 前端实时过滤
  const handleInputChange = useCallback((e) => {
    const newValue = e.target.value
    setValue(newValue)
    if (onFilterChange) {
      onFilterChange(newValue)
    }
  }, [onFilterChange])

  // 处理回车搜索 - 调用后端API
  const handleSearch = useCallback((searchValue) => {
    if (onSearch) {
      onSearch(searchValue)
    }
  }, [onSearch])

  return (
    <div className="document-search">
      <Space size="middle" style={{ width: '100%', display: 'flex' }}>
        <Search
          placeholder={t('document.searchPlaceholder') || '输入搜索（实时过滤），回车调用API'}
          allowClear
          enterButton="API搜索"
          size="large"
          value={value}
          onChange={handleInputChange}
          onSearch={handleSearch}
          prefix={<SearchOutlined />}
          className="document-search__input"
          style={{ flex: 1 }}
        />
        
        <Button
          size="large"
          icon={<FilterOutlined />}
          onClick={onAdvancedSearch}
        >
          高级搜索
        </Button>
        
        <Select
          value={sortBy}
          onChange={onSortChange}
          size="large"
          style={{ width: 150 }}
          suffixIcon={sortOrder === 'asc' ? <SortAscendingOutlined /> : <SortDescendingOutlined />}
        >
          <Select.Option value="uploadTime">按时间排序</Select.Option>
          <Select.Option value="name">按名称排序</Select.Option>
          <Select.Option value="size">按大小排序</Select.Option>
        </Select>
      </Space>
    </div>
  )
}

export default DocumentSearch

