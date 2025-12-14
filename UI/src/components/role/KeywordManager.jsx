/**
 * 关键词管理组件 (Keyword Manager Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { Tag, Input, Button, Space } from 'antd'
import { PlusOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/role/keyword-manager.css'

function KeywordManager(props) {
  const { keywords = [], onChange } = props
  const { t } = useLanguage()
  const [inputValue, setInputValue] = useState('')
  const [inputVisible, setInputVisible] = useState(false)

  const handleAdd = () => {
    const value = inputValue.trim()
    if (value && !keywords.includes(value)) {
      onChange([...keywords, value])
      setInputValue('')
      setInputVisible(false)
    }
  }

  const handleRemove = (keyword) => {
    onChange(keywords.filter(k => k !== keyword))
  }

  return (
    <div className="keyword-manager">
      <Space wrap className="keyword-manager__list">
        {keywords.map((keyword) => (
          <Tag
            key={keyword}
            closable
            onClose={() => handleRemove(keyword)}
            className="keyword-manager__tag"
          >
            {keyword}
          </Tag>
        ))}

        {inputVisible ? (
          <Input
            size="small"
            value={inputValue}
            onChange={(e) => setInputValue(e.target.value)}
            onBlur={handleAdd}
            onPressEnter={handleAdd}
            placeholder={t('role.keywordPlaceholder')}
            className="keyword-manager__input"
            autoFocus
          />
        ) : (
          <Button
            size="small"
            type="dashed"
            icon={<PlusOutlined />}
            onClick={() => setInputVisible(true)}
          >
            {t('role.addKeyword')}
          </Button>
        )}
      </Space>

      <p className="keyword-manager__hint">
        {t('role.keywordHint')}
      </p>
    </div>
  )
}

export default KeywordManager

