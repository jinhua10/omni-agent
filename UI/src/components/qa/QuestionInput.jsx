/**
 * 问题输入框组件 (Question Input Component)
 *
 * 支持多行输入、快捷键提交、字数统计、历史记录导航（方向键上下翻找）
 * (Supports multi-line input, keyboard shortcuts, character count, history navigation with arrow keys)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useRef, useCallback, useEffect } from 'react'
import { Button, Input } from 'antd'
import { SendOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/qa/question-input.css'

const { TextArea } = Input

// 历史记录存储键
const HISTORY_STORAGE_KEY = 'qa_question_history'
const MAX_HISTORY_SIZE = 50

function QuestionInput(props) {
  const { onSubmit, loading, placeholder } = props
  const { t } = useLanguage()
  const [value, setValue] = useState('')
  const [history, setHistory] = useState([])
  const [historyIndex, setHistoryIndex] = useState(-1)
  const [tempValue, setTempValue] = useState('') // 保存用户正在输入的内容
  const textAreaRef = useRef(null)

  // 从 localStorage 加载历史记录
  useEffect(() => {
    try {
      const saved = localStorage.getItem(HISTORY_STORAGE_KEY)
      if (saved) {
        const parsed = JSON.parse(saved)
        setHistory(Array.isArray(parsed) ? parsed : [])
      }
    } catch (error) {
      console.error('Failed to load question history:', error)
    }
  }, [])

  const handleChange = useCallback((e) => {
    setValue(e.target.value)
    // 当用户开始输入时，重置历史索引
    setHistoryIndex(-1)
  }, [])

  const handleSubmit = useCallback(() => {
    const trimmedValue = value.trim()
    if (trimmedValue && !loading) {
      onSubmit(trimmedValue)
      
      // 保存到历史记录
      setHistory(prev => {
        const newHistory = [trimmedValue, ...prev.filter(q => q !== trimmedValue)]
        const trimmedHistory = newHistory.slice(0, MAX_HISTORY_SIZE)
        
        // 保存到 localStorage
        try {
          localStorage.setItem(HISTORY_STORAGE_KEY, JSON.stringify(trimmedHistory))
        } catch (error) {
          console.error('Failed to save question history:', error)
        }
        
        return trimmedHistory
      })
      
      setValue('')
      setHistoryIndex(-1)
      setTempValue('')
      
      if (textAreaRef.current) {
        textAreaRef.current.focus()
      }
    }
  }, [value, loading, onSubmit])

  const handleKeyDown = useCallback((e) => {
    // Ctrl/Cmd + Enter: 提交
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
      e.preventDefault()
      handleSubmit()
      return
    }

    // 方向键上下翻找历史记录（仿 VSCode Copilot）
    if (e.key === 'ArrowUp' || e.key === 'ArrowDown') {
      // 只在光标在第一行（ArrowUp）或最后一行（ArrowDown）时触发历史导航
      const textarea = textAreaRef.current?.resizableTextArea?.textArea
      if (!textarea) return

      const { selectionStart, selectionEnd, value: currentValue } = textarea
      const lines = currentValue.split('\n')
      let currentLine = 0
      let charCount = 0

      // 计算光标所在行
      for (let i = 0; i < lines.length; i++) {
        charCount += lines[i].length + 1 // +1 for \n
        if (selectionStart < charCount) {
          currentLine = i
          break
        }
      }

      const isFirstLine = currentLine === 0
      const isLastLine = currentLine === lines.length - 1

      // ArrowUp: 只在第一行且光标在开始位置时触发
      if (e.key === 'ArrowUp' && isFirstLine && selectionStart === selectionEnd) {
        if (history.length === 0) return

        e.preventDefault()
        
        // 第一次按上键：保存当前输入
        if (historyIndex === -1) {
          setTempValue(value)
          setHistoryIndex(0)
          setValue(history[0])
        } 
        // 继续向上翻
        else if (historyIndex < history.length - 1) {
          setHistoryIndex(historyIndex + 1)
          setValue(history[historyIndex + 1])
        }
      }
      
      // ArrowDown: 只在最后一行时触发
      else if (e.key === 'ArrowDown' && isLastLine && selectionStart === selectionEnd) {
        if (historyIndex === -1) return

        e.preventDefault()
        
        // 向下翻
        if (historyIndex > 0) {
          setHistoryIndex(historyIndex - 1)
          setValue(history[historyIndex - 1])
        } 
        // 回到用户输入的内容
        else {
          setHistoryIndex(-1)
          setValue(tempValue)
        }
      }
    }
  }, [handleSubmit, history, historyIndex, value, tempValue])

  const getCharCountText = () => {
    const count = value.length
    if (count === 0) return ''
    return `${count} ${t('qa.input.characters')}`
  }

  return (
    <div className="question-input">
      <div className="question-input__container">
        <TextArea
          ref={textAreaRef}
          value={value}
          onChange={handleChange}
          onKeyDown={handleKeyDown}
          placeholder={placeholder || t('qa.input.placeholder')}
          autoSize={{ minRows: 2, maxRows: 6 }}
          disabled={loading}
          className="question-input__textarea"
        />

        <div className="question-input__footer">
          <div className="question-input__hints">
            <span className="question-input__hint">{t('qa.input.hint')}</span>
            {value.length > 0 && (
              <span className="question-input__char-count">{getCharCountText()}</span>
            )}
          </div>

          <Button
            type="primary"
            icon={<SendOutlined />}
            onClick={handleSubmit}
            loading={loading}
            disabled={!value.trim() || loading}
            className="question-input__submit-btn"
          >
            {t('qa.input.send')}
          </Button>
        </div>
      </div>
    </div>
  )
}

export default QuestionInput

