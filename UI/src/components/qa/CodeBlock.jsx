/**
 * 代码块组件 (Code Block Component)
 *
 * 展示代码块，支持语法高亮和复制功能
 * (Displays code blocks with syntax highlighting and copy functionality)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { Button, Tooltip } from 'antd'
import { CopyOutlined, CheckOutlined } from '@ant-design/icons'
import '../../assets/css/qa/code-block.css'

function CodeBlock(props) {
  const { code, language = 'text' } = props
  const [copied, setCopied] = useState(false)

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(code)
      setCopied(true)
      setTimeout(() => setCopied(false), 2000)
    } catch (error) {
      console.error('Failed to copy code:', error)
    }
  }

  const getLanguageDisplayName = () => {
    const languageMap = {
      js: 'JavaScript',
      jsx: 'JavaScript (JSX)',
      ts: 'TypeScript',
      tsx: 'TypeScript (TSX)',
      java: 'Java',
      python: 'Python',
      py: 'Python',
      bash: 'Bash',
      shell: 'Shell',
      json: 'JSON',
      xml: 'XML',
      html: 'HTML',
      css: 'CSS',
      sql: 'SQL',
      yaml: 'YAML',
      yml: 'YAML',
      md: 'Markdown',
      text: 'Text',
    }
    return languageMap[language.toLowerCase()] || language.toUpperCase()
  }

  return (
    <div className="code-block">
      <div className="code-block__header">
        <span className="code-block__language">{getLanguageDisplayName()}</span>
        <Tooltip title={copied ? 'Copied!' : 'Copy code'}>
          <Button
            type="text"
            size="small"
            icon={copied ? <CheckOutlined /> : <CopyOutlined />}
            onClick={handleCopy}
            className="code-block__copy-btn"
          />
        </Tooltip>
      </div>

      <pre className="code-block__pre">
        <code className={`code-block__code language-${language}`}>{code}</code>
      </pre>
    </div>
  )
}

export default CodeBlock

