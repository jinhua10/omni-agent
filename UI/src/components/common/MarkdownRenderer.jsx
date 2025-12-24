/**
 * Markdown渲染器组件
 * (Markdown Renderer Component)
 *
 * 提供带代码高亮和复制功能的Markdown渲染
 * (Provides Markdown rendering with syntax highlighting and copy functionality)
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */

import React, { useState } from 'react'
import { Button, App } from 'antd'
import { CopyOutlined, CheckOutlined } from '@ant-design/icons'
import ReactMarkdown from 'react-markdown'
import remarkGfm from 'remark-gfm'
import { Prism as SyntaxHighlighter } from 'react-syntax-highlighter'
import { vscDarkPlus } from 'react-syntax-highlighter/dist/esm/styles/prism'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/common/MarkdownRenderer.css'

/**
 * 代码块组件（带复制按钮）
 */
const CodeBlock = ({ inline, className, children, ...props }) => {
  const { t } = useLanguage()
  const { message } = App.useApp()
  const [copied, setCopied] = useState(false)
  const match = /language-(\w+)/.exec(className || '')
  const language = match ? match[1] : ''
  const codeString = String(children).replace(/\n$/, '')

  const handleCopy = async () => {
    try {
      await navigator.clipboard.writeText(codeString)
      setCopied(true)
      message.success(t('common.copySuccess') || '复制成功')
      setTimeout(() => setCopied(false), 2000)
    } catch (err) {
      message.error(t('common.copyFailed') || '复制失败')
    }
  }

  // 内联代码
  if (inline) {
    return (
      <code className="inline-code" {...props}>
        {children}
      </code>
    )
  }

  // 代码块
  return (
    <div className="markdown-code-block-wrapper">
      <div className="markdown-code-block-header">
        <span className="code-language">{language || 'text'}</span>
        <Button
          type="text"
          size="small"
          icon={copied ? <CheckOutlined /> : <CopyOutlined />}
          onClick={handleCopy}
          className="code-copy-button"
        >
          {copied ? (t('common.copied') || '已复制') : (t('common.copyCode') || '复制代码')}
        </Button>
      </div>
      <SyntaxHighlighter
        language={language || 'text'}
        style={vscDarkPlus}
        customStyle={{
          margin: 0,
          borderRadius: '0 0 6px 6px',
          fontSize: '13px',
        }}
        showLineNumbers={codeString.split('\n').length > 5}
        wrapLines={true}
        {...props}
      >
        {codeString}
      </SyntaxHighlighter>
    </div>
  )
}

/**
 * Markdown渲染器组件
 * 
 * @param {Object} props
 * @param {string} props.content - Markdown内容
 * @param {string} props.className - 自定义类名
 * @param {Object} props.style - 自定义样式
 */
function MarkdownRenderer({ content, className = '', style = {} }) {
  return (
    <div className={`markdown-renderer ${className}`} style={style}>
      <ReactMarkdown
        remarkPlugins={[remarkGfm]}
        components={{
          code: CodeBlock,
          // 自定义其他元素样式
          h1: ({ node, ...props }) => <h1 className="markdown-h1" {...props} />,
          h2: ({ node, ...props }) => <h2 className="markdown-h2" {...props} />,
          h3: ({ node, ...props }) => <h3 className="markdown-h3" {...props} />,
          h4: ({ node, ...props }) => <h4 className="markdown-h4" {...props} />,
          h5: ({ node, ...props }) => <h5 className="markdown-h5" {...props} />,
          h6: ({ node, ...props }) => <h6 className="markdown-h6" {...props} />,
          p: ({ node, ...props }) => <p className="markdown-p" {...props} />,
          a: ({ node, ...props }) => <a className="markdown-link" target="_blank" rel="noopener noreferrer" {...props} />,
          ul: ({ node, ...props }) => <ul className="markdown-ul" {...props} />,
          ol: ({ node, ...props }) => <ol className="markdown-ol" {...props} />,
          li: ({ node, ...props }) => <li className="markdown-li" {...props} />,
          blockquote: ({ node, ...props }) => <blockquote className="markdown-blockquote" {...props} />,
          table: ({ node, ...props }) => (
            <div className="markdown-table-wrapper">
              <table className="markdown-table" {...props} />
            </div>
          ),
          th: ({ node, ...props }) => <th className="markdown-th" {...props} />,
          td: ({ node, ...props }) => <td className="markdown-td" {...props} />,
          img: ({ node, ...props }) => <img className="markdown-img" {...props} />,
          hr: ({ node, ...props }) => <hr className="markdown-hr" {...props} />,
        }}
      >
        {content || ''}
      </ReactMarkdown>
    </div>
  )
}

export default MarkdownRenderer
