/**
 * 流式答案组件 (Streaming Answer Component)
 *
 * 实现 VSCode Copilot 风格的流式 Markdown 渲染
 * (Implements VSCode Copilot-style streaming Markdown rendering)
 *
 * 性能优化：流式输出时使用纯文本，完成后再渲染Markdown
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React, { useState, useEffect } from 'react'
import MarkdownRenderer from './MarkdownRenderer'
import '../../assets/css/qa/streaming-answer.css'

const StreamingAnswer = React.memo(function StreamingAnswer(props) {
  const { content, streaming = true } = props
  const [showMarkdown, setShowMarkdown] = useState(!streaming)

  useEffect(() => {
    if (!streaming) {
      // 流式结束后，延迟100ms再渲染Markdown（给UI一个喘息时间）
      const timer = setTimeout(() => setShowMarkdown(true), 100)
      return () => clearTimeout(timer)
    } else {
      setShowMarkdown(false)
    }
  }, [streaming])

  if (streaming && !showMarkdown) {
    // ⚡ 性能优化：流式输出时使用纯文本显示（快速渲染）
    return (
      <div className="streaming-answer">
        <pre className="streaming-answer__plain-text" style={{
          whiteSpace: 'pre-wrap',
          wordWrap: 'break-word',
          fontFamily: '-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif',
          fontSize: '14px',
          lineHeight: '1.6',
          margin: 0,
          padding: 0
        }}>{content}</pre>
        <span className="streaming-answer__cursor">|</span>
      </div>
    )
  }

  return (
    <div className="streaming-answer">
      <MarkdownRenderer content={content} isStreaming={false} />
      {streaming && <span className="streaming-answer__cursor">|</span>}
    </div>
  )
}, (prevProps, nextProps) => {
  // 自定义比较函数：只在内容或streaming状态变化时才重渲染
  return prevProps.content === nextProps.content &&
         prevProps.streaming === nextProps.streaming
})

export default StreamingAnswer

