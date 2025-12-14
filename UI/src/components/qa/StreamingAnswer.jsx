/**
 * 流式答案组件 (Streaming Answer Component)
 *
 * 实现 VSCode Copilot 风格的流式 Markdown 渲染
 * (Implements VSCode Copilot-style streaming Markdown rendering)
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React from 'react'
import MarkdownRenderer from './MarkdownRenderer'
import '../../assets/css/qa/streaming-answer.css'

function StreamingAnswer(props) {
  const { content, streaming = true } = props

  return (
    <div className="streaming-answer">
      <MarkdownRenderer content={content} isStreaming={streaming} />
      {streaming && <span className="streaming-answer__cursor">|</span>}
    </div>
  )
}

export default StreamingAnswer

