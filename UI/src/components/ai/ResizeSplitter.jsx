/**
 * 可调整大小的分隔线组件 (Resizable Splitter Component)
 *
 * 用于调整停靠面板和主内容区的比例
 * (For adjusting the ratio between docked panel and main content)
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React, { useState, useCallback, useEffect, useRef } from 'react'
import '../../assets/css/ai/resize-splitter.css'

function ResizeSplitter({ direction, onResize }) {
  const [dragging, setDragging] = useState(false)
  const rafRef = useRef(null)

  const handleMouseDown = useCallback((e) => {
    setDragging(true)
    e.preventDefault()
  }, [])

  const handleMouseMove = useCallback((e) => {
    if (!dragging) return

    if (rafRef.current) {
      cancelAnimationFrame(rafRef.current)
    }

    rafRef.current = requestAnimationFrame(() => {
      if (direction === 'horizontal') {
        // 左右分屏，调整宽度
        onResize(e.clientX)
      } else {
        // 上下分屏，调整高度
        onResize(e.clientY)
      }
    })
  }, [dragging, direction, onResize])

  const handleMouseUp = useCallback(() => {
    setDragging(false)
  }, [])

  useEffect(() => {
    if (dragging) {
      document.addEventListener('mousemove', handleMouseMove)
      document.addEventListener('mouseup', handleMouseUp)
      document.body.style.cursor = direction === 'horizontal' ? 'ew-resize' : 'ns-resize'
      document.body.style.userSelect = 'none'

      return () => {
        document.removeEventListener('mousemove', handleMouseMove)
        document.removeEventListener('mouseup', handleMouseUp)
        document.body.style.cursor = ''
        document.body.style.userSelect = ''
      }
    }
  }, [dragging, handleMouseMove, handleMouseUp, direction])

  const splitterClass = `resize-splitter resize-splitter--${direction} ${dragging ? 'resize-splitter--dragging' : ''}`

  return (
    <div
      className={splitterClass}
      onMouseDown={handleMouseDown}
    >
      <div className="resize-splitter__handle" />
    </div>
  )
}

export default ResizeSplitter
