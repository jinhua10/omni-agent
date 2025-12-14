/**
 * 停靠预览区域组件 (Dock Drop Zone Component)
 *
 * 显示拖拽时的停靠预览效果
 * (Shows dock preview effect when dragging)
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React from 'react'
import { DOCK_POSITIONS } from './FloatingAIPanel'
import '../../assets/css/ai/dock-drop-zone.css'

function DockDropZone({ previewDock }) {
  if (previewDock === DOCK_POSITIONS.NONE) {
    return null
  }

  const getZoneStyle = () => {
    const style = {}
    
    switch (previewDock) {
      case DOCK_POSITIONS.LEFT:
        style.left = 0
        style.top = 0
        style.width = '50%'
        style.height = '100%'
        break
      case DOCK_POSITIONS.RIGHT:
        style.right = 0
        style.top = 0
        style.width = '50%'
        style.height = '100%'
        break
      case DOCK_POSITIONS.TOP:
        style.left = 0
        style.top = 0
        style.width = '100%'
        style.height = '50%'
        break
      case DOCK_POSITIONS.BOTTOM:
        style.left = 0
        style.bottom = 0
        style.width = '100%'
        style.height = '50%'
        break
      default:
        return {}
    }
    
    return style
  }

  return (
    <div className="dock-drop-zone" style={getZoneStyle()}>
      <div className="dock-drop-zone__overlay">
        <div className="dock-drop-zone__icon">
          {previewDock === DOCK_POSITIONS.LEFT && '← 停靠到左侧'}
          {previewDock === DOCK_POSITIONS.RIGHT && '停靠到右侧 →'}
          {previewDock === DOCK_POSITIONS.TOP && '↑ 停靠到顶部'}
          {previewDock === DOCK_POSITIONS.BOTTOM && '↓ 停靠到底部'}
        </div>
      </div>
    </div>
  )
}

export default DockDropZone
