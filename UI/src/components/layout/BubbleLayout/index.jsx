/**
 * 梦幻气泡布局 / Dreamy Bubble Layout
 * 
 * 完整的系统级主题 - 革命性的UI设计
 * Complete system-level theme - Revolutionary UI design
 * 
 * 设计理念 / Design Philosophy:
 * - 沉浸式3D空间 (Immersive 3D Space)
 * - 流体有机形态 (Fluid Organic Forms)
 * - 深度玻璃态射主义 (Deep Glassmorphism)
 * - 动态粒子宇宙 (Dynamic Particle Universe)
 * - 情感化交互设计 (Emotional Interaction Design)
 * 
 * @author World-Class Aesthetic Designer
 * @since 2025-12-12
 */

import React, { useState, useEffect, useRef } from 'react';
import BubbleNavigation from './BubbleNavigation';
import BubbleHeader from './BubbleHeader';
import BubbleBackground from './BubbleBackground';
import './bubble-layout.css';

/**
 * 梦幻气泡布局组件
 */
function BubbleLayout({ children, activeKey, onMenuChange }) {
  const [isMenuExpanded, setIsMenuExpanded] = useState(false);
  const [mousePosition, setMousePosition] = useState({ x: 0, y: 0 });
  const [scrollProgress, setScrollProgress] = useState(0);
  const contentRef = useRef(null);

  // 追踪鼠标位置用于视差效果
  useEffect(() => {
    const handleMouseMove = (e) => {
      setMousePosition({
        x: (e.clientX / window.innerWidth) * 2 - 1,
        y: (e.clientY / window.innerHeight) * 2 - 1
      });
    };

    window.addEventListener('mousemove', handleMouseMove);
    return () => window.removeEventListener('mousemove', handleMouseMove);
  }, []);

  // 追踪滚动进度
  useEffect(() => {
    const handleScroll = () => {
      if (contentRef.current) {
        const scrollTop = contentRef.current.scrollTop;
        const scrollHeight = contentRef.current.scrollHeight - contentRef.current.clientHeight;
        const progress = scrollHeight > 0 ? scrollTop / scrollHeight : 0;
        setScrollProgress(progress);
      }
    };

    const content = contentRef.current;
    if (content) {
      content.addEventListener('scroll', handleScroll);
      return () => content.removeEventListener('scroll', handleScroll);
    }
  }, []);

  return (
    <div 
      className="bubble-universe" 
      data-menu-expanded={isMenuExpanded}
      style={{
        '--mouse-x': mousePosition.x,
        '--mouse-y': mousePosition.y,
        '--scroll-progress': scrollProgress
      }}
    >
      {/* 动态宇宙背景 */}
      <BubbleBackground mousePosition={mousePosition} />

      {/* 浮动导航球 */}
      <BubbleNavigation 
        activeKey={activeKey}
        onMenuChange={onMenuChange}
        isExpanded={isMenuExpanded}
        onToggleExpand={() => setIsMenuExpanded(!isMenuExpanded)}
      />

      {/* 顶部头部栏 */}
      <BubbleHeader scrollProgress={scrollProgress} />

      {/* 主内容区域 */}
      <main className="bubble-main-stage" ref={contentRef}>
        <div className="bubble-content-container">
          {/* 内容玻璃面板 */}
          <div className="bubble-content-panel">
            <div className="panel-glass-effect"></div>
            <div className="panel-glow-border"></div>
            <div className="panel-content">
              {children}
            </div>
          </div>

          {/* 装饰性浮动元素 */}
          <div className="bubble-decorative-elements">
            <div className="deco-orb deco-1"></div>
            <div className="deco-orb deco-2"></div>
            <div className="deco-orb deco-3"></div>
            <div className="deco-ring ring-1"></div>
            <div className="deco-ring ring-2"></div>
          </div>
        </div>

        {/* 滚动进度指示器 */}
        <div className="bubble-scroll-indicator">
          <div 
            className="scroll-progress-bar" 
            style={{ transform: `scaleY(${scrollProgress})` }}
          ></div>
        </div>
      </main>

      {/* 环境光效 */}
      <div className="bubble-ambient-lights">
        <div className="ambient-light light-1"></div>
        <div className="ambient-light light-2"></div>
        <div className="ambient-light light-3"></div>
      </div>

      {/* 粒子层 */}
      <div className="bubble-particle-layer">
        <canvas id="bubble-particles-canvas"></canvas>
      </div>

      {/* 交互式光标效果 */}
      <div 
        className="bubble-cursor-glow"
        style={{
          left: `${(mousePosition.x + 1) * 50}%`,
          top: `${(mousePosition.y + 1) * 50}%`
        }}
      ></div>
    </div>
  );
}

export default BubbleLayout;
