/**
 * 气泡主题 - 首页 Shell / Bubble Theme - Home Shell
 */

import React from 'react';
import './bubble-common.css';

function HomeShell() {
  return (
    <div className="bubble-home-shell">
      <div className="home-hero-section">
        <div className="hero-orb-large">
          <div className="orb-glow"></div>
          <div className="orb-content">
            <span className="hero-icon">🏠</span>
            <h1 className="hero-title">AI智能审查系统</h1>
            <p className="hero-subtitle">欢迎来到梦幻气泡主题</p>
          </div>
        </div>
      </div>

      <div className="home-features-grid">
        <div className="feature-card bubble-glass-card bubble-hover-lift">
          <div className="card-icon">💬</div>
          <h3>智能问答</h3>
          <p>AI驱动的智能对话系统</p>
        </div>
        
        <div className="feature-card bubble-glass-card bubble-hover-lift">
          <div className="card-icon">📄</div>
          <h3>文档管理</h3>
          <p>高效的文档处理平台</p>
        </div>
        
        <div className="feature-card bubble-glass-card bubble-hover-lift">
          <div className="card-icon">👥</div>
          <h3>协作空间</h3>
          <p>团队协作新体验</p>
        </div>
        
        <div className="feature-card bubble-glass-card bubble-hover-lift">
          <div className="card-icon">📊</div>
          <h3>数据分析</h3>
          <p>深度数据洞察</p>
        </div>
      </div>
    </div>
  );
}

export default HomeShell;
