/**
 * 气泡主题 - 数据分析 Shell / Bubble Theme - Analytics Shell
 */

import React from 'react';
import './bubble-common.css';

function AnalyticsShell() {
  return (
    <div className="bubble-analytics-shell">
      <div className="analytics-hero-section">
        <div className="hero-orb">
          <div className="orb-glow"></div>
          <div className="orb-content">
            <span className="hero-icon">📊</span>
            <h1 className="hero-title">数据分析</h1>
            <p className="hero-subtitle">深度数据洞察与可视化</p>
          </div>
        </div>
      </div>

      <div className="analytics-content-grid">
        <div className="analytics-card bubble-glass-card">
          <div className="card-icon">📈</div>
          <h3>趋势分析</h3>
          <p>数据趋势实时监控</p>
        </div>
        
        <div className="analytics-card bubble-glass-card">
          <div className="card-icon">🎯</div>
          <h3>性能指标</h3>
          <p>关键指标跟踪</p>
        </div>
        
        <div className="analytics-card bubble-glass-card">
          <div className="card-icon">🔔</div>
          <h3>智能预警</h3>
          <p>异常数据自动提醒</p>
        </div>
        
        <div className="analytics-card bubble-glass-card">
          <div className="card-icon">📑</div>
          <h3>报表生成</h3>
          <p>一键生成分析报告</p>
        </div>
      </div>
    </div>
  );
}

export default AnalyticsShell;
