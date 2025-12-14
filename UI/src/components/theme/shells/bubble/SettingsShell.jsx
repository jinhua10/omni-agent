/**
 * 气泡主题 - 系统设置 Shell / Bubble Theme - Settings Shell
 */

import React from 'react';
import './bubble-common.css';

function SettingsShell() {
  return (
    <div className="bubble-settings-shell">
      <div className="settings-hero-section">
        <div className="hero-orb">
          <div className="orb-glow"></div>
          <div className="orb-content">
            <span className="hero-icon">⚙️</span>
            <h1 className="hero-title">系统设置</h1>
            <p className="hero-subtitle">个性化配置与管理</p>
          </div>
        </div>
      </div>

      <div className="settings-content-grid">
        <div className="settings-card bubble-glass-card">
          <div className="card-icon">🎨</div>
          <h3>主题设置</h3>
          <p>个性化界面风格</p>
        </div>
        
        <div className="settings-card bubble-glass-card">
          <div className="card-icon">👤</div>
          <h3>账户管理</h3>
          <p>个人信息与安全</p>
        </div>
        
        <div className="settings-card bubble-glass-card">
          <div className="card-icon">🔔</div>
          <h3>通知设置</h3>
          <p>消息提醒配置</p>
        </div>
        
        <div className="settings-card bubble-glass-card">
          <div className="card-icon">🌐</div>
          <h3>语言设置</h3>
          <p>多语言支持</p>
        </div>
      </div>
    </div>
  );
}

export default SettingsShell;
