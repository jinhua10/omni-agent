/**
 * 梦幻气泡导航 / Dreamy Bubble Navigation
 * 
 * 革命性的轨道式导航系统
 * Revolutionary Orbital Navigation System
 * 
 * 特性 / Features:
 * - 3D轨道布局 (3D Orbital Layout)
 * - 引力感应效果 (Gravity Sensing Effect)
 * - 流体动画过渡 (Fluid Animation Transitions)
 * - 沉浸式玻璃球体 (Immersive Glass Spheres)
 */

import React, { useState, useEffect } from 'react';
import { 
  HomeOutlined, 
  MessageOutlined, 
  FileTextOutlined, 
  TeamOutlined,
  BarChartOutlined,
  SettingOutlined,
  AppstoreOutlined
} from '@ant-design/icons';
import { useLanguage } from '../../../contexts/LanguageContext';

/**
 * 导航配置 (Navigation configuration)
 */
const getNavigationItems = (t) => [
  {
    key: 'qa',
    icon: MessageOutlined,
    label: t('nav.qa'),
    color: '#667eea',
    gradient: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)'
  },
  {
    key: 'home',
    icon: HomeOutlined,
    label: t('nav.home'),
    color: '#f093fb',
    gradient: 'linear-gradient(135deg, #f093fb 0%, #f5576c 100%)'
  },
  {
    key: 'documents',
    icon: FileTextOutlined,
    label: t('nav.documents'),
    color: '#4facfe',
    gradient: 'linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)'
  },
  {
    key: 'collaboration',
    icon: TeamOutlined,
    label: t('nav.collaboration'),
    color: '#43e97b',
    gradient: 'linear-gradient(135deg, #43e97b 0%, #38f9d7 100%)'
  },
  {
    key: 'analytics',
    icon: BarChartOutlined,
    label: t('nav.analytics'),
    color: '#fa709a',
    gradient: 'linear-gradient(135deg, #fa709a 0%, #fee140 100%)'
  },
  {
    key: 'settings',
    icon: SettingOutlined,
    label: t('nav.settings'),
    color: '#30cfd0',
    gradient: 'linear-gradient(135deg, #30cfd0 0%, #330867 100%)'
  }
];

/**
 * 梦幻气泡导航组件
 */
function BubbleNavigation({ activeKey, onMenuChange, isExpanded, onToggleExpand }) {
  const { t } = useLanguage();
  const [hoveredKey, setHoveredKey] = useState(null);
  const [rotationAngle, setRotationAngle] = useState(0);

  const navigationItems = getNavigationItems(t);

  // 自动旋转效果
  useEffect(() => {
    if (!isExpanded) return;

    const interval = setInterval(() => {
      setRotationAngle(prev => (prev + 0.5) % 360);
    }, 50);

    return () => clearInterval(interval);
  }, [isExpanded]);

  /**
   * 计算轨道位置
   */
  const getOrbitalPosition = (index, total) => {
    const angle = (index / total) * Math.PI * 2 + (rotationAngle * Math.PI / 180);
    const radius = isExpanded ? 160 : 80;
    
    return {
      x: Math.cos(angle) * radius,
      y: Math.sin(angle) * radius,
      z: Math.sin(angle * 2) * 20
    };
  };

  return (
    <nav className="bubble-navigation">
      {/* 中心控制球 */}
      <div 
        className={`nav-center-orb ${isExpanded ? 'expanded' : ''}`}
        onClick={onToggleExpand}
      >
        <div className="orb-glass-shell">
          <div className="orb-inner-glow"></div>
          <AppstoreOutlined className="center-icon" />
        </div>
        <div className="orb-rings">
          <div className="ring ring-1"></div>
          <div className="ring ring-2"></div>
          <div className="ring ring-3"></div>
        </div>
      </div>

      {/* 轨道导航项 */}
      <div className="nav-orbital-space">
        {navigationItems.map((item, index) => {
          const position = getOrbitalPosition(index, navigationItems.length);
          const isActive = activeKey === item.key;
          const isHovered = hoveredKey === item.key;

          return (
            <div
              key={item.key}
              className={`nav-orb-container ${isActive ? 'active' : ''} ${isHovered ? 'hovered' : ''}`}
              style={{
                transform: `translate3d(${position.x}px, ${position.y}px, ${position.z}px)`,
                opacity: isExpanded ? 1 : 0,
                pointerEvents: isExpanded ? 'auto' : 'none'
              }}
              onClick={() => onMenuChange(item.key)}
              onMouseEnter={() => setHoveredKey(item.key)}
              onMouseLeave={() => setHoveredKey(null)}
            >
              {/* 导航球体 */}
              <div 
                className="nav-orb"
                style={{
                  background: item.gradient,
                  '--orb-color': item.color
                }}
              >
                <div className="orb-glass-layer"></div>
                <div className="orb-reflection"></div>
                <item.icon className="orb-icon" />
                
                {/* 活动指示器 */}
                {isActive && (
                  <div className="active-indicator">
                    <div className="indicator-ring"></div>
                    <div className="indicator-pulse"></div>
                  </div>
                )}

                {/* 连接线 */}
                <div className="connection-line"></div>
              </div>

              {/* 标签 */}
              <div className="nav-label">
                <div className="label-glass"></div>
                <span className="label-text">{item.label}</span>
              </div>

              {/* 悬停光晕 */}
              {isHovered && (
                <div className="hover-halo" style={{ background: item.gradient }}></div>
              )}
            </div>
          );
        })}
      </div>

      {/* 轨道轨迹 */}
      {isExpanded && (
        <div className="orbital-tracks">
          <div className="track track-1"></div>
          <div className="track track-2"></div>
          <div className="track track-3"></div>
        </div>
      )}

      {/* 能量场 */}
      <div className="nav-energy-field">
        {[...Array(12)].map((_, i) => (
          <div 
            key={i} 
            className="energy-particle"
            style={{
              animationDelay: `${i * 0.2}s`,
              opacity: isExpanded ? 1 : 0
            }}
          ></div>
        ))}
      </div>
    </nav>
  );
}

export default BubbleNavigation;
