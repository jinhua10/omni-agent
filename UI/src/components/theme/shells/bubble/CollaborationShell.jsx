/**
 * 协作面板 - 梦幻气泡主题UI壳子 / Collaboration Panel - Dreamy Bubble Theme UI Shell
 *
 * 创新性的流体玻璃态UI设计，融合3D效果和粒子系统
 * Innovative fluid glassmorphism UI design with 3D effects and particle system
 *
 * @author AI Reviewer Team - World-Class Aesthetic Designer
 * @since 2025-12-12
 */

import React, { useEffect, useState, useRef } from 'react';
import { useLanguage } from '../../../../contexts/LanguageContext';
import { useCollaborationBinding } from '../../../../adapters/CollaborationAdapter';
import './bubble-collaboration.css';

/**
 * 梦幻气泡主题 - 协作面板UI壳子
 * Dreamy Bubble Theme - Collaboration Panel UI Shell
 *
 * 设计理念：
 * - 液态玻璃形态主义 (Liquid Glassmorphism)
 * - 3D深度空间感 (3D Depth Perception)
 * - 动态粒子系统 (Dynamic Particle System)
 * - 流体动画效果 (Fluid Animations)
 * - 渐变光影交互 (Gradient Light Interaction)
 */
function BubbleCollaborationShell() {
  const { t } = useLanguage();
  const { state, actions } = useCollaborationBinding();
  const canvasRef = useRef(null);
  const [mousePos, setMousePos] = useState({ x: 0, y: 0 });
  const [hoveredBubble, setHoveredBubble] = useState(null);

  useEffect(() => {
    if (state.activeTab === 'peers' && state.peers.length === 0) {
      actions.loadPeers?.();
    }
  }, [state.activeTab]); // eslint-disable-line

  // 粒子系统动画 / Particle system animation
  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;

    const ctx = canvas.getContext('2d');
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;

    const particles = [];
    const particleCount = 50;

    class Particle {
      constructor() {
        this.reset();
      }

      reset() {
        this.x = Math.random() * canvas.width;
        this.y = Math.random() * canvas.height;
        this.vx = (Math.random() - 0.5) * 0.5;
        this.vy = (Math.random() - 0.5) * 0.5;
        this.radius = Math.random() * 3 + 1;
        this.opacity = Math.random() * 0.5 + 0.2;
        this.hue = Math.random() * 60 + 200; // 蓝紫色调
      }

      update() {
        this.x += this.vx;
        this.y += this.vy;

        if (this.x < 0 || this.x > canvas.width) this.vx *= -1;
        if (this.y < 0 || this.y > canvas.height) this.vy *= -1;

        // 鼠标吸引效果
        const dx = mousePos.x - this.x;
        const dy = mousePos.y - this.y;
        const distance = Math.sqrt(dx * dx + dy * dy);
        if (distance < 150) {
          this.x += dx * 0.01;
          this.y += dy * 0.01;
        }
      }

      draw() {
        ctx.beginPath();
        ctx.arc(this.x, this.y, this.radius, 0, Math.PI * 2);
        ctx.fillStyle = `hsla(${this.hue}, 70%, 60%, ${this.opacity})`;
        ctx.fill();

        // 发光效果
        ctx.shadowBlur = 15;
        ctx.shadowColor = `hsla(${this.hue}, 70%, 60%, 0.8)`;
      }
    }

    // 初始化粒子
    for (let i = 0; i < particleCount; i++) {
      particles.push(new Particle());
    }

    let animationId;
    const animate = () => {
      ctx.clearRect(0, 0, canvas.width, canvas.height);

      // 绘制连线
      particles.forEach((p1, i) => {
        particles.slice(i + 1).forEach(p2 => {
          const dx = p1.x - p2.x;
          const dy = p1.y - p2.y;
          const distance = Math.sqrt(dx * dx + dy * dy);

          if (distance < 120) {
            ctx.beginPath();
            ctx.moveTo(p1.x, p1.y);
            ctx.lineTo(p2.x, p2.y);
            ctx.strokeStyle = `rgba(139, 92, 246, ${0.15 * (1 - distance / 120)})`;
            ctx.lineWidth = 0.5;
            ctx.stroke();
          }
        });
      });

      // 更新并绘制粒子
      particles.forEach(particle => {
        particle.update();
        particle.draw();
      });

      animationId = requestAnimationFrame(animate);
    };

    animate();

    const handleResize = () => {
      canvas.width = window.innerWidth;
      canvas.height = window.innerHeight;
    };

    window.addEventListener('resize', handleResize);

    return () => {
      cancelAnimationFrame(animationId);
      window.removeEventListener('resize', handleResize);
    };
  }, [mousePos]);

  // 跟踪鼠标位置
  useEffect(() => {
    const handleMouseMove = (e) => {
      setMousePos({ x: e.clientX, y: e.clientY });
    };
    window.addEventListener('mousemove', handleMouseMove);
    return () => window.removeEventListener('mousemove', handleMouseMove);
  }, []);

  // 梦幻导航球体配置 / Dreamy navigation orb configuration
  const dreamOrbs = [
    {
      key: 'peers',
      icon: '👥',
      label: t('collaboration.peers'),
      gradient: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
      shadowColor: 'rgba(102, 126, 234, 0.5)',
      glowColor: '#667eea'
    },
    {
      key: 'exchange',
      icon: '🔄',
      label: t('collaboration.exchange'),
      gradient: 'linear-gradient(135deg, #f093fb 0%, #f5576c 100%)',
      shadowColor: 'rgba(240, 147, 251, 0.5)',
      glowColor: '#f093fb'
    },
    {
      key: 'topology',
      icon: '🌐',
      label: t('collaboration.topology'),
      gradient: 'linear-gradient(135deg, #4facfe 0%, #00f2fe 100%)',
      shadowColor: 'rgba(79, 172, 254, 0.5)',
      glowColor: '#4facfe'
    },
    {
      key: 'sync',
      icon: '⚡',
      label: t('collaboration.sync'),
      gradient: 'linear-gradient(135deg, #43e97b 0%, #38f9d7 100%)',
      shadowColor: 'rgba(67, 233, 123, 0.5)',
      glowColor: '#43e97b'
    },
  ];

  // 渲染梦幻内容 / Render dreamy content
  const renderDreamContent = () => {
    const currentOrb = dreamOrbs.find(orb => orb.key === state.activeTab);
    
    switch (state.activeTab) {
      case 'peers':
        return (
          <div className="dream-content" data-tab="peers">
            <div className="dream-header">
              <div className="dream-title-wrapper">
                <span className="dream-icon" style={{ filter: `drop-shadow(0 0 20px ${currentOrb?.glowColor})` }}>
                  {currentOrb?.icon}
                </span>
                <h2 className="dream-title">{t('collaboration.peers')}</h2>
              </div>
              <div className="dream-stats">
                <div className="stat-orb">
                  <span className="stat-value">{state.peers.length}</span>
                  <span className="stat-label">在线节点</span>
                </div>
              </div>
            </div>
            
            {state.loading ? (
              <div className="dream-loading">
                <div className="loading-orbs">
                  <div className="loading-orb orb-1"></div>
                  <div className="loading-orb orb-2"></div>
                  <div className="loading-orb orb-3"></div>
                </div>
                <p>正在加载梦幻数据...</p>
              </div>
            ) : (
              <div className="dream-grid">
                {state.peers.map((peer, index) => (
                  <div 
                    key={peer.id} 
                    className="dream-card peer-card"
                    style={{ 
                      animationDelay: `${index * 0.1}s`,
                      background: currentOrb?.gradient
                    }}
                    onMouseEnter={() => setHoveredBubble(peer.id)}
                    onMouseLeave={() => setHoveredBubble(null)}
                  >
                    <div className="card-glass-overlay"></div>
                    <div className="card-content">
                      <div className="peer-avatar">
                        <div className="avatar-ring"></div>
                        <span className="avatar-emoji">🌟</span>
                      </div>
                      <div className="peer-info">
                        <h3 className="peer-name">{peer.name}</h3>
                        <div className="peer-status-badge">
                          <span className="status-dot"></span>
                          <span className="status-text">{peer.status}</span>
                        </div>
                      </div>
                    </div>
                    {hoveredBubble === peer.id && (
                      <div className="card-glow" style={{ boxShadow: `0 0 60px ${currentOrb?.shadowColor}` }}></div>
                    )}
                  </div>
                ))}
              </div>
            )}
          </div>
        );
        
      case 'exchange':
        return (
          <div className="dream-content" data-tab="exchange">
            <div className="dream-header">
              <div className="dream-title-wrapper">
                <span className="dream-icon" style={{ filter: `drop-shadow(0 0 20px ${currentOrb?.glowColor})` }}>
                  {currentOrb?.icon}
                </span>
                <h2 className="dream-title">{t('collaboration.exchange')}</h2>
              </div>
            </div>
            
            {state.loading ? (
              <div className="dream-loading">
                <div className="loading-orbs">
                  <div className="loading-orb orb-1"></div>
                  <div className="loading-orb orb-2"></div>
                  <div className="loading-orb orb-3"></div>
                </div>
              </div>
            ) : (
              <div className="dream-timeline">
                {state.exchanges.map((ex, index) => (
                  <div 
                    key={ex.id} 
                    className="timeline-item"
                    style={{ animationDelay: `${index * 0.15}s` }}
                  >
                    <div className="timeline-orb" style={{ background: currentOrb?.gradient }}></div>
                    <div className="timeline-card" style={{ borderImage: currentOrb?.gradient + ' 1' }}>
                      <div className="card-glass-overlay"></div>
                      <div className="exchange-content">
                        <div className="exchange-route">
                          <span className="exchange-from">{ex.from}</span>
                          <div className="exchange-arrow">
                            <div className="arrow-line"></div>
                            <span className="arrow-icon">→</span>
                          </div>
                          <span className="exchange-to">{ex.to}</span>
                        </div>
                        <span className="exchange-time">{ex.time}</span>
                      </div>
                    </div>
                  </div>
                ))}
              </div>
            )}
          </div>
        );
        
      case 'topology':
        return (
          <div className="dream-content" data-tab="topology">
            <div className="dream-header">
              <div className="dream-title-wrapper">
                <span className="dream-icon" style={{ filter: `drop-shadow(0 0 20px ${currentOrb?.glowColor})` }}>
                  {currentOrb?.icon}
                </span>
                <h2 className="dream-title">{t('collaboration.topology')}</h2>
              </div>
            </div>
            
            <div className="dream-topology">
              <div className="topology-3d-space">
                {state.topology.nodes?.map((node, index) => {
                  const angle = (index / (state.topology.nodes.length || 1)) * Math.PI * 2;
                  const radius = 200;
                  const x = Math.cos(angle) * radius;
                  const y = Math.sin(angle) * radius;
                  
                  return (
                    <div 
                      key={node.id} 
                      className="topology-node"
                      style={{ 
                        left: `calc(50% + ${x}px)`,
                        top: `calc(50% + ${y}px)`,
                        background: currentOrb?.gradient,
                        animationDelay: `${index * 0.2}s`
                      }}
                    >
                      <div className="node-core"></div>
                      <div className="node-ring ring-1"></div>
                      <div className="node-ring ring-2"></div>
                      <span className="node-label">{node.label}</span>
                    </div>
                  );
                })}
                <div className="topology-center" style={{ background: currentOrb?.gradient }}>
                  <span className="center-icon">⭐</span>
                </div>
              </div>
            </div>
          </div>
        );
        
      case 'sync':
        return (
          <div className="dream-content" data-tab="sync">
            <div className="dream-header">
              <div className="dream-title-wrapper">
                <span className="dream-icon" style={{ filter: `drop-shadow(0 0 20px ${currentOrb?.glowColor})` }}>
                  {currentOrb?.icon}
                </span>
                <h2 className="dream-title">{t('collaboration.sync')}</h2>
              </div>
            </div>
            
            <div className="dream-sync-panel">
              <div className="sync-orb-display">
                <div className="sync-orb-outer" style={{ background: currentOrb?.gradient }}>
                  <div className="sync-orb-middle">
                    <div className="sync-orb-inner">
                      <span className="sync-percentage">98%</span>
                    </div>
                  </div>
                </div>
                <div className="sync-waves">
                  <div className="wave wave-1"></div>
                  <div className="wave wave-2"></div>
                  <div className="wave wave-3"></div>
                </div>
              </div>
              
              <div className="sync-info-cards">
                <div className="sync-card" style={{ borderImage: currentOrb?.gradient + ' 1' }}>
                  <div className="card-glass-overlay"></div>
                  <div className="sync-card-content">
                    <span className="sync-card-label">{t('collaboration.shell.lastSyncLabel')}</span>
                    <span className="sync-card-value">{state.syncStatus.lastSync || t('collaboration.shell.lastSyncDefault')}</span>
                  </div>
                </div>
                <div className="sync-card" style={{ borderImage: currentOrb?.gradient + ' 1' }}>
                  <div className="card-glass-overlay"></div>
                  <div className="sync-card-content">
                    <span className="sync-card-label">{t('collaboration.shell.syncStatusLabel')}</span>
                    <span className="sync-card-value">{state.syncStatus.status || t('collaboration.shell.syncStatusDefault')}</span>
                  </div>
                </div>
              </div>
            </div>
          </div>
        );
        
      default:
        return null;
    }
  };

  return (
    <div className="dream-bubble-universe">
      {/* 粒子背景画布 / Particle background canvas */}
      <canvas ref={canvasRef} className="particle-canvas"></canvas>
      
      {/* 梦幻渐变背景层 / Dreamy gradient background */}
      <div className="dream-background">
        <div className="gradient-orb orb-1"></div>
        <div className="gradient-orb orb-2"></div>
        <div className="gradient-orb orb-3"></div>
      </div>

      {/* 3D浮动导航球体 / 3D Floating navigation orbs */}
      <div className="dream-nav-space">
        <div className="nav-orb-container">
          {dreamOrbs.map((orb, index) => (
            <div
              key={orb.key}
              className={`nav-orb ${state.activeTab === orb.key ? 'active' : ''}`}
              style={{
                background: orb.gradient,
                boxShadow: state.activeTab === orb.key 
                  ? `0 20px 60px ${orb.shadowColor}, 0 0 80px ${orb.shadowColor}, inset 0 0 30px rgba(255,255,255,0.3)`
                  : `0 10px 30px ${orb.shadowColor}`,
                animationDelay: `${index * 0.2}s`
              }}
              onClick={() => actions.switchTab(orb.key)}
              onMouseEnter={() => setHoveredBubble(orb.key)}
              onMouseLeave={() => setHoveredBubble(null)}
            >
              {/* 玻璃反光层 */}
              <div className="orb-glass-reflection"></div>
              
              {/* 内容 */}
              <div className="orb-content">
                <span className="orb-icon">{orb.icon}</span>
                <span className="orb-label">{orb.label}</span>
              </div>
              
              {/* 激活状态的光环 */}
              {state.activeTab === orb.key && (
                <>
                  <div className="orb-halo halo-1" style={{ borderColor: orb.glowColor }}></div>
                  <div className="orb-halo halo-2" style={{ borderColor: orb.glowColor }}></div>
                  <div className="orb-halo halo-3" style={{ borderColor: orb.glowColor }}></div>
                </>
              )}
              
              {/* 悬停时的能量波 */}
              {hoveredBubble === orb.key && (
                <div className="orb-energy-wave" style={{ borderColor: orb.glowColor }}></div>
              )}
            </div>
          ))}
        </div>
      </div>

      {/* 主内容玻璃容器 / Main content glass container */}
      <div className="dream-main-container">
        <div className="glass-panel">
          {/* 玻璃态射效果 */}
          <div className="glass-overlay"></div>
          <div className="glass-border"></div>
          
          {/* 内容区 */}
          <div className="dream-content-wrapper">
            {renderDreamContent()}
          </div>
        </div>
      </div>

      {/* 浮动装饰元素 / Floating decorative elements */}
      <div className="floating-decorations">
        <div className="float-orb float-1"></div>
        <div className="float-orb float-2"></div>
        <div className="float-orb float-3"></div>
        <div className="float-orb float-4"></div>
        <div className="float-ring ring-1"></div>
        <div className="float-ring ring-2"></div>
      </div>
    </div>
  );
}

export default BubbleCollaborationShell;
