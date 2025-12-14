/**
 * 梦幻气泡背景 / Dreamy Bubble Background
 * 
 * 动态粒子宇宙背景系统
 * Dynamic Particle Universe Background System
 */

import React, { useEffect, useRef } from 'react';

/**
 * 粒子类
 */
class Particle {
  constructor(canvas) {
    this.canvas = canvas;
    this.reset();
  }

  reset() {
    this.x = Math.random() * this.canvas.width;
    this.y = Math.random() * this.canvas.height;
    this.z = Math.random() * 1000;
    this.vx = (Math.random() - 0.5) * 0.5;
    this.vy = (Math.random() - 0.5) * 0.5;
    this.vz = Math.random() * 2 + 1;
    this.radius = Math.random() * 2 + 1;
    this.color = this.getRandomColor();
    this.life = 1;
    this.maxLife = Math.random() * 200 + 100;
  }

  getRandomColor() {
    const colors = [
      'rgba(102, 126, 234, 0.6)',
      'rgba(240, 147, 251, 0.6)',
      'rgba(79, 172, 254, 0.6)',
      'rgba(67, 233, 123, 0.6)',
      'rgba(250, 112, 154, 0.6)'
    ];
    return colors[Math.floor(Math.random() * colors.length)];
  }

  update(mouseX, mouseY) {
    // 移动
    this.x += this.vx;
    this.y += this.vy;
    this.z -= this.vz;

    // 鼠标引力
    const dx = mouseX - this.x;
    const dy = mouseY - this.y;
    const distance = Math.sqrt(dx * dx + dy * dy);
    
    if (distance < 150) {
      const force = (150 - distance) / 150;
      this.vx += dx * force * 0.001;
      this.vy += dy * force * 0.001;
    }

    // 边界检测
    if (this.x < 0 || this.x > this.canvas.width ||
        this.y < 0 || this.y > this.canvas.height ||
        this.z < 0) {
      this.reset();
    }

    // 生命周期
    this.life++;
    if (this.life > this.maxLife) {
      this.reset();
    }
  }

  draw(ctx) {
    const scale = 1000 / (1000 + this.z);
    const x = this.x;
    const y = this.y;
    const radius = this.radius * scale;
    const opacity = 1 - (this.z / 1000);

    // 发光效果
    const gradient = ctx.createRadialGradient(x, y, 0, x, y, radius * 3);
    gradient.addColorStop(0, this.color);
    gradient.addColorStop(1, 'rgba(0, 0, 0, 0)');

    ctx.fillStyle = gradient;
    ctx.beginPath();
    ctx.arc(x, y, radius * 3, 0, Math.PI * 2);
    ctx.fill();

    // 核心
    ctx.fillStyle = this.color.replace('0.6', String(opacity));
    ctx.beginPath();
    ctx.arc(x, y, radius, 0, Math.PI * 2);
    ctx.fill();
  }
}

/**
 * 梦幻气泡背景组件
 */
function BubbleBackground({ mousePosition }) {
  const canvasRef = useRef(null);
  const particlesRef = useRef([]);
  const animationRef = useRef(null);
  const mouseRef = useRef({ x: 0, y: 0 });

  useEffect(() => {
    const canvas = canvasRef.current;
    if (!canvas) return;

    const ctx = canvas.getContext('2d');
    
    // 设置画布大小
    const resizeCanvas = () => {
      canvas.width = window.innerWidth;
      canvas.height = window.innerHeight;
    };
    resizeCanvas();
    window.addEventListener('resize', resizeCanvas);

    // 初始化粒子
    particlesRef.current = Array.from({ length: 100 }, () => new Particle(canvas));

    // 动画循环
    const animate = () => {
      ctx.clearRect(0, 0, canvas.width, canvas.height);

      // 更新和绘制粒子
      particlesRef.current.forEach(particle => {
        particle.update(mouseRef.current.x, mouseRef.current.y);
        particle.draw(ctx);
      });

      // 绘制连接线
      drawConnections(ctx);

      animationRef.current = requestAnimationFrame(animate);
    };
    animate();

    return () => {
      window.removeEventListener('resize', resizeCanvas);
      if (animationRef.current) {
        cancelAnimationFrame(animationRef.current);
      }
    };
  }, []);

  // 更新鼠标位置
  useEffect(() => {
    if (canvasRef.current) {
      mouseRef.current = {
        x: (mousePosition.x + 1) * canvasRef.current.width / 2,
        y: (mousePosition.y + 1) * canvasRef.current.height / 2
      };
    }
  }, [mousePosition]);

  /**
   * 绘制粒子连接线
   */
  const drawConnections = (ctx) => {
    const particles = particlesRef.current;
    const maxDistance = 120;

    for (let i = 0; i < particles.length; i++) {
      for (let j = i + 1; j < particles.length; j++) {
        const p1 = particles[i];
        const p2 = particles[j];

        const dx = p1.x - p2.x;
        const dy = p1.y - p2.y;
        const distance = Math.sqrt(dx * dx + dy * dy);

        if (distance < maxDistance) {
          const opacity = (1 - distance / maxDistance) * 0.2;
          ctx.strokeStyle = `rgba(102, 126, 234, ${opacity})`;
          ctx.lineWidth = 1;
          ctx.beginPath();
          ctx.moveTo(p1.x, p1.y);
          ctx.lineTo(p2.x, p2.y);
          ctx.stroke();
        }
      }
    }
  };

  return (
    <div className="bubble-background">
      {/* 粒子画布 */}
      <canvas 
        ref={canvasRef} 
        className="background-particles-canvas"
      ></canvas>

      {/* 渐变背景层 */}
      <div className="background-gradient-layer">
        <div className="gradient-orb orb-1"></div>
        <div className="gradient-orb orb-2"></div>
        <div className="gradient-orb orb-3"></div>
        <div className="gradient-orb orb-4"></div>
      </div>

      {/* 网格层 */}
      <div className="background-grid-layer">
        <div className="grid-line grid-horizontal"></div>
        <div className="grid-line grid-vertical"></div>
      </div>

      {/* 光束效果 */}
      <div className="background-light-beams">
        <div className="light-beam beam-1"></div>
        <div className="light-beam beam-2"></div>
        <div className="light-beam beam-3"></div>
      </div>

      {/* 噪点纹理 */}
      <div className="background-noise-texture"></div>
    </div>
  );
}

export default BubbleBackground;
