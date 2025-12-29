/**
 * OmniAgent 官方首页 - Landing Page
 * 类似CodeGeex的炫酷展示页面
 *
 * @author Jinhua Yu
 * @since 2025-12-29
 */

import React, { useState, useEffect } from 'react'
import { Button, Row, Col, Card, Typography, Space, Statistic } from 'antd'
import {
  RocketOutlined,
  ThunderboltOutlined,
  SafetyOutlined,
  ApiOutlined,
  CloudOutlined,
  DatabaseOutlined,
  BulbOutlined,
  GithubOutlined,
  MailOutlined,
  BookOutlined,
  ArrowRightOutlined,
  CheckCircleOutlined,
  CodeOutlined,
  FileTextOutlined,
  ShareAltOutlined,
  UserOutlined,
  LeftOutlined,
  RightOutlined
} from '@ant-design/icons'
import GiteeIcon from '../icons/GiteeIcon'
import './LandingPage.css'

const { Title, Paragraph, Text } = Typography


const LandingPage = ({ onEnterApp }) => {
  const [animatedStats, setAnimatedStats] = useState({
    modules: 0,
    codeLines: 0,
    formats: 0,
    strategies: 0
  })

  const [currentStatsPage, setCurrentStatsPage] = useState(0)

  // 扩展的统计数据 - 根据系统架构
  const statsPages = [
    // 第一页：核心架构
    [
      { title: '模块数量', value: 22, suffix: '个', icon: 'ApiOutlined', color: '#667eea' },
      { title: 'Java代码', value: 85000, suffix: '+行', icon: 'CodeOutlined', color: '#52c41a' },
      { title: '文档格式', value: 10, suffix: '类', icon: 'FileTextOutlined', color: '#faad14' },
      { title: '分块方式', value: 6, suffix: '种', icon: 'ThunderboltOutlined', color: '#f5222d' },
    ],
    // 第二页：RAG能力
    [
      { title: 'RAG方案', value: 6, suffix: '种', icon: 'DatabaseOutlined', color: '#1890ff' },
      { title: '向量模型', value: 3, suffix: '类', icon: 'ThunderboltOutlined', color: '#722ed1' },
      { title: '存储引擎', value: 6, suffix: '种', icon: 'SafetyOutlined', color: '#eb2f96' },
      { title: '多实例', value: 1, suffix: '支持', icon: 'CheckCircleOutlined', color: '#52c41a' },
    ],
    // 第三页：增强特性
    [
      { title: '模型接入', value: 3, suffix: '类', icon: 'BulbOutlined', color: '#faad14' },
      { title: '知识网络', value: 1, suffix: '启用', icon: 'ShareAltOutlined', color: '#13c2c2' },
      { title: 'HOPE', value: 1, suffix: '启用', icon: 'RocketOutlined', color: '#f5222d' },
      { title: 'P2P', value: 1, suffix: '启用', icon: 'CloudOutlined', color: '#1890ff' },
    ],
    // 第四页：技术栈
    [
      { title: 'Spring Boot', value: 3.4, suffix: '', icon: 'CloudOutlined', color: '#52c41a' },
      { title: 'Java', value: 21, suffix: '', icon: 'CodeOutlined', color: '#fa8c16' },
      { title: '编译状态', value: 100, suffix: '%', icon: 'CheckCircleOutlined', color: '#52c41a' },
      { title: '生产可用', value: 100, suffix: '%', icon: 'SafetyOutlined', color: '#1890ff' },
    ],
  ]

  // 数字动画效果
  useEffect(() => {
    const duration = 2000
    const steps = 60
    const interval = duration / steps

    const targets = {
      modules: 20,
      codeLines: 15000,
      formats: 10,  // Office文档(5种) + 所有文本格式
      strategies: 6
    }

    let step = 0
    const timer = setInterval(() => {
      step++
      const progress = step / steps

      setAnimatedStats({
        modules: Math.floor(targets.modules * progress),
        codeLines: Math.floor(targets.codeLines * progress),
        formats: Math.floor(targets.formats * progress),
        strategies: Math.floor(targets.strategies * progress)
      })

      if (step >= steps) {
        clearInterval(timer)
        setAnimatedStats(targets)
      }
    }, interval)

    return () => clearInterval(timer)
  }, [])

  // 自动轮播统计数据
  useEffect(() => {
    const autoScroll = setInterval(() => {
      setCurrentStatsPage((prev) => (prev + 1) % statsPages.length)
    }, 5000) // 每5秒切换一次

    return () => clearInterval(autoScroll)
  }, [statsPages.length])

  // 手动切换统计页
  const handleStatsPageChange = (index) => {
    setCurrentStatsPage(index)
  }

  return (
    <div className="landing-page">
      {/* Hero Section - 英雄区 */}
      <section className="hero-section">
        <div className="hero-background">
          <div className="floating-particles"></div>
        </div>

        <div className="hero-content">
          <div className="logo-container">
            <div className="logo-icon">
              <ApiOutlined style={{ fontSize: 64, color: '#1890ff' }} />
            </div>
            <Title level={1} className="hero-title">
              OmniAgent
            </Title>
          </div>

          <Title level={2} className="hero-subtitle">
            全场景企业级Agent框架
          </Title>

          <div className="hero-slogan">
            <Text className="hero-slogan-text">
              🚀 让Agent遍地开花，Agent元年正式开启！
            </Text>
          </div>

          <Paragraph className="hero-description">
            基于知识域隔离的智能Agent平台 | 构建分布式企业级AI应用
          </Paragraph>

          <div className="hero-features">
            <Row gutter={[16, 16]} justify="center">
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <RocketOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">智能Agent</Text>
                </div>
              </Col>
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <CloudOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">服务平台</Text>
                </div>
              </Col>
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <BulbOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">智能AI应用</Text>
                </div>
              </Col>
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <CodeOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">代码分析</Text>
                </div>
              </Col>
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <ThunderboltOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">自动化测试</Text>
                </div>
              </Col>
            </Row>
          </div>

          <Space size="large" className="hero-actions">
            <Button
              type="primary"
              size="large"
              icon={<RocketOutlined />}
              onClick={onEnterApp}
              className="btn-primary"
            >
              立即开始
            </Button>
            <Button
              size="large"
              icon={<GithubOutlined />}
              href="https://github.com/jinhua10/omni-agent"
              target="_blank"
              className="btn-secondary"
            >
              GitHub
            </Button>
            <Button
              size="large"
              icon={<GiteeIcon />}
              href="https://gitee.com/gnnu/omni-agent"
              target="_blank"
              className="btn-secondary"
              style={{ background: 'rgba(196, 26, 22, 0.15)' }}
            >
              Gitee
            </Button>
            <Button
              size="large"
              icon={<BookOutlined />}
              href="https://yumbo.blog.csdn.net/"
              target="_blank"
              className="btn-secondary"
            >
              技术博客
            </Button>
          </Space>

          {/* 统计数据 - 轮播展示 */}
          <div className="stats-carousel-container">
            <div className="stats-carousel-wrapper">
              <Button
                className="stats-nav-btn stats-nav-prev"
                icon={<LeftOutlined />}
                onClick={() => handleStatsPageChange((currentStatsPage - 1 + statsPages.length) % statsPages.length)}
                shape="circle"
              />

              <div className="stats-row-wrapper">
                <Row gutter={[24, 24]} className="stats-row">
                  {statsPages[currentStatsPage].map((stat, index) => {
                    const IconComponent = eval(stat.icon)
                    return (
                      <Col xs={12} sm={6} key={index}>
                        <div className="stat-item">
                          <div className="stat-icon" style={{ background: `linear-gradient(135deg, ${stat.color} 0%, ${stat.color}cc 100%)` }}>
                            <IconComponent style={{ fontSize: 24, color: '#fff' }} />
                          </div>
                          <Statistic
                            title={stat.title}
                            value={stat.value}
                            suffix={stat.suffix}
                            precision={stat.value < 10 ? 1 : 0}
                            styles={{
                              value: { color: '#fff', fontSize: 28, fontWeight: 700 },
                              title: { color: 'rgba(255, 255, 255, 0.85)', fontSize: 13, marginBottom: 4 }
                            }}
                          />
                        </div>
                      </Col>
                    )
                  })}
                </Row>
              </div>

              <Button
                className="stats-nav-btn stats-nav-next"
                icon={<RightOutlined />}
                onClick={() => handleStatsPageChange((currentStatsPage + 1) % statsPages.length)}
                shape="circle"
              />
            </div>

            {/* 指示器 */}
            <div className="stats-indicators">
              {statsPages.map((_, index) => (
                <div
                  key={index}
                  className={`stats-indicator ${index === currentStatsPage ? 'active' : ''}`}
                  onClick={() => handleStatsPageChange(index)}
                />
              ))}
            </div>

            {/* 说明文字 */}
            <div className="stats-description">
              <Text style={{ color: 'rgba(255, 255, 255, 0.7)', fontSize: 13 }}>
                {currentStatsPage === 0 && '模块化设计 · 企业级代码质量'}
                {currentStatsPage === 1 && '多策略RAG · 灵活存储 · 支持多实例'}
                {currentStatsPage === 2 && '知识网络 · 自学习 · P2P分布式'}
                {currentStatsPage === 3 && '最新技术栈 · 开箱即用'}
              </Text>
            </div>
          </div>
        </div>
      </section>

      {/* 核心理念对比 */}
      <section className="comparison-section">
        <div className="container">
          <Title level={2} className="section-title">
            为什么选择 OmniAgent？
          </Title>

          <Row gutter={48} align="middle">
            <Col xs={24} md={11}>
              <Card className="problem-card">
                <Title level={4} style={{ color: '#f5222d', marginBottom: 24 }}>
                  ❌ 传统RAG的根本性缺陷
                </Title>

                {/* 单一向量空间示意图 */}
                <div className="problem-diagram">
                  <Title level={5} style={{ textAlign: 'center', marginBottom: 16, color: '#f5222d' }}>
                    单一向量空间混乱
                  </Title>
                  <div className="mixed-index">
                    <DatabaseOutlined style={{ fontSize: 48, color: '#ff4d4f' }} />
                    <Text style={{ color: '#8c8c8c', fontSize: 13, marginTop: 8 }}>
                      📄 技术文档 + 📊 财务报表 + 💼 合同 + 📧 邮件...
                    </Text>
                  </div>
                </div>

                {/* 根本性缺陷 */}
                <div className="problem-points">
                  <Row gutter={[12, 12]}>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#ff4d4f' }}>
                          <CodeOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>语义割裂</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>固定分块破坏语义</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#ff7875' }}>
                          <ShareAltOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>上下文断裂</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>跨块信息无法关联</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#fa541c' }}>
                          <DatabaseOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>向量空间污染</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>多领域混杂降低精度</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#cf1322' }}>
                          <SafetyOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>单一存储</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>存在单点故障</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#ff9c6e' }}>
                          <ThunderboltOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>固定维度</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>无法适配不同场景</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#d4380d' }}>
                          <BulbOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>静态知识</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>缺乏持续学习</Text>
                        </div>
                      </div>
                    </Col>
                  </Row>
                </div>
              </Card>
            </Col>

            <Col xs={24} md={2} className="vs-divider">
              <div className="vs-icon">VS</div>
            </Col>

            <Col xs={24} md={11}>
              <Card className="solution-card">
                <Title level={4} style={{ color: '#52c41a', marginBottom: 24 }}>
                  ✅ OmniAgent的架构创新
                </Title>

                {/* 知识域隔离示意图 */}
                <div className="solution-diagram">
                  <Title level={5} style={{ textAlign: 'center', marginBottom: 16, color: '#52c41a' }}>
                    知识域隔离架构
                  </Title>
                  <Row gutter={12}>
                    <Col span={8}>
                      <div className="domain-box">
                        <FileTextOutlined style={{ fontSize: 24 }} />
                        <Text style={{ fontSize: 12 }}>技术文档域</Text>
                      </div>
                    </Col>
                    <Col span={8}>
                      <div className="domain-box">
                        <DatabaseOutlined style={{ fontSize: 24 }} />
                        <Text style={{ fontSize: 12 }}>财务报表域</Text>
                      </div>
                    </Col>
                    <Col span={8}>
                      <div className="domain-box">
                        <SafetyOutlined style={{ fontSize: 24 }} />
                        <Text style={{ fontSize: 12 }}>合同域</Text>
                      </div>
                    </Col>
                  </Row>
                </div>

                {/* 核心创新点 */}
                <div className="innovation-points">
                  <Row gutter={[12, 12]}>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#52c41a' }}>
                          <CodeOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>6种分块策略</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>智能保留语义</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#1890ff' }}>
                          <ShareAltOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>知识域隔离</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>独立向量空间</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#722ed1' }}>
                          <DatabaseOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>多元存储</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>灾备冗余方案</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#faad14' }}>
                          <ThunderboltOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>多维度向量</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>并行Embedding</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#13c2c2' }}>
                          <ApiOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>知识网络</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>跨域关联增强</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#f5222d' }}>
                          <RocketOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>HOPE架构</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>自我学习进化</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#eb2f96' }}>
                          <BulbOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>智能角色</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>AI生成提示词</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#fa8c16' }}>
                          <CloudOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>分布式架构</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>高可用集群</Text>
                        </div>
                      </div>
                    </Col>
                  </Row>
                </div>
              </Card>
            </Col>
          </Row>
        </div>
      </section>

      {/* 工作原理 - 流程图 */}
      <section className="workflow-section">
        <div className="container">
          <Title level={2} className="section-title">
            智能化全流程
          </Title>

          {/* 主流程 */}
          <div className="workflow-diagram">
            <div className="workflow-step enhanced">
              <div className="step-icon">
                <CloudOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>1. 文档上传</Title>
                <Text className="step-highlight">全格式支持</Text>
                <div className="step-details">
                  <Text>• Office: Word, Excel, PPT, PDF</Text>
                  <Text>• 文本: TXT, MD, JSON, XML, CSV...</Text>
                  <Text>• 代码: 所有编程语言</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <FileTextOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>2. 智能提取</Title>
                <Text className="step-highlight">多引擎可选</Text>
                <div className="step-details">
                  <Text>• 本地模型: 离线处理</Text>
                  <Text>• Ollama: 本地部署</Text>
                  <Text>• 在线API: 千问3-VL等</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <CodeOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>3. 智能分块</Title>
                <Text className="step-highlight">6种策略可选</Text>
                <div className="step-details">
                  <Text>• 固定长度/段落/句子</Text>
                  <Text>• 困惑度智能分块（推荐）</Text>
                  <Text>• 语义分块（TF-IDF）</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <ThunderboltOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>4. 向量化</Title>
                <Text className="step-highlight">多种方式并存</Text>
                <div className="step-details">
                  <Text>• ONNX本地模型: BGE系列</Text>
                  <Text>• Ollama: 本地向量化</Text>
                  <Text>• 在线API: OpenAI, 千帆...</Text>
                  <Text style={{ color: '#52c41a', fontWeight: 600 }}>✓ 支持多套RAG系统同时运行</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <DatabaseOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>5. 域索引构建</Title>
                <Text className="step-highlight">知识域隔离架构</Text>
                <div className="step-details">
                  <Text>• 按领域独立向量空间</Text>
                  <Text>• 避免跨域干扰</Text>
                  <Text>• 精准检索定位</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <BulbOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>6. 多次RAG检索</Title>
                <Text className="step-highlight">多策略并行</Text>
                <div className="step-details">
                  <Text>• 向量检索 + 关键词检索</Text>
                  <Text>• 结果融合与重排序</Text>
                  <Text>• 提高召回率和准确率</Text>
                </div>
              </div>
            </div>
          </div>

          {/* 域索引架构详解 */}
          <div className="domain-architecture-section" style={{ marginTop: 80 }}>
            <Title level={3} className="section-subtitle" style={{ marginBottom: 32, textAlign: 'center' }}>
              知识域隔离架构详解
            </Title>
            <Row gutter={48} align="middle">
              <Col xs={24} md={12}>
                <Card style={{ background: '#f9f9f9', border: '2px solid #e8e8e8' }}>
                  <Title level={4} style={{ color: '#8c8c8c', marginBottom: 16 }}>
                    ❌ 传统方式：单一索引池
                  </Title>
                  <div style={{ textAlign: 'center', padding: '20px 0' }}>
                    <DatabaseOutlined style={{ fontSize: 64, color: '#ff4d4f' }} />
                    <Paragraph style={{ marginTop: 16, color: '#8c8c8c' }}>
                      所有文档混在一个向量空间<br/>
                      技术、财务、法律文档相互干扰<br/>
                      检索精度低，噪音多
                    </Paragraph>
                  </div>
                </Card>
              </Col>
              <Col xs={24} md={12}>
                <Card style={{ background: 'linear-gradient(135deg, #f6ffed 0%, #d9f7be 100%)', border: '2px solid #52c41a' }}>
                  <Title level={4} style={{ color: '#52c41a', marginBottom: 16 }}>
                    ✅ OmniAgent：知识域隔离
                  </Title>
                  <Row gutter={[8, 8]} style={{ marginTop: 20 }}>
                    <Col span={8}>
                      <div style={{ textAlign: 'center', padding: '16px 8px', background: '#fff', borderRadius: 8, border: '1px solid #b7eb8f' }}>
                        <FileTextOutlined style={{ fontSize: 32, color: '#52c41a' }} />
                        <div style={{ marginTop: 8, fontSize: 12, fontWeight: 600 }}>技术文档域</div>
                        <div style={{ fontSize: 11, color: '#8c8c8c' }}>独立索引</div>
                      </div>
                    </Col>
                    <Col span={8}>
                      <div style={{ textAlign: 'center', padding: '16px 8px', background: '#fff', borderRadius: 8, border: '1px solid #b7eb8f' }}>
                        <DatabaseOutlined style={{ fontSize: 32, color: '#52c41a' }} />
                        <div style={{ marginTop: 8, fontSize: 12, fontWeight: 600 }}>财务数据域</div>
                        <div style={{ fontSize: 11, color: '#8c8c8c' }}>独立索引</div>
                      </div>
                    </Col>
                    <Col span={8}>
                      <div style={{ textAlign: 'center', padding: '16px 8px', background: '#fff', borderRadius: 8, border: '1px solid #b7eb8f' }}>
                        <SafetyOutlined style={{ fontSize: 32, color: '#52c41a' }} />
                        <div style={{ marginTop: 8, fontSize: 12, fontWeight: 600 }}>法律合同域</div>
                        <div style={{ fontSize: 11, color: '#8c8c8c' }}>独立索引</div>
                      </div>
                    </Col>
                  </Row>
                  <Paragraph style={{ marginTop: 20, color: '#389e0d', textAlign: 'center', fontWeight: 500 }}>
                    每个领域独立向量空间<br/>
                    互不干扰，精准检索<br/>
                    检索精度提升50%+
                  </Paragraph>
                </Card>
              </Col>
            </Row>
          </div>

          {/* 知识网络增强层 */}
          <Title level={3} className="section-subtitle" style={{ marginTop: 64, marginBottom: 32, textAlign: 'center' }}>
            知识网络增强层
          </Title>

          <div className="workflow-diagram">
            <div className="workflow-step">
              <div className="step-icon">
                <ApiOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>知识提取</Title>
                <Text>AI智能分析</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <ShareAltOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>跨域关联</Title>
                <Text>语义图谱构建</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <ThunderboltOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>HOPE学习</Title>
                <Text>自我优化进化</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <BulbOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>上下文补全</Title>
                <Text>智能问答增强</Text>
              </div>
            </div>
          </div>

          {/* P2P分布式架构 */}
          <div className="p2p-architecture-section" style={{ marginTop: 80 }}>
            <Title level={3} className="section-subtitle" style={{ marginBottom: 32, textAlign: 'center' }}>
              P2P分布式架构 - 让知识在网络中传递
            </Title>

            <Row gutter={[48, 32]}>
              <Col xs={24}>
                <Card className="p2p-card">
                  <Row gutter={48} align="middle">
                    <Col xs={24} md={12}>
                      <Title level={4} style={{ color: '#1890ff', marginBottom: 24 }}>
                        🌐 分布式知识网络
                      </Title>
                      <div className="p2p-features">
                        <div className="p2p-feature-item">
                          <div className="p2p-feature-icon" style={{ background: '#1890ff' }}>
                            <ShareAltOutlined />
                          </div>
                          <div>
                            <Text strong style={{ fontSize: 15, display: 'block', marginBottom: 4 }}>
                              连接码快速配对
                            </Text>
                            <Text type="secondary" style={{ fontSize: 13 }}>
                              输入连接码即可建立P2P连接，无需复杂配置
                            </Text>
                          </div>
                        </div>

                        <div className="p2p-feature-item">
                          <div className="p2p-feature-icon" style={{ background: '#52c41a' }}>
                            <CloudOutlined />
                          </div>
                          <div>
                            <Text strong style={{ fontSize: 15, display: 'block', marginBottom: 4 }}>
                              企业级知识共享
                            </Text>
                            <Text type="secondary" style={{ fontSize: 13 }}>
                              团队、部门、跨组织知识实时同步与共享
                            </Text>
                          </div>
                        </div>

                        <div className="p2p-feature-item">
                          <div className="p2p-feature-icon" style={{ background: '#722ed1' }}>
                            <SafetyOutlined />
                          </div>
                          <div>
                            <Text strong style={{ fontSize: 15, display: 'block', marginBottom: 4 }}>
                              分布式单体架构
                            </Text>
                            <Text type="secondary" style={{ fontSize: 13 }}>
                              每个节点独立可用，集群协同工作，高可用保障
                            </Text>
                          </div>
                        </div>

                        <div className="p2p-feature-item">
                          <div className="p2p-feature-icon" style={{ background: '#fa8c16' }}>
                            <ThunderboltOutlined />
                          </div>
                          <div>
                            <Text strong style={{ fontSize: 15, display: 'block', marginBottom: 4 }}>
                              知识自动传播
                            </Text>
                            <Text type="secondary" style={{ fontSize: 13 }}>
                              新知识自动推送到连接的节点，持续学习和进化
                            </Text>
                          </div>
                        </div>
                      </div>
                    </Col>

                    <Col xs={24} md={12}>
                      <div className="p2p-diagram">
                        <div className="p2p-network">
                          <div className="p2p-node central">
                            <DatabaseOutlined style={{ fontSize: 32, color: '#fff' }} />
                            <Text style={{ color: '#fff', fontSize: 12, marginTop: 8 }}>企业节点</Text>
                          </div>

                          <div className="p2p-connections">
                            <div className="p2p-node satellite" style={{ top: '0%', left: '50%' }}>
                              <CloudOutlined style={{ fontSize: 20, color: '#fff' }} />
                              <Text style={{ color: '#fff', fontSize: 10 }}>部门A</Text>
                            </div>
                            <div className="p2p-node satellite" style={{ top: '50%', left: '90%' }}>
                              <CloudOutlined style={{ fontSize: 20, color: '#fff' }} />
                              <Text style={{ color: '#fff', fontSize: 10 }}>部门B</Text>
                            </div>
                            <div className="p2p-node satellite" style={{ top: '100%', left: '50%' }}>
                              <CloudOutlined style={{ fontSize: 20, color: '#fff' }} />
                              <Text style={{ color: '#fff', fontSize: 10 }}>部门C</Text>
                            </div>
                            <div className="p2p-node satellite" style={{ top: '50%', left: '10%' }}>
                              <CloudOutlined style={{ fontSize: 20, color: '#fff' }} />
                              <Text style={{ color: '#fff', fontSize: 10 }}>远程团队</Text>
                            </div>
                          </div>
                        </div>

                        <div style={{ textAlign: 'center', marginTop: 24 }}>
                          <Text strong style={{ color: '#1890ff', fontSize: 14 }}>
                            🔗 通过连接码建立分布式知识网络
                          </Text>
                          <br />
                          <Text type="secondary" style={{ fontSize: 12 }}>
                            单体可用 + 分布式协同 = 全能Agent架构
                          </Text>
                        </div>
                      </div>
                    </Col>
                  </Row>
                </Card>
              </Col>
            </Row>
          </div>
        </div>
      </section>

      {/* 核心特性 */}
      <section className="features-section">
        <div className="container">
          <Title level={2} className="section-title">
            核心特性
          </Title>

          <Row gutter={[32, 32]}>
            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <ShareAltOutlined className="feature-icon" />
                <Title level={4}>知识域隔离</Title>
                <Paragraph>
                  每个知识域独立向量空间，避免知识混淆，提升检索精度50%+
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> 多域管理</li>
                  <li><CheckCircleOutlined /> 智能路由</li>
                  <li><CheckCircleOutlined /> 权限隔离</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <FileTextOutlined className="feature-icon" />
                <Title level={4}>全格式文档支持</Title>
                <Paragraph>
                  Office全家桶 + 所有文本格式，自动Markdown转换，完美适配LLM
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> <strong>Office</strong>: PDF, Word, Excel, PowerPoint</li>
                  <li><CheckCircleOutlined /> <strong>文本</strong>: TXT, MD, JSON, XML, CSV, LOG...</li>
                  <li><CheckCircleOutlined /> <strong>代码</strong>: Java, Python, JS, C++... 所有编程语言</li>
                  <li><CheckCircleOutlined /> 支持构建代码项目独立知识库</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <CodeOutlined className="feature-icon" />
                <Title level={4}>6种分块策略</Title>
                <Paragraph>
                  从简单到智能，根据文档类型自动选择最佳分块策略
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> 固定长度/段落/句子</li>
                  <li><CheckCircleOutlined /> PPL智能分块</li>
                  <li><CheckCircleOutlined /> 语义分块（TF-IDF）</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <ThunderboltOutlined className="feature-icon" />
                <Title level={4}>向量化检索</Title>
                <Paragraph>
                  集成ONNX Runtime，支持本地Embedding，无需调用在线API
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> BGE-base-zh-v1.5</li>
                  <li><CheckCircleOutlined /> 语义搜索</li>
                  <li><CheckCircleOutlined /> 优雅降级</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <ApiOutlined className="feature-icon" />
                <Title level={4}>智能角色与知识网络</Title>
                <Paragraph>
                  创建角色，通过角色描述智能构建提示词，自动生成专用知识库
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> AI智能提示词生成</li>
                  <li><CheckCircleOutlined /> 角色专用知识库</li>
                  <li><CheckCircleOutlined /> 知识图谱与跨域关联</li>
                  <li><CheckCircleOutlined /> 代码项目智能分析</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <SafetyOutlined className="feature-icon" />
                <Title level={4}>企业级架构</Title>
                <Paragraph>
                  Spring Boot标准，可插拔设计，生产就绪
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> 20+模块化设计</li>
                  <li><CheckCircleOutlined /> 完整异常处理</li>
                  <li><CheckCircleOutlined /> 编译100%通过</li>
                </ul>
              </Card>
            </Col>
          </Row>
        </div>
      </section>

      {/* 应用场景 */}
      <section className="use-cases-section">
        <div className="container">
          <Title level={2} className="section-title">
            全场景Agent应用
          </Title>

          <Row gutter={[32, 32]}>
            <Col xs={24} md={12}>
              <Card className="use-case-card" hoverable>
                <div className="use-case-header">
                  <RocketOutlined className="use-case-icon" style={{ color: '#1890ff' }} />
                  <Title level={3}>智能代码助手</Title>
                </div>
                <Paragraph className="use-case-desc">
                  类似 GitHub Copilot / Cursor 的智能编程助手
                </Paragraph>
                <ul className="use-case-list">
                  <li><CheckCircleOutlined /> <strong>完整项目上下文</strong>：分析整个代码仓库，提供精准建议</li>
                  <li><CheckCircleOutlined /> <strong>代码智能补全</strong>：基于项目风格和历史代码</li>
                  <li><CheckCircleOutlined /> <strong>Bug检测</strong>：自动发现潜在问题并给出修复方案</li>
                  <li><CheckCircleOutlined /> <strong>文档生成</strong>：自动生成API文档和注释</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} md={12}>
              <Card className="use-case-card" hoverable>
                <div className="use-case-header">
                  <ThunderboltOutlined className="use-case-icon" style={{ color: '#52c41a' }} />
                  <Title level={3}>自动化测试平台</Title>
                </div>
                <Paragraph className="use-case-desc">
                  智能测试用例生成与执行
                </Paragraph>
                <ul className="use-case-list">
                  <li><CheckCircleOutlined /> <strong>测试用例自动生成</strong>：基于代码逻辑生成测试</li>
                  <li><CheckCircleOutlined /> <strong>边界条件识别</strong>：AI发现边界和异常场景</li>
                  <li><CheckCircleOutlined /> <strong>回归测试</strong>：智能选择需要执行的测试</li>
                  <li><CheckCircleOutlined /> <strong>测试报告分析</strong>：自动生成详细测试报告</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} md={12}>
              <Card className="use-case-card" hoverable>
                <div className="use-case-header">
                  <FileTextOutlined className="use-case-icon" style={{ color: '#faad14' }} />
                  <Title level={3}>项目分析报告</Title>
                </div>
                <Paragraph className="use-case-desc">
                  深度项目洞察与优化建议
                </Paragraph>
                <ul className="use-case-list">
                  <li><CheckCircleOutlined /> <strong>架构分析</strong>：评估系统架构合理性</li>
                  <li><CheckCircleOutlined /> <strong>代码质量</strong>：检测代码smell和反模式</li>
                  <li><CheckCircleOutlined /> <strong>安全审计</strong>：发现安全漏洞和风险点</li>
                  <li><CheckCircleOutlined /> <strong>性能优化</strong>：识别性能瓶颈并给出建议</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} md={12}>
              <Card className="use-case-card" hoverable>
                <div className="use-case-header">
                  <CloudOutlined className="use-case-icon" style={{ color: '#722ed1' }} />
                  <Title level={3}>企业Agent服务平台</Title>
                </div>
                <Paragraph className="use-case-desc">
                  构建分布式Agent服务生态
                </Paragraph>
                <ul className="use-case-list">
                  <li><CheckCircleOutlined /> <strong>多租户隔离</strong>：企业级安全和权限管理</li>
                  <li><CheckCircleOutlined /> <strong>Agent市场</strong>：发布和订阅各类专业Agent</li>
                  <li><CheckCircleOutlined /> <strong>工作流编排</strong>：组合多个Agent完成复杂任务</li>
                  <li><CheckCircleOutlined /> <strong>API网关</strong>：统一接口，灵活集成</li>
                </ul>
              </Card>
            </Col>
          </Row>
        </div>
      </section>

      {/* 技术栈 */}
      <section className="tech-stack-section">
        <div className="container">
          <Title level={2} className="section-title">
            技术栈
          </Title>

          <Row gutter={[24, 24]} justify="center">
            <Col xs={12} sm={8} md={6} lg={4}>
              <div className="tech-item">
                <img src="https://img.shields.io/badge/Java-21-orange?style=for-the-badge&logo=openjdk" alt="Java" />
              </div>
            </Col>
            <Col xs={12} sm={8} md={6} lg={4}>
              <div className="tech-item">
                <img src="https://img.shields.io/badge/Spring_Boot-3.4.1-brightgreen?style=for-the-badge&logo=springboot" alt="Spring Boot" />
              </div>
            </Col>
            <Col xs={12} sm={8} md={6} lg={4}>
              <div className="tech-item">
                <img src="https://img.shields.io/badge/Apache_Lucene-9.10-blue?style=for-the-badge&logo=apache" alt="Lucene" />
              </div>
            </Col>
            <Col xs={12} sm={8} md={6} lg={4}>
              <div className="tech-item">
                <img src="https://img.shields.io/badge/ONNX_Runtime-latest-purple?style=for-the-badge&logo=onnx" alt="ONNX" />
              </div>
            </Col>
            <Col xs={12} sm={8} md={6} lg={4}>
              <div className="tech-item">
                <img src="https://img.shields.io/badge/React-18-61DAFB?style=for-the-badge&logo=react" alt="React" />
              </div>
            </Col>
            <Col xs={12} sm={8} md={6} lg={4}>
              <div className="tech-item">
                <img src="https://img.shields.io/badge/Ant_Design-5-0170FE?style=for-the-badge&logo=antdesign" alt="Ant Design" />
              </div>
            </Col>
          </Row>
        </div>
      </section>

      {/* 快速开始 */}
      <section className="quickstart-section">
        <div className="container">
          <Title level={2} className="section-title">
            三步启动
          </Title>

          <Row gutter={[32, 32]} justify="center">
            <Col xs={24} md={8}>
              <Card className="quickstart-card">
                <div className="step-number">1</div>
                <Title level={4}>克隆项目（二选一）</Title>
                <div className="code-block">
                  <pre>{`# GitHub
git clone https://github.com/jinhua10/omni-agent.git

# 或 Gitee（国内推荐）
git clone https://gitee.com/gnnu/omni-agent.git`}</pre>
                </div>
              </Card>
            </Col>

            <Col xs={24} md={8}>
              <Card className="quickstart-card">
                <div className="step-number">2</div>
                <Title level={4}>启动后端</Title>
                <div className="code-block">
                  <pre>{`mvn clean install -DskipTests

cd omni-agent-example-basic

mvn spring-boot:run`}</pre>
                </div>
              </Card>
            </Col>

            <Col xs={24} md={8}>
              <Card className="quickstart-card">
                <div className="step-number">3</div>
                <Title level={4}>启动前端</Title>
                <div className="code-block">
                  <pre>{`cd UI

npm install

npm run dev`}</pre>
                </div>
              </Card>
            </Col>
          </Row>

          <div className="quickstart-action">
            <Button
              type="primary"
              size="large"
              icon={<RocketOutlined />}
              onClick={onEnterApp}
            >
              体验在线Demo
            </Button>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer className="landing-footer">
        <div className="container">
          <Row gutter={48}>
            <Col xs={24} md={8}>
              <Title level={4}>关于 OmniAgent</Title>
              <Paragraph>
                基于知识域隔离的企业级智能知识管理平台，
                让每个领域的知识独立闪耀。
              </Paragraph>
            </Col>

            <Col xs={24} md={8}>
              <Title level={4}>快速链接</Title>
              <ul className="footer-links">
                <li><a href="https://github.com/jinhua10/omni-agent" target="_blank" rel="noopener noreferrer">
                  <GithubOutlined /> GitHub
                </a></li>
                <li><a href="https://gitee.com/gnnu" target="_blank" rel="noopener noreferrer">
                  <GithubOutlined /> Gitee
                </a></li>
                <li><a href="https://yumbo.blog.csdn.net/" target="_blank" rel="noopener noreferrer">
                  <BookOutlined /> 技术博客
                </a></li>
              </ul>
            </Col>

            <Col xs={24} md={8}>
              <Title level={4}>联系我们</Title>
              <ul className="footer-links">
                <li><MailOutlined /> 1015770492@qq.com</li>
                <li><GithubOutlined /> github.com/jinhua10</li>
              </ul>
            </Col>
          </Row>

          <div className="footer-bottom">
            <Text type="secondary">
              © 2025 OmniAgent. Apache License 2.0. Made with ❤️ by Jinhua Yu
            </Text>
          </div>
        </div>
      </footer>
    </div>
  )
}

export default LandingPage

