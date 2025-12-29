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
  ShareAltOutlined
} from '@ant-design/icons'
import './LandingPage.css'

const { Title, Paragraph, Text } = Typography

const LandingPage = ({ onEnterApp }) => {
  const [animatedStats, setAnimatedStats] = useState({
    modules: 0,
    codeLines: 0,
    formats: 0,
    strategies: 0
  })

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
            智能知识网络平台
          </Title>

          <Paragraph className="hero-description">
            基于知识域隔离的企业级RAG系统 | 让每个领域的知识独立闪耀
          </Paragraph>

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
              icon={<BookOutlined />}
              href="https://yumbo.blog.csdn.net/"
              target="_blank"
              className="btn-secondary"
            >
              技术博客
            </Button>
          </Space>

          {/* 统计数据 */}
          <Row gutter={32} className="stats-row">
            <Col xs={12} sm={6}>
              <Statistic
                title="Maven模块"
                value={animatedStats.modules}
                suffix="+"
                styles={{ value: { color: '#1890ff' } }}
              />
            </Col>
            <Col xs={12} sm={6}>
              <Statistic
                title="代码行数"
                value={animatedStats.codeLines}
                suffix="+"
                styles={{ value: { color: '#52c41a' } }}
              />
            </Col>
            <Col xs={12} sm={6}>
              <Statistic
                title="支持格式"
                value={animatedStats.formats}
                suffix="+ 类型"
                styles={{ value: { color: '#faad14' } }}
              />
            </Col>
            <Col xs={12} sm={6}>
              <Statistic
                title="分块策略"
                value={animatedStats.strategies}
                suffix="种"
                styles={{ value: { color: '#f5222d' } }}
              />
            </Col>
          </Row>
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
                <Title level={4} style={{ color: '#f5222d' }}>
                  ❌ 传统RAG的问题
                </Title>
                <div className="problem-diagram">
                  <div className="mixed-index">
                    <DatabaseOutlined style={{ fontSize: 48 }} />
                    <Text>单一RAG索引池</Text>
                  </div>
                  <Paragraph className="problem-list">
                    📄 技术文档 + 📊 财务报表 + 💼 合同 + 📧 邮件...
                  </Paragraph>
                  <div className="problem-results">
                    <Text type="danger">• 向量空间混乱</Text>
                    <Text type="danger">• 检索精度低下</Text>
                    <Text type="danger">• 无法专业化处理</Text>
                  </div>
                </div>
              </Card>
            </Col>

            <Col xs={24} md={2} className="vs-divider">
              <div className="vs-icon">VS</div>
            </Col>

            <Col xs={24} md={11}>
              <Card className="solution-card">
                <Title level={4} style={{ color: '#52c41a' }}>
                  ✅ OmniAgent的解决方案
                </Title>
                <div className="solution-diagram">
                  <Row gutter={16}>
                    <Col span={8}>
                      <div className="domain-box">
                        <FileTextOutlined />
                        <Text>技术文档域</Text>
                      </div>
                    </Col>
                    <Col span={8}>
                      <div className="domain-box">
                        <DatabaseOutlined />
                        <Text>财务报表域</Text>
                      </div>
                    </Col>
                    <Col span={8}>
                      <div className="domain-box">
                        <SafetyOutlined />
                        <Text>合同域</Text>
                      </div>
                    </Col>
                  </Row>
                  <div className="solution-results">
                    <Text type="success">• 语义检索精准</Text>
                    <Text type="success">• 专业知识提取</Text>
                    <Text type="success">• 智能路由优化</Text>
                  </div>
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

          <div className="workflow-diagram">
            <div className="workflow-step">
              <div className="step-icon">
                <CloudOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>文档上传</Title>
                <Text>Office+文本+代码</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <FileTextOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>智能提取</Title>
                <Text>全格式支持</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <CodeOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>智能分块</Title>
                <Text>策略自动选择</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <ThunderboltOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>向量化</Title>
                <Text>ONNX Embedding</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <DatabaseOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>域索引</Title>
                <Text>知识域隔离</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <BulbOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>语义检索</Title>
                <Text>AI增强搜索</Text>
              </div>
            </div>
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
                <Title level={4}>克隆项目</Title>
                <div className="code-block">
                  <code>
                    git clone https://github.com/jinhua10/omni-agent.git
                  </code>
                </div>
              </Card>
            </Col>

            <Col xs={24} md={8}>
              <Card className="quickstart-card">
                <div className="step-number">2</div>
                <Title level={4}>编译项目</Title>
                <div className="code-block">
                  <code>
                    mvn clean install -DskipTests
                  </code>
                </div>
              </Card>
            </Col>

            <Col xs={24} md={8}>
              <Card className="quickstart-card">
                <div className="step-number">3</div>
                <Title level={4}>运行示例</Title>
                <div className="code-block">
                  <code>
                    mvn spring-boot:run
                  </code>
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

