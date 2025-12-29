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
            全场景企业级Agent框架
          </Title>

          <Paragraph className="hero-description">
            基于知识域隔离的智能Agent平台 | 构建分布式企业级AI应用
          </Paragraph>

          <div className="hero-features">
            <Row gutter={[24, 16]} justify="center">
              <Col xs={24} sm={12} md={6}>
                <div className="hero-feature-item">
                  <RocketOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">智能Agent构建</Text>
                </div>
              </Col>
              <Col xs={24} sm={12} md={6}>
                <div className="hero-feature-item">
                  <CloudOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">Agent服务平台</Text>
                </div>
              </Col>
              <Col xs={24} sm={12} md={6}>
                <div className="hero-feature-item">
                  <CodeOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">代码智能分析</Text>
                </div>
              </Col>
              <Col xs={24} sm={12} md={6}>
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
              icon={<BookOutlined />}
              href="https://yumbo.blog.csdn.net/"
              target="_blank"
              className="btn-secondary"
            >
              技术博客
            </Button>
          </Space>

          {/* 统计数据 */}
          <Row gutter={[24, 24]} className="stats-row">
            <Col xs={12} sm={6}>
              <div className="stat-item">
                <div className="stat-icon" style={{ background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)' }}>
                  <ApiOutlined style={{ fontSize: 24, color: '#fff' }} />
                </div>
                <Statistic
                  title="Maven模块"
                  value={animatedStats.modules}
                  suffix="+"
                  styles={{
                    value: { color: '#fff', fontSize: 28, fontWeight: 700 },
                    title: { color: 'rgba(255, 255, 255, 0.85)', fontSize: 13, marginBottom: 4 }
                  }}
                />
              </div>
            </Col>
            <Col xs={12} sm={6}>
              <div className="stat-item">
                <div className="stat-icon" style={{ background: 'linear-gradient(135deg, #52c41a 0%, #95de64 100%)' }}>
                  <CodeOutlined style={{ fontSize: 24, color: '#fff' }} />
                </div>
                <Statistic
                  title="代码行数"
                  value={animatedStats.codeLines}
                  suffix="+"
                  styles={{
                    value: { color: '#fff', fontSize: 28, fontWeight: 700 },
                    title: { color: 'rgba(255, 255, 255, 0.85)', fontSize: 13, marginBottom: 4 }
                  }}
                />
              </div>
            </Col>
            <Col xs={12} sm={6}>
              <div className="stat-item">
                <div className="stat-icon" style={{ background: 'linear-gradient(135deg, #faad14 0%, #ffc53d 100%)' }}>
                  <FileTextOutlined style={{ fontSize: 24, color: '#fff' }} />
                </div>
                <Statistic
                  title="支持格式"
                  value={animatedStats.formats}
                  suffix="+ 类型"
                  styles={{
                    value: { color: '#fff', fontSize: 28, fontWeight: 700 },
                    title: { color: 'rgba(255, 255, 255, 0.85)', fontSize: 13, marginBottom: 4 }
                  }}
                />
              </div>
            </Col>
            <Col xs={12} sm={6}>
              <div className="stat-item">
                <div className="stat-icon" style={{ background: 'linear-gradient(135deg, #f5222d 0%, #ff7875 100%)' }}>
                  <ThunderboltOutlined style={{ fontSize: 24, color: '#fff' }} />
                </div>
                <Statistic
                  title="分块策略"
                  value={animatedStats.strategies}
                  suffix="种"
                  styles={{
                    value: { color: '#fff', fontSize: 28, fontWeight: 700 },
                    title: { color: 'rgba(255, 255, 255, 0.85)', fontSize: 13, marginBottom: 4 }
                  }}
                />
              </div>
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
                  ❌ 传统RAG的根本性缺陷
                </Title>
                <div className="problem-diagram">
                  <div className="mixed-index">
                    <DatabaseOutlined style={{ fontSize: 48 }} />
                    <Text>单一向量空间 + 固定分块</Text>
                  </div>
                  <Paragraph className="problem-list">
                    📄 技术文档 + 📊 财务报表 + 💼 合同 + 📧 邮件...
                  </Paragraph>
                  <div className="problem-results">
                    <Text type="danger">• <strong>语义割裂</strong>：固定分块破坏语义完整性</Text>
                    <Text type="danger">• <strong>上下文断裂</strong>：跨块信息无法关联</Text>
                    <Text type="danger">• <strong>向量空间污染</strong>：多领域混杂降低检索精度</Text>
                    <Text type="danger">• <strong>单一存储</strong>：无灾备方案，存在单点故障</Text>
                    <Text type="danger">• <strong>固定维度</strong>：无法适配不同场景需求</Text>
                    <Text type="danger">• <strong>静态知识</strong>：缺乏持续学习和优化能力</Text>
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
                  ✅ OmniAgent的架构创新
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
                    <Text type="success">• <strong>6种分块策略</strong>：智能保留语义完整性</Text>
                    <Text type="success">• <strong>知识域隔离</strong>：独立向量空间，精准检索</Text>
                    <Text type="success">• <strong>多元存储</strong>：文件+数据库+向量库，灾备冗余</Text>
                    <Text type="success">• <strong>多维度向量</strong>：支持不同Embedding模型并行</Text>
                    <Text type="success">• <strong>知识网络</strong>：跨域关联，语义补全上下文</Text>
                    <Text type="success">• <strong>HOPE架构</strong>：自我学习与持续优化</Text>
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
                <Title level={4}>多次RAG</Title>
                <Text>多策略并行检索</Text>
              </div>
            </div>
          </div>

          {/* 知识网络流程 */}
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

