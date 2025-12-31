/**
 * OmniAgent 官方首页 - Landing Page
 * 类似CodeGeex的炫酷展示页面
 *
 * @author Jinhua Yu
 * @since 2025-12-29
 */

import React, { useState, useEffect, useMemo } from 'react'
import { Button, Row, Col, Card, Typography, Space, Statistic, Image } from 'antd'
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
  RightOutlined,
  HeartOutlined,
  WechatOutlined
} from '@ant-design/icons'
import GiteeIcon from '../icons/GiteeIcon'
import { useLanguage } from '../../contexts/LanguageContext'
import './LandingPage.css'
import KnowledgeNetworkDiagram from './KnowledgeNetworkDiagram'

// 导入二维码图片
import PaymentQRCode from '../../assets/images/Payment QR Code.png'
import ConnectMeQRCode from '../../assets/images/Connect Me.png'

const { Title, Paragraph, Text } = Typography


const LandingPage = ({ onEnterApp }) => {
  const { t, language, toggleLanguage } = useLanguage()

  // 图标映射对象 - 避免使用eval导致生产环境问题
  const iconMap = {
    ApiOutlined,
    CodeOutlined,
    FileTextOutlined,
    ThunderboltOutlined,
    DatabaseOutlined,
    SafetyOutlined,
    CheckCircleOutlined,
    BulbOutlined,
    ShareAltOutlined,
    RocketOutlined,
    CloudOutlined,
    UserOutlined
  }

  const [animatedStats, setAnimatedStats] = useState({
    modules: 0,
    codeLines: 0,
    formats: 0,
    strategies: 0
  })

  const [currentStatsPage, setCurrentStatsPage] = useState(0)

  // 使用 useMemo 优化 statsPages，避免每次渲染都重新创建
  const statsPages = useMemo(() => [
    // 第一页：核心架构
    [
      { title: t('landingPage.stats.modules'), value: 22, suffix: t('landingPage.stats.modulesUnit'), icon: 'ApiOutlined', color: '#667eea' },
      { title: t('landingPage.stats.codeLines'), value: 85000, suffix: t('landingPage.stats.codeLinesUnit'), icon: 'CodeOutlined', color: '#52c41a' },
      { title: t('landingPage.stats.docFormats'), value: 10, suffix: t('landingPage.stats.docFormatsUnit'), icon: 'FileTextOutlined', color: '#faad14' },
      { title: t('landingPage.stats.chunkingStrategies'), value: 6, suffix: t('landingPage.stats.chunkingStrategiesUnit'), icon: 'ThunderboltOutlined', color: '#f5222d' },
    ],
    // 第二页：RAG能力
    [
      { title: t('landingPage.stats.ragSolutions'), value: 6, suffix: t('landingPage.stats.ragSolutionsUnit'), icon: 'DatabaseOutlined', color: '#1890ff' },
      { title: t('landingPage.stats.vectorModels'), value: 3, suffix: t('landingPage.stats.vectorModelsUnit'), icon: 'ThunderboltOutlined', color: '#722ed1' },
      { title: t('landingPage.stats.storageEngines'), value: 6, suffix: t('landingPage.stats.storageEnginesUnit'), icon: 'SafetyOutlined', color: '#eb2f96' },
      { title: t('landingPage.stats.multiInstance'), value: 1, suffix: t('landingPage.stats.multiInstanceUnit'), icon: 'CheckCircleOutlined', color: '#52c41a' },
    ],
    // 第三页：增强特性
    [
      { title: t('landingPage.stats.modelIntegration'), value: 3, suffix: t('landingPage.stats.modelIntegrationUnit'), icon: 'BulbOutlined', color: '#faad14' },
      { title: t('landingPage.stats.knowledgeNetwork'), value: 1, suffix: t('landingPage.stats.knowledgeNetworkUnit'), icon: 'ShareAltOutlined', color: '#13c2c2' },
      { title: t('landingPage.stats.hope'), value: 1, suffix: t('landingPage.stats.hopeUnit'), icon: 'RocketOutlined', color: '#f5222d' },
      { title: t('landingPage.stats.p2p'), value: 1, suffix: t('landingPage.stats.p2pUnit'), icon: 'CloudOutlined', color: '#1890ff' },
    ],
    // 第四页：技术栈
    [
      { title: t('landingPage.stats.springBoot'), value: 3.4, suffix: '', icon: 'CloudOutlined', color: '#52c41a' },
      { title: t('landingPage.stats.java'), value: 21, suffix: '', icon: 'CodeOutlined', color: '#fa8c16' },
      { title: t('landingPage.stats.compileStatus'), value: 100, suffix: t('landingPage.stats.percentUnit'), icon: 'CheckCircleOutlined', color: '#52c41a' },
      { title: t('landingPage.stats.productionReady'), value: 100, suffix: t('landingPage.stats.percentUnit'), icon: 'SafetyOutlined', color: '#1890ff' },
    ],
  ], [t, language])

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

          {/* 语言切换按钮 */}
          <div className="language-switch">
            <Button
              className="lang-btn"
              onClick={toggleLanguage}
              size="small"
              style={{
                background: 'rgba(255, 255, 255, 0.1)',
                border: '1px solid rgba(255, 255, 255, 0.2)',
                color: '#fff',
                backdropFilter: 'blur(10px)',
                borderRadius: 20,
                padding: '4px 16px',
                fontWeight: 500,
                transition: 'all 0.3s ease',
              }}
              onMouseEnter={(e) => {
                e.currentTarget.style.background = 'rgba(255, 255, 255, 0.2)'
              }}
              onMouseLeave={(e) => {
                e.currentTarget.style.background = 'rgba(255, 255, 255, 0.1)'
              }}
            >
              {language === 'zh' ? 'EN' : '中文'}
            </Button>
          </div>

          <Title level={2} className="hero-subtitle">
            {t('landingPage.hero.subtitle')}
          </Title>

          <div className="hero-slogan">
            <Text className="hero-slogan-text">
              {t('landingPage.hero.slogan')}
            </Text>
          </div>

          <Paragraph className="hero-description">
            {t('landingPage.hero.description')}
          </Paragraph>

          <div className="hero-features">
            <Row gutter={[16, 16]} justify="center">
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <RocketOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">{t('landingPage.hero.featureAgent')}</Text>
                </div>
              </Col>
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <CloudOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">{t('landingPage.hero.featureService')}</Text>
                </div>
              </Col>
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <BulbOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">{t('landingPage.hero.featureAI')}</Text>
                </div>
              </Col>
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <CodeOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">{t('landingPage.hero.featureCode')}</Text>
                </div>
              </Col>
              <Col xs={12} sm={8} md={4}>
                <div className="hero-feature-item">
                  <ThunderboltOutlined className="hero-feature-icon" />
                  <Text className="hero-feature-text">{t('landingPage.hero.featureTest')}</Text>
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
              {t('landingPage.hero.startButton')}
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
              {t('landingPage.hero.techBlog')}
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
                    const IconComponent = iconMap[stat.icon]
                    return (
                      <Col xs={12} sm={6} key={index}>
                        <div className="stat-item">
                          <div className="stat-icon" style={{ background: `linear-gradient(135deg, ${stat.color} 0%, ${stat.color}cc 100%)` }}>
                            {IconComponent && <IconComponent style={{ fontSize: 24, color: '#fff' }} />}
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
                {currentStatsPage === 0 && t('landingPage.stats.desc1')}
                {currentStatsPage === 1 && t('landingPage.stats.desc2')}
                {currentStatsPage === 2 && t('landingPage.stats.desc3')}
                {currentStatsPage === 3 && t('landingPage.stats.desc4')}
              </Text>
            </div>
          </div>
        </div>
      </section>

      {/* 核心理念对比 */}
      <section className="comparison-section">
        <div className="container">
          <Title level={2} className="section-title">
            {t('landingPage.comparison.title')}
          </Title>

          <Row gutter={48} align="middle">
            <Col xs={24} md={11}>
              <Card className="problem-card">
                <Title level={4} style={{ color: '#f5222d', marginBottom: 24 }}>
                  {t('landingPage.comparison.traditionalDefect')}
                </Title>

                {/* 单一向量空间示意图 */}
                <div className="problem-diagram">
                  <Title level={5} style={{ textAlign: 'center', marginBottom: 16, color: '#f5222d' }}>
                    {t('landingPage.comparison.singleVectorChaos')}
                  </Title>
                  <div className="mixed-index">
                    <DatabaseOutlined style={{ fontSize: 48, color: '#ff4d4f' }} />
                    <Text style={{ color: '#8c8c8c', fontSize: 13, marginTop: 8 }}>
                      {t('landingPage.comparison.mixedDocs')}
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
                          <Text strong>{t('landingPage.comparison.semanticFragmentation')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.semanticFragmentationDesc')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#ff7875' }}>
                          <ShareAltOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>{t('landingPage.comparison.contextBreak')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.contextBreakDesc')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#fa541c' }}>
                          <DatabaseOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>{t('landingPage.comparison.vectorPollution')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.vectorPollutionDesc')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#cf1322' }}>
                          <SafetyOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>{t('landingPage.comparison.singleStorage')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.singleStorageDesc')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#ff9c6e' }}>
                          <ThunderboltOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>{t('landingPage.comparison.fixedDimension')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.fixedDimensionDesc')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="problem-item">
                        <div className="problem-icon" style={{ background: '#d4380d' }}>
                          <BulbOutlined />
                        </div>
                        <div className="problem-content">
                          <Text strong>{t('landingPage.comparison.staticKnowledge')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.staticKnowledgeDesc')}</Text>
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
                  {t('landingPage.comparison.innovation')}
                </Title>

                {/* 知识域隔离示意图 */}
                <div className="solution-diagram">
                  <Title level={5} style={{ textAlign: 'center', marginBottom: 16, color: '#52c41a' }}>
                    {t('landingPage.comparison.domainIsolation')}
                  </Title>
                  <Row gutter={12}>
                    <Col span={8}>
                      <div className="domain-box">
                        <FileTextOutlined style={{ fontSize: 24 }} />
                        <Text style={{ fontSize: 12 }}>{t('landingPage.comparison.techDocDomain')}</Text>
                      </div>
                    </Col>
                    <Col span={8}>
                      <div className="domain-box">
                        <DatabaseOutlined style={{ fontSize: 24 }} />
                        <Text style={{ fontSize: 12 }}>{t('landingPage.comparison.financeDomain')}</Text>
                      </div>
                    </Col>
                    <Col span={8}>
                      <div className="domain-box">
                        <SafetyOutlined style={{ fontSize: 24 }} />
                        <Text style={{ fontSize: 12 }}>{t('landingPage.comparison.contractDomain')}</Text>
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
                          <Text strong>{t('landingPage.comparison.sixStrategies')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.semanticPreserved')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#1890ff' }}>
                          <ShareAltOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>{t('landingPage.comparison.domainIsolationFeature')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.independentVector')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#722ed1' }}>
                          <DatabaseOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>{t('landingPage.comparison.multiStorage')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.disasterBackup')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#faad14' }}>
                          <ThunderboltOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>{t('landingPage.comparison.multiDimension')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.parallelEmbedding')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#13c2c2' }}>
                          <ApiOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>{t('landingPage.comparison.knowledgeNetworkFeature')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.crossDomain')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#f5222d' }}>
                          <RocketOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>{t('landingPage.comparison.hopeArch')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.selfLearning')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#eb2f96' }}>
                          <BulbOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>{t('landingPage.comparison.smartRole')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.aiPrompts')}</Text>
                        </div>
                      </div>
                    </Col>
                    <Col span={12}>
                      <div className="innovation-item">
                        <div className="innovation-icon" style={{ background: '#fa8c16' }}>
                          <CloudOutlined />
                        </div>
                        <div className="innovation-content">
                          <Text strong>{t('landingPage.comparison.distributedArch')}</Text>
                          <Text type="secondary" style={{ fontSize: 12 }}>{t('landingPage.comparison.highAvailability')}</Text>
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
            {t('landingPage.workflow.title')}
          </Title>

          {/* 主流程 */}
          <div className="workflow-diagram">
            <div className="workflow-step enhanced">
              <div className="step-icon">
                <CloudOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.step1Title')}</Title>
                <Text className="step-highlight">{t('landingPage.workflow.step1Highlight')}</Text>
                <div className="step-details">
                  <Text>{t('landingPage.workflow.step1Detail1')}</Text>
                  <Text>{t('landingPage.workflow.step1Detail2')}</Text>
                  <Text>{t('landingPage.workflow.step1Detail3')}</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <FileTextOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.step2Title')}</Title>
                <Text className="step-highlight">{t('landingPage.workflow.step2Highlight')}</Text>
                <div className="step-details">
                  <Text>{t('landingPage.workflow.step2Detail1')}</Text>
                  <Text>{t('landingPage.workflow.step2Detail2')}</Text>
                  <Text>{t('landingPage.workflow.step2Detail3')}</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <CodeOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.step3Title')}</Title>
                <Text className="step-highlight">{t('landingPage.workflow.step3Highlight')}</Text>
                <div className="step-details">
                  <Text>{t('landingPage.workflow.step3Detail1')}</Text>
                  <Text>{t('landingPage.workflow.step3Detail2')}</Text>
                  <Text>{t('landingPage.workflow.step3Detail3')}</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <ThunderboltOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.step4Title')}</Title>
                <Text className="step-highlight">{t('landingPage.workflow.step4Highlight')}</Text>
                <div className="step-details">
                  <Text>{t('landingPage.workflow.step4Detail1')}</Text>
                  <Text>{t('landingPage.workflow.step4Detail2')}</Text>
                  <Text>{t('landingPage.workflow.step4Detail3')}</Text>
                  <Text style={{ color: '#52c41a', fontWeight: 600 }}>{t('landingPage.workflow.step4Detail4')}</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <DatabaseOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.step5Title')}</Title>
                <Text className="step-highlight">{t('landingPage.workflow.step5Highlight')}</Text>
                <div className="step-details">
                  <Text>{t('landingPage.workflow.step5Detail1')}</Text>
                  <Text>{t('landingPage.workflow.step5Detail2')}</Text>
                  <Text>{t('landingPage.workflow.step5Detail3')}</Text>
                </div>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step enhanced">
              <div className="step-icon">
                <BulbOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.step6Title')}</Title>
                <Text className="step-highlight">{t('landingPage.workflow.step6Highlight')}</Text>
                <div className="step-details">
                  <Text>{t('landingPage.workflow.step6Detail1')}</Text>
                  <Text>{t('landingPage.workflow.step6Detail2')}</Text>
                  <Text>{t('landingPage.workflow.step6Detail3')}</Text>
                </div>
              </div>
            </div>
          </div>

          {/* 知识网络增强层 */}
          <Title level={3} className="section-subtitle" style={{ marginTop: 64, marginBottom: 32, textAlign: 'center' }}>
            {t('landingPage.workflow.knowledgeNetworkLayer')}
          </Title>

          <div className="workflow-diagram">
            <div className="workflow-step">
              <div className="step-icon">
                <ApiOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.knowledgeExtraction')}</Title>
                <Text>{t('landingPage.workflow.aiAnalysis')}</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <ShareAltOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.crossDomainAssociation')}</Title>
                <Text>{t('landingPage.workflow.semanticGraph')}</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <ThunderboltOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.hopeLearning')}</Title>
                <Text>{t('landingPage.workflow.selfOptimization')}</Text>
              </div>
            </div>

            <ArrowRightOutlined className="workflow-arrow" />

            <div className="workflow-step">
              <div className="step-icon">
                <BulbOutlined />
              </div>
              <div className="step-content">
                <Title level={4}>{t('landingPage.workflow.contextCompletion')}</Title>
                <Text>{t('landingPage.workflow.qaEnhancement')}</Text>
              </div>
            </div>
          </div>
          {/* 知识网络架构 */}
          <KnowledgeNetworkDiagram />
          {/* P2P分布式架构 */}
          <div className="p2p-architecture-section" style={{ marginTop: 80 }}>
            <Title level={3} className="section-subtitle" style={{ marginBottom: 32, textAlign: 'center' }}>
              {t('landingPage.workflow.p2pArchTitle')}
            </Title>

            <Row gutter={[48, 32]}>
              <Col xs={24}>
                <Card className="p2p-card">
                  <Row gutter={48} align="middle">
                    <Col xs={24} md={12}>
                      <Title level={4} style={{ color: '#1890ff', marginBottom: 24 }}>
                        {t('landingPage.workflow.distributedNetwork')}
                      </Title>
                      <div className="p2p-features">
                        <div className="p2p-feature-item">
                          <div className="p2p-feature-icon" style={{ background: '#1890ff' }}>
                            <ShareAltOutlined />
                          </div>
                          <div>
                            <Text strong style={{ fontSize: 15, display: 'block', marginBottom: 4 }}>
                              {t('landingPage.workflow.connectCodePairing')}
                            </Text>
                            <Text type="secondary" style={{ fontSize: 13 }}>
                              {t('landingPage.workflow.connectCodeDesc')}
                            </Text>
                          </div>
                        </div>

                        <div className="p2p-feature-item">
                          <div className="p2p-feature-icon" style={{ background: '#52c41a' }}>
                            <CloudOutlined />
                          </div>
                          <div>
                            <Text strong style={{ fontSize: 15, display: 'block', marginBottom: 4 }}>
                              {t('landingPage.workflow.crossNetworkIP')}
                            </Text>
                            <Text type="secondary" style={{ fontSize: 13 }}>
                              {t('landingPage.workflow.crossNetworkIPDesc')}
                            </Text>
                          </div>
                        </div>

                        <div className="p2p-feature-item">
                          <div className="p2p-feature-icon" style={{ background: '#722ed1' }}>
                            <SafetyOutlined />
                          </div>
                          <div>
                            <Text strong style={{ fontSize: 15, display: 'block', marginBottom: 4 }}>
                              {t('landingPage.workflow.distributedMonolithic')}
                            </Text>
                            <Text type="secondary" style={{ fontSize: 13 }}>
                              {t('landingPage.workflow.distributedMonolithicDesc')}
                            </Text>
                          </div>
                        </div>

                        <div className="p2p-feature-item">
                          <div className="p2p-feature-icon" style={{ background: '#fa8c16' }}>
                            <ThunderboltOutlined />
                          </div>
                          <div>
                            <Text strong style={{ fontSize: 15, display: 'block', marginBottom: 4 }}>
                              {t('landingPage.workflow.knowledgePropagation')}
                            </Text>
                            <Text type="secondary" style={{ fontSize: 13 }}>
                              {t('landingPage.workflow.knowledgePropagationDesc')}
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
                            <Text style={{ color: '#fff', fontSize: 12, marginTop: 8 }}>{t('landingPage.workflow.enterpriseNode')}</Text>
                          </div>

                          <div className="p2p-connections">
                            <div className="p2p-node satellite" style={{ top: '0%', left: '50%' }}>
                              <CloudOutlined style={{ fontSize: 20, color: '#fff' }} />
                              <Text style={{ color: '#fff', fontSize: 10 }}>{t('landingPage.workflow.departmentA')}</Text>
                            </div>
                            <div className="p2p-node satellite" style={{ top: '50%', left: '90%' }}>
                              <CloudOutlined style={{ fontSize: 20, color: '#fff' }} />
                              <Text style={{ color: '#fff', fontSize: 10 }}>{t('landingPage.workflow.departmentB')}</Text>
                            </div>
                            <div className="p2p-node satellite" style={{ top: '100%', left: '50%' }}>
                              <CloudOutlined style={{ fontSize: 20, color: '#fff' }} />
                              <Text style={{ color: '#fff', fontSize: 10 }}>{t('landingPage.workflow.departmentC')}</Text>
                            </div>
                            <div className="p2p-node satellite" style={{ top: '50%', left: '10%' }}>
                              <CloudOutlined style={{ fontSize: 20, color: '#fff' }} />
                              <Text style={{ color: '#fff', fontSize: 10 }}>{t('landingPage.workflow.remoteTeam')}</Text>
                            </div>
                          </div>
                        </div>

                        <div style={{ textAlign: 'center', marginTop: 24 }}>
                          <Text strong style={{ color: '#1890ff', fontSize: 14 }}>
                            {t('landingPage.workflow.connectCodeTip')}
                          </Text>
                          <br />
                          <Text type="secondary" style={{ fontSize: 12 }}>
                            {t('landingPage.workflow.monolithicDistributed')}
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
            {t('landingPage.features.title')}
          </Title>

          <Row gutter={[32, 32]}>
            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <ShareAltOutlined className="feature-icon" />
                <Title level={4}>{t('landingPage.features.domainIsolation')}</Title>
                <Paragraph>
                  {t('landingPage.features.domainIsolationDesc')}
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> {t('landingPage.features.multiDomain')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.smartRouting')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.permissionIsolation')}</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <FileTextOutlined className="feature-icon" />
                <Title level={4}>{t('landingPage.features.fullFormatSupport')}</Title>
                <Paragraph>
                  {t('landingPage.features.fullFormatSupportDesc')}
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> {t('landingPage.features.officeDocs')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.textDocs')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.codeDocs')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.codeProjectKB')}</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <CodeOutlined className="feature-icon" />
                <Title level={4}>{t('landingPage.features.sixChunkingStrategies')}</Title>
                <Paragraph>
                  {t('landingPage.features.sixChunkingStrategiesDesc')}
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> {t('landingPage.features.fixedParaSentence')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.pplSmart')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.semanticTFIDF')}</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <ThunderboltOutlined className="feature-icon" />
                <Title level={4}>{t('landingPage.features.vectorSearch')}</Title>
                <Paragraph>
                  {t('landingPage.features.vectorSearchDesc')}
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> {t('landingPage.features.bgeBase')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.semanticSearch')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.gracefulDegradation')}</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <ApiOutlined className="feature-icon" />
                <Title level={4}>{t('landingPage.features.smartRoleKnowledge')}</Title>
                <Paragraph>
                  {t('landingPage.features.smartRoleKnowledgeDesc')}
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> {t('landingPage.features.aiPromptGen')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.roleKB')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.knowledgeGraph')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.codeProjectAnalysis')}</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} sm={12} md={8}>
              <Card className="feature-card" hoverable>
                <SafetyOutlined className="feature-icon" />
                <Title level={4}>{t('landingPage.features.enterpriseArch')}</Title>
                <Paragraph>
                  {t('landingPage.features.enterpriseArchDesc')}
                </Paragraph>
                <ul className="feature-list">
                  <li><CheckCircleOutlined /> {t('landingPage.features.modularDesign')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.exceptionHandling')}</li>
                  <li><CheckCircleOutlined /> {t('landingPage.features.compileSuccess')}</li>
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
            {t('landingPage.useCases.title')}
          </Title>

          <Row gutter={[32, 32]}>
            <Col xs={24} md={12}>
              <Card className="use-case-card" hoverable>
                <div className="use-case-header">
                  <RocketOutlined className="use-case-icon" style={{ color: '#1890ff' }} />
                  <Title level={3}>{t('landingPage.useCases.smartCodeAssistant')}</Title>
                </div>
                <Paragraph className="use-case-desc">
                  {t('landingPage.useCases.smartCodeAssistantDesc')}
                </Paragraph>
                <ul className="use-case-list">
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.fullProjectContext')}</strong>{t('landingPage.useCases.fullProjectContextDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.smartCompletion')}</strong>{t('landingPage.useCases.smartCompletionDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.bugDetection')}</strong>{t('landingPage.useCases.bugDetectionDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.docGeneration')}</strong>{t('landingPage.useCases.docGenerationDesc')}</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} md={12}>
              <Card className="use-case-card" hoverable>
                <div className="use-case-header">
                  <ThunderboltOutlined className="use-case-icon" style={{ color: '#52c41a' }} />
                  <Title level={3}>{t('landingPage.useCases.autoTestPlatform')}</Title>
                </div>
                <Paragraph className="use-case-desc">
                  {t('landingPage.useCases.autoTestPlatformDesc')}
                </Paragraph>
                <ul className="use-case-list">
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.testCaseGen')}</strong>{t('landingPage.useCases.testCaseGenDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.boundaryDetection')}</strong>{t('landingPage.useCases.boundaryDetectionDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.regressionTest')}</strong>{t('landingPage.useCases.regressionTestDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.testReportAnalysis')}</strong>{t('landingPage.useCases.testReportAnalysisDesc')}</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} md={12}>
              <Card className="use-case-card" hoverable>
                <div className="use-case-header">
                  <FileTextOutlined className="use-case-icon" style={{ color: '#faad14' }} />
                  <Title level={3}>{t('landingPage.useCases.projectAnalysisReport')}</Title>
                </div>
                <Paragraph className="use-case-desc">
                  {t('landingPage.useCases.projectAnalysisReportDesc')}
                </Paragraph>
                <ul className="use-case-list">
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.architectureAnalysis')}</strong>{t('landingPage.useCases.architectureAnalysisDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.codeQuality')}</strong>{t('landingPage.useCases.codeQualityDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.securityAudit')}</strong>{t('landingPage.useCases.securityAuditDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.performanceOptimization')}</strong>{t('landingPage.useCases.performanceOptimizationDesc')}</li>
                </ul>
              </Card>
            </Col>

            <Col xs={24} md={12}>
              <Card className="use-case-card" hoverable>
                <div className="use-case-header">
                  <CloudOutlined className="use-case-icon" style={{ color: '#722ed1' }} />
                  <Title level={3}>{t('landingPage.useCases.enterpriseAgentService')}</Title>
                </div>
                <Paragraph className="use-case-desc">
                  {t('landingPage.useCases.enterpriseAgentServiceDesc')}
                </Paragraph>
                <ul className="use-case-list">
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.multiTenantIsolation')}</strong>{t('landingPage.useCases.multiTenantIsolationDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.agentMarketplace')}</strong>{t('landingPage.useCases.agentMarketplaceDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.workflowOrchestration')}</strong>{t('landingPage.useCases.workflowOrchestrationDesc')}</li>
                  <li><CheckCircleOutlined /> <strong>{t('landingPage.useCases.apiGateway')}</strong>{t('landingPage.useCases.apiGatewayDesc')}</li>
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
            {t('landingPage.techStack.title')}
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
            {t('landingPage.quickStart.title')}
          </Title>

          <Row gutter={[32, 32]} justify="center">
            <Col xs={24} md={8}>
              <Card className="quickstart-card">
                <div className="step-number">1</div>
                <Title level={4}>{t('landingPage.quickStart.step1Title')}</Title>
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
                <Title level={4}>{t('landingPage.quickStart.step2Title')}</Title>
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
                <Title level={4}>{t('landingPage.quickStart.step3Title')}</Title>
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
              {t('landingPage.quickStart.demoButton')}
            </Button>
          </div>
        </div>
      </section>

      {/* Footer */}
      <footer className="landing-footer">
        <div className="container">
          <Row gutter={48}>
            <Col xs={24} md={8}>
              <Title level={4}>{t('landingPage.footer.aboutTitle')}</Title>
              <Paragraph>
                {t('landingPage.footer.aboutDesc')}
              </Paragraph>
            </Col>

            <Col xs={24} md={8}>
              <Title level={4}>{t('landingPage.footer.quickLinks')}</Title>
              <ul className="footer-links">
                <li><a href="https://github.com/jinhua10/omni-agent" target="_blank" rel="noopener noreferrer">
                  <GithubOutlined /> {t('landingPage.footer.githubLink')}
                </a></li>
                <li><a href="https://gitee.com/gnnu" target="_blank" rel="noopener noreferrer">
                  <GithubOutlined /> {t('landingPage.footer.giteeLink')}
                </a></li>
                <li><a href="https://yumbo.blog.csdn.net/" target="_blank" rel="noopener noreferrer">
                  <BookOutlined /> {t('landingPage.footer.techBlogLink')}
                </a></li>
              </ul>
            </Col>

            <Col xs={24} md={8}>
              <Title level={4}>{t('landingPage.footer.contactUs')}</Title>
              <ul className="footer-links">
                <li><MailOutlined /> {t('landingPage.footer.email')}</li>
                <li><GithubOutlined /> {t('landingPage.footer.github')}</li>
              </ul>
            </Col>
          </Row>

          {/* 二维码区域 */}
          <div className="footer-qr-codes" style={{
            marginTop: 48,
            paddingTop: 32,
            borderTop: '1px solid rgba(255, 255, 255, 0.1)'
          }}>
            <Title level={4} style={{ textAlign: 'center', marginBottom: 32 }}>
              {t('landingPage.footer.qrCodes')}
            </Title>
            <Row gutter={[48, 24]} justify="center">
              <Col xs={24} sm={12} md={8} style={{ textAlign: 'center' }}>
                <div className="qr-code-card">
                  <div className="qr-code-icon" style={{
                    fontSize: 32,
                    color: '#ff4d4f',
                    marginBottom: 16
                  }}>
                    <HeartOutlined />
                  </div>
                  <Title level={5} style={{ marginBottom: 12 }}>
                    {t('landingPage.footer.sponsorTitle')}
                  </Title>
                  <Image
                    src={PaymentQRCode}
                    alt={t('landingPage.footer.sponsorTitle')}
                    width={180}
                    preview={{
                      classNames: {
                        cover: 'custom-preview-mask'
                      }
                    }}
                    style={{
                      border: '2px solid rgba(255, 255, 255, 0.1)',
                      borderRadius: 8,
                      marginBottom: 12
                    }}
                  />
                  <Text type="secondary" style={{ fontSize: 13, display: 'block' }}>
                    {t('landingPage.footer.sponsorDesc')}
                  </Text>
                </div>
              </Col>

              <Col xs={24} sm={12} md={8} style={{ textAlign: 'center' }}>
                <div className="qr-code-card">
                  <div className="qr-code-icon" style={{
                    fontSize: 32,
                    color: '#52c41a',
                    marginBottom: 16
                  }}>
                    <WechatOutlined />
                  </div>
                  <Title level={5} style={{ marginBottom: 12 }}>
                    {t('landingPage.footer.contactTitle')}
                  </Title>
                  <Image
                    src={ConnectMeQRCode}
                    alt={t('landingPage.footer.contactTitle')}
                    width={180}
                    preview={{
                      classNames: {
                        cover: 'custom-preview-mask'
                      }
                    }}
                    style={{
                      border: '2px solid rgba(255, 255, 255, 0.1)',
                      borderRadius: 8,
                      marginBottom: 12
                    }}
                  />
                  <Text type="secondary" style={{ fontSize: 13, display: 'block' }}>
                    {t('landingPage.footer.contactDesc')}
                  </Text>
                </div>
              </Col>
            </Row>
          </div>

          <div className="footer-bottom">
            <Text type="secondary">
              {t('landingPage.footer.copyright')}
            </Text>
          </div>
        </div>
      </footer>
    </div>
  )
}

export default LandingPage

