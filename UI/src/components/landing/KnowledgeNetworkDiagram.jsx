/**
 * 知识网络架构图组件
 * 直观展示OmniAgent的核心优势，清晰展示与传统RAG的本质区别
 *
 * @author OmniAgent Team
 * @since 2025-12-30
 */

import React from 'react'
import { Typography, Row, Col, Card } from 'antd'
import {
  FileTextOutlined,
  CodeOutlined,
  UserOutlined,
  BankOutlined,
  LinkOutlined,
  ApartmentOutlined,
  PieChartOutlined,
  AimOutlined,
  SearchOutlined,
  SafetyOutlined,
  CheckCircleOutlined,
  CloseCircleOutlined,
  ApiOutlined,
  RobotOutlined,
  AlertOutlined,
  PlusCircleOutlined
} from '@ant-design/icons'
import { motion } from 'framer-motion'
import { useLanguage } from '../../contexts/LanguageContext'
import './KnowledgeNetworkDiagram.css'

const { Title, Text, Paragraph } = Typography

// Motion组件包装
const MotionCard = motion(Card)
const MotionDiv = motion.div

const KnowledgeNetworkDiagram = () => {
  const { t } = useLanguage()

  // 传统RAG的问题
  const traditionalProblems = [
    { icon: '💔', title: t('landingPage.knowledgeNetworkArch.problem1'), desc: t('landingPage.knowledgeNetworkArch.problem1Desc') },
    { icon: '🎲', title: t('landingPage.knowledgeNetworkArch.problem2'), desc: t('landingPage.knowledgeNetworkArch.problem2Desc') },
    { icon: '🌀', title: t('landingPage.knowledgeNetworkArch.problem3'), desc: t('landingPage.knowledgeNetworkArch.problem3Desc') },
    { icon: '❓', title: t('landingPage.knowledgeNetworkArch.problem4'), desc: t('landingPage.knowledgeNetworkArch.problem4Desc') },
  ]

  // OmniAgent的解决方案
  const solutions = [
    { icon: '🎯', title: t('landingPage.knowledgeNetworkArch.solution1'), desc: t('landingPage.knowledgeNetworkArch.solution1Desc') },
    { icon: '🧠', title: t('landingPage.knowledgeNetworkArch.solution2'), desc: t('landingPage.knowledgeNetworkArch.solution2Desc') },
    { icon: '🔗', title: t('landingPage.knowledgeNetworkArch.solution3'), desc: t('landingPage.knowledgeNetworkArch.solution3Desc') },
    { icon: '⚡', title: t('landingPage.knowledgeNetworkArch.solution4'), desc: t('landingPage.knowledgeNetworkArch.solution4Desc') },
  ]

  // 知识域配置
  const knowledgeDomains = [
    { icon: <FileTextOutlined />, name: t('landingPage.knowledgeNetworkArch.docDomain'), desc: t('landingPage.knowledgeNetworkArch.docDomainDesc'), dim: '768维', color: '#4a90e2' },
    { icon: <CodeOutlined />, name: t('landingPage.knowledgeNetworkArch.codeDomain'), desc: t('landingPage.knowledgeNetworkArch.codeDomainDesc'), dim: '768维', color: '#50c878' },
    { icon: <UserOutlined />, name: t('landingPage.knowledgeNetworkArch.roleDomain'), desc: t('landingPage.knowledgeNetworkArch.roleDomainDesc'), dim: '384维', color: '#f39c12' },
    { icon: <BankOutlined />, name: t('landingPage.knowledgeNetworkArch.bizDomain'), desc: t('landingPage.knowledgeNetworkArch.bizDomainDesc'), dim: '768维', color: '#9b59b6' },
  ]

  // 知识网络层功能
  const networkFeatures = [
    { icon: <LinkOutlined />, name: t('landingPage.knowledgeNetworkArch.crossDomain') },
    { icon: <ApartmentOutlined />, name: t('landingPage.knowledgeNetworkArch.dependency') },
    { icon: <PieChartOutlined />, name: t('landingPage.knowledgeNetworkArch.completeness') },
    { icon: <AimOutlined />, name: t('landingPage.knowledgeNetworkArch.smartRoute') },
  ]

  // 角色提取步骤
  const extractionSteps = [
    { num: 1, icon: <UserOutlined />, title: t('landingPage.knowledgeNetworkArch.step1Title'), desc: t('landingPage.knowledgeNetworkArch.step1Desc') },
    { num: 2, icon: <RobotOutlined />, title: t('landingPage.knowledgeNetworkArch.step2Title'), desc: t('landingPage.knowledgeNetworkArch.step2Desc') },
    { num: 3, icon: <AlertOutlined />, title: t('landingPage.knowledgeNetworkArch.step3Title'), desc: t('landingPage.knowledgeNetworkArch.step3Desc') },
    { num: 4, icon: <PlusCircleOutlined />, title: t('landingPage.knowledgeNetworkArch.step4Title'), desc: t('landingPage.knowledgeNetworkArch.step4Desc') },
  ]

  // RAG检索能力
  const ragCapabilities = [
    { icon: <AimOutlined />, title: t('landingPage.knowledgeNetworkArch.cap1'), desc: t('landingPage.knowledgeNetworkArch.cap1Desc'), color: '#667eea' },
    { icon: <SearchOutlined />, title: t('landingPage.knowledgeNetworkArch.cap2'), desc: t('landingPage.knowledgeNetworkArch.cap2Desc'), color: '#50c878' },
    { icon: <SafetyOutlined />, title: t('landingPage.knowledgeNetworkArch.cap3'), desc: t('landingPage.knowledgeNetworkArch.cap3Desc'), color: '#f39c12' },
    { icon: <CheckCircleOutlined />, title: t('landingPage.knowledgeNetworkArch.cap4'), desc: t('landingPage.knowledgeNetworkArch.cap4Desc'), color: '#9b59b6' },
  ]

  // 核心优势
  const advantages = [
    { icon: '🎯', title: t('landingPage.knowledgeNetworkArch.adv1'), desc: t('landingPage.knowledgeNetworkArch.adv1Desc') },
    { icon: '🔗', title: t('landingPage.knowledgeNetworkArch.adv2'), desc: t('landingPage.knowledgeNetworkArch.adv2Desc') },
    { icon: '🧠', title: t('landingPage.knowledgeNetworkArch.adv3'), desc: t('landingPage.knowledgeNetworkArch.adv3Desc') },
    { icon: '⚡', title: t('landingPage.knowledgeNetworkArch.adv4'), desc: t('landingPage.knowledgeNetworkArch.adv4Desc') },
  ]

  return (
    <section className="knowledge-network-section">
      <div className="container">
        {/* 标题 */}
        <MotionDiv
          initial={{ opacity: 0, y: 30 }}
          whileInView={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.6 }}
          viewport={{ once: true }}
          className="section-header"
        >
          <Title level={2} className="section-title">
            {t('landingPage.knowledgeNetworkArch.title')}
          </Title>
          <Paragraph className="section-subtitle">
            {t('landingPage.knowledgeNetworkArch.subtitle')}
          </Paragraph>
        </MotionDiv>

        {/* 第一部分：传统RAG vs OmniAgent 对比 */}
        <MotionDiv
          initial={{ opacity: 0, y: 40 }}
          whileInView={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.6, delay: 0.2 }}
          viewport={{ once: true }}
          className="comparison-section"
        >
          <Row gutter={[32, 32]}>
            {/* 左侧：传统RAG的问题 */}
            <Col xs={24} lg={12}>
              <div className="comparison-card traditional">
                <div className="comparison-header">
                  <CloseCircleOutlined className="header-icon error" />
                  <Title level={3}>{t('landingPage.knowledgeNetworkArch.traditionalTitle')}</Title>
                </div>
                <div className="comparison-content">
                  {traditionalProblems.map((item, index) => (
                    <MotionDiv
                      key={index}
                      initial={{ opacity: 0, x: -20 }}
                      whileInView={{ opacity: 1, x: 0 }}
                      transition={{ duration: 0.4, delay: 0.1 * index }}
                      viewport={{ once: true }}
                      className="problem-item"
                    >
                      <span className="problem-icon">{item.icon}</span>
                      <div className="problem-text">
                        <Text strong>{item.title}</Text>
                        <Text type="secondary">{item.desc}</Text>
                      </div>
                    </MotionDiv>
                  ))}
                </div>
              </div>
            </Col>

            {/* 右侧：OmniAgent的解决方案 */}
            <Col xs={24} lg={12}>
              <div className="comparison-card solution">
                <div className="comparison-header">
                  <CheckCircleOutlined className="header-icon success" />
                  <Title level={3}>{t('landingPage.knowledgeNetworkArch.solutionTitle')}</Title>
                </div>
                <div className="comparison-content">
                  {solutions.map((item, index) => (
                    <MotionDiv
                      key={index}
                      initial={{ opacity: 0, x: 20 }}
                      whileInView={{ opacity: 1, x: 0 }}
                      transition={{ duration: 0.4, delay: 0.1 * index }}
                      viewport={{ once: true }}
                      className="solution-item"
                    >
                      <span className="solution-icon">{item.icon}</span>
                      <div className="solution-text">
                        <Text strong>{item.title}</Text>
                        <Text type="secondary">{item.desc}</Text>
                      </div>
                    </MotionDiv>
                  ))}
                </div>
              </div>
            </Col>
          </Row>
        </MotionDiv>

        {/* 第二部分：知识网络架构（4层） */}
        <MotionDiv
          initial={{ opacity: 0, y: 40 }}
          whileInView={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.6, delay: 0.3 }}
          viewport={{ once: true }}
          className="architecture-section"
        >
          <Title level={3} className="architecture-title">
            {t('landingPage.knowledgeNetworkArch.archTitle')}
          </Title>

          {/* 第1层：知识域层 */}
          <div className="layer layer-1">
            <div className="layer-label">
              <span className="layer-num">1</span>
              <span className="layer-name">{t('landingPage.knowledgeNetworkArch.layer1')}</span>
            </div>
            <div className="layer-content">
              <Row gutter={[16, 16]}>
                {knowledgeDomains.map((domain, index) => (
                  <Col xs={12} md={6} key={index}>
                    <MotionCard
                      className="domain-card"
                      whileHover={{ scale: 1.05, boxShadow: `0 8px 30px ${domain.color}40` }}
                      transition={{ type: 'spring', stiffness: 300 }}
                      style={{ borderTop: `4px solid ${domain.color}` }}
                    >
                      <div className="domain-icon" style={{ color: domain.color }}>
                        {domain.icon}
                      </div>
                      <Text strong className="domain-name">{domain.name}</Text>
                      <Text type="secondary" className="domain-desc">{domain.desc}</Text>
                      <div className="domain-dim" style={{ background: `${domain.color}20`, color: domain.color }}>
                        {domain.dim}
                      </div>
                    </MotionCard>
                  </Col>
                ))}
              </Row>
            </div>
          </div>

          {/* 连接箭头 */}
          <div className="layer-connector">
            <MotionDiv
              animate={{ y: [0, 8, 0] }}
              transition={{ repeat: Infinity, duration: 1.5 }}
              className="connector-arrow"
            >
              ↓
            </MotionDiv>
          </div>

          {/* 第2层：知识网络层 */}
          <div className="layer layer-2">
            <div className="layer-label">
              <span className="layer-num">2</span>
              <span className="layer-name">{t('landingPage.knowledgeNetworkArch.layer2')}</span>
            </div>
            <div className="layer-content network-layer">
              <div className="network-features">
                {networkFeatures.map((feature, index) => (
                  <MotionDiv
                    key={index}
                    whileHover={{ scale: 1.1 }}
                    className="network-feature"
                  >
                    <div className="feature-icon">{feature.icon}</div>
                    <Text>{feature.name}</Text>
                  </MotionDiv>
                ))}
              </div>
              {/* 知识图谱可视化 */}
              <div className="knowledge-graph">
                <svg viewBox="0 0 300 120" className="graph-svg">
                  {/* 节点 */}
                  <circle cx="50" cy="60" r="20" fill="#4a90e2" opacity="0.8" />
                  <circle cx="150" cy="30" r="18" fill="#50c878" opacity="0.8" />
                  <circle cx="250" cy="60" r="20" fill="#f39c12" opacity="0.8" />
                  <circle cx="150" cy="90" r="16" fill="#9b59b6" opacity="0.8" />
                  <circle cx="100" cy="60" r="12" fill="#667eea" opacity="0.6" />
                  <circle cx="200" cy="60" r="12" fill="#667eea" opacity="0.6" />
                  {/* 连线 */}
                  <line x1="50" y1="60" x2="100" y2="60" stroke="#667eea" strokeWidth="2" opacity="0.5" />
                  <line x1="100" y1="60" x2="150" y2="30" stroke="#667eea" strokeWidth="2" opacity="0.5" />
                  <line x1="150" y1="30" x2="200" y2="60" stroke="#667eea" strokeWidth="2" opacity="0.5" />
                  <line x1="200" y1="60" x2="250" y2="60" stroke="#667eea" strokeWidth="2" opacity="0.5" />
                  <line x1="100" y1="60" x2="150" y2="90" stroke="#667eea" strokeWidth="2" opacity="0.5" />
                  <line x1="150" y1="90" x2="200" y2="60" stroke="#667eea" strokeWidth="2" opacity="0.5" />
                </svg>
              </div>
            </div>
          </div>

          {/* 连接箭头 */}
          <div className="layer-connector">
            <MotionDiv
              animate={{ y: [0, 8, 0] }}
              transition={{ repeat: Infinity, duration: 1.5, delay: 0.3 }}
              className="connector-arrow"
            >
              ↓
            </MotionDiv>
          </div>

          {/* 第3层：角色知识提取层 */}
          <div className="layer layer-3">
            <div className="layer-label">
              <span className="layer-num">3</span>
              <span className="layer-name">{t('landingPage.knowledgeNetworkArch.layer3')}</span>
            </div>
            <div className="layer-content extraction-layer">
              <div className="extraction-steps">
                {extractionSteps.map((step, index) => (
                  <React.Fragment key={index}>
                    <MotionDiv
                      whileHover={{ scale: 1.05 }}
                      className="extraction-step"
                    >
                      <div className="step-num">{step.num}</div>
                      <div className="step-icon">{step.icon}</div>
                      <Text strong className="step-title">{step.title}</Text>
                      <Text type="secondary" className="step-desc">{step.desc}</Text>
                    </MotionDiv>
                    {index < extractionSteps.length - 1 && (
                      <div className="step-arrow">→</div>
                    )}
                  </React.Fragment>
                ))}
              </div>
            </div>
          </div>

          {/* 连接箭头 */}
          <div className="layer-connector">
            <MotionDiv
              animate={{ y: [0, 8, 0] }}
              transition={{ repeat: Infinity, duration: 1.5, delay: 0.6 }}
              className="connector-arrow"
            >
              ↓
            </MotionDiv>
          </div>

          {/* 第4层：RAG检索层 */}
          <div className="layer layer-4">
            <div className="layer-label">
              <span className="layer-num">4</span>
              <span className="layer-name">{t('landingPage.knowledgeNetworkArch.layer4')}</span>
            </div>
            <div className="layer-content">
              <Row gutter={[16, 16]}>
                {ragCapabilities.map((cap, index) => (
                  <Col xs={12} md={6} key={index}>
                    <MotionDiv
                      whileHover={{ scale: 1.05 }}
                      className="capability-card"
                      style={{ borderLeft: `4px solid ${cap.color}` }}
                    >
                      <div className="capability-icon" style={{ color: cap.color }}>
                        {cap.icon}
                      </div>
                      <Text strong>{cap.title}</Text>
                      <Text type="secondary">{cap.desc}</Text>
                    </MotionDiv>
                  </Col>
                ))}
              </Row>
            </div>
          </div>
        </MotionDiv>

        {/* 第三部分：核心优势总结 */}
        <MotionDiv
          initial={{ opacity: 0, y: 40 }}
          whileInView={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.6, delay: 0.4 }}
          viewport={{ once: true }}
          className="advantages-section"
        >
          <Title level={3} className="advantages-title">
            {t('landingPage.knowledgeNetworkArch.advTitle')}
          </Title>
          <Row gutter={[24, 24]}>
            {advantages.map((adv, index) => (
              <Col xs={24} sm={12} md={6} key={index}>
                <MotionCard
                  className="advantage-card"
                  whileHover={{
                    scale: 1.05,
                    boxShadow: '0 20px 40px rgba(102, 126, 234, 0.2)'
                  }}
                  transition={{ type: 'spring', stiffness: 300 }}
                >
                  <div className="advantage-icon">{adv.icon}</div>
                  <Title level={4}>{adv.title}</Title>
                  <Paragraph type="secondary">{adv.desc}</Paragraph>
                </MotionCard>
              </Col>
            ))}
          </Row>
        </MotionDiv>

        {/* 向量检索说明 */}
        <MotionDiv
          initial={{ opacity: 0, y: 30 }}
          whileInView={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.6, delay: 0.5 }}
          viewport={{ once: true }}
          className="vector-note"
        >
          <Card className="vector-note-card">
            <Title level={4}>💡 {t('landingPage.knowledgeNetworkArch.vectorQuestion')}</Title>
            <Paragraph>
              {t('landingPage.knowledgeNetworkArch.vectorAnswer')}
            </Paragraph>
          </Card>
        </MotionDiv>
      </div>
    </section>
  )
}

export default KnowledgeNetworkDiagram

