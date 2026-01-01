import React, { useMemo } from 'react';
import { motion } from 'framer-motion';
import { useLanguage } from '../../contexts/LanguageContext';
import '../../assets/css/landing/SystemArchitectureDiagram.css';

const SystemArchitectureDiagram = () => {
  const { language } = useLanguage();

  // 使用 useMemo 缓存翻译对象
  const t = useMemo(() => {
    const translations = {
      zh: {
        title: 'OmniAgent 智能问答系统',
        subtitle: '(基于 HOPE 的智能问答系统)',

      // 顶层
      userLayer: '用户交互层',
      orchestrationLayer: '编排协调层',
      knowledgeLayer: '知识处理层',

      // 核心智能层
      coreIntelligence: '核心智能处理层',

      // HOPE 系统
      hopeSystem: 'HOPE 自学习系统 ⭐核心⭐',
      hopeSubtitle: '(分层智能持久化引擎)',

      questionClassifier: '问题分类器',
      layerManager: '知识层级管理器',
      learningModule: '智能学习模块',

      permanentLayer: '持久层',
      ordinaryLayer: '普通层',
      highFreqLayer: '高频层',

      coreKnowledge: '核心知识',
      systemDocs: '系统文档',
      longTerm: '长期稳定',

      generalKnowledge: '一般性知识',
      businessDocs: '业务文档',
      dynamicUpdate: '动态更新',

      hotKnowledge: '热点知识',
      frequentAccess: '频繁访问',
      autoAdjust: '自动调整',

      // 对话管理
      conversationIntent: '对话管理与意图分析',
      conversationManager: '对话管理器',
      conversationDesc: '多轮对话',
      intentAnalyzer: '意图分析器',
      intentDesc: '意图理解',
      contextExtractor: '上下文提取器',
      contextDesc: '上下文提取',

      // 知识检索
      knowledgeRetrieval: '知识检索引擎',
      domainRouter: '域路由',
      domainRouterDesc: '域路由',
      knowledgeExtraction: '知识提取',
      knowledgeExtractionDesc: 'RAG 语义搜索',
      userPreference: '用户偏好',
      userPreferenceDesc: '个性化优化',
      crossDomainQuery: '跨域查询',
      crossDomainQueryDesc: '跨域查询',
      queryCache: '查询缓存',
      queryCacheDesc: '查询缓存',
      domainQuality: '域质量评分',
      domainQualityDesc: '质量评分',

      // 知识缺口
      knowledgeGap: '知识缺口管理',
      gapDetector: '缺口检测器',
      gapDetectorDesc: '缺口检测',
      interactiveLearner: '交互式学习器',
      interactiveLearnerDesc: '交互式学习',
      knowledgeValidator: '知识验证器',
      knowledgeValidatorDesc: '知识验证',

      // 响应生成
      responseGeneration: '响应生成',
      responseGenerator: '回答生成器',
      responseGeneratorDesc: '回答生成',
      answerFormatter: '格式化器',
      answerFormatterDesc: '格式化',
      knowledgeIntegrator: '知识整合器',
      knowledgeIntegratorDesc: '知识整合',

      // 底层服务
      baseServices: '基础服务层',
      storageLayer: '存储层',
      aiServices: 'AI 服务层',

      ragService: 'RAG Service',
      documentProcessing: 'Document Processing',
      chunking: 'Chunking',
      embedding: 'Embedding',

      fileStorage: 'File Storage',
      mongodb: 'MongoDB',
      elasticsearch: 'Elasticsearch',
      sqlite: 'SQLite',
      redis: 'Redis',
      h2: 'H2',

      onlineAPI: 'Online API',
      ollama: 'Ollama',
      customLLM: 'Custom LLM',
    },
    en: {
      title: 'OmniAgent Intelligent Q&A System',
      subtitle: '(Intelligent Q&A System with HOPE)',

      // Top layer
      userLayer: 'User Interface Layer',
      orchestrationLayer: 'Orchestration Layer',
      knowledgeLayer: 'Knowledge Processing',

      // Core intelligence
      coreIntelligence: 'Core Intelligence Layer',

      // HOPE system
      hopeSystem: 'HOPE Self-Learning System ⭐CORE⭐',
      hopeSubtitle: '(Hierarchical Omni-Agent Persistent Engine)',

      questionClassifier: 'Question Classifier',
      layerManager: 'Layer Manager',
      learningModule: 'Learning Module',

      permanentLayer: 'Permanent',
      ordinaryLayer: 'Ordinary',
      highFreqLayer: 'High Freq',

      coreKnowledge: 'Core Knowledge',
      systemDocs: 'System Docs',
      longTerm: 'Long-term Stable',

      generalKnowledge: 'General Knowledge',
      businessDocs: 'Business Docs',
      dynamicUpdate: 'Dynamic Update',

      hotKnowledge: 'Hot Topics',
      frequentAccess: 'Frequent Access',
      autoAdjust: 'Auto Adjust',

      // Conversation
      conversationIntent: 'Conversation & Intent Analysis',
      conversationManager: 'Conversation Manager',
      conversationDesc: 'Multi-turn Dialog',
      intentAnalyzer: 'Intent Analyzer',
      intentDesc: 'Intent Understanding',
      contextExtractor: 'Context Extractor',
      contextDesc: 'Context Extraction',

      // Knowledge retrieval
      knowledgeRetrieval: 'Knowledge Retrieval Engine',
      domainRouter: 'Domain Router',
      domainRouterDesc: 'Domain Routing',
      knowledgeExtraction: 'Knowledge Extraction',
      knowledgeExtractionDesc: 'RAG Semantic Search',
      userPreference: 'User Preference',
      userPreferenceDesc: 'Personalization',
      crossDomainQuery: 'Cross-Domain Query',
      crossDomainQueryDesc: 'Cross-Domain',
      queryCache: 'Query Cache',
      queryCacheDesc: 'Query Caching',
      domainQuality: 'Domain Quality',
      domainQualityDesc: 'Quality Scoring',

      // Knowledge gap
      knowledgeGap: 'Knowledge Gap Management',
      gapDetector: 'Gap Detector',
      gapDetectorDesc: 'Gap Detection',
      interactiveLearner: 'Interactive Learner',
      interactiveLearnerDesc: 'Interactive Learning',
      knowledgeValidator: 'Knowledge Validator',
      knowledgeValidatorDesc: 'Validation',

      // Response generation
      responseGeneration: 'Response Generation',
      responseGenerator: 'Response Generator',
      responseGeneratorDesc: 'Answer Generation',
      answerFormatter: 'Answer Formatter',
      answerFormatterDesc: 'Formatting',
      knowledgeIntegrator: 'Knowledge Integrator',
      knowledgeIntegratorDesc: 'Integration',

      // Bottom services
      baseServices: 'Base Services Layer',
      storageLayer: 'Storage Layer',
      aiServices: 'AI Services Layer',

      ragService: 'RAG Service',
      documentProcessing: 'Document Processing',
      chunking: 'Chunking',
      embedding: 'Embedding',

      fileStorage: 'File Storage',
      mongodb: 'MongoDB',
      elasticsearch: 'Elasticsearch',
      sqlite: 'SQLite',
      redis: 'Redis',
      h2: 'H2',

      onlineAPI: 'Online API',
      ollama: 'Ollama',
      customLLM: 'Custom LLM',
    },
  };

  return translations[language] || translations.zh;
  }, [language]);

  return (
    <div className="system-architecture-diagram">
      {/* 标题 */}
      <motion.div
        className="architecture-title"
        initial={{ opacity: 0, y: -20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.6 }}
      >
        <h2>{t.title}</h2>
        <p className="subtitle">{t.subtitle}</p>
      </motion.div>

      <div className="architecture-container">
        {/* 顶层 - 三个层次 */}
        <motion.div
          className="top-layer"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 0.2 }}
        >
          <div className="layer-box user-layer">
            <div className="layer-title">{t.userLayer}</div>
          </div>

          <div className="layer-box orchestration-layer">
            <div className="layer-title">{t.orchestrationLayer}</div>
          </div>

          <div className="layer-box knowledge-layer">
            <div className="layer-title">{t.knowledgeLayer}</div>
          </div>
        </motion.div>

        {/* 核心智能层容器 */}
        <motion.div
          className="core-intelligence-container"
          initial={{ opacity: 0, scale: 0.95 }}
          animate={{ opacity: 1, scale: 1 }}
          transition={{ delay: 0.4 }}
        >
          <div className="section-header">
            <h3>{t.coreIntelligence}</h3>
          </div>

          {/* HOPE 系统 */}
          <motion.div
            className="hope-system-container"
            initial={{ opacity: 0 }}
            animate={{ opacity: 1 }}
            transition={{ delay: 0.6 }}
          >
            <div className="hope-header">
              <div className="hope-title">{t.hopeSystem}</div>
              <div className="hope-subtitle">{t.hopeSubtitle}</div>
            </div>

            {/* HOPE 三个组件 */}
            <div className="hope-components">
              <div className="hope-component">
                <div className="component-name">{t.questionClassifier}</div>
              </div>
              <div className="arrow">→</div>
              <div className="hope-component">
                <div className="component-name">{t.layerManager}</div>
              </div>
              <div className="arrow">→</div>
              <div className="hope-component">
                <div className="component-name">{t.learningModule}</div>
              </div>
            </div>

            {/* 三层知识结构 */}
            <div className="knowledge-layers">
              <div className="knowledge-layer permanent">
                <div className="layer-name">{t.permanentLayer}</div>
                <div className="layer-content">
                  <div>{t.coreKnowledge}</div>
                  <div>{t.systemDocs}</div>
                  <div>{t.longTerm}</div>
                </div>
              </div>

              <div className="knowledge-layer ordinary">
                <div className="layer-name">{t.ordinaryLayer}</div>
                <div className="layer-content">
                  <div>{t.generalKnowledge}</div>
                  <div>{t.businessDocs}</div>
                  <div>{t.dynamicUpdate}</div>
                </div>
              </div>

              <div className="knowledge-layer high-freq">
                <div className="layer-name">{t.highFreqLayer}</div>
                <div className="layer-content">
                  <div>{t.hotKnowledge}</div>
                  <div>{t.frequentAccess}</div>
                  <div>{t.autoAdjust}</div>
                </div>
              </div>
            </div>
          </motion.div>

          {/* 对话管理与意图分析 */}
          <motion.div
            className="feature-section"
            initial={{ opacity: 0, x: -20 }}
            animate={{ opacity: 1, x: 0 }}
            transition={{ delay: 0.8 }}
          >
            <div className="section-title">{t.conversationIntent}</div>
            <div className="feature-components">
              <div className="feature-item">
                <div className="item-title">{t.conversationManager}</div>
                <div className="item-desc">{t.conversationDesc}</div>
              </div>
              <div className="arrow">→</div>
              <div className="feature-item">
                <div className="item-title">{t.intentAnalyzer}</div>
                <div className="item-desc">{t.intentDesc}</div>
              </div>
              <div className="arrow">→</div>
              <div className="feature-item">
                <div className="item-title">{t.contextExtractor}</div>
                <div className="item-desc">{t.contextDesc}</div>
              </div>
            </div>
          </motion.div>

          {/* 知识检索引擎 */}
          <motion.div
            className="feature-section"
            initial={{ opacity: 0, x: 20 }}
            animate={{ opacity: 1, x: 0 }}
            transition={{ delay: 1.0 }}
          >
            <div className="section-title">{t.knowledgeRetrieval}</div>
            <div className="feature-grid">
              <div className="feature-item">
                <div className="item-title">{t.domainRouter}</div>
                <div className="item-desc">{t.domainRouterDesc}</div>
              </div>
              <div className="feature-item">
                <div className="item-title">{t.knowledgeExtraction}</div>
                <div className="item-desc">{t.knowledgeExtractionDesc}</div>
              </div>
              <div className="feature-item">
                <div className="item-title">{t.userPreference}</div>
                <div className="item-desc">{t.userPreferenceDesc}</div>
              </div>
              <div className="feature-item">
                <div className="item-title">{t.crossDomainQuery}</div>
                <div className="item-desc">{t.crossDomainQueryDesc}</div>
              </div>
              <div className="feature-item">
                <div className="item-title">{t.queryCache}</div>
                <div className="item-desc">{t.queryCacheDesc}</div>
              </div>
              <div className="feature-item">
                <div className="item-title">{t.domainQuality}</div>
                <div className="item-desc">{t.domainQualityDesc}</div>
              </div>
            </div>
          </motion.div>

          {/* 知识缺口管理 */}
          <motion.div
            className="feature-section"
            initial={{ opacity: 0, x: -20 }}
            animate={{ opacity: 1, x: 0 }}
            transition={{ delay: 1.2 }}
          >
            <div className="section-title">{t.knowledgeGap}</div>
            <div className="feature-components">
              <div className="feature-item">
                <div className="item-title">{t.gapDetector}</div>
                <div className="item-desc">{t.gapDetectorDesc}</div>
              </div>
              <div className="arrow">→</div>
              <div className="feature-item">
                <div className="item-title">{t.interactiveLearner}</div>
                <div className="item-desc">{t.interactiveLearnerDesc}</div>
              </div>
              <div className="arrow">→</div>
              <div className="feature-item">
                <div className="item-title">{t.knowledgeValidator}</div>
                <div className="item-desc">{t.knowledgeValidatorDesc}</div>
              </div>
            </div>
          </motion.div>

          {/* 响应生成 */}
          <motion.div
            className="feature-section"
            initial={{ opacity: 0, x: 20 }}
            animate={{ opacity: 1, x: 0 }}
            transition={{ delay: 1.4 }}
          >
            <div className="section-title">{t.responseGeneration}</div>
            <div className="feature-components">
              <div className="feature-item">
                <div className="item-title">{t.responseGenerator}</div>
                <div className="item-desc">{t.responseGeneratorDesc}</div>
              </div>
              <div className="arrow">→</div>
              <div className="feature-item">
                <div className="item-title">{t.answerFormatter}</div>
                <div className="item-desc">{t.answerFormatterDesc}</div>
              </div>
              <div className="arrow">→</div>
              <div className="feature-item">
                <div className="item-title">{t.knowledgeIntegrator}</div>
                <div className="item-desc">{t.knowledgeIntegratorDesc}</div>
              </div>
            </div>
          </motion.div>
        </motion.div>

        {/* 底层服务 */}
        <motion.div
          className="bottom-layer"
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: 1.6 }}
        >
          <div className="service-box base-services">
            <div className="service-title">{t.baseServices}</div>
            <div className="service-list">
              <div>• {t.ragService}</div>
              <div>• {t.documentProcessing}</div>
              <div>• {t.chunking}</div>
              <div>• {t.embedding}</div>
            </div>
          </div>

          <div className="service-box storage-services">
            <div className="service-title">{t.storageLayer}</div>
            <div className="service-list">
              <div>• {t.fileStorage}</div>
              <div>• {t.mongodb}</div>
              <div>• {t.elasticsearch}</div>
              <div>• {t.sqlite}</div>
              <div>• {t.redis}</div>
              <div>• {t.h2}</div>
            </div>
          </div>

          <div className="service-box ai-services">
            <div className="service-title">{t.aiServices}</div>
            <div className="service-list">
              <div>• {t.onlineAPI}</div>
              <div>• {t.ollama}</div>
              <div>• {t.customLLM}</div>
            </div>
          </div>
        </motion.div>
      </div>
    </div>
  );
};

export default SystemArchitectureDiagram;

