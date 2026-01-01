import React, { useMemo } from 'react';
import { useLanguage } from '../../contexts/LanguageContext';
import { AnimatedContainer } from '../common';
import '../../assets/css/landing/DataFlowDiagram.css';

const DataFlowDiagram = () => {
  const { language } = useLanguage();

  // 使用 useMemo 缓存翻译对象
  const t = useMemo(() => {
    const translations = {
      zh: {
        title: '查询-学习-优化循环',
        subtitle: '完整的数据流转示意',

        // 查询阶段
        phase1: '查询阶段',
        userQuestion: '用户提问: "如何实现 Spring Boot JWT 认证？"',
        conversationMgr: 'Conversation Manager',
        conversationDesc: '管理对话上下文',
        intentAnalyzer: 'Intent Analyzer',
        intentDesc: '分析意图',
        intentResult: 'intent: "实现JWT认证"',
        intentType: 'type: "procedural"',
        intentEntities: 'entities: ["Spring Boot", "JWT", "认证"]',
        hopeClassifier: 'HOPE QuestionClassifier',
        questionType: '问题类型: procedural',
        suggestLayer: '建议层级: ordinary',

        // 检索阶段
        phase2: '检索阶段',
        layerManager: 'HOPE Layer Manager 分层检索策略',

        layer1: '1️⃣ 高频层 (HighFreq) - 优先检索',
        layer1Query: '查询: "Spring Boot JWT 认证"',
        layer1Result: '结果: 命中! (相似度: 0.95)',
        layer1Return: '返回: 缓存的完整答案 ✅',
        layer1Hit: '如果命中 → 直接返回答案（跳过后续检索）',
        layer1Miss: '如果未命中 ↓',

        layer2: '2️⃣ 普通层 (Ordinary) - 常规检索',
        layer2Router: 'Domain Router 路由到相关域',
        layer2Domain1: '→ security-domain',
        layer2Domain2: '→ authentication-domain',
        layer2RAG: 'RAG Service 语义搜索',
        layer2Result1: '→ Spring Security 配置 (0.92)',
        layer2Result2: '→ JWT Token 生成 (0.89)',
        layer2Result3: '→ 认证 Filter (0.85)',
        layer2Enough: '如果知识充足 → 生成答案',
        layer2NotEnough: '如果知识不足 ↓',

        layer3: '3️⃣ 持久层 (Permanent) - 核心知识',
        layer3Query: '查询核心概念和最佳实践',
        layer3Result1: '→ 用户认证原理 (0.88)',
        layer3Result2: '→ 安全认证最佳实践 (0.82)',

        // 知识评估阶段
        phase3: '知识评估阶段',
        gapManager: 'Knowledge Gap Manager',
        gapTitle: '评估知识完整性',
        gapScore: '知识完整性评分: 0.85 (良好) ✅',
        gapHave: '已有知识:',
        gapHave1: '✅ Spring Security 配置',
        gapHave2: '✅ JWT 生成和验证',
        gapHave3: '✅ 认证流程实现',
        gapMissing: '缺失知识: (无)',
        gapDecision: '决策: 可以生成完整答案',
        gapLow: '如果评分 < 0.7:',
        gapLowTitle: '知识不足，需要用户补充',
        gapQuestion1: '1. 您使用的 Spring Boot 版本？',
        gapQuestion2: '2. 是否需要第三方登录？',
        gapInteractive: '→ Interactive Learner 交互式学习',

        // 回答生成阶段
        phase4: '回答生成阶段',
        responseGen: 'Response Generator 生成回答',
        step1: '1. 整合多层知识',
        step1Item1: '• 高频层: 最佳实践',
        step1Item2: '• 普通层: 具体实现',
        step1Item3: '• 持久层: 核心概念',
        step2: '2. AI 生成结构化回答',
        step2Item1: '• 步骤说明',
        step2Item2: '• 代码示例',
        step2Item3: '• 注意事项',
        step3: '3. 格式化输出',
        step3Item1: '• Markdown 格式',
        step3Item2: '• 语法高亮',
        step3Item3: '• 清晰排版',

        // 学习优化阶段
        phase5: '学习优化阶段 ⭐',
        learning: 'HOPE Learning Module 自学习机制',
        learn1: '✅ 1. 记录统计',
        learn1Title: 'LayerStats 更新:',
        learn1Item1: '• 查询次数 +1',
        learn1Item2: '• 命中率计算',
        learn1Item3: '• 平均响应时间更新',
        learn2: '✅ 2. 频率跟踪',
        learn2Query: '"Spring Boot JWT 认证"',
        learn2Count: '访问次数: 3 次 (今天)',
        learn2Total: '总访问: 5 次',
        learn2Threshold: '→ 达到阈值！',
        learn3: '✅ 3. 自动提升',
        learn3Decision: '决策: 提升到高频层 ⬆️',
        learn3Action: '操作:',
        learn3Item1: '1. 保存完整答案到高频层',
        learn3Item2: '2. 关联问题变体',
        learn3Item3: '3. 设置过期时间 (7天)',
        learn4: '✅ 4. 用户偏好学习',
        learn4Title: 'UserPreferenceLearner',
        learn4Item1: '• 技术栈偏好: Spring Boot',
        learn4Item2: '• 问题类型: procedural',
        learn4Item3: '• 详细程度: 代码示例优先',

        // 下次查询优化
        phase6: '下次查询优化 🚀',
        nextQuery: '下次类似问题: "Spring Boot JWT 怎么做？"',
        optimized: '⚡ 高频层直接命中!',
        path: '检索路径:',
        pathResult: 'HighFreq → 命中 ✅ (0.1秒)',
        pathSkip: '(跳过 Ordinary 和 Permanent 层)',
        performance: '性能提升:',
        perfBefore: '原来: 3秒 (多层检索 + AI生成)',
        perfAfter: '现在: 0.1秒 (直接返回)',
        perfGain: '提升: 30倍! 🎉',
      },
      en: {
        title: 'Query-Learn-Optimize Loop',
        subtitle: 'Complete Data Flow Diagram',

        phase1: 'Query Phase',
        userQuestion: 'User Question: "How to implement Spring Boot JWT auth?"',
        conversationMgr: 'Conversation Manager',
        conversationDesc: 'Manage conversation context',
        intentAnalyzer: 'Intent Analyzer',
        intentDesc: 'Analyze intent',
        intentResult: 'intent: "Implement JWT Auth"',
        intentType: 'type: "procedural"',
        intentEntities: 'entities: ["Spring Boot", "JWT", "Auth"]',
        hopeClassifier: 'HOPE QuestionClassifier',
        questionType: 'Question Type: procedural',
        suggestLayer: 'Suggest Layer: ordinary',

        phase2: 'Retrieval Phase',
        layerManager: 'HOPE Layer Manager Multi-layer Strategy',

        layer1: '1️⃣ High Frequency Layer - Priority',
        layer1Query: 'Query: "Spring Boot JWT Auth"',
        layer1Result: 'Result: Hit! (Similarity: 0.95)',
        layer1Return: 'Return: Cached complete answer ✅',
        layer1Hit: 'If Hit → Return directly (Skip subsequent)',
        layer1Miss: 'If Miss ↓',

        layer2: '2️⃣ Ordinary Layer - Regular',
        layer2Router: 'Domain Router to related domains',
        layer2Domain1: '→ security-domain',
        layer2Domain2: '→ authentication-domain',
        layer2RAG: 'RAG Service semantic search',
        layer2Result1: '→ Spring Security Config (0.92)',
        layer2Result2: '→ JWT Token Generation (0.89)',
        layer2Result3: '→ Auth Filter (0.85)',
        layer2Enough: 'If Sufficient → Generate answer',
        layer2NotEnough: 'If Insufficient ↓',

        layer3: '3️⃣ Permanent Layer - Core',
        layer3Query: 'Query core concepts and best practices',
        layer3Result1: '→ User Auth Principles (0.88)',
        layer3Result2: '→ Security Auth Best Practices (0.82)',

        phase3: 'Knowledge Evaluation Phase',
        gapManager: 'Knowledge Gap Manager',
        gapTitle: 'Evaluate Knowledge Completeness',
        gapScore: 'Completeness Score: 0.85 (Good) ✅',
        gapHave: 'Available Knowledge:',
        gapHave1: '✅ Spring Security Config',
        gapHave2: '✅ JWT Generation & Validation',
        gapHave3: '✅ Auth Flow Implementation',
        gapMissing: 'Missing Knowledge: (None)',
        gapDecision: 'Decision: Can generate complete answer',
        gapLow: 'If Score < 0.7:',
        gapLowTitle: 'Insufficient, need user input',
        gapQuestion1: '1. Your Spring Boot version?',
        gapQuestion2: '2. Need third-party login?',
        gapInteractive: '→ Interactive Learner',

        phase4: 'Response Generation Phase',
        responseGen: 'Response Generator',
        step1: '1. Integrate Multi-layer Knowledge',
        step1Item1: '• High Freq: Best Practices',
        step1Item2: '• Ordinary: Specific Implementation',
        step1Item3: '• Permanent: Core Concepts',
        step2: '2. AI Generate Structured Answer',
        step2Item1: '• Step Instructions',
        step2Item2: '• Code Examples',
        step2Item3: '• Notes',
        step3: '3. Format Output',
        step3Item1: '• Markdown Format',
        step3Item2: '• Syntax Highlighting',
        step3Item3: '• Clear Layout',

        phase5: 'Learning & Optimization Phase ⭐',
        learning: 'HOPE Learning Module Self-Learning',
        learn1: '✅ 1. Record Statistics',
        learn1Title: 'LayerStats Update:',
        learn1Item1: '• Query Count +1',
        learn1Item2: '• Hit Rate Calculation',
        learn1Item3: '• Avg Response Time Update',
        learn2: '✅ 2. Frequency Tracking',
        learn2Query: '"Spring Boot JWT Auth"',
        learn2Count: 'Access Count: 3 times (today)',
        learn2Total: 'Total Access: 5 times',
        learn2Threshold: '→ Threshold Reached!',
        learn3: '✅ 3. Auto Promotion',
        learn3Decision: 'Decision: Promote to High Freq ⬆️',
        learn3Action: 'Actions:',
        learn3Item1: '1. Save complete answer to high freq',
        learn3Item2: '2. Associate question variants',
        learn3Item3: '3. Set expiration (7 days)',
        learn4: '✅ 4. User Preference Learning',
        learn4Title: 'UserPreferenceLearner',
        learn4Item1: '• Tech Stack: Spring Boot',
        learn4Item2: '• Question Type: procedural',
        learn4Item3: '• Detail Level: Code examples first',

        phase6: 'Next Query Optimization 🚀',
        nextQuery: 'Next Similar: "How to do Spring Boot JWT?"',
        optimized: '⚡ High Freq Direct Hit!',
        path: 'Retrieval Path:',
        pathResult: 'HighFreq → Hit ✅ (0.1s)',
        pathSkip: '(Skip Ordinary & Permanent layers)',
        performance: 'Performance Boost:',
        perfBefore: 'Before: 3s (Multi-layer + AI gen)',
        perfAfter: 'Now: 0.1s (Direct return)',
        perfGain: 'Boost: 30x! 🎉',
      },
    };

    return translations[language] || translations.zh;
  }, [language]);

  return (
    <div className="data-flow-diagram">
      {/* 标题 */}
      <AnimatedContainer
        className="flow-title"
        initial={{ opacity: 0, y: -20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.6 }}
      >
        <h2>{t.title}</h2>
        <p className="flow-subtitle">{t.subtitle}</p>
      </AnimatedContainer>

      <div className="flow-container">
        {/* 阶段1: 查询阶段 */}
        <AnimatedContainer
          className="phase-section phase-query"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 0.2 }}
        >
          <div className="phase-header">{t.phase1}</div>

          <div className="user-question-box">
            {t.userQuestion}
          </div>

          <div className="process-flow">
            <div className="process-item">
              <div className="process-name">{t.conversationMgr}</div>
              <div className="process-desc">{t.conversationDesc}</div>
            </div>

            <div className="process-item">
              <div className="process-name">{t.intentAnalyzer}</div>
              <div className="process-desc">{t.intentDesc}</div>
              <div className="intent-result">
                <div>• {t.intentResult}</div>
                <div>• {t.intentType}</div>
                <div>• {t.intentEntities}</div>
              </div>
            </div>

            <div className="process-item hope-core">
              <div className="process-name">⭐ {t.hopeClassifier} ⭐</div>
              <div className="process-result">
                <div>→ {t.questionType}</div>
                <div>→ {t.suggestLayer}</div>
              </div>
            </div>
          </div>
        </AnimatedContainer>

        <div className="phase-arrow">↓</div>

        {/* 阶段2: 检索阶段 */}
        <AnimatedContainer
          className="phase-section phase-retrieval"
          initial={{ opacity: 0 }}
          animate={{ opacity: 1 }}
          transition={{ delay: 0.4 }}
        >
          <div className="phase-header">{t.phase2}</div>
          <div className="phase-subtitle">{t.layerManager}</div>

          {/* 三层检索 */}
          <div className="layers-retrieval">
            {/* 高频层 */}
            <div className="retrieval-layer layer-highfreq">
              <div className="data-flow-layer-header">{t.layer1}</div>
              <div className="data-flow-layer-content">
                <div>• {t.layer1Query}</div>
                <div>• {t.layer1Result}</div>
                <div className="highlight">• {t.layer1Return}</div>
              </div>
              <div className="data-flow-layer-decision">
                <div className="hit">✓ {t.layer1Hit}</div>
                <div className="miss">✗ {t.layer1Miss}</div>
              </div>
            </div>

            {/* 普通层 */}
            <div className="retrieval-layer layer-ordinary">
              <div className="data-flow-layer-header">{t.layer2}</div>
              <div className="data-flow-layer-content">
                <div className="sub-title">{t.layer2Router}</div>
                <div className="indent">• {t.layer2Domain1}</div>
                <div className="indent">• {t.layer2Domain2}</div>
                <div className="sub-title">{t.layer2RAG}</div>
                <div className="indent">• {t.layer2Result1}</div>
                <div className="indent">• {t.layer2Result2}</div>
                <div className="indent">• {t.layer2Result3}</div>
              </div>
              <div className="data-flow-layer-decision">
                <div className="enough">✓ {t.layer2Enough}</div>
                <div className="not-enough">✗ {t.layer2NotEnough}</div>
              </div>
            </div>

            {/* 持久层 */}
            <div className="retrieval-layer layer-permanent">
              <div className="data-flow-layer-header">{t.layer3}</div>
              <div className="data-flow-layer-content">
                <div>• {t.layer3Query}</div>
                <div className="indent">• {t.layer3Result1}</div>
                <div className="indent">• {t.layer3Result2}</div>
              </div>
            </div>
          </div>
        </AnimatedContainer>

        <div className="phase-arrow">↓</div>

        {/* 阶段3-5: 紧凑布局 */}
        <div className="compact-phases">
          {/* 知识评估 */}
          <AnimatedContainer
            className="phase-section phase-compact"
            initial={{ opacity: 0, x: -20 }}
            animate={{ opacity: 1, x: 0 }}
            transition={{ delay: 0.6 }}
          >
            <div className="phase-header">{t.phase3}</div>
            <div className="compact-content">
              <div className="data-flow-content-title">{t.gapManager}</div>
              <div className="gap-score">{t.gapScore}</div>
              <div className="gap-list">
                <div className="list-title">{t.gapHave}</div>
                <div>{t.gapHave1}</div>
                <div>{t.gapHave2}</div>
                <div>{t.gapHave3}</div>
              </div>
              <div className="gap-decision">{t.gapDecision}</div>
            </div>
          </AnimatedContainer>

          {/* 回答生成 */}
          <AnimatedContainer
            className="phase-section phase-compact"
            initial={{ opacity: 0, x: 20 }}
            animate={{ opacity: 1, x: 0 }}
            transition={{ delay: 0.7 }}
          >
            <div className="phase-header">{t.phase4}</div>
            <div className="compact-content">
              <div className="data-flow-content-title">{t.responseGen}</div>
              <div className="steps-list">
                <div className="data-flow-step-group">
                  <div className="data-flow-step-title">{t.step1}</div>
                  <div className="data-flow-step-item">{t.step1Item1}</div>
                  <div className="data-flow-step-item">{t.step1Item2}</div>
                  <div className="data-flow-step-item">{t.step1Item3}</div>
                </div>
              </div>
            </div>
          </AnimatedContainer>
        </div>

        <div className="phase-arrow">↓</div>

        {/* 阶段5: 学习优化 */}
        <AnimatedContainer
          className="phase-section phase-learning"
          initial={{ opacity: 0, scale: 0.95 }}
          animate={{ opacity: 1, scale: 1 }}
          transition={{ delay: 0.9 }}
        >
          <div className="phase-header">{t.phase5}</div>
          <div className="phase-subtitle">{t.learning}</div>

          <div className="learning-grid">
            {/* 记录统计 */}
            <div className="learning-box">
              <div className="learning-title">{t.learn1}</div>
              <div className="learning-subtitle">{t.learn1Title}</div>
              <div className="learning-content">
                <div>{t.learn1Item1}</div>
                <div>{t.learn1Item2}</div>
                <div>{t.learn1Item3}</div>
              </div>
            </div>

            {/* 频率跟踪 */}
            <div className="learning-box">
              <div className="learning-title">{t.learn2}</div>
              <div className="learning-query">{t.learn2Query}</div>
              <div className="learning-content">
                <div>{t.learn2Count}</div>
                <div>{t.learn2Total}</div>
                <div className="threshold">{t.learn2Threshold}</div>
              </div>
            </div>

            {/* 自动提升 */}
            <div className="learning-box highlight">
              <div className="learning-title">{t.learn3}</div>
              <div className="learning-decision">{t.learn3Decision}</div>
              <div className="learning-subtitle">{t.learn3Action}</div>
              <div className="learning-content">
                <div>{t.learn3Item1}</div>
                <div>{t.learn3Item2}</div>
                <div>{t.learn3Item3}</div>
              </div>
            </div>

            {/* 用户偏好 */}
            <div className="learning-box">
              <div className="learning-title">{t.learn4}</div>
              <div className="learning-subtitle">{t.learn4Title}</div>
              <div className="learning-content">
                <div>{t.learn4Item1}</div>
                <div>{t.learn4Item2}</div>
                <div>{t.learn4Item3}</div>
              </div>
            </div>
          </div>
        </AnimatedContainer>

        <div className="phase-arrow">↓</div>

        {/* 阶段6: 下次优化 */}
        <AnimatedContainer
          className="phase-section phase-optimized"
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: 1.1 }}
        >
          <div className="phase-header">{t.phase6}</div>

          <div className="next-query">{t.nextQuery}</div>

          <div className="optimized-result">
            <div className="optimized-title">{t.optimized}</div>

            <div className="result-details">
              <div className="detail-section">
                <div className="detail-title">{t.path}</div>
                <div className="detail-content">
                  <div className="success">{t.pathResult}</div>
                  <div>{t.pathSkip}</div>
                </div>
              </div>

              <div className="detail-section">
                <div className="detail-title">{t.performance}</div>
                <div className="detail-content">
                  <div>{t.perfBefore}</div>
                  <div>{t.perfAfter}</div>
                  <div className="boost">{t.perfGain}</div>
                </div>
              </div>
            </div>
          </div>
        </AnimatedContainer>
      </div>
    </div>
  );
};

export default React.memo(DataFlowDiagram);




