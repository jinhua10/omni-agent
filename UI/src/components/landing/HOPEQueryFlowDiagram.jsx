import React, { useMemo } from 'react';
import { useLanguage } from '../../contexts/LanguageContext';
import { AnimatedContainer } from '../common';
import '../../assets/css/landing/HOPEQueryFlowDiagram.css';

const HOPEQueryFlowDiagram = () => {
  const { language } = useLanguage();

  // 使用 useMemo 缓存翻译对象
  const t = useMemo(() => {
    const translations = {
      zh: {
        title: '完整查询流程',
        subtitle: 'HOPE 系统核心定位',

        // 步骤1
        step1: '1. 用户提问',
        step1Question: '"如何实现用户认证？"',

        // 步骤2
        step2: '2. Conversation Manager',
        step2Item1: '创建/获取对话',
        step2Item2: '管理历史消息',

        // 步骤3
        step3: '3. Intent Analyzer',
        step3Item1: '分析用户意图',
        step3Item2: '提取关键实体',
        step3Item3: '识别缺失信息',

        // 步骤4 - HOPE 核心
        step4: '4. HOPE 系统介入',
        step4Core: '⭐ 核心 ⭐',

        // 4.1 问题分类
        step4_1: '4.1 问题分类',
        step4_1_title: 'QuestionClassifier',
        step4_1_input: '输入: "如何实现用户认证？"',
        step4_1_output: '输出: questionType = "procedural" (步骤类)',

        // 4.2 层级选择
        step4_2: '4.2 层级选择',
        step4_2_title: 'Layer Manager',
        step4_2_suggest: '根据问题类型选择知识层级:',
        step4_2_result: 'procedural → 建议使用 "ordinary" 层',
        step4_2_priority: '查询优先级:',

        // 高频层
        highFreq: 'High Frequency Layer (高频层)',
        highFreqCheck: '检查最近是否有类似问题',
        highFreqHit: '命中: 直接返回',

        // 普通层
        ordinary: 'Ordinary Layer (普通层)',
        ordinaryRAG: 'RAG 语义搜索相关文档',
        ordinaryFound: '找到: Spring Security + JWT 文档',

        // 持久层
        permanent: 'Permanent Layer (持久层)',
        permanentCore: '查找核心认证概念',
        permanentFound: '找到: 用户认证最佳实践',

        // 4.3 自学习
        step4_3: '4.3 自学习机制',
        step4_3_title: 'Learning Module',
        step4_3_item1: '记录查询统计',
        step4_3_item2: '更新访问频率',
        step4_3_item3: '调整知识层级 (如果需要)',
        step4_3_item4: '学习用户偏好',

        // 步骤5-9
        step5: '5. Domain Router',
        step5Item: '路由到相关域',
        step5Domain: 'security-domain',

        step6: '6. Knowledge Extraction',
        step6Item1: 'RAG 语义搜索',
        step6Item2: '跨域查询',
        step6Item3: '用户偏好优化',

        step7: '7. Knowledge Gap Detection',
        step7Item1: '评估知识完整性',
        step7Item2: '识别缺口',
        step7Item3: '生成问题（如需要）',

        step8: '8. Response Generation',
        step8Item1: '整合多源知识',
        step8Item2: 'AI 生成回答',
        step8Item3: '格式化输出',

        step9: '9. 返回给用户',
        step9Content: '包含: 答案 + 代码 + 建议',
      },
      en: {
        title: 'Complete Query Flow',
        subtitle: 'HOPE System Core Position',

        step1: '1. User Question',
        step1Question: '"How to implement user authentication?"',

        step2: '2. Conversation Manager',
        step2Item1: 'Create/Get conversation',
        step2Item2: 'Manage history messages',

        step3: '3. Intent Analyzer',
        step3Item1: 'Analyze user intent',
        step3Item2: 'Extract key entities',
        step3Item3: 'Identify missing info',

        step4: '4. HOPE System Intervention',
        step4Core: '⭐ CORE ⭐',

        step4_1: '4.1 Question Classification',
        step4_1_title: 'QuestionClassifier',
        step4_1_input: 'Input: "How to implement user authentication?"',
        step4_1_output: 'Output: questionType = "procedural"',

        step4_2: '4.2 Layer Selection',
        step4_2_title: 'Layer Manager',
        step4_2_suggest: 'Select knowledge layer by question type:',
        step4_2_result: 'procedural → Suggest "ordinary" layer',
        step4_2_priority: 'Query Priority:',

        highFreq: 'High Frequency Layer',
        highFreqCheck: 'Check for similar recent questions',
        highFreqHit: 'Hit: Return directly',

        ordinary: 'Ordinary Layer',
        ordinaryRAG: 'RAG semantic search related docs',
        ordinaryFound: 'Found: Spring Security + JWT docs',

        permanent: 'Permanent Layer',
        permanentCore: 'Search core auth concepts',
        permanentFound: 'Found: User auth best practices',

        step4_3: '4.3 Self-Learning',
        step4_3_title: 'Learning Module',
        step4_3_item1: 'Record query statistics',
        step4_3_item2: 'Update access frequency',
        step4_3_item3: 'Adjust knowledge layers (if needed)',
        step4_3_item4: 'Learn user preferences',

        step5: '5. Domain Router',
        step5Item: 'Route to related domains',
        step5Domain: 'security-domain',

        step6: '6. Knowledge Extraction',
        step6Item1: 'RAG semantic search',
        step6Item2: 'Cross-domain query',
        step6Item3: 'User preference optimization',

        step7: '7. Knowledge Gap Detection',
        step7Item1: 'Evaluate knowledge completeness',
        step7Item2: 'Identify gaps',
        step7Item3: 'Generate questions (if needed)',

        step8: '8. Response Generation',
        step8Item1: 'Integrate multi-source knowledge',
        step8Item2: 'AI generate answer',
        step8Item3: 'Format output',

        step9: '9. Return to User',
        step9Content: 'Contains: Answer + Code + Suggestions',
      },
    };

    return translations[language] || translations.zh;
  }, [language]);

  return (
    <div className="hope-query-flow-diagram">
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
        {/* 步骤 1: 用户提问 */}
        <AnimatedContainer
          className="flow-step step-user"
          initial={{ opacity: 0, x: -20 }}
          animate={{ opacity: 1, x: 0 }}
          transition={{ delay: 0.2 }}
        >
          <div className="step-header">{t.step1}</div>
          <div className="step-content">
            <div className="user-question">{t.step1Question}</div>
          </div>
        </AnimatedContainer>

        <div className="flow-arrow">↓</div>

        {/* 步骤 2: Conversation Manager */}
        <AnimatedContainer
          className="flow-step step-normal"
          initial={{ opacity: 0, x: 20 }}
          animate={{ opacity: 1, x: 0 }}
          transition={{ delay: 0.3 }}
        >
          <div className="step-header">{t.step2}</div>
          <div className="step-content">
            <div className="step-item">• {t.step2Item1}</div>
            <div className="step-item">• {t.step2Item2}</div>
          </div>
        </AnimatedContainer>

        <div className="flow-arrow">↓</div>

        {/* 步骤 3: Intent Analyzer */}
        <AnimatedContainer
          className="flow-step step-normal"
          initial={{ opacity: 0, x: -20 }}
          animate={{ opacity: 1, x: 0 }}
          transition={{ delay: 0.4 }}
        >
          <div className="step-header">{t.step3}</div>
          <div className="step-content">
            <div className="step-item">• {t.step3Item1}</div>
            <div className="step-item">• {t.step3Item2}</div>
            <div className="step-item">• {t.step3Item3}</div>
          </div>
        </AnimatedContainer>

        <div className="flow-arrow">↓</div>

        {/* 步骤 4: HOPE 系统核心 */}
        <AnimatedContainer
          className="flow-step step-hope"
          initial={{ opacity: 0, scale: 0.95 }}
          animate={{ opacity: 1, scale: 1 }}
          transition={{ delay: 0.5 }}
        >
          <div className="hope-header">
            <div className="hope-title">{t.step4} {t.step4Core}</div>
          </div>

          {/* 4.1 问题分类 */}
          <div className="hope-section">
            <div className="section-title">{t.step4_1}</div>
            <div className="section-content">
              <div className="content-line">{t.step4_1_input}</div>
              <div className="content-line output">{t.step4_1_output}</div>
            </div>
          </div>

          <div className="section-arrow">↓</div>

          {/* 4.2 层级选择 */}
          <div className="hope-section">
            <div className="section-title">{t.step4_2}</div>
            <div className="section-content">
              <div className="content-line">{t.step4_2_suggest}</div>
              <div className="content-line result">{t.step4_2_result}</div>
              <div className="content-line priority">{t.step4_2_priority}</div>

              {/* 三层检索 */}
              <div className="layers-container">
                {/* 高频层 */}
                <div className="layer-box high-freq">
                  <div className="layer-number">1️⃣</div>
                  <div className="layer-name">{t.highFreq}</div>
                  <div className="layer-item">• {t.highFreqCheck}</div>
                  <div className="layer-item hit">• {t.highFreqHit} ✅</div>
                </div>

                {/* 普通层 */}
                <div className="layer-box ordinary">
                  <div className="layer-number">2️⃣</div>
                  <div className="layer-name">{t.ordinary}</div>
                  <div className="layer-item">• {t.ordinaryRAG}</div>
                  <div className="layer-item">• {t.ordinaryFound}</div>
                </div>

                {/* 持久层 */}
                <div className="layer-box permanent">
                  <div className="layer-number">3️⃣</div>
                  <div className="layer-name">{t.permanent}</div>
                  <div className="layer-item">• {t.permanentCore}</div>
                  <div className="layer-item">• {t.permanentFound}</div>
                </div>
              </div>
            </div>
          </div>

          <div className="section-arrow">↓</div>

          {/* 4.3 自学习机制 */}
          <div className="hope-section">
            <div className="section-title">{t.step4_3}</div>
            <div className="section-subtitle">{t.step4_3_title}</div>
            <div className="section-content">
              <div className="step-item">• {t.step4_3_item1}</div>
              <div className="step-item">• {t.step4_3_item2}</div>
              <div className="step-item">• {t.step4_3_item3}</div>
              <div className="step-item">• {t.step4_3_item4}</div>
            </div>
          </div>
        </AnimatedContainer>

        <div className="flow-arrow">↓</div>

        {/* 后续步骤 - 紧凑布局 */}
        <div className="compact-steps">
          {/* 步骤 5-6 */}
          <div className="compact-row">
            <AnimatedContainer
              className="flow-step step-compact"
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ delay: 0.6 }}
            >
              <div className="step-header">{t.step5}</div>
              <div className="step-content">
                <div className="step-item">• {t.step5Item}</div>
                <div className="step-item domain">→ {t.step5Domain}</div>
              </div>
            </AnimatedContainer>

            <AnimatedContainer
              className="flow-step step-compact"
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ delay: 0.7 }}
            >
              <div className="step-header">{t.step6}</div>
              <div className="step-content">
                <div className="step-item">• {t.step6Item1}</div>
                <div className="step-item">• {t.step6Item2}</div>
                <div className="step-item">• {t.step6Item3}</div>
              </div>
            </AnimatedContainer>
          </div>

          {/* 步骤 7-8 */}
          <div className="compact-row">
            <AnimatedContainer
              className="flow-step step-compact"
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ delay: 0.8 }}
            >
              <div className="step-header">{t.step7}</div>
              <div className="step-content">
                <div className="step-item">• {t.step7Item1}</div>
                <div className="step-item">• {t.step7Item2}</div>
                <div className="step-item">• {t.step7Item3}</div>
              </div>
            </AnimatedContainer>

            <AnimatedContainer
              className="flow-step step-compact"
              initial={{ opacity: 0, y: 20 }}
              animate={{ opacity: 1, y: 0 }}
              transition={{ delay: 0.9 }}
            >
              <div className="step-header">{t.step8}</div>
              <div className="step-content">
                <div className="step-item">• {t.step8Item1}</div>
                <div className="step-item">• {t.step8Item2}</div>
                <div className="step-item">• {t.step8Item3}</div>
              </div>
            </AnimatedContainer>
          </div>
        </div>

        <div className="flow-arrow">↓</div>

        {/* 步骤 9: 返回结果 */}
        <AnimatedContainer
          className="flow-step step-result"
          initial={{ opacity: 0, scale: 0.95 }}
          animate={{ opacity: 1, scale: 1 }}
          transition={{ delay: 1.0 }}
        >
          <div className="step-header">{t.step9}</div>
          <div className="step-content">
            <div className="result-content">{t.step9Content}</div>
          </div>
        </AnimatedContainer>
      </div>
    </div>
  );
};

export default React.memo(HOPEQueryFlowDiagram);


