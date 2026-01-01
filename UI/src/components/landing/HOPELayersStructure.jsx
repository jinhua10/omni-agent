import React, { useMemo, useState } from 'react';
import { useLanguage } from '../../contexts/LanguageContext';
import { AnimatedContainer } from '../common';
import '../../assets/css/landing/HOPELayersStructure.css';

const HOPELayersStructure = ({ selectedLayer = null }) => {
  const { language } = useLanguage();
  const [showAll, setShowAll] = useState(false);

  // 根据 selectedLayer 和 showAll 决定显示哪些层
  const shouldShowLayer = (layerType) => {
    if (!selectedLayer || showAll) return true; // 没有选择或点击"显示全部"时显示所有层
    return selectedLayer === layerType; // 只显示选中的层
  };

  // 使用 useMemo 缓存翻译对象
  const t = useMemo(() => {
    const translations = {
      zh: {
        title: 'HOPE 三层知识结构',
        subtitle: '分层智能持久化引擎',
        showAllLayers: '显示全部三层',

        // 持久层
        permanentTitle: '持久层',
        permanentSubtitle: 'Permanent Layer',
        permanentFeature: '特征:',
        permanentF1: '📌 长期稳定、很少变化',
        permanentF2: '🎓 权威可靠、经过验证',
        permanentF3: '🔒 手动管理、精心维护',
        permanentContent: '存储内容:',
        permanentC1: '• 系统核心概念定义',
        permanentC2: '• 官方文档和使用说明',
        permanentC3: '• 架构设计原则',
        permanentC4: '• 最佳实践和规范',
        permanentC5: '• 常见问题解答 (FAQ)',
        permanentType: '问题类型:',
        permanentT1: '• factual (事实类): "什么是RAG?"',
        permanentT2: '• conceptual (概念类): "微服务架构是什么?"',
        permanentPriority: '检索优先级: 🔴 最高 (第3顺位检索)',

        // 普通层
        ordinaryTitle: '普通层',
        ordinarySubtitle: 'Ordinary Layer',
        ordinaryFeature: '特征:',
        ordinaryF1: '🔄 动态更新、常规维护',
        ordinaryF2: '📚 一般性知识、业务文档',
        ordinaryF3: '🎯 常规检索、中等优先级',
        ordinaryContent: '存储内容:',
        ordinaryC1: '• 业务流程说明',
        ordinaryC2: '• 功能实现文档',
        ordinaryC3: '• 技术方案设计',
        ordinaryC4: '• 开发指南',
        ordinaryC5: '• 用户学习知识 (从对话中积累)',
        ordinaryType: '问题类型:',
        ordinaryT1: '• procedural (步骤类): "如何配置Spring Security?"',
        ordinaryT2: '• analytical (分析类): "为什么要用JWT?"',
        ordinaryPriority: '检索优先级: 🟡 中等 (第2顺位检索)',

        // 高频层
        highFreqTitle: '高频层',
        highFreqSubtitle: 'High Frequency Layer',
        highFreqFeature: '特征:',
        highFreqF1: '⚡ 自动调整、实时更新',
        highFreqF2: '🔥 热点知识、频繁访问',
        highFreqF3: '🚀 优先检索、最快响应',
        highFreqContent: '存储内容:',
        highFreqC1: '• 最近问答记录',
        highFreqC2: '• 高频问题答案',
        highFreqC3: '• 用户常问问题',
        highFreqC4: '• 热门话题知识',
        highFreqC5: '• 个性化知识缓存',
        highFreqMechanism: '自动管理机制:',
        highFreqM1Title: '1. 访问频率统计',
        highFreqM1_1: '• 记录每个知识点的访问次数',
        highFreqM1_2: '• 计算访问频率和时间衰减',
        highFreqM2Title: '2. 动态提升',
        highFreqM2_1: '• 访问次数 > 阈值 → 提升到高频层',
        highFreqM2_2: '• 最近1天访问3次以上',
        highFreqM3Title: '3. 自动降级',
        highFreqM3_1: '• 长期未访问 → 降回普通层',
        highFreqM3_2: '• 7天未访问自动清理',
        highFreqPriority: '检索优先级: 🟢 最高 (第1顺位检索)',
      },
      en: {
        title: 'HOPE Three-Layer Structure',
        subtitle: 'Hierarchical Omni-Agent Persistent Engine',
        showAllLayers: 'Show All Three Layers',

        // Permanent Layer
        permanentTitle: 'Permanent Layer',
        permanentSubtitle: 'Permanent Layer',
        permanentFeature: 'Features:',
        permanentF1: '📌 Long-term stable, rarely changes',
        permanentF2: '🎓 Authoritative, verified',
        permanentF3: '🔒 Manual management, carefully maintained',
        permanentContent: 'Storage Content:',
        permanentC1: '• Core system concept definitions',
        permanentC2: '• Official docs and instructions',
        permanentC3: '• Architecture design principles',
        permanentC4: '• Best practices and standards',
        permanentC5: '• FAQ (Frequently Asked Questions)',
        permanentType: 'Question Types:',
        permanentT1: '• factual: "What is RAG?"',
        permanentT2: '• conceptual: "What is microservices?"',
        permanentPriority: 'Priority: 🔴 Highest (3rd order)',

        // Ordinary Layer
        ordinaryTitle: 'Ordinary Layer',
        ordinarySubtitle: 'Ordinary Layer',
        ordinaryFeature: 'Features:',
        ordinaryF1: '🔄 Dynamic updates, regular maintenance',
        ordinaryF2: '📚 General knowledge, business docs',
        ordinaryF3: '🎯 Regular retrieval, medium priority',
        ordinaryContent: 'Storage Content:',
        ordinaryC1: '• Business process descriptions',
        ordinaryC2: '• Feature implementation docs',
        ordinaryC3: '• Technical solution designs',
        ordinaryC4: '• Development guides',
        ordinaryC5: '• User learning knowledge (from conversations)',
        ordinaryType: 'Question Types:',
        ordinaryT1: '• procedural: "How to configure Spring Security?"',
        ordinaryT2: '• analytical: "Why use JWT?"',
        ordinaryPriority: 'Priority: 🟡 Medium (2nd order)',

        // High Frequency Layer
        highFreqTitle: 'High Frequency Layer',
        highFreqSubtitle: 'High Frequency Layer',
        highFreqFeature: 'Features:',
        highFreqF1: '⚡ Auto-adjust, real-time updates',
        highFreqF2: '🔥 Hot topics, frequent access',
        highFreqF3: '🚀 Priority retrieval, fastest response',
        highFreqContent: 'Storage Content:',
        highFreqC1: '• Recent Q&A records',
        highFreqC2: '• High-frequency answers',
        highFreqC3: '• User common questions',
        highFreqC4: '• Popular topic knowledge',
        highFreqC5: '• Personalized knowledge cache',
        highFreqMechanism: 'Auto-Management:',
        highFreqM1Title: '1. Access Frequency Stats',
        highFreqM1_1: '• Record access count per knowledge',
        highFreqM1_2: '• Calculate frequency & time decay',
        highFreqM2Title: '2. Dynamic Promotion',
        highFreqM2_1: '• Access > threshold → Promote to high freq',
        highFreqM2_2: '• 3+ times in recent 1 day',
        highFreqM3Title: '3. Auto Demotion',
        highFreqM3_1: '• Long-term unused → Back to ordinary',
        highFreqM3_2: '• Auto-clean after 7 days',
        highFreqPriority: 'Priority: 🟢 Highest (1st order)',
      },
    };

    return translations[language] || translations.zh;
  }, [language]);

  return (
    <div className="hope-layers-structure">
      {/* 标题 */}
      <AnimatedContainer
        className="hope-layers-title"
        initial={{ opacity: 0, y: -20 }}
        animate={{ opacity: 1, y: 0 }}
        transition={{ duration: 0.6 }}
      >
        <h2>{t.title}</h2>
        <p className="hope-layers-subtitle">{t.subtitle}</p>

        {/* 显示全部按钮 - 仅在有 selectedLayer 且未显示全部时显示 */}
        {selectedLayer && !showAll && (
          <button
            className="show-all-btn"
            onClick={() => setShowAll(true)}
          >
            {t.showAllLayers}
          </button>
        )}
      </AnimatedContainer>

      {/* 三层结构 */}
      <div className="hope-layers-container">
        {/* 持久层 */}
        {shouldShowLayer('permanent') && (
          <AnimatedContainer
            className="hope-layers-card layer-permanent"
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ delay: 0.2 }}
          >
            <div className="hope-layers-layer-header">
            <div className="hope-layers-layer-title">{t.permanentTitle}</div>
            <div className="hope-layers-layer-subtitle">{t.permanentSubtitle}</div>
          </div>

          <div className="hope-layers-layer-body">
            {/* 特征 */}
            <div className="hope-layers-layer-section">
              <div className="hope-layers-section-title">{t.permanentFeature}</div>
              <div className="hope-layers-section-content">
                <div className="hope-layers-feature-item">{t.permanentF1}</div>
                <div className="hope-layers-feature-item">{t.permanentF2}</div>
                <div className="hope-layers-feature-item">{t.permanentF3}</div>
              </div>
            </div>

            {/* 存储内容 */}
            <div className="hope-layers-layer-section">
              <div className="hope-layers-section-title">{t.permanentContent}</div>
              <div className="hope-layers-content-box">
                <div className="hope-layers-content-item">{t.permanentC1}</div>
                <div className="hope-layers-content-item">{t.permanentC2}</div>
                <div className="hope-layers-content-item">{t.permanentC3}</div>
                <div className="hope-layers-content-item">{t.permanentC4}</div>
                <div className="hope-layers-content-item">{t.permanentC5}</div>
              </div>
            </div>

            {/* 问题类型 */}
            <div className="hope-layers-layer-section">
              <div className="hope-layers-section-title">{t.permanentType}</div>
              <div className="hope-layers-section-content">
                <div className="hope-layers-type-item">{t.permanentT1}</div>
                <div className="hope-layers-type-item">{t.permanentT2}</div>
              </div>
            </div>

            {/* 检索优先级 */}
            <div className="hope-layers-priority">
              {t.permanentPriority}
            </div>
          </div>
        </AnimatedContainer>
        )}

        {/* 普通层 */}
        {shouldShowLayer('ordinary') && (
          <AnimatedContainer
          className="hope-layers-card layer-ordinary"
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: 0.4 }}
        >
          <div className="hope-layers-layer-header">
            <div className="hope-layers-layer-title">{t.ordinaryTitle}</div>
            <div className="hope-layers-layer-subtitle">{t.ordinarySubtitle}</div>
          </div>

          <div className="hope-layers-layer-body">
            {/* 特征 */}
            <div className="hope-layers-layer-section">
              <div className="hope-layers-section-title">{t.ordinaryFeature}</div>
              <div className="hope-layers-section-content">
                <div className="hope-layers-feature-item">{t.ordinaryF1}</div>
                <div className="hope-layers-feature-item">{t.ordinaryF2}</div>
                <div className="hope-layers-feature-item">{t.ordinaryF3}</div>
              </div>
            </div>

            {/* 存储内容 */}
            <div className="hope-layers-layer-section">
              <div className="hope-layers-section-title">{t.ordinaryContent}</div>
              <div className="hope-layers-content-box">
                <div className="hope-layers-content-item">{t.ordinaryC1}</div>
                <div className="hope-layers-content-item">{t.ordinaryC2}</div>
                <div className="hope-layers-content-item">{t.ordinaryC3}</div>
                <div className="hope-layers-content-item">{t.ordinaryC4}</div>
                <div className="hope-layers-content-item">{t.ordinaryC5}</div>
              </div>
            </div>

            {/* 问题类型 */}
            <div className="hope-layers-layer-section">
              <div className="hope-layers-section-title">{t.ordinaryType}</div>
              <div className="hope-layers-section-content">
                <div className="hope-layers-type-item">{t.ordinaryT1}</div>
                <div className="hope-layers-type-item">{t.ordinaryT2}</div>
              </div>
            </div>

            {/* 检索优先级 */}
            <div className="hope-layers-priority">
              {t.ordinaryPriority}
            </div>
          </div>
        </AnimatedContainer>
        )}

        {/* 高频层 */}
        {shouldShowLayer('highfreq') && (
          <AnimatedContainer
          className="hope-layers-card layer-highfreq"
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ delay: 0.6 }}
        >
          <div className="hope-layers-layer-header">
            <div className="hope-layers-layer-title">{t.highFreqTitle}</div>
            <div className="hope-layers-layer-subtitle">{t.highFreqSubtitle}</div>
          </div>

          <div className="hope-layers-layer-body">
            {/* 特征 */}
            <div className="hope-layers-layer-section">
              <div className="hope-layers-section-title">{t.highFreqFeature}</div>
              <div className="hope-layers-section-content">
                <div className="hope-layers-feature-item">{t.highFreqF1}</div>
                <div className="hope-layers-feature-item">{t.highFreqF2}</div>
                <div className="hope-layers-feature-item">{t.highFreqF3}</div>
              </div>
            </div>

            {/* 存储内容 */}
            <div className="hope-layers-layer-section">
              <div className="hope-layers-section-title">{t.highFreqContent}</div>
              <div className="hope-layers-content-box">
                <div className="hope-layers-content-item">{t.highFreqC1}</div>
                <div className="hope-layers-content-item">{t.highFreqC2}</div>
                <div className="hope-layers-content-item">{t.highFreqC3}</div>
                <div className="hope-layers-content-item">{t.highFreqC4}</div>
                <div className="hope-layers-content-item">{t.highFreqC5}</div>
              </div>
            </div>

            {/* 自动管理机制 */}
            <div className="hope-layers-layer-section">
              <div className="hope-layers-section-title">{t.highFreqMechanism}</div>
              <div className="hope-layers-mechanism-box">
                <div className="hope-layers-mechanism-item">
                  <div className="hope-layers-mechanism-title">{t.highFreqM1Title}</div>
                  <div className="hope-layers-mechanism-detail">{t.highFreqM1_1}</div>
                  <div className="hope-layers-mechanism-detail">{t.highFreqM1_2}</div>
                </div>
                <div className="hope-layers-mechanism-item">
                  <div className="hope-layers-mechanism-title">{t.highFreqM2Title}</div>
                  <div className="hope-layers-mechanism-detail">{t.highFreqM2_1}</div>
                  <div className="hope-layers-mechanism-detail">{t.highFreqM2_2}</div>
                </div>
                <div className="hope-layers-mechanism-item">
                  <div className="hope-layers-mechanism-title">{t.highFreqM3Title}</div>
                  <div className="hope-layers-mechanism-detail">{t.highFreqM3_1}</div>
                  <div className="hope-layers-mechanism-detail">{t.highFreqM3_2}</div>
                </div>
              </div>
            </div>

            {/* 检索优先级 */}
            <div className="hope-layers-priority">
              {t.highFreqPriority}
            </div>
          </div>
        </AnimatedContainer>
        )}
      </div>
    </div>
  );
};

export default React.memo(HOPELayersStructure);




