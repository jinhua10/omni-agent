/**
 * 气泡主题 - QA Shell / Bubble Theme - QA Shell
 * 智能问答页面的气泡主题实现
 * 
 * 【重要】使用统一的数据适配器获取数据
 * 后端联调时只需修改 PageDataAdapter.jsx
 */

import React from 'react';
import { useLanguage } from '../../../../contexts/LanguageContext';
import { useQAPageData } from '../../../../adapters/PageDataAdapter';
import './bubble-common.css';

function QAShell() {
  const { t } = useLanguage();
  
  // 使用统一的数据适配器 (Use unified data adapter)
  const { stats, systemStatus, loading, error } = useQAPageData();

  if (loading) {
    return (
      <div className="bubble-qa-shell">
        <div className="qa-hero-section">
          <div className="hero-orb">
            <div className="orb-glow"></div>
            <div className="orb-content">
              <span className="hero-icon">⏳</span>
              <h1 className="hero-title">{t('qa.shell.loading')}</h1>
              <p className="hero-subtitle">{t('qa.shell.loadingData')}</p>
            </div>
          </div>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="bubble-qa-shell">
        <div className="qa-hero-section">
          <div className="hero-orb">
            <div className="orb-glow"></div>
            <div className="orb-content">
              <span className="hero-icon">❌</span>
              <h1 className="hero-title">{t('qa.shell.loadFailed')}</h1>
              <p className="hero-subtitle">{error}</p>
            </div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="bubble-qa-shell">
      <div className="qa-hero-section">
        <div className="hero-orb">
          <div className="orb-glow"></div>
          <div className="orb-content">
            <span className="hero-icon">💬</span>
            <h1 className="hero-title">{t('qa.shell.heroTitle')}</h1>
            <p className="hero-subtitle">{t('qa.shell.heroSubtitle')}</p>
            {stats && (
              <p className="hero-stats">
                {systemStatus === 'ok' || systemStatus === 'online' ? t('qa.shell.systemOnline') : t('qa.shell.systemOffline') + ' ' + systemStatus}
              </p>
            )}
          </div>
        </div>
      </div>

      <div className="qa-content-grid">
        <div className="qa-card bubble-glass-card">
          <div className="card-icon">📚</div>
          <h3>{t('qa.shell.stats.knowledgeBase')}</h3>
          <p>{t('qa.shell.stats.knowledgeBaseDesc')}</p>
          {stats && (
            <span className="card-stat">
              {stats.documentCount} {t('qa.shell.stats.documentsCount')}
            </span>
          )}
        </div>
        
        <div className="qa-card bubble-glass-card">
          <div className="card-icon">✅</div>
          <h3>{t('qa.shell.stats.indexed')}</h3>
          <p>{t('qa.shell.stats.indexedDesc')}</p>
          {stats && (
            <span className="card-stat">
              {stats.indexedDocumentCount} {t('qa.shell.stats.documentsCount')}
            </span>
          )}
        </div>
        
        <div className="qa-card bubble-glass-card">
          <div className="card-icon">📊</div>
          <h3>{t('qa.shell.stats.indexProgress')}</h3>
          <p>{t('qa.shell.stats.indexProgressDesc')}</p>
          {stats && (
            <span className="card-stat">
              {stats.indexProgress}%
            </span>
          )}
        </div>
        
        <div className="qa-card bubble-glass-card">
          <div className="card-icon">🎯</div>
          <h3>{t('qa.shell.stats.systemStatus')}</h3>
          <p>{t('qa.shell.stats.systemStatusDesc')}</p>
          {stats && (
            <span className="card-stat" style={{
              color: stats.needsIndexing ? '#FFA726' : '#43e97b'
            }}>
              {stats.needsIndexing ? t('qa.shell.stats.needsIndexing') : t('qa.shell.stats.running')}
            </span>
          )}
        </div>
      </div>
    </div>
  );
}

export default QAShell;
