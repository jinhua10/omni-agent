/**
 * 气泡主题 - 文档管理 Shell / Bubble Theme - Documents Shell
 * 
 * 【重要】使用统一的数据适配器获取数据
 * 后端联调时只需修改 PageDataAdapter.jsx
 */

import React from 'react';
import { useLanguage } from '../../../../contexts/LanguageContext';
import { useDocumentsPageData } from '../../../../adapters/PageDataAdapter';
import './bubble-common.css';

function DocumentsShell() {
  const { t } = useLanguage();
  
  // 使用统一的数据适配器 (Use unified data adapter)
  const { documents, stats, loading, error } = useDocumentsPageData();

  if (loading) {
    return (
      <div className="bubble-documents-shell">
        <div className="docs-hero-section">
          <div className="hero-orb">
            <div className="orb-glow"></div>
            <div className="orb-content">
              <span className="hero-icon">⏳</span>
              <h1 className="hero-title">{t('document.shell.loading')}</h1>
              <p className="hero-subtitle">{t('document.shell.loadingData')}</p>
            </div>
          </div>
        </div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="bubble-documents-shell">
        <div className="docs-hero-section">
          <div className="hero-orb error-orb">
            <div className="orb-glow"></div>
            <div className="orb-content">
              <span className="hero-icon">⚠️</span>
              <h1 className="hero-title">{t('document.shell.loadFailed')}</h1>
              <p className="hero-subtitle">{error}</p>
            </div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="bubble-documents-shell">
      <div className="docs-hero-section">
        <div className="hero-orb">
          <div className="orb-glow"></div>
          <div className="orb-content">
            <span className="hero-icon">📄</span>
            <h1 className="hero-title">{t('document.shell.heroTitle')}</h1>
            <p className="hero-subtitle">{t('document.shell.heroSubtitle')}</p>
            {stats && (
              <p className="hero-stats">
                {stats.total} {t('document.shell.stats.docs')} | {stats.indexed} {t('document.shell.stats.indexed')}
              </p>
            )}
          </div>
        </div>
      </div>

      <div className="docs-content-grid">
        <div className="docs-card bubble-glass-card">
          <div className="card-icon">📊</div>
          <h3>{t('document.shell.stats.totalDocs')}</h3>
          <div className="card-value">{stats.total}</div>
          <p className="card-desc">{t('document.shell.stats.totalDocsDesc')}</p>
        </div>
        
        <div className="docs-card bubble-glass-card">
          <div className="card-icon">✅</div>
          <h3>{t('document.shell.stats.indexed')}</h3>
          <div className="card-value">{stats.indexed}</div>
          <p className="card-desc">{t('document.shell.stats.indexedDesc')}</p>
        </div>
        
        <div className="docs-card bubble-glass-card">
          <div className="card-icon">⏳</div>
          <h3>{t('document.shell.stats.unindexed')}</h3>
          <div className="card-value">{stats.unindexed}</div>
          <p className="card-desc">{t('document.shell.stats.unindexedDesc')}</p>
        </div>
        
        <div className="docs-card bubble-glass-card">
          <div className="card-icon">📑</div>
          <h3>{t('document.shell.stats.fileTypes')}</h3>
          <div className="card-value">{stats.fileTypes.length}</div>
          <p className="card-desc">{t('document.shell.stats.fileTypesDesc')}</p>
        </div>
      </div>

      <div className="docs-features-section">
        <h2 className="section-title">{t('document.title')}</h2>
        <div className="features-grid">
          <div className="feature-card bubble-glass-card">
            <div className="card-icon">📁</div>
            <h3>{t('document.shell.features.library')}</h3>
            <p>{t('document.shell.features.libraryDesc')}</p>
          </div>
          
          <div className="feature-card bubble-glass-card">
            <div className="card-icon">🔍</div>
            <h3>{t('document.shell.features.search')}</h3>
            <p>{t('document.shell.features.searchDesc')}</p>
          </div>
          
          <div className="feature-card bubble-glass-card">
            <div className="card-icon">✏️</div>
            <h3>{t('document.shell.features.edit')}</h3>
            <p>{t('document.shell.features.editDesc')}</p>
          </div>
          
          <div className="feature-card bubble-glass-card">
            <div className="card-icon">🔒</div>
            <h3>{t('document.shell.features.security')}</h3>
            <p>{t('document.shell.features.securityDesc')}</p>
          </div>
        </div>
      </div>
    </div>
  );
}

export default DocumentsShell;
