/**
 * Footer 页脚组件 (Footer Component)
 *
 * 提供应用底部信息展示
 * (Provides application footer information)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { useLanguage } from '@contexts/LanguageContext'
import { GithubOutlined, HeartFilled } from '@ant-design/icons'

/**
 * Footer 组件
 *
 * @example
 * <Footer />
 */
function Footer() {
  const { language } = useLanguage()
  const currentYear = new Date().getFullYear()

  return (
    <footer className="app-footer">
      <div className="app-footer__container">
        {/* 版权信息 (Copyright) */}
        <div className="app-footer__copyright">
          <span>© {currentYear} AI Reviewer Team. </span>
          {language === 'zh' ? (
            <>
              使用 <HeartFilled className="app-footer__heart" /> 构建
            </>
          ) : (
            <>
              Built with <HeartFilled className="app-footer__heart" />
            </>
          )}
        </div>

        {/* 链接区域 (Links) */}
        <div className="app-footer__links">
          <a
            href="https://github.com"
            target="_blank"
            rel="noopener noreferrer"
            className="app-footer__link"
          >
            <GithubOutlined /> GitHub
          </a>
          <span className="app-footer__divider">|</span>
          <a
            href="/docs"
            className="app-footer__link"
          >
            {t('common.documentation')}
          </a>
          <span className="app-footer__divider">|</span>
          <a
            href="/about"
            className="app-footer__link"
          >
            {t('common.about')}
          </a>
        </div>
      </div>
    </footer>
  )
}

export default Footer

