/**
 * 语言上下文 (Language Context)
 *
 * 提供全局的语言切换和翻译功能
 * (Provides global language switching and translation)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { createContext, useContext, useState, useCallback } from 'react'
import zhLang from '../lang/zh'
import enLang from '../lang/en'
import { ConfigProvider } from 'antd'
import zhCN from 'antd/locale/zh_CN'
import enUS from 'antd/locale/en_US'

// 语言包映射 (Language pack mapping)
const languages = {
  zh: zhLang,
  en: enLang,
}

// Ant Design 语言包映射 (Ant Design locale mapping)
const antdLocales = {
  zh: zhCN,
  en: enUS,
}

// 创建 Context (Create context)
const LanguageContext = createContext()

/**
 * 语言提供者组件 (Language Provider Component)
 *
 * @param {Object} props - 组件属性
 * @param {ReactNode} props.children - 子组件
 */
export function LanguageProvider({ children }) {
  // 从 localStorage 读取语言设置 (Read language from localStorage)
  const [language, setLanguage] = useState(() => {
    const saved = localStorage.getItem('language')
    return saved || 'zh'
  })

  /**
   * 切换语言 (Toggle language)
   */
  const toggleLanguage = useCallback(() => {
    const newLang = language === 'zh' ? 'en' : 'zh'
    setLanguage(newLang)
    localStorage.setItem('language', newLang)

    // 更新 HTML lang 属性 (Update HTML lang attribute)
    document.documentElement.lang = newLang === 'zh' ? 'zh-CN' : 'en'
  }, [language])

  /**
   * 设置语言 (Set language)
   *
   * @param {string} lang - 语言代码 (zh/en)
   */
  const setLang = useCallback((lang) => {
    if (lang !== language && languages[lang]) {
      setLanguage(lang)
      localStorage.setItem('language', lang)
      document.documentElement.lang = lang === 'zh' ? 'zh-CN' : 'en'
    }
  }, [language])

  /**
   * 翻译函数 (Translation function)
   *
   * @param {string} key - 翻译键，支持点号分隔（如 'common.confirm'）
   * @param {Object} params - 参数替换（如 {count: 10}）
   * @returns {string} 翻译后的文本
   */
  const t = useCallback((key, params) => {
    // 通过点号分隔键名，支持嵌套结构 (Support nested keys with dot notation)
    const keys = key.split('.')
    let value = languages[language]

    for (const k of keys) {
      value = value?.[k]
      if (value === undefined) {
        console.warn(`Translation key not found: ${key}`)
        return key
      }
    }

    // 如果提供了参数，进行替换 (Replace parameters if provided)
    if (params && typeof value === 'string') {
      return value.replace(/\{(\w+)\}/g, (match, paramKey) => {
        return params[paramKey] !== undefined ? params[paramKey] : match
      })
    }

    return value
  }, [language])

  /**
   * 获取当前语言的完整语言包 (Get full language pack)
   *
   * @returns {Object} 语言包对象
   */
  const getLangPack = useCallback(() => {
    return languages[language]
  }, [language])

  // Context 值 (Context value)
  const value = {
    language,        // 当前语言 (current language)
    toggleLanguage,  // 切换语言函数 (toggle function)
    setLanguage: setLang, // 设置语言函数 (set language function)
    t,               // 翻译函数 (translation function)
    getLangPack,     // 获取语言包 (get language pack)
  }

  return (
    <LanguageContext.Provider value={value}>
      <ConfigProvider locale={antdLocales[language]}>
        {children}
      </ConfigProvider>
    </LanguageContext.Provider>
  )
}

/**
 * 使用语言 Hook (Use language hook)
 *
 * @returns {Object} 语言相关的方法和状态
 *
 * @example
 * const { t, language, toggleLanguage } = useLanguage()
 * console.log(t('common.confirm')) // "确认" or "Confirm"
 */
export function useLanguage() {
  const context = useContext(LanguageContext)

  if (!context) {
    throw new Error('useLanguage must be used within LanguageProvider')
  }

  return context
}

export default LanguageContext

