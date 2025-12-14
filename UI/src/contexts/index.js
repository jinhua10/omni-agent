/**
 * Context 模块统一导出 (Context Modules Export)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

// Context Providers
export { AppProvider, useApp } from './AppContext'
export { UserProvider, useUser, useAuth } from './UserContext'
export { RoleProvider, useRole } from './RoleContext'
export { KnowledgeProvider, useKnowledge } from './KnowledgeContext'
export { FeedbackProvider, useFeedback } from './FeedbackContext'
export { WishProvider, useWish } from './WishContext'
export { LanguageProvider, useLanguage } from './LanguageContext'
export { QAProvider, useQA } from './QAContext'

// Context Objects
export { default as AppContext } from './AppContext'
export { default as UserContext } from './UserContext'
export { default as RoleContext } from './RoleContext'
export { default as KnowledgeContext } from './KnowledgeContext'
export { default as FeedbackContext } from './FeedbackContext'
export { default as WishContext } from './WishContext'
export { default as LanguageContext } from './LanguageContext'
export { default as QAContext } from './QAContext'

