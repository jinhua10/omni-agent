/**
 * Hooks 统一导出 (Hooks Export)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

// API Hooks
export { default as useApi } from './useApi'
export { default as useFetch } from './useFetch'

// UI Hooks
export { default as useMessage } from './useMessage'

// Context Hooks (re-export from contexts)
export { useApp } from '../contexts/AppContext'
export { useUser, useAuth } from '../contexts/UserContext'
export { useRole } from '../contexts/RoleContext'
export { useKnowledge } from '../contexts/KnowledgeContext'
export { useFeedback } from '../contexts/FeedbackContext'
export { useWish } from '../contexts/WishContext'
export { useLanguage } from '../contexts/LanguageContext'

