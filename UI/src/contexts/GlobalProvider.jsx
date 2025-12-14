/**
 * GlobalProvider 全局 Provider 组合组件
 * (Global Provider Combination Component)
 *
 * 组合所有 Context Provider，简化应用入口配置
 * (Combines all Context Providers to simplify app entry configuration)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import PropTypes from 'prop-types'
import {
  LanguageProvider,
  AppProvider,
  UserProvider,
  RoleProvider,
  KnowledgeProvider,
  FeedbackProvider,
  WishProvider,
  QAProvider,
} from './index'

/**
 * GlobalProvider 组件
 *
 * 将所有 Context Provider 组合在一起
 *
 * @param {Object} props - 组件属性
 * @param {ReactNode} props.children - 子组件
 *
 * @example
 * <GlobalProvider>
 *   <App />
 * </GlobalProvider>
 */
function GlobalProvider({ children }) {
  return (
    <LanguageProvider>
      <AppProvider>
        <UserProvider>
          <RoleProvider>
            <KnowledgeProvider>
              <FeedbackProvider>
                <WishProvider>
                  <QAProvider>
                    {children}
                  </QAProvider>
                </WishProvider>
              </FeedbackProvider>
            </KnowledgeProvider>
          </RoleProvider>
        </UserProvider>
      </AppProvider>
    </LanguageProvider>
  )
}

GlobalProvider.propTypes = {
  children: PropTypes.node.isRequired,
}

export default GlobalProvider

