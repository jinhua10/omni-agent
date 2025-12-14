/**
 * UserContext 用户信息上下文 (User Information Context)
 *
 * 管理用户认证状态和个人信息
 * (Manages user authentication state and personal information)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { createContext, useContext, useState, useCallback, useEffect } from 'react'
import PropTypes from 'prop-types'

// 创建 Context (Create context)
const UserContext = createContext()

/**
 * UserProvider 组件
 *
 * @param {Object} props - 组件属性
 * @param {ReactNode} props.children - 子组件
 */
export function UserProvider({ children }) {
  // 用户信息 (User info)
  const [user, setUser] = useState(null)

  // 认证状态 (Authentication state)
  const [isAuthenticated, setIsAuthenticated] = useState(false)

  // 加载状态 (Loading state)
  const [loading, setLoading] = useState(true)

  /**
   * 初始化用户信息 (Initialize user info)
   */
  useEffect(() => {
    const initUser = () => {
      try {
        const token = localStorage.getItem('token')
        const userInfo = localStorage.getItem('userInfo')

        if (token && userInfo) {
          setUser(JSON.parse(userInfo))
          setIsAuthenticated(true)
        }
      } catch (error) {
        console.error('Failed to initialize user:', error)
      } finally {
        setLoading(false)
      }
    }

    initUser()
  }, [])

  /**
   * 登录 (Login)
   */
  const login = useCallback((userInfo, token) => {
    setUser(userInfo)
    setIsAuthenticated(true)
    localStorage.setItem('token', token)
    localStorage.setItem('userInfo', JSON.stringify(userInfo))
  }, [])

  /**
   * 登出 (Logout)
   */
  const logout = useCallback(() => {
    setUser(null)
    setIsAuthenticated(false)
    localStorage.removeItem('token')
    localStorage.removeItem('userInfo')
  }, [])

  /**
   * 更新用户信息 (Update user info)
   */
  const updateUser = useCallback((updates) => {
    setUser((prev) => {
      const newUser = { ...prev, ...updates }
      localStorage.setItem('userInfo', JSON.stringify(newUser))
      return newUser
    })
  }, [])

  /**
   * 检查权限 (Check permission)
   */
  const hasPermission = useCallback((permission) => {
    if (!user || !user.permissions) {
      return false
    }
    return user.permissions.includes(permission)
  }, [user])

  /**
   * 检查角色 (Check role)
   */
  const hasRole = useCallback((role) => {
    if (!user || !user.roles) {
      return false
    }
    return user.roles.includes(role)
  }, [user])

  // Context 值 (Context value)
  const value = {
    // 状态 (States)
    user,
    isAuthenticated,
    loading,

    // 方法 (Methods)
    login,
    logout,
    updateUser,
    hasPermission,
    hasRole,
  }

  return (
    <UserContext.Provider value={value}>
      {children}
    </UserContext.Provider>
  )
}

UserProvider.propTypes = {
  children: PropTypes.node.isRequired,
}

/**
 * useUser Hook
 *
 * @returns {Object} 用户状态和方法
 *
 * @example
 * const { user, isAuthenticated, login, logout } = useUser()
 */
export function useUser() {
  const context = useContext(UserContext)

  if (!context) {
    throw new Error('useUser must be used within UserProvider')
  }

  return context
}

/**
 * useAuth Hook (别名)
 * Alias for useUser
 */
export const useAuth = useUser

export default UserContext

