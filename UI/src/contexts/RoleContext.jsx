/**
 * RoleContext 角色管理上下文 (Role Management Context)
 *
 * 管理角色相关的状态和操作
 * (Manages role-related state and operations)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { createContext, useContext, useState, useCallback } from 'react'
import PropTypes from 'prop-types'
import api from '@api/modules'

// 创建 Context (Create context)
const RoleContext = createContext()

/**
 * RoleProvider 组件
 *
 * @param {Object} props - 组件属性
 * @param {ReactNode} props.children - 子组件
 */
export function RoleProvider({ children }) {
  // 角色列表 (Role list)
  const [roles, setRoles] = useState([])

  // 当前选中的角色 (Current selected role)
  const [currentRole, setCurrentRole] = useState(null)

  // 加载状态 (Loading state)
  const [loading, setLoading] = useState(false)

  /**
   * 获取角色列表 (Get role list)
   */
  const fetchRoles = useCallback(async (params) => {
    try {
      setLoading(true)
      const data = await api.role.getList(params)
      setRoles(data.list || data)
      return data
    } catch (error) {
      console.error('Failed to fetch roles:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  /**
   * 创建角色 (Create role)
   */
  const createRole = useCallback(async (roleData) => {
    try {
      setLoading(true)
      const newRole = await api.role.create(roleData)
      setRoles((prev) => [...prev, newRole])
      return newRole
    } catch (error) {
      console.error('Failed to create role:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  /**
   * 更新角色 (Update role)
   */
  const updateRole = useCallback(async (roleId, roleData) => {
    try {
      setLoading(true)
      const updatedRole = await api.role.update(roleId, roleData)
      setRoles((prev) =>
        prev.map((role) => (role.id === roleId ? updatedRole : role))
      )
      return updatedRole
    } catch (error) {
      console.error('Failed to update role:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  /**
   * 删除角色 (Delete role)
   */
  const deleteRole = useCallback(async (roleId) => {
    try {
      setLoading(true)
      await api.role.delete(roleId)
      setRoles((prev) => prev.filter((role) => role.id !== roleId))
    } catch (error) {
      console.error('Failed to delete role:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  /**
   * 检测问题所属角色 (Detect question role)
   */
  const detectRole = useCallback(async (question) => {
    try {
      const result = await api.role.detect(question)
      setCurrentRole(result.role)
      return result
    } catch (error) {
      console.error('Failed to detect role:', error)
      throw error
    }
  }, [])

  /**
   * 获取角色统计 (Get role statistics)
   */
  const getStatistics = useCallback(async () => {
    try {
      const stats = await api.role.getStatistics()
      return stats
    } catch (error) {
      console.error('Failed to get role statistics:', error)
      throw error
    }
  }, [])

  // Context 值 (Context value)
  const value = {
    // 状态 (States)
    roles,
    currentRole,
    loading,

    // 方法 (Methods)
    fetchRoles,
    createRole,
    updateRole,
    deleteRole,
    detectRole,
    getStatistics,
    setCurrentRole,
  }

  return (
    <RoleContext.Provider value={value}>
      {children}
    </RoleContext.Provider>
  )
}

RoleProvider.propTypes = {
  children: PropTypes.node.isRequired,
}

/**
 * useRole Hook
 *
 * @returns {Object} 角色状态和方法
 *
 * @example
 * const { roles, fetchRoles, createRole, detectRole } = useRole()
 */
export function useRole() {
  const context = useContext(RoleContext)

  if (!context) {
    throw new Error('useRole must be used within RoleProvider')
  }

  return context
}

export default RoleContext

