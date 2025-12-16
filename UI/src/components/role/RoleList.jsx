/**
 * 角色列表组件 (Role List Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect, useCallback } from 'react'
import { Button, Space, Switch, Modal, Input, Select, Pagination } from 'antd'
import { PlusOutlined, ReloadOutlined, AppstoreOutlined, UnorderedListOutlined } from '@ant-design/icons'
import RoleCard from './RoleCard'
import RoleEditor from './RoleEditor'
import RoleStatistics from './RoleStatistics'
import { Loading } from '../common'
import { useLanguage } from '../../contexts/LanguageContext'
import { useMessage } from '../../hooks/useMessage'
import roleApi from '../../api/modules/role'
import '../../assets/css/role/role-list.css'

function RoleList() {
  const { t } = useLanguage()
  const message = useMessage()
  const [roles, setRoles] = useState([])
  const [loading, setLoading] = useState(false)
  const [editorVisible, setEditorVisible] = useState(false)
  const [statsVisible, setStatsVisible] = useState(false)
  const [selectedRole, setSelectedRole] = useState(null)
  const [viewMode, setViewMode] = useState('grid')
  
  // 分页和搜索状态
  const [searchKeyword, setSearchKeyword] = useState('')
  const [pagination, setPagination] = useState({
    current: 1,
    pageSize: 10,
    total: 0,
  })
  const [sortBy, setSortBy] = useState('priority')
  const [sortOrder, setSortOrder] = useState('desc')
  const [enabledFilter, setEnabledFilter] = useState(null)

  const loadRoles = useCallback(async (params = {}) => {
    setLoading(true)
    try {
      const queryParams = {
        page: pagination.current,
        pageSize: pagination.pageSize,
        keyword: searchKeyword,
        sortBy: sortBy,
        sortOrder: sortOrder,
        enabled: enabledFilter,
        ...params
      }
      
      const response = await roleApi.getList(queryParams)
      if (response) {
        setRoles(response.list || [])
        setPagination(prev => ({
          ...prev,
          total: response.total || 0,
          current: response.page || prev.current,
        }))
      }
    } catch (error) {
      console.error('Failed to load roles:', error)
      message.error(t('role.loadFailed'))
    } finally {
      setLoading(false)
    }
  }, [t, pagination.current, pagination.pageSize, searchKeyword, sortBy, sortOrder, enabledFilter])

  useEffect(() => {
    loadRoles()
  }, [pagination.current, pagination.pageSize, searchKeyword, sortBy, sortOrder, enabledFilter])
  
  // 搜索处理
  const handleSearch = (value) => {
    setSearchKeyword(value)
    setPagination(prev => ({ ...prev, current: 1 }))
  }
  
  // 分页变化处理
  const handlePageChange = (page, pageSize) => {
    setPagination(prev => ({
      ...prev,
      current: page,
      pageSize: pageSize,
    }))
  }
  
  // 排序变化处理
  const handleSortChange = (field) => {
    if (sortBy === field) {
      setSortOrder(sortOrder === 'asc' ? 'desc' : 'asc')
    } else {
      setSortBy(field)
      setSortOrder('desc')
    }
  }

  const handleCreate = () => {
    setSelectedRole(null)
    setEditorVisible(true)
  }

  const handleEdit = (role) => {
    setSelectedRole(role)
    setEditorVisible(true)
  }

  const handleDelete = (role) => {
    Modal.confirm({
      title: t('role.deleteConfirm'),
      content: role.name,
      okText: t('common.confirm'),
      cancelText: t('common.cancel'),
      okType: 'danger',
      onOk: async () => {
        try {
          await roleApi.delete(role.id)
          message.success(t('role.deleteSuccess'))
          loadRoles()
        } catch (error) {
          console.error('Failed to delete role:', error)
          message.error(t('role.deleteFailed'))
        }
      },
    })
  }

  const handleToggleStatus = async (role) => {
    try {
      await roleApi.update(role.id, { enabled: !role.enabled })
      message.success(t('role.updateSuccess'))
      loadRoles()
    } catch (error) {
      console.error('Failed to toggle role status:', error)
      message.error(t('role.updateFailed'))
    }
  }

  const handleSaveRole = async (data) => {
    try {
      if (selectedRole) {
        await roleApi.update(selectedRole.id, data)
        message.success(t('role.updateSuccess'))
      } else {
        await roleApi.create(data)
        message.success(t('role.createSuccess'))
      }
      setEditorVisible(false)
      loadRoles()
    } catch (error) {
      console.error('Failed to save role:', error)
      message.error(selectedRole ? t('role.updateFailed') : t('role.createFailed'))
    }
  }

  return (
    <div className="role-list">
      <div className="role-list__header">
        <div className="role-list__title">
          <h2>{t('role.title')}</h2>
          <span className="role-list__count">
            {t('role.total', { count: pagination.total })}
          </span>
        </div>

        <Space>
          <Button
            icon={<ReloadOutlined />}
            onClick={() => loadRoles()}
            loading={loading}
          >
            {t('common.refresh')}
          </Button>
          <Button onClick={() => setStatsVisible(true)}>
            {t('role.statistics')}
          </Button>
          <Switch
            checkedChildren={<AppstoreOutlined />}
            unCheckedChildren={<UnorderedListOutlined />}
            checked={viewMode === 'grid'}
            onChange={(checked) => setViewMode(checked ? 'grid' : 'list')}
          />
          <Button
            type="primary"
            icon={<PlusOutlined />}
            onClick={handleCreate}
          >
            {t('role.create')}
          </Button>
        </Space>
      </div>

      {/* 搜索和过滤工具栏 */}
      <div className="role-list__toolbar">
        <Input.Search
          placeholder={t('role.searchPlaceholder') || '搜索角色名称、描述或关键词...'}
          allowClear
          style={{ width: 300 }}
          onSearch={handleSearch}
          onChange={(e) => !e.target.value && handleSearch('')}
        />
        
        <Space>
          <Select
            placeholder="状态过滤"
            allowClear
            style={{ width: 120 }}
            value={enabledFilter}
            onChange={setEnabledFilter}
          >
            <Select.Option value={true}>已启用</Select.Option>
            <Select.Option value={false}>已禁用</Select.Option>
          </Select>
          
          <Select
            placeholder="排序方式"
            style={{ width: 150 }}
            value={`${sortBy}-${sortOrder}`}
            onChange={(value) => {
              const [field, order] = value.split('-')
              setSortBy(field)
              setSortOrder(order)
            }}
          >
            <Select.Option value="priority-desc">优先级 ↓</Select.Option>
            <Select.Option value="priority-asc">优先级 ↑</Select.Option>
            <Select.Option value="name-asc">名称 A-Z</Select.Option>
            <Select.Option value="name-desc">名称 Z-A</Select.Option>
            <Select.Option value="weight-desc">权重 ↓</Select.Option>
            <Select.Option value="weight-asc">权重 ↑</Select.Option>
          </Select>
        </Space>
      </div>

      <div className="role-list__content">
        {loading ? (
          <Loading spinning={true} tip={t('common.loading')} />
        ) : roles.length === 0 ? (
          <div className="role-list__empty">
            <div className="role-list__empty-icon">👤</div>
            <p className="role-list__empty-text">
              {searchKeyword ? '未找到匹配的角色' : t('role.noRoles')}
            </p>
            {!searchKeyword && (
              <Button type="primary" icon={<PlusOutlined />} onClick={handleCreate}>
                {t('role.createFirst')}
              </Button>
            )}
          </div>
        ) : (
          <>
            <div className={`role-list__${viewMode}`}>
              {roles.map((role) => (
                <RoleCard
                  key={role.id}
                  role={role}
                  viewMode={viewMode}
                  onEdit={handleEdit}
                  onDelete={handleDelete}
                  onToggleStatus={handleToggleStatus}
                />
              ))}
            </div>
            
            {/* 分页组件 */}
            <div className="role-list__pagination">
              <Pagination
                current={pagination.current}
                pageSize={pagination.pageSize}
                total={pagination.total}
                showSizeChanger
                showQuickJumper
                showTotal={(total) => `共 ${total} 个角色`}
                onChange={handlePageChange}
                pageSizeOptions={['10', '20', '50', '100']}
              />
            </div>
          </>
        )}
      </div>

      <RoleEditor
        visible={editorVisible}
        role={selectedRole}
        onCancel={() => setEditorVisible(false)}
        onSave={handleSaveRole}
      />

      <RoleStatistics
        visible={statsVisible}
        onClose={() => setStatsVisible(false)}
      />
    </div>
  )
}

export default RoleList

