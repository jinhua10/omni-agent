/**
 * 角色统计组件 (Role Statistics Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect } from 'react'
import { Modal, Table, Spin } from 'antd'
import { useLanguage } from '../../contexts/LanguageContext'
import roleApi from '../../api/modules/role'

function RoleStatistics(props) {
  const { visible, onClose } = props
  const { t } = useLanguage()
  const [loading, setLoading] = useState(false)
  const [data, setData] = useState([])

  useEffect(() => {
    if (visible) {
      loadStatistics()
    }
  }, [visible])

  const loadStatistics = async () => {
    setLoading(true)
    try {
      const response = await roleApi.getStatistics()
      if (response) {
        // axios 拦截器已返回 response.data (Axios interceptor returns response.data)
        setData(response)
      }
    } catch (error) {
      console.error('Failed to load statistics:', error)
    } finally {
      setLoading(false)
    }
  }

  const columns = [
    {
      title: t('role.name'),
      dataIndex: 'name',
      key: 'name',
    },
    {
      title: t('role.usageCount'),
      dataIndex: 'usageCount',
      key: 'usageCount',
      sorter: (a, b) => a.usageCount - b.usageCount,
    },
    {
      title: t('role.successRate'),
      dataIndex: 'successRate',
      key: 'successRate',
      render: (rate) => `${(rate * 100).toFixed(1)}%`,
      sorter: (a, b) => a.successRate - b.successRate,
    },
  ]

  return (
    <Modal
      title={t('role.statistics')}
      open={visible}
      onCancel={onClose}
      footer={null}
      width={700}
    >
      {loading ? (
        <div style={{ textAlign: 'center', padding: '40px' }}>
          <Spin tip={t('common.loading')}>
            <div style={{ padding: 50 }} />
          </Spin>
        </div>
      ) : (
        <Table
          dataSource={data}
          columns={columns}
          rowKey="id"
          pagination={{ pageSize: 10 }}
        />
      )}
    </Modal>
  )
}

export default RoleStatistics

