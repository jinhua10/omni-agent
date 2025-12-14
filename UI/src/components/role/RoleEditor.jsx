/**
 * 角色编辑器组件 (Role Editor Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect } from 'react'
import { Modal, Form, Input, Switch } from 'antd'
import KeywordManager from './KeywordManager'
import { useLanguage } from '../../contexts/LanguageContext'

const { TextArea } = Input

function RoleEditor(props) {
  const { visible, role, onCancel, onSave } = props
  const { t } = useLanguage()
  const [form] = Form.useForm()
  const [keywords, setKeywords] = useState([])

  useEffect(() => {
    if (visible) {
      if (role) {
        form.setFieldsValue({
          name: role.name,
          description: role.description,
          icon: role.icon,
          enabled: role.enabled,
        })
        setKeywords(role.keywords || [])
      } else {
        form.resetFields()
        setKeywords([])
      }
    }
  }, [visible, role, form])

  const handleOk = async () => {
    try {
      const values = await form.validateFields()
      onSave && onSave({
        ...values,
        keywords,
      })
    } catch (error) {
      console.error('Validation failed:', error)
    }
  }

  return (
    <Modal
      title={role ? t('role.edit') : t('role.create')}
      open={visible}
      onCancel={onCancel}
      onOk={handleOk}
      width={700}
      okText={t('common.save')}
      cancelText={t('common.cancel')}
    >
      <Form form={form} layout="vertical">
        <Form.Item
          name="name"
          label={t('role.name')}
          rules={[{ required: true, message: t('role.nameRequired') }]}
        >
          <Input placeholder={t('role.namePlaceholder')} />
        </Form.Item>

        <Form.Item
          name="description"
          label={t('role.description')}
          rules={[{ required: true, message: t('role.descriptionRequired') }]}
        >
          <TextArea
            rows={3}
            placeholder={t('role.descriptionPlaceholder')}
          />
        </Form.Item>

        <Form.Item name="icon" label={t('role.icon')}>
          <Input placeholder="👤" maxLength={2} />
        </Form.Item>

        <Form.Item name="enabled" label={t('role.status')} valuePropName="checked">
          <Switch
            checkedChildren={t('role.enabled')}
            unCheckedChildren={t('role.disabled')}
          />
        </Form.Item>

        <Form.Item label={t('role.keywords')}>
          <KeywordManager
            keywords={keywords}
            onChange={setKeywords}
          />
        </Form.Item>
      </Form>
    </Modal>
  )
}

export default RoleEditor

