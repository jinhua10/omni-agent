/**
 * 角色卡片组件 (Role Card Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { Card, Tag, Button, Space, Tooltip, Switch } from 'antd'
import { EditOutlined, DeleteOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/role/role-card.css'

function RoleCard(props) {
  const { role, viewMode, onEdit, onDelete, onToggleStatus } = props
  const { t } = useLanguage()

  const renderGridView = () => (
    <Card
      className="role-card"
      hoverable
      onClick={() => onEdit && onEdit(role)}
    >
      <div className="role-card__header">
        <div className="role-card__icon">
          {role.icon || '👤'}
        </div>
        <Switch
          checked={role.enabled}
          onChange={() => {
            onToggleStatus && onToggleStatus(role)
          }}
          onClick={(e) => e.stopPropagation()}
          checkedChildren={t('role.enabled')}
          unCheckedChildren={t('role.disabled')}
          className="role-card__switch"
        />
      </div>

      <div className="role-card__info">
        <h3 className="role-card__name">{role.name}</h3>
        <p className="role-card__description">{role.description}</p>

        {role.keywords && role.keywords.length > 0 && (
          <div className="role-card__keywords">
            {role.keywords.slice(0, 5).map((keyword, index) => (
              <Tag key={index} className="role-card__keyword">
                {keyword}
              </Tag>
            ))}
            {role.keywords.length > 5 && (
              <Tag className="role-card__keyword">
                +{role.keywords.length - 5}
              </Tag>
            )}
          </div>
        )}
      </div>

      <div className="role-card__footer" onClick={(e) => e.stopPropagation()}>
        <div className="role-card__stats">
          <span>{t('role.usageCount')}: {role.usageCount || 0}</span>
        </div>
        <Space>
          <Tooltip title={t('role.edit')}>
            <Button
              type="text"
              size="small"
              icon={<EditOutlined />}
              onClick={() => onEdit && onEdit(role)}
            />
          </Tooltip>
          <Tooltip title={t('role.delete')}>
            <Button
              type="text"
              size="small"
              danger
              icon={<DeleteOutlined />}
              onClick={() => onDelete && onDelete(role)}
            />
          </Tooltip>
        </Space>
      </div>
    </Card>
  )

  const renderListView = () => (
    <div className="role-card-list" onClick={() => onEdit && onEdit(role)}>
      <div className="role-card-list__icon">{role.icon || '👤'}</div>
      <div className="role-card-list__info">
        <h4 className="role-card-list__name">{role.name}</h4>
        <p className="role-card-list__description">{role.description}</p>
      </div>
      <div className="role-card-list__keywords">
        {role.keywords?.slice(0, 3).map((keyword, index) => (
          <Tag key={index} size="small">{keyword}</Tag>
        ))}
      </div>
      <div className="role-card-list__stats">
        {role.usageCount || 0}次
      </div>
      <div className="role-card-list__actions" onClick={(e) => e.stopPropagation()}>
        <Switch
          checked={role.enabled}
          onChange={() => onToggleStatus && onToggleStatus(role)}
          size="small"
        />
        <Button
          type="text"
          size="small"
          icon={<EditOutlined />}
          onClick={() => onEdit && onEdit(role)}
        />
        <Button
          type="text"
          size="small"
          danger
          icon={<DeleteOutlined />}
          onClick={() => onDelete && onDelete(role)}
        />
      </div>
    </div>
  )

  return viewMode === 'grid' ? renderGridView() : renderListView()
}

export default RoleCard

