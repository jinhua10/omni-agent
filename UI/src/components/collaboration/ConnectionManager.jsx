/**
 * 连接管理器组件 (Connection Manager Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState } from 'react'
import { Modal, Tabs, Input, Button } from 'antd'
import { QrcodeOutlined, KeyOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import { useMessage } from '../../hooks/useMessage'
import collaborationApi from '../../api/modules/collaboration'
import '../../assets/css/collaboration/connection-manager.css'

function ConnectionManager(props) {
  const { visible, onCancel, onSuccess } = props
  const { t } = useLanguage()
  const message = useMessage()
  const [activeTab, setActiveTab] = useState('generate')
  const [connectionCode, setConnectionCode] = useState('')
  const [generatedCode, setGeneratedCode] = useState('')
  const [loading, setLoading] = useState(false)

  const handleGenerate = async () => {
    setLoading(true)
    try {
      const response = await collaborationApi.generateCode()
      if (response?.code) {
        // axios 拦截器已返回 response.data (Axios interceptor returns response.data)
        setGeneratedCode(response.code)
        message.success(t('collaboration.codeGenerated'))
      }
    } catch (error) {
      console.error('Failed to generate code:', error)
      message.error(t('collaboration.generateFailed'))
    } finally {
      setLoading(false)
    }
  }

  const handleConnect = async () => {
    if (!connectionCode.trim()) {
      message.warning(t('collaboration.enterCode'))
      return
    }

    setLoading(true)
    try {
      await collaborationApi.connect({ code: connectionCode })
      onSuccess && onSuccess()
    } catch (error) {
      console.error('Failed to connect:', error)
      message.error(t('collaboration.connectFailed'))
    } finally {
      setLoading(false)
    }
  }

  const handleCopy = () => {
    navigator.clipboard.writeText(generatedCode)
    message.success(t('collaboration.codeCopied'))
  }

  const items = [
    {
      key: 'generate',
      label: (
        <span>
          <QrcodeOutlined />
          {t('collaboration.generateCode')}
        </span>
      ),
      children: (
        <div className="connection-manager__generate">
          <p className="connection-manager__hint">
            {t('collaboration.generateHint')}
          </p>
          <Button
            type="primary"
            size="large"
            block
            onClick={handleGenerate}
            loading={loading}
            className="connection-manager__btn"
          >
            {t('collaboration.generateCode')}
          </Button>
          {generatedCode && (
            <div className="connection-manager__code-box">
              <div className="connection-manager__code">{generatedCode}</div>
              <Button onClick={handleCopy} block>
                {t('collaboration.copyCode')}
              </Button>
            </div>
          )}
        </div>
      ),
    },
    {
      key: 'enter',
      label: (
        <span>
          <KeyOutlined />
          {t('collaboration.enterCode')}
        </span>
      ),
      children: (
        <div className="connection-manager__enter">
          <p className="connection-manager__hint">
            {t('collaboration.enterHint')}
          </p>
          <Input
            size="large"
            placeholder={t('collaboration.codePlaceholder')}
            value={connectionCode}
            onChange={(e) => setConnectionCode(e.target.value)}
            className="connection-manager__input"
          />
          <Button
            type="primary"
            size="large"
            block
            onClick={handleConnect}
            loading={loading}
            disabled={!connectionCode.trim()}
            className="connection-manager__btn"
          >
            {t('collaboration.connect')}
          </Button>
        </div>
      ),
    },
  ]

  return (
    <Modal
      title={t('collaboration.addPeer')}
      open={visible}
      onCancel={onCancel}
      footer={null}
      width={500}
    >
      <Tabs
        activeKey={activeTab}
        onChange={setActiveTab}
        items={items}
        className="connection-manager__tabs"
      />
    </Modal>
  )
}

export default ConnectionManager

