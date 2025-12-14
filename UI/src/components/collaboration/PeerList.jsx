/**
 * 伙伴列表组件 (Peer List Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect, useCallback } from 'react'
import { Button, Space, message, Empty } from 'antd'
import { PlusOutlined, ReloadOutlined } from '@ant-design/icons'
import PeerCard from './PeerCard'
import ConnectionManager from './ConnectionManager'
import { Loading } from '../common'
import { useLanguage } from '../../contexts/LanguageContext'
import collaborationApi from '../../api/modules/collaboration'
import '../../assets/css/collaboration/peer-list.css'

function PeerList() {
  const { t } = useLanguage()
  const [peers, setPeers] = useState([])
  const [loading, setLoading] = useState(false)
  const [connectionVisible, setConnectionVisible] = useState(false)

  const loadPeers = useCallback(async () => {
    setLoading(true)
    try {
      const response = await collaborationApi.getPeers()
      console.log('Peers API Response:', response) // 调试日志 (Debug log)
      if (response) {
        // 后端 PeersResponse 返回 peers 字段 (Backend PeersResponse returns peers field)
        // PeersResponse: { success: boolean, peers: PeerInfo[], total: number }
        const peersList = response.peers || response.data?.peers || response.data || []
        // 确保是数组 (Ensure it's an array)
        setPeers(Array.isArray(peersList) ? peersList : [])
      } else {
        setPeers([])
      }
    } catch (error) {
      console.error('Failed to load peers:', error)
      message.error(t('collaboration.loadFailed'))
      setPeers([]) // 设置为空数组防止崩溃 (Set to empty array to prevent crash)
    } finally {
      setLoading(false)
    }
  }, [t])

  useEffect(() => {
    loadPeers()
  }, [loadPeers])

  const handleDisconnect = async (peer) => {
    try {
      await collaborationApi.disconnect(peer.id)
      message.success(t('collaboration.disconnectSuccess'))
      loadPeers()
    } catch (error) {
      console.error('Failed to disconnect:', error)
      message.error(t('collaboration.disconnectFailed'))
    }
  }

  const handleSync = async (peer) => {
    try {
      await collaborationApi.syncWith(peer.id)
      message.success(t('collaboration.syncSuccess'))
    } catch (error) {
      console.error('Failed to sync:', error)
      message.error(t('collaboration.syncFailed'))
    }
  }

  const handleConnectionSuccess = () => {
    setConnectionVisible(false)
    message.success(t('collaboration.connectSuccess'))
    loadPeers()
  }

  return (
    <div className="peer-list">
      <div className="peer-list__toolbar">
        <Space>
          <Button
            icon={<ReloadOutlined />}
            onClick={loadPeers}
            loading={loading}
          >
            {t('common.refresh')}
          </Button>
          <Button
            type="primary"
            icon={<PlusOutlined />}
            onClick={() => setConnectionVisible(true)}
          >
            {t('collaboration.addPeer')}
          </Button>
        </Space>
      </div>

      <div className="peer-list__content">
        {loading ? (
          <Loading spinning={true} tip={t('common.loading')} />
        ) : peers.length === 0 ? (
          <Empty
            image={Empty.PRESENTED_IMAGE_SIMPLE}
            description={t('collaboration.noPeers')}
          />
        ) : (
          <div className="peer-list__grid">
            {peers.map((peer) => (
              <PeerCard
                key={peer.id}
                peer={peer}
                onDisconnect={handleDisconnect}
                onSync={handleSync}
              />
            ))}
          </div>
        )}
      </div>

      <ConnectionManager
        visible={connectionVisible}
        onCancel={() => setConnectionVisible(false)}
        onSuccess={handleConnectionSuccess}
      />
    </div>
  )
}

export default PeerList

