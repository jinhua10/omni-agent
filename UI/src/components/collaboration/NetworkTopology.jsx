/**
 * 网络拓扑组件 (Network Topology Component)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useState, useEffect } from 'react'
import { Card, Empty, Spin } from 'antd'
import { useLanguage } from '../../contexts/LanguageContext'
import collaborationApi from '../../api/modules/collaboration'
import '../../assets/css/collaboration/network-topology.css'

function NetworkTopology() {
  const { t } = useLanguage()
  const [loading, setLoading] = useState(false)
  const [topology, setTopology] = useState(null)

  useEffect(() => {
    loadTopology()
  }, [])

  const loadTopology = async () => {
    setLoading(true)
    try {
      const response = await collaborationApi.getTopology()
      if (response) {
        // axios 拦截器已返回 response.data (Axios interceptor returns response.data)
        // 确保 nodes 是数组 (Ensure nodes is an array)
        const topologyData = {
          ...response,
          nodes: Array.isArray(response.nodes) ? response.nodes : 
                 Array.isArray(response.data?.nodes) ? response.data.nodes : []
        }
        setTopology(topologyData)
      }
    } catch (error) {
      console.error('Failed to load topology:', error)
      setTopology(null) // 设置为null防止崩溃 (Set to null to prevent crash)
    } finally {
      setLoading(false)
    }
  }

  if (loading) {
    return (
      <div className="network-topology__loading">
        <Spin tip={t('common.loading')} size="large">
          <div style={{ padding: 50 }} />
        </Spin>
      </div>
    )
  }

  if (!topology || topology.nodes?.length === 0) {
    return (
      <Empty
        image={Empty.PRESENTED_IMAGE_SIMPLE}
        description={t('collaboration.noTopology')}
      />
    )
  }

  return (
    <div className="network-topology">
      <Card className="network-topology__card">
        <div className="network-topology__canvas">
          <div className="network-topology__center-node">
            <div className="network-topology__node network-topology__node--me">
              {t('collaboration.me')}
            </div>
          </div>

          <div className="network-topology__peers">
            {topology.nodes?.map((node, index) => (
              <div
                key={node.id}
                className="network-topology__peer-wrapper"
                style={{
                  transform: `rotate(${(360 / topology.nodes.length) * index}deg) translateY(-120px)`,
                }}
              >
                <div
                  className="network-topology__node network-topology__node--peer"
                  style={{
                    transform: `rotate(-${(360 / topology.nodes.length) * index}deg)`,
                  }}
                >
                  {node.name}
                </div>
                <div className="network-topology__connection" />
              </div>
            ))}
          </div>
        </div>

        <div className="network-topology__stats">
          <div className="network-topology__stat">
            <span className="network-topology__stat-label">
              {t('collaboration.totalPeers')}
            </span>
            <span className="network-topology__stat-value">
              {topology.nodes?.length || 0}
            </span>
          </div>
          <div className="network-topology__stat">
            <span className="network-topology__stat-label">
              {t('collaboration.totalConnections')}
            </span>
            <span className="network-topology__stat-value">
              {topology.connections || 0}
            </span>
          </div>
        </div>
      </Card>
    </div>
  )
}

export default NetworkTopology

