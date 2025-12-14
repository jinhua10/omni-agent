/**
 * 协作面板 - 现代主题UI壳子 / Collaboration Panel - Modern Theme UI Shell
 *
 * 纯UI展示层，数据和actions通过引擎绑定
 * Pure UI presentation layer, data and actions bound via engine
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { useEffect } from 'react';
import { Tabs } from 'antd';
import { TeamOutlined, SwapOutlined, ClusterOutlined, SyncOutlined } from '@ant-design/icons';
import { useLanguage } from '../../../../contexts/LanguageContext';
import { useCollaborationBinding } from '../../../../adapters/CollaborationAdapter';
import PeerList from '../../../collaboration/PeerList';
import ExchangeHistory from '../../../collaboration/ExchangeHistory';
import NetworkTopology from '../../../collaboration/NetworkTopology';
import SyncMonitor from '../../../collaboration/SyncMonitor';
import '../../../../assets/css/collaboration/collaboration-panel.css';

/**
 * 现代主题 - 协作面板UI壳子 / Modern Theme - Collaboration Panel UI Shell
 *
 * @param {object} binding - 引擎绑定的数据和actions / Engine-bound data and actions
 */
function ModernCollaborationShell() {
  const { t } = useLanguage();

  // 获取数据绑定 / Get data binding
  const { state, actions } = useCollaborationBinding();

  // 初始化加载数据 / Initialize load data
  useEffect(() => {
    if (state.activeTab === 'peers' && state.peers.length === 0) {
      actions.loadPeers?.();
    }
  }, [state.activeTab]); // eslint-disable-line

  // Tab配置 / Tab configuration
  const items = [
    {
      key: 'peers',
      label: (
        <span>
          <TeamOutlined />
          {t('collaboration.peers')}
        </span>
      ),
      children: <PeerList data={state.peers} loading={state.loading} />,
    },
    {
      key: 'exchange',
      label: (
        <span>
          <SwapOutlined />
          {t('collaboration.exchange')}
        </span>
      ),
      children: <ExchangeHistory data={state.exchanges} loading={state.loading} />,
    },
    {
      key: 'topology',
      label: (
        <span>
          <ClusterOutlined />
          {t('collaboration.topology')}
        </span>
      ),
      children: <NetworkTopology data={state.topology} loading={state.loading} />,
    },
    {
      key: 'sync',
      label: (
        <span>
          <SyncOutlined />
          {t('collaboration.sync')}
        </span>
      ),
      children: <SyncMonitor data={state.syncStatus} loading={state.loading} />,
    },
  ];

  return (
    <div className="collaboration-panel">
      <div className="collaboration-panel__header">
        <h2>{t('collaboration.title')}</h2>
      </div>

      <div className="collaboration-panel__content">
        <Tabs
          activeKey={state.activeTab}
          onChange={actions.switchTab}
          items={items}
          size="large"
          className="collaboration-panel__tabs"
        />
      </div>
    </div>
  );
}

export default ModernCollaborationShell;
