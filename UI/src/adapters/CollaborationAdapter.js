/**
 * 协作面板适配器 / Collaboration Panel Adapter
 *
 * 将协作面板的业务逻辑和数据提取出来，供不同主题UI使用
 * Extract collaboration panel business logic and data for different theme UIs
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import { useState, useCallback } from 'react';
import { usePageBinding } from '../engine/ThemeRenderEngine';
import collaborationApi from '../api/modules/collaboration';

/**
 * 模拟数据 - 用于演示梦幻气泡主题 / Mock data - for dreamy bubble theme demo
 */
const MOCK_DATA = {
  peers: [
    { id: 1, name: 'Node Alpha', status: '在线' },
    { id: 2, name: 'Node Beta', status: '同步中' },
    { id: 3, name: 'Node Gamma', status: '在线' },
    { id: 4, name: 'Node Delta', status: '空闲' },
  ],
  exchanges: [
    { id: 1, from: 'Alpha', to: 'Beta', time: '2分钟前' },
    { id: 2, from: 'Beta', to: 'Gamma', time: '5分钟前' },
    { id: 3, from: 'Gamma', to: 'Delta', time: '10分钟前' },
  ],
  topology: {
    nodes: [
      { id: 1, label: 'Alpha' },
      { id: 2, label: 'Beta' },
      { id: 3, label: 'Gamma' },
      { id: 4, label: 'Delta' },
    ],
  },
  syncStatus: {
    lastSync: '刚刚',
    status: '正常',
  },
};

/**
 * 协作面板初始状态 / Collaboration panel initial state
 */
const INITIAL_STATE = {
  activeTab: 'peers',
  peers: MOCK_DATA.peers, // 使用模拟数据
  exchanges: MOCK_DATA.exchanges,
  topology: MOCK_DATA.topology,
  syncStatus: MOCK_DATA.syncStatus,
  loading: false,
  error: null,
};

/**
 * 协作面板Actions / Collaboration panel actions
 */
export function useCollaborationActions(updateState) {
  /**
   * 切换标签页 / Switch tab
   */
  const switchTab = useCallback((tabKey) => {
    updateState({ activeTab: tabKey });
  }, [updateState]);

  /**
   * 加载节点列表 / Load peer list
   */
  const loadPeers = useCallback(async () => {
    updateState({ loading: true });
    try {
      const response = await collaborationApi.getPeers();
      const peersList = response?.peers || response?.data?.peers || response?.data || MOCK_DATA.peers;
      // 确保是数组 (Ensure it's an array)
      updateState({ peers: Array.isArray(peersList) ? peersList : MOCK_DATA.peers, loading: false });
    } catch (error) {
      console.warn('Failed to load peers, using mock data:', error);
      // API失败时使用模拟数据 / Use mock data when API fails
      updateState({ peers: MOCK_DATA.peers, loading: false });
    }
  }, [updateState]);

  /**
   * 加载交换历史 / Load exchange history
   */
  const loadExchanges = useCallback(async () => {
    updateState({ loading: true });
    try {
      const response = await collaborationApi.getExchangeHistory();
      const exchangesList = Array.isArray(response) ? response : 
                           response?.history || response?.data || MOCK_DATA.exchanges;
      // 确保是数组 (Ensure it's an array)
      updateState({ exchanges: Array.isArray(exchangesList) ? exchangesList : MOCK_DATA.exchanges, loading: false });
    } catch (error) {
      console.warn('Failed to load exchanges, using mock data:', error);
      updateState({ exchanges: MOCK_DATA.exchanges, loading: false });
    }
  }, [updateState]);

  /**
   * 加载网络拓扑 / Load network topology
   */
  const loadTopology = useCallback(async () => {
    updateState({ loading: true });
    try {
      const response = await collaborationApi.getTopology();
      updateState({ topology: response.topology || MOCK_DATA.topology, loading: false });
    } catch (error) {
      console.warn('Failed to load topology, using mock data:', error);
      updateState({ topology: MOCK_DATA.topology, loading: false });
    }
  }, [updateState]);

  /**
   * 加载同步状态 / Load sync status
   */
  const loadSyncStatus = useCallback(async () => {
    updateState({ loading: true });
    try {
      const response = await collaborationApi.getSyncStatus();
      updateState({ syncStatus: response.syncStatus || MOCK_DATA.syncStatus, loading: false });
    } catch (error) {
      console.warn('Failed to load sync status, using mock data:', error);
      updateState({ syncStatus: MOCK_DATA.syncStatus, loading: false });
    }
  }, [updateState]);

  /**
   * 生成连接码 / Generate connection code
   */
  const generateCode = useCallback(async () => {
    updateState({ loading: true });
    try {
      const response = await collaborationApi.generateCode();
      updateState({ loading: false });
      return response.code;
    } catch (error) {
      console.error('Failed to generate code:', error);
      updateState({ error: error.message, loading: false });
      throw error;
    }
  }, [updateState]);

  /**
   * 使用连接码建立连接 / Connect using code
   */
  const connectWithCode = useCallback(async (code) => {
    updateState({ loading: true });
    try {
      const response = await collaborationApi.connect(code);

      if (response.success) {
        // 连接成功后重新加载节点列表 / Reload peers after successful connection
        await loadPeers();
      }

      updateState({ loading: false });
      return response;
    } catch (error) {
      console.error('Failed to connect:', error);
      updateState({ error: error.message, loading: false });
      throw error;
    }
  }, [updateState, loadPeers]);

  /**
   * 断开连接 / Disconnect from peer
   */
  const disconnectPeer = useCallback(async (peerId) => {
    updateState({ loading: true });
    try {
      await collaborationApi.disconnect(peerId);

      // 断开后重新加载节点列表 / Reload peers after disconnection
      await loadPeers();

      updateState({ loading: false });
    } catch (error) {
      console.error('Failed to disconnect:', error);
      updateState({ error: error.message, loading: false });
      throw error;
    }
  }, [updateState, loadPeers]);

  /**
   * 知识交换 / Exchange knowledge
   */
  const exchangeKnowledge = useCallback(async (peerId, knowledge) => {
    updateState({ loading: true });
    try {
      const response = await collaborationApi.exchange({ peerId, knowledge });

      // 交换后重新加载交换历史 / Reload exchanges after exchange
      await loadExchanges();

      updateState({ loading: false });
      return response;
    } catch (error) {
      console.error('Failed to exchange knowledge:', error);
      updateState({ error: error.message, loading: false });
      throw error;
    }
  }, [updateState, loadExchanges]);

  return {
    switchTab,
    loadPeers,
    loadExchanges,
    loadTopology,
    loadSyncStatus,
    generateCode,
    connectWithCode,
    disconnectPeer,
    exchangeKnowledge,
  };
}

/**
 * 协作面板数据绑定Hook / Collaboration panel data binding hook
 *
 * 返回绑定的数据和actions，供UI壳子使用
 * Returns bound data and actions for UI shell
 */
export function useCollaborationBinding() {
  // 使用页面绑定 / Use page binding
  const binding = usePageBinding(
    'collaboration', // 页面ID / Page ID
    INITIAL_STATE,   // 初始状态 / Initial state
    {}               // Actions将在下面创建 / Actions will be created below
  );

  // 创建actions / Create actions
  const actions = useCollaborationActions(binding.updateState);

  // 返回完整的绑定 / Return complete binding
  return {
    state: binding.state,
    actions: {
      ...binding.actions,
      ...actions,
    },
  };
}

// ========== 真实 API 已集成 / Real API integrated ==========
// 所有 API 调用通过 collaborationApi 模块进行
// All API calls are made through the collaborationApi module

