/**
 * 文档配置管理 Hook
 * (Document Configuration Management Hook)
 *
 * 管理文档配置的加载、更新和保存
 *
 * @author OmniAgent Team
 * @since 2025-12-26
 */

import { useState, useCallback } from 'react';
import { App } from 'antd';
import ragStrategyApi from '../api/modules/ragStrategy';

function useDocumentConfig() {
    const { message } = App.useApp();
    const [documentConfigs, setDocumentConfigs] = useState({});

    // 加载单个文档配置
    const loadDocumentConfig = useCallback(async (docId) => {
        try {
            const result = await ragStrategyApi.getDocumentConfig(docId);
            if (result.success && result.data) {
                setDocumentConfigs(prev => ({
                    ...prev,
                    [docId]: result.data
                }));
            }
        } catch (error) {
            console.error('加载文档配置失败:', error);
        }
    }, []);

    // 更新文档配置
    const updateDocumentConfig = useCallback(async (docId, configUpdates) => {
        try {
            let currentConfig = documentConfigs[docId];
            if (!currentConfig) {
                console.warn('⚠️ 文档配置不存在，正在加载配置...');
                await loadDocumentConfig(docId);
                await new Promise(resolve => setTimeout(resolve, 100));
                currentConfig = documentConfigs[docId];

                if (!currentConfig) {
                    console.error('❌ 无法获取文档配置，创建默认配置');
                    currentConfig = {
                        documentId: docId,
                        status: 'PENDING',
                        createdAt: Date.now(),
                        chunkingParams: {}
                    };
                }
            }

            const fullConfig = {
                ...currentConfig,
                ...configUpdates,
                documentId: docId,
                updatedAt: Date.now(),
                chunkingParams: {
                    ...(currentConfig.chunkingParams || {}),
                    ...(configUpdates.chunkingParams || {})
                }
            };

            const encodedDocId = encodeURIComponent(docId);
            const response = await fetch(`/api/system/rag-config/document/${encodedDocId}`, {
                method: 'PUT',
                headers: { 'Content-Type': 'application/json' },
                body: JSON.stringify(fullConfig)
            });

            if (!response.ok) {
                const errorText = await response.text();
                console.error('❌ 服务器响应错误:', response.status, errorText);
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            const result = await response.json();
            if (result.success) {
                setDocumentConfigs(prev => ({
                    ...prev,
                    [docId]: fullConfig
                }));
                loadDocumentConfig(docId);
                message.success('配置已保存');
            } else {
                message.error(result.message || '保存失败');
            }
        } catch (error) {
            console.error('❌ 更新配置失败:', error);
            message.error('保存失败: ' + error.message);
        }
    }, [documentConfigs, loadDocumentConfig, message]);

    return {
        documentConfigs,
        loadDocumentConfig,
        updateDocumentConfig
    };
}

export default useDocumentConfig;

