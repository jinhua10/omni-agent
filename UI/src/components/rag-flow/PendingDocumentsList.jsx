/**
 * 待处理文档列表组件
 * (Pending Documents List Component)
 *
 * 显示所有待处理文档的列表
 *
 * @author OmniAgent Team
 * @since 2025-12-26
 */

import React from 'react';
import { Card, Input, Checkbox, Button, Select, Space, Divider } from 'antd';
import { SearchOutlined, CheckSquareOutlined, BorderOutlined } from '@ant-design/icons';
import { useLanguage } from '../../contexts/LanguageContext';
import DocumentListItem from './DocumentListItem';
import '../../assets/css/rag-flow/PendingDocumentsList.css';

const { Option } = Select;

function PendingDocumentsList({
    documentsList,
    selectedDocId,
    documentsProgress,
    strategyTemplates,
    onSelectDocument,
    onApplyTemplate,
    onDeleteTemplate,
    onStartProcess,
    // ⭐ 批量操作相关
    selectedDocIds = [],
    filterKeyword = '',
    batchTemplateId = null,
    onFilterChange,
    onSelectAll,
    onToggleDocSelect,
    onBatchTemplateChange,
    onBatchProcess,
    // ⭐ 文件类型选择相关
    fileTypeStats = {},
    onToggleFileTypeSelect,
    isFileTypeSelected,
    isFileTypeIndeterminate
}) {
    const { t } = useLanguage();

    // 调试：输出 documentsProgress（仅在键数量变化时）
    const prevProgressKeysRef = React.useRef(0);
    const prevDocCountRef = React.useRef(0);

    React.useEffect(() => {
        const progressKeys = Object.keys(documentsProgress).length;
        const docCount = documentsList.length;

        if (progressKeys !== prevProgressKeysRef.current || docCount !== prevDocCountRef.current) {
            console.log('📋 待处理文档列表 - documentsProgress 键数:', progressKeys);
            console.log('📋 待处理文档列表 - 文档数量:', docCount);
            prevProgressKeysRef.current = progressKeys;
            prevDocCountRef.current = docCount;
        }
    }, [documentsProgress, documentsList]);

    if (!documentsList || documentsList.length === 0) {
        return null;
    }

    return (
        <Card
            title={
                <div className="pending-documents-list__header">
                    <span>{t('ragFlow.component.pendingDocuments')}</span>
                    <span className="pending-documents-list__count">
                        ({documentsList.length})
                    </span>
                </div>
            }
            size="small"
            className="pending-documents-list"
        >
            {/* ⭐ 批量操作工具栏 */}
            <div className="pending-documents-list__toolbar">
                {/* 搜索框 */}
                <Input
                    placeholder={t('ragFlow.component.searchDocuments')}
                    prefix={<SearchOutlined />}
                    value={filterKeyword}
                    onChange={(e) => onFilterChange && onFilterChange(e.target.value)}
                    size="small"
                    allowClear
                    className="pending-documents-list__search"
                />

                {/* 全选 */}
                <div className="pending-documents-list__select-all">
                    <Checkbox
                        indeterminate={selectedDocIds.length > 0 && selectedDocIds.length < documentsList.length}
                        checked={selectedDocIds.length === documentsList.length && documentsList.length > 0}
                        onChange={onSelectAll}
                    >
                        {selectedDocIds.length > 0
                            ? t('ragFlow.component.selectedCount').replace('{count}', selectedDocIds.length)
                            : t('ragFlow.component.selectAll')}
                    </Checkbox>
                </div>

                {/* ⭐ 按文件类型选择 */}
                {Object.keys(fileTypeStats).length > 1 && (
                    <div className="pending-documents-list__file-types">
                        <div className="pending-documents-list__file-types-label">
                            {t('ragFlow.component.selectByFileType')}:
                        </div>
                        <div className="pending-documents-list__file-types-list">
                            {Object.entries(fileTypeStats)
                                .sort((a, b) => b[1].count - a[1].count)
                                .map(([fileType, stats]) => (
                                    <Checkbox
                                        key={fileType}
                                        indeterminate={isFileTypeIndeterminate && isFileTypeIndeterminate(fileType)}
                                        checked={isFileTypeSelected && isFileTypeSelected(fileType)}
                                        onChange={() => onToggleFileTypeSelect && onToggleFileTypeSelect(fileType)}
                                        className="pending-documents-list__file-type-checkbox"
                                    >
                                        <span className="pending-documents-list__file-type-label">
                                            .{fileType}
                                        </span>
                                        <span className="pending-documents-list__file-type-count">
                                            ({stats.count})
                                        </span>
                                    </Checkbox>
                                ))}
                        </div>
                    </div>
                )}

                {/* 批量操作区 */}
                {selectedDocIds.length > 0 && (
                    <div className="pending-documents-list__batch-actions">
                        <Divider style={{ margin: '8px 0' }} />
                        <Space direction="vertical" style={{ width: '100%' }} size="small">
                            <Select
                                placeholder={t('ragFlow.component.selectBatchTemplate')}
                                style={{ width: '100%' }}
                                size="small"
                                value={batchTemplateId}
                                onChange={onBatchTemplateChange}
                            >
                                {strategyTemplates.map(template => (
                                    <Option key={template.id} value={template.id}>
                                        {template.name}
                                    </Option>
                                ))}
                            </Select>
                            <Button
                                type="primary"
                                size="small"
                                block
                                onClick={onBatchProcess}
                                disabled={!batchTemplateId}
                            >
                                {t('ragFlow.component.batchProcess')}
                            </Button>
                        </Space>
                    </div>
                )}
            </div>

            {/* 文档列表 */}
            <div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
                {documentsList.map((doc) => (
                    <div key={doc.documentId} className="pending-documents-list__item-wrapper">
                        {/* 复选框 */}
                        <Checkbox
                            checked={selectedDocIds.includes(doc.documentId)}
                            onChange={() => onToggleDocSelect && onToggleDocSelect(doc.documentId)}
                            className="pending-documents-list__checkbox"
                        />
                        {/* 文档项 */}
                        <div style={{ flex: 1 }}>
                            <DocumentListItem
                                doc={doc}
                                isSelected={selectedDocId === doc.documentId}
                                progress={documentsProgress[doc.documentId]}
                                strategyTemplates={strategyTemplates}
                                onSelect={() => onSelectDocument(doc.documentId)}
                                onApplyTemplate={(templateId) => onApplyTemplate(doc.documentId, templateId)}
                                onDeleteTemplate={onDeleteTemplate}
                                onStartProcess={() => onStartProcess(doc.documentId)}
                            />
                        </div>
                    </div>
                ))}
            </div>
        </Card>
    );
}

export default PendingDocumentsList;

