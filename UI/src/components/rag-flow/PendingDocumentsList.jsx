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
import { Card } from 'antd';
import { useLanguage } from '../../contexts/LanguageContext';
import DocumentListItem from './DocumentListItem';

function PendingDocumentsList({
    documentsList,
    selectedDocId,
    documentsProgress,
    strategyTemplates,
    onSelectDocument,
    onApplyTemplate,
    onDeleteTemplate,
    onStartProcess
}) {
    const { t } = useLanguage();

    if (!documentsList || documentsList.length === 0) {
        return null;
    }

    return (
        <Card
            title={t('ragFlow.component.pendingDocuments')}
            size="small"
            style={{ marginBottom: 16, maxHeight: '400px', overflow: 'auto' }}
        >
            <div style={{ display: 'flex', flexDirection: 'column', gap: '8px' }}>
                {documentsList.map((doc) => (
                    <DocumentListItem
                        key={doc.documentId}
                        doc={doc}
                        isSelected={selectedDocId === doc.documentId}
                        progress={documentsProgress[doc.documentId]}
                        strategyTemplates={strategyTemplates}
                        onSelect={() => onSelectDocument(doc.documentId)}
                        onApplyTemplate={(templateId) => onApplyTemplate(doc.documentId, templateId)}
                        onDeleteTemplate={onDeleteTemplate}
                        onStartProcess={() => onStartProcess(doc.documentId)}
                    />
                ))}
            </div>
        </Card>
    );
}

export default PendingDocumentsList;

