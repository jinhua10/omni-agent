/**
 * 工作流市场组件 (Workflow Market Component)
 *
 * 集成到主应用的菜单系统中
 *
 * @author OmniAgent Team
 * @since 2025-12-20
 */

import React, { useState } from 'react';
import MarketBrowser from './MarketBrowser';
import WorkflowDetail from './WorkflowDetail';
import '../../assets/css/workflow/workflow-market.css';

const WorkflowMarket = () => {
  const [currentView, setCurrentView] = useState('browser'); // 'browser' | 'detail'
  const [selectedWorkflowId, setSelectedWorkflowId] = useState(null);

  const handleViewDetail = (workflowId) => {
    setSelectedWorkflowId(workflowId);
    setCurrentView('detail');
  };

  const handleBackToBrowser = () => {
    setCurrentView('browser');
    setSelectedWorkflowId(null);
  };

  return (
    <div className="workflow-market">
      {currentView === 'browser' && (
        <MarketBrowser onViewDetail={handleViewDetail} />
      )}
      {currentView === 'detail' && (
        <WorkflowDetail
          workflowId={selectedWorkflowId}
          onBack={handleBackToBrowser}
        />
      )}
    </div>
  );
};

export default WorkflowMarket;

