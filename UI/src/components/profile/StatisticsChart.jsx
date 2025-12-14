import React from 'react';
import { useLanguage } from '../../contexts/LanguageContext';

const StatisticsChart = ({ data }) => {
  const { t, currentLanguage } = useLanguage();
  
  // 简化版图表组件，实际项目中可以使用 ECharts 或 Chart.js / Simplified chart component, can use ECharts or Chart.js in production
  const displayText = data 
    ? (currentLanguage === 'zh' ? `显示${data.length}条数据的图表` : `Chart with ${data.length} data points`)
    : t('common.loading');
    
  return (
    <div className="statistics-chart">
      <div className="statistics-chart__placeholder">
        📊 {displayText}
      </div>
    </div>
  );
};

export default StatisticsChart;

