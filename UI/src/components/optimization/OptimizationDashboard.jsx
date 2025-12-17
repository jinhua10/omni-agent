import React, { useState, useEffect } from 'react';
import {
  LineChart, Line, BarChart, Bar, PieChart, Pie, Cell,
  XAxis, YAxis, CartesianGrid, Tooltip, Legend, ResponsiveContainer
} from 'recharts';
import './OptimizationDashboard.css';

/**
 * RAG优化效果可视化Dashboard
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
const OptimizationDashboard = () => {
  const [dashboardData, setDashboardData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);
  const [selectedAlgorithm, setSelectedAlgorithm] = useState('all');
  const [timeRange, setTimeRange] = useState('24h');

  // API基础URL
  const API_BASE_URL = import.meta.env.VITE_API_BASE_URL || 'http://localhost:8080/api/optimization';

  // 颜色方案
  const COLORS = {
    ppl: '#8884d8',
    hyde: '#82ca9d',
    rerank: '#ffc658',
    query_expansion: '#ff8042',
    metadata_filter: '#a4de6c',
    context_compression: '#d0ed57',
    semantic_chunking: '#83a6ed',
    hybrid_search: '#8dd1e1',
    knowledge_graph: '#d084d0',
    hope_routing: '#ffa07a',
    behavior_analysis: '#98d98e',
    multi_model_voting: '#ff6b9d'
  };

  // 获取Dashboard数据
  const fetchDashboardData = async () => {
    try {
      setLoading(true);
      const response = await fetch(`${API_BASE_URL}/dashboard`);
      if (!response.ok) {
        throw new Error('Failed to fetch dashboard data');
      }
      const data = await response.json();
      setDashboardData(data);
      setError(null);
    } catch (err) {
      setError(err.message);
      console.error('Error fetching dashboard data:', err);
    } finally {
      setLoading(false);
    }
  };

  // 组件加载时获取数据，并设置定时刷新
  useEffect(() => {
    fetchDashboardData();
    const interval = setInterval(fetchDashboardData, 30000); // 每30秒刷新
    return () => clearInterval(interval);
  }, []);

  // 加载状态
  if (loading && !dashboardData) {
    return (
      <div className="dashboard-loading">
        <div className="spinner"></div>
        <p>Loading Dashboard...</p>
      </div>
    );
  }

  // 错误状态
  if (error) {
    return (
      <div className="dashboard-error">
        <h3>⚠️ Error Loading Dashboard</h3>
        <p>{error}</p>
        <button onClick={fetchDashboardData}>Retry</button>
      </div>
    );
  }

  // 无数据状态
  if (!dashboardData) {
    return (
      <div className="dashboard-no-data">
        <h3>📊 No Data Available</h3>
        <p>Start using RAG optimization algorithms to see statistics</p>
      </div>
    );
  }

  const { algorithmStats, overall, trends, recentMetrics } = dashboardData;

  // 准备算法对比数据
  const algorithmComparisonData = Object.entries(algorithmStats || {}).map(([name, stats]) => ({
    name: name.replace(/_/g, ' '),
    precision: parseFloat(stats.avgPrecisionGain.toFixed(2)),
    latency: parseInt(stats.avgLatencyMs.toFixed(0)),
    executions: stats.totalExecutions
  }));

  // 准备趋势数据
  const trendChartData = trends?.map(trend => ({
    time: trend.label,
    ...Object.entries(trend.values).reduce((acc, [alg, val]) => {
      acc[alg] = parseFloat(val.toFixed(2));
      return acc;
    }, {})
  })) || [];

  // 准备使用率数据
  const usageData = Object.entries(algorithmStats || {}).map(([name, stats]) => ({
    name: name.replace(/_/g, ' '),
    value: stats.totalExecutions
  }));

  return (
    <div className="optimization-dashboard">
      {/* 头部 */}
      <header className="dashboard-header">
        <h1>🎯 RAG Optimization Dashboard</h1>
        <div className="header-actions">
          <select
            value={selectedAlgorithm}
            onChange={(e) => setSelectedAlgorithm(e.target.value)}
            className="algorithm-selector"
          >
            <option value="all">All Algorithms</option>
            {Object.keys(algorithmStats || {}).map(alg => (
              <option key={alg} value={alg}>{alg.replace(/_/g, ' ')}</option>
            ))}
          </select>
          <select
            value={timeRange}
            onChange={(e) => setTimeRange(e.target.value)}
            className="time-range-selector"
          >
            <option value="1h">Last Hour</option>
            <option value="24h">Last 24 Hours</option>
            <option value="7d">Last 7 Days</option>
          </select>
          <button onClick={fetchDashboardData} className="refresh-button">
            🔄 Refresh
          </button>
        </div>
      </header>

      {/* 整体统计卡片 */}
      <div className="stats-cards">
        <StatCard
          title="Total Queries"
          value={overall?.totalQueries || 0}
          icon="📊"
          color="#4a90e2"
        />
        <StatCard
          title="Avg Precision Gain"
          value={`+${(overall?.avgPrecisionGain || 0).toFixed(1)}%`}
          icon="📈"
          color="#50c878"
        />
        <StatCard
          title="Avg Latency"
          value={`${(overall?.avgLatencyMs || 0).toFixed(0)}ms`}
          icon="⚡"
          color="#ffa500"
        />
        <StatCard
          title="Best Algorithm"
          value={overall?.bestPerformingAlgorithm?.replace(/_/g, ' ') || 'N/A'}
          icon="🏆"
          color="#ff6b9d"
        />
      </div>

      {/* 主要图表区域 */}
      <div className="charts-grid">
        {/* 精度趋势图 */}
        <div className="chart-container">
          <h3>📈 Precision Gain Trends</h3>
          <ResponsiveContainer width="100%" height={300}>
            <LineChart data={trendChartData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="time" />
              <YAxis label={{ value: 'Precision Gain (%)', angle: -90, position: 'insideLeft' }} />
              <Tooltip />
              <Legend />
              {Object.keys(algorithmStats || {}).map((alg, index) => (
                <Line
                  key={alg}
                  type="monotone"
                  dataKey={alg}
                  stroke={COLORS[alg] || `hsl(${index * 30}, 70%, 50%)`}
                  strokeWidth={2}
                  dot={{ r: 3 }}
                />
              ))}
            </LineChart>
          </ResponsiveContainer>
        </div>

        {/* 算法对比柱状图 */}
        <div className="chart-container">
          <h3>⚖️ Algorithm Comparison</h3>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={algorithmComparisonData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="name" angle={-45} textAnchor="end" height={80} />
              <YAxis />
              <Tooltip />
              <Legend />
              <Bar dataKey="precision" fill="#82ca9d" name="Precision Gain (%)" />
              <Bar dataKey="latency" fill="#ffc658" name="Latency (ms)" />
            </BarChart>
          </ResponsiveContainer>
        </div>

        {/* 使用率饼图 */}
        <div className="chart-container">
          <h3>🥧 Algorithm Usage Distribution</h3>
          <ResponsiveContainer width="100%" height={300}>
            <PieChart>
              <Pie
                data={usageData}
                cx="50%"
                cy="50%"
                labelLine={false}
                label={({ name, percent }) => `${name}: ${(percent * 100).toFixed(0)}%`}
                outerRadius={80}
                fill="#8884d8"
                dataKey="value"
              >
                {usageData.map((entry, index) => (
                  <Cell
                    key={`cell-${index}`}
                    fill={COLORS[entry.name.replace(/ /g, '_')] || COLORS[Object.keys(COLORS)[index % Object.keys(COLORS).length]]}
                  />
                ))}
              </Pie>
              <Tooltip />
            </PieChart>
          </ResponsiveContainer>
        </div>

        {/* 延迟分布图 */}
        <div className="chart-container">
          <h3>⏱️ Latency Distribution</h3>
          <ResponsiveContainer width="100%" height={300}>
            <BarChart data={algorithmComparisonData}>
              <CartesianGrid strokeDasharray="3 3" />
              <XAxis dataKey="name" angle={-45} textAnchor="end" height={80} />
              <YAxis label={{ value: 'Latency (ms)', angle: -90, position: 'insideLeft' }} />
              <Tooltip />
              <Bar dataKey="latency" fill="#ffc658">
                {algorithmComparisonData.map((entry, index) => (
                  <Cell
                    key={`cell-${index}`}
                    fill={COLORS[entry.name.replace(/ /g, '_')] || `hsl(${index * 30}, 70%, 50%)`}
                  />
                ))}
              </Bar>
            </BarChart>
          </ResponsiveContainer>
        </div>
      </div>

      {/* 算法详细统计表 */}
      <div className="algorithms-table-container">
        <h3>📋 Algorithm Statistics</h3>
        <table className="algorithms-table">
          <thead>
            <tr>
              <th>Algorithm</th>
              <th>Executions</th>
              <th>Avg Precision Gain</th>
              <th>Avg Latency</th>
              <th>Success Rate</th>
            </tr>
          </thead>
          <tbody>
            {Object.entries(algorithmStats || {}).map(([name, stats]) => (
              <tr key={name} onClick={() => setSelectedAlgorithm(name)}>
                <td>
                  <span className="algorithm-badge" style={{ backgroundColor: COLORS[name] }}>
                    {name.replace(/_/g, ' ')}
                  </span>
                </td>
                <td>{stats.totalExecutions}</td>
                <td className="precision-cell">+{stats.avgPrecisionGain.toFixed(2)}%</td>
                <td className="latency-cell">{stats.avgLatencyMs.toFixed(0)}ms</td>
                <td className="success-cell">{stats.successRate.toFixed(1)}%</td>
              </tr>
            ))}
          </tbody>
        </table>
      </div>

      {/* 最近活动 */}
      <div className="recent-activity">
        <h3>🕐 Recent Activity</h3>
        <div className="activity-list">
          {recentMetrics?.slice(0, 10).map((metric, index) => (
            <div key={metric.metricId || index} className="activity-item">
              <span className="activity-time">
                {new Date(metric.timestamp).toLocaleTimeString()}
              </span>
              <span className="activity-algorithm" style={{ color: COLORS[metric.algorithmType] }}>
                {metric.algorithmType.replace(/_/g, ' ')}
              </span>
              <span className="activity-precision">+{metric.precisionGain.toFixed(1)}%</span>
              <span className="activity-latency">{metric.latencyMs}ms</span>
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};

// 统计卡片组件
const StatCard = ({ title, value, icon, color }) => (
  <div className="stat-card" style={{ borderLeftColor: color }}>
    <div className="stat-icon">{icon}</div>
    <div className="stat-content">
      <div className="stat-title">{title}</div>
      <div className="stat-value" style={{ color }}>{value}</div>
    </div>
  </div>
);

export default OptimizationDashboard;

