# 📊 RAG Optimization Dashboard

RAG优化效果可视化Dashboard组件，提供实时性能监控和算法效果分析。

## 🎯 功能特性

### 核心功能
- ✅ **实时数据展示** - 30秒自动刷新
- ✅ **多维度统计** - 精度、延迟、使用率
- ✅ **可视化图表** - 趋势图、柱状图、饼图
- ✅ **算法对比** - 多算法性能对比
- ✅ **活动监控** - 最近10条执行记录

### 数据展示
- 📊 总查询数统计
- 📈 平均精度提升
- ⚡ 平均延迟时间
- 🏆 最佳性能算法

## 🚀 快速开始

### 1. 在页面中使用

```jsx
import OptimizationDashboard from '@/components/optimization/OptimizationDashboard';

function MyPage() {
  return (
    <div>
      <OptimizationDashboard />
    </div>
  );
}
```

### 2. 添加到路由

```jsx
// 在 src/pages 中创建页面
import OptimizationDashboard from '@/components/optimization/OptimizationDashboard';

export default function OptimizationPage() {
  return <OptimizationDashboard />;
}
```

### 3. 配置API地址

在 `.env` 文件中配置后端API地址：

```env
VITE_API_BASE_URL=http://localhost:8080/api/optimization
```

## 📦 依赖要求

确保已安装以下依赖：

```bash
npm install recharts
# 或
pnpm install recharts
```

### package.json
```json
{
  "dependencies": {
    "recharts": "^2.10.0"
  }
}
```

## 🔌 后端API要求

Dashboard需要以下REST API端点：

### 1. 获取Dashboard数据
```
GET /api/optimization/dashboard

Response:
{
  "timestamp": 1702800000000,
  "algorithmStats": {
    "ppl": {
      "algorithmType": "ppl",
      "totalExecutions": 150,
      "avgPrecisionGain": 22.5,
      "avgLatencyMs": 15.0,
      "successRate": 100.0
    }
  },
  "overall": {
    "totalQueries": 500,
    "avgPrecisionGain": 35.2,
    "avgLatencyMs": 125.0,
    "mostUsedAlgorithm": "ppl",
    "bestPerformingAlgorithm": "hope_routing"
  },
  "trends": [...],
  "recentMetrics": [...]
}
```

### 2. 记录指标
```
POST /api/optimization/metrics

Body:
{
  "documentId": "doc-123",
  "algorithmType": "ppl",
  "precisionGain": 22.5,
  "latencyMs": 15,
  "relevanceScore": 0.92,
  "resultCount": 10
}
```

## 🎨 自定义样式

可以通过覆盖CSS变量来自定义样式：

```css
.optimization-dashboard {
  --primary-color: #667eea;
  --success-color: #50c878;
  --warning-color: #ffa500;
  --danger-color: #dc3545;
}
```

## 📊 图表说明

### 1. 精度趋势图（Line Chart）
- 展示各算法在时间轴上的精度变化
- 支持多算法对比
- 自动颜色区分

### 2. 算法对比图（Bar Chart）
- 横向对比各算法的精度提升和延迟
- 双柱状图并列显示

### 3. 使用率分布图（Pie Chart）
- 展示各算法的使用频率分布
- 百分比标签显示

### 4. 延迟分布图（Bar Chart）
- 各算法的延迟对比
- 颜色编码识别

## 🔧 开发指南

### 本地开发

```bash
# 安装依赖
pnpm install

# 启动开发服务器
pnpm dev

# 访问
http://localhost:3000
```

### 构建生产版本

```bash
pnpm build
```

### 调试模式

在浏览器控制台查看日志：
```javascript
// 启用详细日志
localStorage.setItem('debug', 'optimization:*');
```

## 📝 数据结构

### OptimizationMetric
```typescript
interface OptimizationMetric {
  metricId: string;
  documentId: string;
  algorithmType: string;
  timestamp: number;
  precisionGain: number;    // 精度提升(%)
  latencyMs: number;         // 延迟(ms)
  relevanceScore: number;    // 相关度评分
  resultCount: number;       // 结果数量
  metadata: Record<string, any>;
}
```

### AlgorithmStatistics
```typescript
interface AlgorithmStatistics {
  algorithmType: string;
  totalExecutions: number;
  avgPrecisionGain: number;
  avgLatencyMs: number;
  successRate: number;
  lastUpdated: number;
  precisionHistory: number[];
  latencyHistory: number[];
}
```

## 🎯 使用示例

### 示例1: 嵌入到管理后台

```jsx
import { Layout } from '@/components/layout';
import OptimizationDashboard from '@/components/optimization/OptimizationDashboard';

export default function AdminDashboard() {
  return (
    <Layout>
      <div className="admin-content">
        <h1>系统监控</h1>
        <OptimizationDashboard />
      </div>
    </Layout>
  );
}
```

### 示例2: 作为独立页面

```jsx
// src/pages/optimization.jsx
import OptimizationDashboard from '@/components/optimization/OptimizationDashboard';

export default function OptimizationPage() {
  return (
    <div className="page-container">
      <OptimizationDashboard />
    </div>
  );
}
```

### 示例3: 集成到Tab页面

```jsx
import { Tabs } from '@/components/common';
import OptimizationDashboard from '@/components/optimization/OptimizationDashboard';

export default function MonitoringPage() {
  return (
    <Tabs>
      <Tabs.Panel label="性能监控">
        <OptimizationDashboard />
      </Tabs.Panel>
      <Tabs.Panel label="其他监控">
        {/* 其他内容 */}
      </Tabs.Panel>
    </Tabs>
  );
}
```

## 🐛 故障排查

### 问题1: 无法加载数据

**症状**: 显示"Error Loading Dashboard"

**解决方案**:
1. 检查后端API是否启动: `http://localhost:8080/api/optimization/dashboard`
2. 检查网络请求是否被CORS阻止
3. 确认`.env`文件中的API地址配置正确

### 问题2: 图表不显示

**症状**: 白屏或图表区域空白

**解决方案**:
1. 确认`recharts`已正确安装
2. 检查浏览器控制台是否有错误
3. 确认数据格式正确

### 问题3: 样式错乱

**症状**: 布局混乱或样式丢失

**解决方案**:
1. 确认CSS文件已正确导入
2. 检查是否有CSS冲突
3. 清除浏览器缓存

## 📞 技术支持

- 文档: [完整文档](../../docs/AUTO_OPTIMIZATION_SELECTOR_GUIDE.md)
- 示例: [使用示例](../../omni-agent-core/src/main/java/top/yumbo/ai/omni/example)
- Issues: [GitHub Issues](https://github.com/omni-agent/issues)

## 📄 License

MIT License

---

**版本**: 3.0.0  
**作者**: OmniAgent Team  
**最后更新**: 2025-12-17

