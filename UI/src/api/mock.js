/**
 * Mock 数据服务 (Mock Data Service)
 *
 * 在后端未启动时提供模拟数据
 * (Provides mock data when backend is not available)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

// 是否启用 Mock 数据 (Enable mock data)
const ENABLE_MOCK = import.meta.env.VITE_ENABLE_MOCK === 'true' || false

/**
 * 模拟延迟 (Simulate delay)
 */
const delay = (ms = 500) => new Promise(resolve => setTimeout(resolve, ms))

/**
 * Mock 数据生成器 (Mock data generators)
 */
export const mockData = {
  // 文档列表
  documents: {
    list: [
      {
        id: '1',
        name: 'React 开发指南.pdf',
        size: 2048000,
        uploadTime: new Date().toISOString(),
        tags: ['前端', 'React'],
        category: '技术文档',
      },
      {
        id: '2',
        name: 'Spring Boot 最佳实践.docx',
        size: 1536000,
        uploadTime: new Date(Date.now() - 86400000).toISOString(),
        tags: ['后端', 'Java'],
        category: '技术文档',
      },
      {
        id: '3',
        name: '项目需求文档.md',
        size: 512000,
        uploadTime: new Date(Date.now() - 172800000).toISOString(),
        tags: ['需求', '项目'],
        category: '项目文档',
      },
    ],
    total: 3,
  },

  // 角色列表
  roles: {
    list: [
      {
        id: '1',
        name: '前端开发',
        description: '负责前端相关的问题',
        keywords: ['React', 'Vue', 'JavaScript', 'CSS', 'HTML'],
        icon: '💻',
        enabled: true,
        usageCount: 150,
      },
      {
        id: '2',
        name: '后端开发',
        description: '负责后端相关的问题',
        keywords: ['Java', 'Spring', 'MySQL', 'Redis'],
        icon: '🔧',
        enabled: true,
        usageCount: 120,
      },
      {
        id: '3',
        name: '运维部署',
        description: '负责部署和运维相关的问题',
        keywords: ['Docker', 'K8s', 'CI/CD', 'Nginx'],
        icon: '🚀',
        enabled: false,
        usageCount: 80,
      },
    ],
  },

  // 冲突列表
  conflicts: {
    list: [
      {
        id: '1',
        question: 'React Hooks 的最佳实践是什么？',
        conceptA: '使用 useEffect 处理所有副作用',
        conceptB: '优先使用 useMemo 和 useCallback 优化性能',
        status: 'pending',
        voteA: 0,
        voteB: 0,
        createdAt: new Date().toISOString(),
      },
      {
        id: '2',
        question: 'Spring Boot 如何配置数据源？',
        conceptA: '使用 application.yml 配置',
        conceptB: '使用 Java Config 类配置',
        status: 'voting',
        voteA: 15,
        voteB: 8,
        createdAt: new Date(Date.now() - 86400000).toISOString(),
      },
    ],
  },

  // 协作伙伴
  peers: {
    list: [
      {
        id: '1',
        name: '开发服务器-01',
        status: 'online',
        sharedDocs: 25,
        lastSync: new Date(Date.now() - 3600000).toISOString(),
      },
      {
        id: '2',
        name: '测试环境',
        status: 'offline',
        sharedDocs: 12,
        lastSync: new Date(Date.now() - 86400000).toISOString(),
      },
    ],
  },

  // 演化历史
  evolution: [
    {
      id: '1',
      type: 'created',
      title: '创建新概念',
      description: 'React Hooks 概念已创建',
      timestamp: new Date(Date.now() - 172800000).toISOString(),
    },
    {
      id: '2',
      type: 'updated',
      title: '概念更新',
      description: 'React Hooks 最佳实践已更新',
      timestamp: new Date(Date.now() - 86400000).toISOString(),
      changes: {
        before: '旧的实践方式',
        after: '新的实践方式',
      },
    },
  ],

  // 质量监控
  quality: {
    totalConflicts: 45,
    resolvedConflicts: 32,
    pendingConflicts: 13,
    averageQuality: 0.85,
    concepts: [
      {
        concept: 'React Hooks',
        conflictCount: 8,
        resolvedCount: 6,
        qualityScore: 0.9,
      },
      {
        concept: 'Spring Boot',
        conflictCount: 12,
        resolvedCount: 10,
        qualityScore: 0.88,
      },
    ],
  },

  // 交换历史
  exchangeHistory: [
    {
      id: '1',
      timestamp: new Date().toISOString(),
      type: 'send',
      peerName: '开发服务器-01',
      content: '分享了 React 开发指南',
      status: 'success',
    },
    {
      id: '2',
      timestamp: new Date(Date.now() - 3600000).toISOString(),
      type: 'receive',
      peerName: '测试环境',
      content: '接收了测试文档',
      status: 'success',
    },
  ],

  // 网络拓扑
  topology: {
    nodes: [
      { id: '1', name: '开发服务器-01' },
      { id: '2', name: '测试环境' },
      { id: '3', name: '生产环境' },
    ],
    connections: 3,
  },

  // 同步状态
  syncStatus: {
    totalSyncs: 120,
    successSyncs: 110,
    failedSyncs: 10,
    recentSyncs: [
      {
        id: '1',
        peerName: '开发服务器-01',
        status: 'success',
        description: '同步完成',
        timestamp: new Date().toISOString(),
        progress: 100,
      },
      {
        id: '2',
        peerName: '测试环境',
        status: 'failed',
        description: '连接超时',
        timestamp: new Date(Date.now() - 3600000).toISOString(),
        progress: 50,
      },
    ],
  },

  // 角色统计
  roleStatistics: [
    {
      id: '1',
      name: '前端开发',
      usageCount: 150,
      successRate: 0.92,
    },
    {
      id: '2',
      name: '后端开发',
      usageCount: 120,
      successRate: 0.88,
    },
  ],

  // 愿望单
  wishes: {
    list: [
      {
        id: '1',
        title: '支持暗色模式',
        description: '希望系统能够支持暗色模式，保护眼睛，特别是在晚上使用时。建议可以自动切换，也可以手动切换。',
        category: 'interface',
        status: 'in_progress',
        votes: 42,
        commentsCount: 8,
        author: {
          id: '1',
          name: '张三',
          avatar: null,
        },
        createdAt: new Date(Date.now() - 3 * 86400000).toISOString(),
        updatedAt: new Date(Date.now() - 86400000).toISOString(),
        userVoted: 'up',
      },
      {
        id: '2',
        title: '添加代码高亮功能',
        description: '在问答中展示代码时，希望能够支持语法高亮，支持多种编程语言，提升代码可读性。',
        category: 'feature',
        status: 'completed',
        votes: 38,
        commentsCount: 12,
        author: {
          id: '2',
          name: '李四',
          avatar: null,
        },
        createdAt: new Date(Date.now() - 7 * 86400000).toISOString(),
        updatedAt: new Date(Date.now() - 2 * 86400000).toISOString(),
        userVoted: null,
      },
      {
        id: '3',
        title: '修复文档上传失败的问题',
        description: '当上传大文件时（>50MB），经常会出现上传失败的情况，希望能够修复这个问题。',
        category: 'bug',
        status: 'pending',
        votes: 35,
        commentsCount: 5,
        author: {
          id: '3',
          name: '王五',
          avatar: null,
        },
        createdAt: new Date(Date.now() - 2 * 86400000).toISOString(),
        updatedAt: new Date(Date.now() - 86400000).toISOString(),
        userVoted: null,
      },
      {
        id: '4',
        title: '增加导出对话记录功能',
        description: '希望能够将问答历史导出为 Markdown 或 PDF 格式，方便保存和分享。',
        category: 'feature',
        status: 'pending',
        votes: 30,
        commentsCount: 3,
        author: {
          id: '4',
          name: '赵六',
          avatar: null,
        },
        createdAt: new Date(Date.now() - 86400000).toISOString(),
        updatedAt: new Date(Date.now() - 3600000).toISOString(),
        userVoted: null,
      },
      {
        id: '5',
        title: '优化搜索功能',
        description: '当前的搜索功能不够智能，希望能够支持模糊搜索、关键词高亮等功能。',
        category: 'interface',
        status: 'pending',
        votes: 28,
        commentsCount: 7,
        author: {
          id: '5',
          name: '孙七',
          avatar: null,
        },
        createdAt: new Date(Date.now() - 12 * 3600000).toISOString(),
        updatedAt: new Date(Date.now() - 6 * 3600000).toISOString(),
        userVoted: null,
      },
    ],
  },

  // 愿望详情（包含状态历史）
  wishDetail: {
    '1': {
      id: '1',
      title: '支持暗色模式',
      description: '希望系统能够支持暗色模式，保护眼睛，特别是在晚上使用时。建议可以自动切换，也可以手动切换。',
      category: 'interface',
      status: 'in_progress',
      votes: 42,
      commentsCount: 8,
      author: {
        id: '1',
        name: '张三',
        avatar: null,
      },
      createdAt: new Date(Date.now() - 3 * 86400000).toISOString(),
      updatedAt: new Date(Date.now() - 86400000).toISOString(),
      statusHistory: [
        {
          status: 'pending',
          timestamp: new Date(Date.now() - 3 * 86400000).toISOString(),
          comment: '愿望已提交，等待审核',
        },
        {
          status: 'in_progress',
          timestamp: new Date(Date.now() - 2 * 86400000).toISOString(),
          comment: '已通过审核，开始实施',
        },
      ],
    },
  },

  // 愿望评论
  wishComments: {
    '1': [
      {
        id: '1',
        content: '非常期待这个功能！',
        author: {
          id: '10',
          name: '用户A',
          avatar: null,
        },
        likes: 5,
        userLiked: false,
        createdAt: new Date(Date.now() - 2 * 86400000).toISOString(),
        replies: [
          {
            id: '2',
            content: '同感！希望能尽快实现',
            author: {
              id: '11',
              name: '用户B',
              avatar: null,
            },
            likes: 2,
            userLiked: false,
            createdAt: new Date(Date.now() - 86400000).toISOString(),
          },
        ],
      },
      {
        id: '3',
        content: '建议参考 GitHub 的暗色模式实现',
        author: {
          id: '12',
          name: '用户C',
          avatar: null,
        },
        likes: 3,
        userLiked: true,
        createdAt: new Date(Date.now() - 86400000).toISOString(),
        replies: [],
      },
    ],
  },

  // 愿望排行榜
  wishRanking: [
    {
      id: '1',
      title: '支持暗色模式',
      votes: 42,
    },
    {
      id: '2',
      title: '添加代码高亮功能',
      votes: 38,
    },
    {
      id: '3',
      title: '修复文档上传失败的问题',
      votes: 35,
    },
    {
      id: '4',
      title: '增加导出对话记录功能',
      votes: 30,
    },
    {
      id: '5',
      title: '优化搜索功能',
      votes: 28,
    },
  ],

  // AI服务
  services: [
    {
      id: '1',
      name: 'PPT生成器',
      description: '根据主题和大纲自动生成精美的PPT演示文稿',
      category: 'generation',
      icon: '📊',
      rating: 4.8,
      usageCount: 1250,
      author: '官方',
      installed: true,
      isPopular: true,
      version: '1.2.0',
      size: '15MB',
      features: [
        '支持多种演示风格',
        '自动生成大纲',
        '智能排版',
        '一键导出'
      ],
      config: {
        enabled: true,
        model: 'local',
      },
    },
    {
      id: '2',
      name: '代码分析助手',
      description: '智能分析代码质量，提供优化建议',
      category: 'analysis',
      icon: '🔍',
      rating: 4.6,
      usageCount: 890,
      author: '官方',
      installed: false,
      isNew: true,
      version: '1.0.0',
      size: '8MB',
      features: [
        '代码质量检测',
        '性能分析',
        '安全漏洞扫描',
        '重构建议'
      ],
    },
    {
      id: '3',
      name: '文档转换器',
      description: '支持多种文档格式之间的智能转换',
      category: 'conversion',
      icon: '🔄',
      rating: 4.5,
      usageCount: 650,
      author: '第三方',
      installed: false,
      version: '2.1.0',
      size: '12MB',
      features: [
        '支持PDF、Word、Markdown等格式',
        '保持原有格式',
        '批量转换',
        '高质量输出'
      ],
    },
  ],

  // 用户资料
  userProfile: {
    id: '1',
    nickname: '张三',
    email: 'zhangsan@example.com',
    bio: '热爱技术，持续学习中...',
    avatar: null,
    statistics: {
      qaCount: 150,
      documentCount: 25,
      feedbackCount: 42,
      contributionScore: 850,
      activeHours: 120,
      trendData: [
        { date: '2025-12-06', count: 10 },
        { date: '2025-12-07', count: 15 },
        { date: '2025-12-08', count: 12 },
        { date: '2025-12-09', count: 20 },
        { date: '2025-12-10', count: 18 },
        { date: '2025-12-11', count: 22 },
        { date: '2025-12-12', count: 25 },
      ],
    },
  },

  // 贡献统计
  contributions: [
    { name: '问答贡献', score: 85 },
    { name: '文档上传', score: 72 },
    { name: '反馈质量', score: 90 },
    { name: '协作贡献', score: 65 },
  ],

  // 成就列表
  achievements: [
    {
      id: '1',
      title: '初来乍到',
      description: '完成首次问答',
      type: 'bronze',
      unlocked: true,
      progress: 100,
    },
    {
      id: '2',
      title: '知识达人',
      description: '累计问答100次',
      type: 'silver',
      unlocked: true,
      progress: 100,
    },
    {
      id: '3',
      title: '贡献之星',
      description: '获得1000贡献分',
      type: 'gold',
      unlocked: false,
      progress: 85,
    },
    {
      id: '4',
      title: '钻石会员',
      description: '成为活跃用户',
      type: 'diamond',
      unlocked: false,
      progress: 60,
    },
  ],

  // 系统日志
  logs: [
    { level: 'INFO', timestamp: '2025-12-12 16:30:00', message: '系统启动成功' },
    { level: 'INFO', timestamp: '2025-12-12 16:31:15', message: '用户登录: zhangsan' },
    { level: 'WARN', timestamp: '2025-12-12 16:32:30', message: 'API响应时间较长: 2500ms' },
    { level: 'ERROR', timestamp: '2025-12-12 16:33:45', message: '数据库连接超时' },
    { level: 'INFO', timestamp: '2025-12-12 16:35:00', message: '文档上传成功' },
  ],

  // 监控指标
  metrics: {
    cpu: 45,
    memory: 68,
    requests: 1250,
    errors: 5,
  },
}

/**
 * Mock API 拦截器
 */
export async function mockRequest(url, method = 'GET', data = null) {
  if (!ENABLE_MOCK) {
    return null // 不使用 mock
  }

  await delay(300) // 模拟网络延迟

  // 文档 API
  if (url.includes('/documents')) {
    if (method === 'GET' && !url.includes('/')) {
      return { data: mockData.documents }
    }
  }

  // 角色 API
  if (url.includes('/roles')) {
    if (method === 'GET' && url === '/roles') {
      return { data: mockData.roles }
    }
    if (url.includes('/statistics')) {
      return { data: mockData.roleStatistics }
    }
  }

  // 反馈 API
  if (url.includes('/feedback/conflicts')) {
    return { data: mockData.conflicts }
  }
  if (url.includes('/feedback/evolution')) {
    return { data: mockData.evolution }
  }
  if (url.includes('/feedback/quality-monitor')) {
    return { data: mockData.quality }
  }

  // 协作 API
  if (url.includes('/collaboration/peers')) {
    return { data: mockData.peers }
  }
  if (url.includes('/collaboration/exchange-history')) {
    return { data: mockData.exchangeHistory }
  }
  if (url.includes('/collaboration/topology')) {
    return { data: mockData.topology }
  }
  if (url.includes('/collaboration/sync-status')) {
    return { data: mockData.syncStatus }
  }

  // 愿望单 API
  if (url.includes('/wishes')) {
    // 获取愿望列表
    if (method === 'GET' && url === '/api/wishes') {
      return { data: mockData.wishes.list }
    }
    // 获取愿望详情
    if (method === 'GET' && url.match(/\/api\/wishes\/\d+$/)) {
      const id = url.split('/').pop()
      return { data: mockData.wishDetail[id] || mockData.wishes.list.find(w => w.id === id) }
    }
    // 提交愿望
    if (method === 'POST' && url === '/api/wishes') {
      const newWish = {
        id: String(mockData.wishes.list.length + 1),
        ...data,
        votes: 0,
        commentsCount: 0,
        status: 'pending',
        author: {
          id: '999',
          name: '当前用户',
          avatar: null,
        },
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        userVoted: null,
      }
      mockData.wishes.list.unshift(newWish)
      return { data: newWish }
    }
    // 投票
    if (method === 'POST' && url.includes('/vote')) {
      return { data: { success: true } }
    }
    // 获取评论
    if (method === 'GET' && url.includes('/comments')) {
      const wishId = url.split('/')[3]
      return { data: mockData.wishComments[wishId] || [] }
    }
    // 添加评论
    if (method === 'POST' && url.includes('/comments')) {
      return { data: { success: true } }
    }
    // 获取排行榜
    if (url.includes('/ranking')) {
      return { data: mockData.wishRanking }
    }
  }

  // 评论点赞
  if (url.includes('/comments/') && url.includes('/like')) {
    return { data: { success: true } }
  }

  // AI服务 API
  if (url.includes('/services')) {
    // 获取服务列表
    if (method === 'GET' && url === '/api/services') {
      return { data: mockData.services }
    }
    // 获取服务详情
    if (method === 'GET' && url.match(/\/api\/services\/\d+$/)) {
      const id = url.split('/').pop()
      return { data: mockData.services.find(s => s.id === id) }
    }
    // 安装服务
    if (method === 'POST' && url.includes('/install')) {
      return { data: { success: true } }
    }
    // 卸载服务
    if (method === 'POST' && url.includes('/uninstall')) {
      return { data: { success: true } }
    }
    // 更新配置
    if (method === 'PUT' && url.includes('/config')) {
      return { data: { success: true } }
    }
    // 生成PPT
    if (url.includes('/ppt/generate')) {
      return { data: { success: true, fileUrl: '/ppt/demo.pptx' } }
    }
    // 切换模型
    if (url.includes('/model/switch')) {
      return { data: { success: true } }
    }
  }

  // 个人中心 API
  if (url.includes('/profile')) {
    // 获取用户信息
    if (method === 'GET' && url === '/api/profile/info') {
      return { data: mockData.userProfile }
    }
    // 更新用户信息
    if (method === 'PUT' && url === '/api/profile/info') {
      return { data: { success: true } }
    }
    // 获取使用统计
    if (url.includes('/statistics')) {
      return { data: mockData.userProfile.statistics }
    }
    // 获取贡献统计
    if (url.includes('/contributions')) {
      return { data: mockData.contributions }
    }
    // 获取成就列表
    if (url.includes('/achievements')) {
      return { data: mockData.achievements }
    }
    // 更新设置
    if (method === 'PUT' && url.includes('/settings')) {
      return { data: { success: true } }
    }
  }

  // 系统管理 API
  if (url.includes('/admin')) {
    // 更新系统配置
    if (method === 'PUT' && url.includes('/system-config')) {
      return { data: { success: true } }
    }
    // 更新模型配置
    if (method === 'PUT' && url.includes('/model-config')) {
      return { data: { success: true } }
    }
    // 获取日志
    if (method === 'GET' && url.includes('/logs')) {
      return { data: mockData.logs }
    }
    // 获取监控指标
    if (method === 'GET' && url.includes('/metrics')) {
      return { data: mockData.metrics }
    }
    // 健康检查
    if (url.includes('/health')) {
      return { data: { status: 'healthy' } }
    }
  }

  // 主题管理 API / Theme Management API
  if (url.includes('/themes')) {
    // 上传主题 / Upload theme
    if (method === 'POST' && url.includes('/upload')) {
      return {
        data: {
          success: true,
          themeId: 'custom-' + Date.now(),
          path: '/static/themes/' + Date.now(),
          message: '主题上传成功 / Theme uploaded successfully'
        }
      }
    }
    // 获取主题列表 / Get theme list
    if (method === 'GET' && url.includes('/list')) {
      return {
        data: [] // 返回空数组，实际使用时从服务器加载 / Return empty array, load from server in production
      }
    }
    // 获取主题详情 / Get theme details
    if (method === 'GET' && url.match(/\/themes\/[^\/]+$/)) {
      const themeId = url.split('/').pop()
      return {
        data: {
          id: themeId,
          name: { zh: '主题名称', en: 'Theme Name' },
          type: 'custom',
          source: 'server'
        }
      }
    }
    // 删除主题 / Delete theme
    if (method === 'DELETE') {
      return { data: { success: true } }
    }
    // 同步主题 / Sync theme
    if (method === 'PUT' && url.includes('/sync')) {
      return { data: { success: true } }
    }
  }

  return null
}

export { ENABLE_MOCK }

