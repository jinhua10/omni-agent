/**
 * 中文语言包 (Chinese Language Pack)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

export default {
  // ============================================================================
  // 通用 (Common)
  // ============================================================================
  common: {
    confirm: '确认',
    cancel: '取消',
    save: '保存',
    delete: '删除',
    edit: '编辑',
    add: '添加',
    search: '搜索',
    filter: '筛选',
    reset: '重置',
    submit: '提交',
    close: '关闭',
    back: '返回',
    next: '下一步',
    prev: '上一步',
    finish: '完成',
    loading: '加载中...',
    success: '成功',
    error: '错误',
    warning: '警告',
    info: '提示',
    upload: '上传',
    download: '下载',
    refresh: '刷新',
    more: '更多',
    collapse: '收起',
    expand: '展开',
    loadMore: '加载更多',
    switchToDark: '切换到暗色模式',
    switchToLight: '切换到浅色模式',
    documentation: '文档',
    about: '关于',
    clearCache: '清除缓存',
    clearCacheTitle: '清除缓存',
    clearCacheDescription: '请选择要清除的缓存项：',
    clearCacheConfirm: '确定清除',
    clearCacheWarning: '⚠️ 清除后页面将自动刷新',
    floatingPanelConfig: '浮动窗口配置',
    themeSettings: '主题设置',
    uiThemeConfig: 'UI主题配置',
    otherCacheData: '其他缓存数据',
  },

  // ============================================================================
  // 主题定制器 (Theme Customizer)
  // ============================================================================
  theme: {
    colorTheme: '颜色主题',
    customizer: {
      title: '主题定制器',
      primary: '主色调',
      background: '背景色',
      surface: '表面色',
      text: '主要文本色',
      textSecondary: '次要文本色',
      border: '边框色',
      apply: '应用',
      reset: '重置',
      applySuccess: '主题已应用',
      resetSuccess: '已重置为默认主题',
      previewText: '这是主要文本',
      previewSecondary: '这是次要文本',
      previewButton: '主要按钮',
    },
  },

  // ============================================================================
  // UI主题引擎 (UI Theme Engine)
  // ============================================================================
  uiTheme: {
    switcher: {
      title: 'UI主题切换器',
      current: '当前',
      apply: '应用',
      builtin: '内置',
      custom: '自定义',
      developing: '开发中',
      comingSoon: '敬请期待',
      version: '版本',
      author: '作者',
      builtinThemes: '内置主题',
      customThemes: '自定义主题',
      management: '主题管理',
      importExport: '导入/导出主题',
      importTheme: '导入主题',
      exportTheme: '导出主题',
      export: '导出',
      uninstall: '卸载',
      confirmUninstall: '确认卸载',
      uninstallWarning: '确定要卸载这个主题吗？',
      switchSuccess: '主题切换成功',
      themeInDev: '该主题正在开发中，敬请期待',
      importSuccess: '主题导入成功',
      importFailed: '主题导入失败',
      exportSuccess: '主题导出成功',
      uninstallSuccess: '主题卸载成功',
      noCustomThemes: '暂无自定义主题',
      importTip: '点击下方按钮导入主题',
      howToUse: '如何使用',
      step1: '从主题市场或AI生成获取主题文件',
      step2: '勾选"上传到服务器"（推荐）',
      step3: '点击"导入主题"按钮选择JSON文件',
      step4: '导入成功后在"自定义主题"标签页应用',
      step5: '上传到服务器的主题会永久保存',
      aiGeneration: 'AI主题生成',
      aiGenerationDesc: '未来功能：使用AI生成独特的UI主题',
      uploadToServer: '上传到服务器（推荐）',
      serverPersistTip: '主题将被持久化到服务器静态资源目录',
      uploadSuccess: '主题已上传到服务器',
      uploading: '上传中...',
    },
  },

  // ============================================================================
  // 导航 (Navigation)
  // ============================================================================
  nav: {
    home: '首页',
    qa: '智能问答',
    documents: '文档管理',
    roles: '角色管理',
    feedback: '反馈与演化',
    collaboration: '协作空间',
    analytics: '数据分析',
    wish: '愿望单',
    aiService: 'AI 服务',
    profile: '个人中心',
    settings: '系统设置',
    admin: '系统管理',
  },

  // ============================================================================
  // 文档管理 (Document Management)
  // ============================================================================
  document: {
    title: '文档管理',

    // 视图模式切换 / View mode toggle
    viewMode: {
      browser: '浏览器视图',
      list: '列表视图',
      card: '卡片视图',
      flow: '流程视图',  // ⭐ RAG流程可视化
    },

    upload: '上传文档',
    uploadTip: '点击或拖拽文件到此区域上传',
    uploadHint: '支持 PDF、Word、Excel、PPT 等格式，单个文件不超过 100MB，支持多文件批量上传',
    uploadSuccess: '上传成功',
    uploadFailed: '上传失败',
    batchUploadSuccess: '批量上传成功：{success}个成功，{failed}个失败',
    batchUploading: '正在上传 {count} 个文件...',
    uploadLimit: '文件大小不能超过 {size}MB',
    uploadFirst: '上传第一个文档',
    list: '文档列表',
    total: '共 {count} 个文档',
    name: '文档名称',
    size: '大小',
    uploadTime: '上传时间',
    action: '操作',
    view: '查看',
    delete: '删除',
    deleteConfirm: '确定要删除这个文档吗？',
    deleteSuccess: '删除成功',
    deleteFailed: '删除失败',
    download: '下载',
    downloadSuccess: '下载成功',
    downloadFailed: '下载失败',
    preview: '预览',
    detail: '详情',
    category: '分类',
    tags: '标签',
    description: '描述',
    searchPlaceholder: '搜索文档名称、标签...',
    noDocuments: '暂无文档',
    noSearchResults: '未找到匹配的文档',
    loadFailed: '加载失败',
    uploading: '上传中...',
    selectFiles: '选择文件',
    uploadProgress: '上传进度',
    
    // 搜索相关 / Search related
    simpleSearch: '简单搜索',
    advancedSearch: '高级搜索',
    search: '搜索',
    keyword: '关键词',
    fileType: '文件类型',
    fileSize: '文件大小',
    minSize: '最小',
    maxSize: '最大',
    indexStatus: '索引状态',
    dateRange: '日期范围',
    sortBy: '排序',
    applyFilters: '应用筛选',
    resetFilters: '重置筛选',
    refresh: '刷新',
    refreshing: '刷新中...',
    loading: '加载中...',
    indexed: '已索引',
    prevPage: '上一页',
    nextPage: '下一页',
    page: '第',
    
    searchMode: {
      contains: '包含',
      exact: '精确匹配',
      regex: '正则表达式',
    },
    
    indexStatus: {
      all: '全部',
      indexed: '已索引',
      unindexed: '未索引',
    },
    
    sortBy: {
      date: '日期',
      name: '名称',
      size: '大小',
    },
    
    sortOrder: {
      asc: '升序',
      desc: '降序',
    },

    // FTP风格浏览器 / FTP-style browser
    browse: {
      root: '根目录',
      name: '名称',
      type: '类型',
      size: '大小',
      modified: '修改时间',
      actions: '操作',
      folder: '文件夹',
      file: '文件',
      files: '文件',
      folders: '文件夹',
      totalSize: '总大小',
      upload: '上传文件',
      uploadTitle: '上传文档',
      createFolder: '新建文件夹',
      createFolderTitle: '创建文件夹',
      createFolderSuccess: '文件夹创建成功',
      createFolderFailed: '文件夹创建失败',
      folderNameRequired: '请输入文件夹名称',
      folderNamePlaceholder: '请输入文件夹名称',
      download: '下载',
      downloadStarted: '开始下载',
      downloadFailed: '下载失败',
      delete: '删除',
      confirmDelete: '确认删除',
      deleteWarning: '此操作不可恢复，确定要删除',
      deleteSuccess: '删除成功',
      deleteFailed: '删除失败',
      viewDetail: '查看详情',
      detailTitle: '文档详情',
      aiChat: 'AI交互',
      loadFailed: '加载失败',
      emptyFolder: '文件夹为空',
      searchResults: '搜索结果',
      // 索引状态 / Index status
      indexStatus: '索引状态',
      statusPending: '待索引',
      statusIndexing: '索引中',
      statusDone: '已完成',
      statusFailed: '失败',
      // 状态过滤 / Status filter
      filterAll: '全部',
      filterIndexing: '索引中',
      filterDone: '已完成',
      filterFailed: '失败',
      // 重建索引 / Rebuild index
      rebuildIndex: '重建索引',
      batchRebuild: '批量重建',
      confirmRebuildIndex: '确认重建索引',
      rebuildIndexWarning: '将重建 {count} 个文件的索引，确定继续吗？',
      rebuildIndexStarted: '索引重建已开始',
      rebuildIndexFailed: '索引重建失败',
      noFilesSelected: '请先选择文件',
      // AI分析 / AI Analysis
      addToAIPanel: '加入AI分析',
      removeFromAIPanel: '移出AI分析',
      batchAddToAI: '批量加入AI分析',
      addToAIPanelSuccess: '已添加 {count} 个文件到AI分析面板',
      removeFromAIPanelSuccess: '已将 {name} 移出AI分析面板',
    },

    // Shell页面专用 / Shell page specific
    shell: {
      loading: '加载中...',
      loadingData: '正在获取文档数据...',
      loadFailed: '加载失败',
      heroTitle: '文档管理',
      heroSubtitle: '智能文档处理与管理平台',
      stats: {
        totalDocs: '文档总数',
        totalDocsDesc: '已上传的文档数量',
        docs: '篇',
        indexed: '已索引',
        indexedDesc: '可用于检索的文档',
        unindexed: '未索引',
        unindexedDesc: '待处理的文档',
        fileTypes: '文件类型',
        fileTypesDesc: '支持的文档格式',
      },
      features: {
        library: '文件库',
        libraryDesc: '集中管理所有文档',
        search: '智能搜索',
        searchDesc: '快速定位所需文件',
        edit: '在线编辑',
        editDesc: '实时协作编辑文档',
        security: '安全存储',
        securityDesc: '企业级数据安全',
      }
    },
  },

  // ============================================================================
  // 智能问答 (Q&A)
  // ============================================================================
  qa: {
    title: '智能问答',
    emptyMessage: '开始提问，开启智能对话之旅',

    // Shell 页面 (Shell Page)
    shell: {
      loading: '加载中...',
      loadingData: '正在获取系统数据...',
      loadFailed: '加载失败',
      heroTitle: '智能问答',
      heroSubtitle: 'AI驱动的智能对话系统',
      systemOnline: '✅ 系统在线',
      systemOffline: '⚠️',
      
      // 统计卡片 (Stats Cards)
      stats: {
        knowledgeBase: '知识库文档',
        knowledgeBaseDesc: '已索引文档数量',
        documentsCount: '篇',
        indexed: '已索引',
        indexedDesc: '可用于问答的文档',
        indexProgress: '索引进度',
        indexProgressDesc: '知识库构建进度',
        systemStatus: '系统状态',
        systemStatusDesc: '当前运行状态',
        needsIndexing: '需要索引',
        running: '运行正常',
      },
    },

    // 输入框 (Input)
    input: {
      placeholder: '请输入您的问题...',
      hint: 'Ctrl+Enter 发送 | ↑↓ 查看历史',
      send: '发送',
      characters: '字符',
    },

    // 相似问题 (Similar Questions)
    similarQuestions: {
      title: '相似问题',
      noResults: '暂无相似问题',
      askFirst: '提问后将显示相似问题',
    },

    // 历史记录 (History)
    history: {
      title: '对话历史',
      searchPlaceholder: '搜索历史记录...',
      noResults: '暂无历史记录',
      today: '今天',
      yesterday: '昨天',
      daysAgo: '天前',
    },

    // 反馈 (Feedback)
    feedback: {
      like: '点赞',
      dislike: '点踩',
      copy: '复制',
      copied: '已复制',
    },

    // 错误 (Error)
    error: {
      failed: '抱歉，回答失败了，请稍后重试',
      network: '网络连接失败',
      timeout: '请求超时',
    },
    clearHistory: '清除历史',
    copyAnswer: '复制回答',
    copySuccess: '复制成功',
    stopGeneration: '停止生成',
    generationStopped: '生成已停止',
    
    // 模式切换 (Mode Toggle)
    mode: {
      streaming: '流式模式',
      nonStreaming: '普通模式',
      switchToStreaming: '切换到流式模式（实时输出）',
      switchToNonStreaming: '切换到普通模式（Thinking 动画）',
    },
    
    // 双轨架构 (Dual-track Architecture)
    dualTrack: {
      hopeAnswerLabel: '💡 HOPE 快速答案',
      llmAnswerLabel: '🤖 LLM 详细回答',
      hopeBadge: 'HOPE',
      leftPanelTitle: '🤖 RAG + LLM 回答',
      rightPanelTitle: '🧠 HOPE智能系统 / 角色专业回答',
      dualTrackOutput: '双轨输出',
      leftPanel: '左轨（传统RAG）',
      rightPanel: '右轨（智能系统）',
      llmBadge: 'LLM',
      confidence: '置信度',
      source: '来源',
      responseTime: '响应时间',
      generatingDetail: '正在生成详细回答...',
      leftDescription: '检索知识库 + LLM生成',
      rightDescription: 'HOPE自我学习 + 算法优化',
      roleRightDescription: '角色专业知识回答',
    },

    // 知识库模式 (Knowledge Mode)
    knowledgeMode: {
      label: '知识库模式',
      none: '不使用RAG',
      rag: '使用RAG',
      role: '角色知识库',
    },

    // 角色 (Role)
    role: {
      general: '通用角色',
      developer: '开发者',
      devops: '运维工程师',
      architect: '架构师',
      researcher: '研究员',
      productManager: '产品经理',
      dataScientist: '数据科学家',
      securityEngineer: '安全工程师',
      tester: '测试工程师',
    },

    // 知识库 (Knowledge Base - 保留兼容)
    knowledgeBase: {
      enabled: 'RAG 模式',
      disabled: 'AI 直问',
      enable: '启用知识库（RAG 检索）',
      disable: '禁用知识库（直接 AI 回答）',
    },

    // 文档引用 (Document References)
    references: {
      title: '引用文档',
      addToAnalysis: '加入AI分析',
      alreadyInAnalysis: '已在分析中',
      download: '下载文档',
      addAllToAnalysis: '全部加入分析',
    },

    // 悬赏系统 (Bounty System)
    bounty: {
      title: '悬赏列表',
      active: '活跃悬赏',
      closed: '已关闭',
      expired: '已过期',
      id: '悬赏ID',
      question: '问题',
      reward: '奖励',
      credits: '积分',
      deadline: '截止时间',
      status: '状态',
      submit: '提交答案',
      submitAnswer: '提交答案',
      answer: '答案内容',
      sources: '资料来源',
      submitting: '提交中...',
      submitSuccess: '提交成功，等待审核',
      submitFailed: '提交失败',
      noActiveBounties: '暂无活跃悬赏',
      viewDetails: '查看详情',
    },

    // 排行榜 (Leaderboard)
    leaderboard: {
      title: '角色贡献排行榜',
      rank: '排名',
      roleName: '角色名称',
      totalCredits: '总积分',
      answerCount: '回答次数',
      bountyWins: '悬赏获胜',
      lastReward: '最近奖励',
      noData: '暂无数据',
      refresh: '刷新',
    },
  },

  // ============================================================================
  // 角色管理 (Role Management)
  // ============================================================================
  role: {
    title: '角色管理',
    list: '角色列表',
    total: '共 {count} 个角色',
    create: '创建角色',
    createFirst: '创建第一个角色',
    createSuccess: '创建成功',
    createFailed: '创建失败',
    edit: '编辑角色',
    updateSuccess: '更新成功',
    updateFailed: '更新失败',
    delete: '删除',
    deleteConfirm: '确定要删除这个角色吗？',
    deleteSuccess: '删除成功',
    deleteFailed: '删除失败',
    name: '角色名称',
    namePlaceholder: '请输入角色名称',
    nameRequired: '请输入角色名称',
    description: '角色描述',
    descriptionPlaceholder: '请输入角色描述',
    descriptionRequired: '请输入角色描述',
    icon: '图标',
    keywords: '关键词',
    keywordPlaceholder: '输入关键词后按回车',
    keywordHint: '添加角色的特征关键词，用于问题匹配',
    addKeyword: '添加关键词',
    status: '状态',
    enabled: '已启用',
    disabled: '已禁用',
    statistics: '使用统计',
    usageCount: '使用次数',
    successRate: '成功率',
    noRoles: '暂无角色',
    loadFailed: '加载失败',
    searchPlaceholder: '搜索角色名称、描述或关键词...',
  },

  // ============================================================================
  // 反馈与演化 (Feedback & Evolution)
  // ============================================================================
  feedback: {
    title: '反馈与演化',
    conflictList: '冲突列表',
    voting: '投票',
    evolution: '演化历史',
    quality: '质量监控',

    // 状态
    all: '全部',
    pending: '待处理',
    voting: '投票中',
    resolved: '已解决',

    // 冲突
    conceptA: '概念 A',
    conceptB: '概念 B',
    conceptConflict: '概念冲突',
    vote: '投票',
    voteA: '选择 A',
    voteB: '选择 B',
    voteSuccess: '投票成功',
    whichBetter: '您认为哪个更好？',
    context: '上下文',

    // 状态标签
    status: {
      pending: '待处理',
      voting: '投票中',
      resolved: '已解决',
    },

    // 时间线
    timeline: {
      created: '创建',
      updated: '更新',
      resolved: '解决',
    },
    before: '修改前',
    after: '修改后',

    // 质量监控
    concept: '概念',
    conflicts: '冲突数',
    totalConflicts: '总冲突数',
    resolvedConflicts: '已解决',
    pendingConflicts: '待处理',
    avgQuality: '平均质量',
    conceptQuality: '概念质量',

    // 空状态
    noConflicts: '暂无冲突',
    noEvolution: '暂无演化历史',
    loadFailed: '加载失败',
  },

  // ============================================================================
  // 协作网络 (Collaboration)
  // ============================================================================
  collaboration: {
    title: '协作网络',
    peers: '协作伙伴',
    exchange: '知识交换',
    topology: '网络拓扑',
    sync: '同步监控',

    // 伙伴管理
    addPeer: '添加伙伴',
    noPeers: '暂无协作伙伴',
    disconnect: '断开连接',
    disconnectSuccess: '断开成功',
    disconnectFailed: '断开失败',
    syncSuccess: '同步成功',
    syncFailed: '同步失败',

    // 连接管理
    connectionCode: '连接码',
    generateCode: '生成连接码',
    enterCode: '输入连接码',
    connect: '连接',
    connectSuccess: '连接成功',
    connectFailed: '连接失败',
    codeGenerated: '连接码已生成',
    generateFailed: '生成失败',
    codeCopied: '已复制连接码',
    copyCode: '复制连接码',
    codePlaceholder: '请输入连接码',
    generateHint: '生成一个连接码，分享给其他伙伴',
    enterHint: '输入其他伙伴的连接码进行连接',

    // 状态
    status: {
      online: '在线',
      offline: '离线',
      syncing: '同步中',
    },

    // 统计
    sharedDocs: '共享文档',
    lastSync: '最后同步',
    totalPeers: '伙伴总数',
    totalConnections: '连接数',
    me: '我',

    // 交换历史
    time: '时间',
    type: '类型',
    peer: '伙伴',
    content: '内容',
    noHistory: '暂无交换历史',
    exchangeType: {
      send: '发送',
      receive: '接收',
      sync: '同步',
    },
    exchangeStatus: {
      success: '成功',
      failed: '失败',
    },

    // 网络拓扑
    noTopology: '暂无网络拓扑',

    // 同步监控
    totalSyncs: '总同步次数',
    successSyncs: '成功次数',
    failedSyncs: '失败次数',
    syncRate: '成功率',
    recentActivity: '最近活动',
    syncStatus: {
      success: '成功',
      failed: '失败',
      pending: '等待中',
    },

    // Shell页面专用 / Shell page specific
    shell: {
      lastSyncLabel: '最后同步',
      lastSyncDefault: '刚刚',
      syncStatusLabel: '同步状态',
      syncStatusDefault: '正常',
    },

    loadFailed: '加载失败',
  },

  // ============================================================================
  // 愿望单 (Wish List)
  // ============================================================================
  wish: {
    title: '愿望单',
    submit: '提交愿望',
    submitTitle: '提交新愿望',
    submitSuccess: '提交成功',
    submitFailed: '提交失败',
    vote: '投票',
    voted: '已投票',
    voteUp: '点赞',
    voteDown: '点踩',
    cancelVote: '取消投票',
    voteSuccess: '投票成功',
    voteFailed: '投票失败',
    votes: '票',
    comments: '评论',
    viewDetail: '查看详情',
    anonymous: '匿名用户',

    // 时间
    minutesAgo: '分钟前',
    hoursAgo: '小时前',
    daysAgo: '天前',

    // 总数
    totalWishes: '个愿望',

    // 视图模式
    view: {
      grid: '网格',
      list: '列表',
    },

    // 搜索和筛选
    searchPlaceholder: '搜索愿望标题或内容...',
    filter: {
      all: '全部',
      status: '按状态筛选',
      category: '按分类筛选',
    },

    // 排序
    sort: {
      latest: '最新',
      hottest: '最热',
      most_voted: '最多投票',
    },

    // 状态
    status: {
      pending: '待审核',
      in_progress: '进行中',
      completed: '已完成',
      rejected: '已拒绝',
    },

    // 分类
    category: {
      feature: '功能增强',
      bug: 'Bug修复',
      interface: '界面优化',
      improvement: '体验优化',
    },

    // 表单
    form: {
      title: '愿望标题',
      titlePlaceholder: '请输入愿望标题（最多50字）',
      titleRequired: '请输入愿望标题',
      titleTooLong: '标题长度不能超过50字',

      description: '愿望描述',
      descriptionPlaceholder: '请详细描述您的愿望（最多500字）',
      descriptionRequired: '请输入愿望描述',
      descriptionTooLong: '描述长度不能超过500字',

      category: '愿望分类',
      categoryPlaceholder: '请选择分类',
      categoryRequired: '请选择愿望分类',

      submit: '提交',
      cancel: '取消',

      // 提示
      tipsTitle: '💡 提交提示：',
      tip1: '请清晰描述您的愿望，方便他人理解和投票',
      tip2: '查看是否已有类似愿望，避免重复提交',
      tip3: '愿望提交后将进入审核，通过后即可展示',
    },

    // 详情页
    detail: {
      description: '详细描述',
      statusHistory: '状态历史',
      comments: '评论',
    },

    // 排行榜
    ranking: {
      title: '愿望排行榜',
      empty: '暂无排行数据',
    },

    // 评论
    comment: {
      placeholder: '发表您的看法...',
      replyPlaceholder: '回复评论...',
      reply: '回复',
      replyTo: '回复',
      submit: '发表评论',
      submitReply: '发表回复',
      submitSuccess: '评论成功',
      submitFailed: '评论失败',
      emptyWarning: '请输入评论内容',
      empty: '暂无评论，快来抢沙发！',
      cancel: '取消',
    },

    // 空状态和加载
    empty: '暂无愿望，快来提交第一个吧！',
    loading: '加载中...',
    loadFailed: '加载失败',
  },

  // ============================================================================
  // AI 服务 (AI Service)
  // ============================================================================
  aiService: {
    title: 'AI 服务市场',
    market: '服务市场',
    all: '全部服务',
    installed: '已安装',
    available: '可用服务',
    services: '个服务',
    install: '安装',
    uninstall: '卸载',
    configure: '配置',
    usage: '使用',
    usages: '次使用',
    author: '作者',
    official: '官方',
    new: '新',
    popular: '热门',

    // 搜索和筛选
    searchPlaceholder: '搜索服务名称或描述...',

    // 分类
    category: {
      all: '全部分类',
      generation: '内容生成',
      analysis: '数据分析',
      conversion: '格式转换',
      optimization: '性能优化',
    },

    // PPT生成器
    pptGenerator: 'PPT 生成器',
    ppt: {
      step1: '输入主题',
      step2: '生成大纲',
      step3: '导出PPT',
      topic: '演示主题',
      topicPlaceholder: '请输入您要制作的PPT主题',
      outline: '大纲内容',
      outlinePlaceholder: '输入大纲内容（可选）',
      style: '演示风格',
      styleBusiness: '商务风格',
      styleAcademic: '学术风格',
      styleCreative: '创意风格',
      generate: '生成PPT',
      generateSuccess: 'PPT生成成功',
      generateFailed: 'PPT生成失败',
    },

    // 模型切换
    modelSwitcher: '模型切换',
    localModel: '本地模型',
    onlineModel: '在线模型',
    model: {
      title: '选择AI模型',
      localDesc: '使用本地部署的AI模型，响应快速，数据安全',
      onlineDesc: '使用云端AI模型，功能强大，持续更新',
      fast: '快速',
      offline: '离线可用',
      powerful: '功能强大',
      latest: '最新版本',
      apply: '应用设置',
      switchSuccess: '模型切换成功',
      switchFailed: '模型切换失败',
    },

    // 配置
    config: {
      enabled: '启用服务',
      model: '选择模型',
      selectModel: '请选择模型',
      apiKey: 'API密钥',
      apiKeyPlaceholder: '请输入API密钥（如需要）',
      saveSuccess: '配置保存成功',
      saveFailed: '配置保存失败',
    },

    // 详情
    detail: {
      overview: '概览',
      configuration: '配置',
      changelog: '更新日志',
      description: '服务描述',
      info: '基本信息',
      version: '版本',
      author: '作者',
      usageCount: '使用次数',
      size: '大小',
      features: '功能特性',
      noChangelog: '暂无更新日志',
    },

    // 状态消息
    installSuccess: '安装成功',
    installFailed: '安装失败',
    uninstallSuccess: '卸载成功',
    uninstallFailed: '卸载失败',
    loading: '加载中...',
    loadFailed: '加载失败',
    empty: '暂无服务',
  },

  // ============================================================================
  // 个人中心 (User Profile)
  // ============================================================================
  profile: {
    title: '个人中心',
    info: '个人信息',
    editInfo: '编辑信息',
    statistics: '使用统计',
    contribution: '贡献统计',
    achievement: '成就',
    settings: '设置',

    // 个人信息
    avatar: '头像',
    nickname: '昵称',
    email: '邮箱',
    bio: '个人简介',
    defaultName: '未设置昵称',
    noBio: '这个人很懒，什么都没留下',

    // 表单验证
    nicknameRequired: '请输入昵称',
    emailRequired: '请输入邮箱',
    emailInvalid: '邮箱格式不正确',
    uploadAvatar: '上传头像',

    // 统计数据
    qaCount: '问答次数',
    documentCount: '文档数量',
    feedbackCount: '反馈次数',
    contributionScore: '贡献分数',
    activeHours: '活跃时长',
    usageTrend: '使用趋势',

    // 贡献
    contributionRanking: '贡献排行',

    // 成就
    unlocked: '已解锁',
    locked: '未解锁',

    // 设置
    language: '语言',
    theme: '主题',
    lightTheme: '浅色主题',
    darkTheme: '深色主题',
    autoTheme: '跟随系统',
    notifications: '通知',

    // 消息
    updateSuccess: '信息更新成功',
    updateFailed: '信息更新失败',
    settingsSaved: '设置已保存',
    settingsFailed: '设置保存失败',
    loadFailed: '加载失败',
  },

  // ============================================================================
  // 系统管理 (Admin)
  // ============================================================================
  admin: {
    title: '系统管理',
    systemConfig: '系统配置',
    modelConfig: '模型配置',
    logViewer: '日志查看',
    monitor: '性能监控',
    healthCheck: '健康检查',
    backup: '备份管理',

    // 系统配置
    config: {
      systemName: '系统名称',
      maxFileSize: '最大文件大小',
      enableCache: '启用缓存',
      saveSuccess: '配置保存成功',
      saveFailed: '配置保存失败',
    },

    // 模型配置
    model: {
      llmModel: 'LLM模型',
      selectModel: '选择模型',
      gpt35: 'GPT-3.5',
      gpt4: 'GPT-4',
      local: '本地模型',
      vectorDB: '向量数据库',
      saveSuccess: '模型配置保存成功',
      saveFailed: '模型配置保存失败',
    },

    // 日志查看
    log: {
      searchPlaceholder: '搜索日志...',
      all: '全部',
      error: '错误',
      warn: '警告',
      info: '信息',
      download: '下载日志',
      noLogs: '暂无日志',
    },

    // 监控
    monitorMetrics: {
      cpu: 'CPU使用率',
      memory: '内存使用率',
      requests: '请求数',
      errors: '错误数',
    },
  },

  // ============================================================================
  // 工作流市场 (Workflow Market)
  // ============================================================================
  workflowMarket: {
    title: '工作流市场',
    subtitle: '发现和分享强大的工作流',

    // 搜索和筛选
    search: {
      placeholder: '搜索工作流...',
      button: '搜索',
      noResults: '没有找到工作流',
      tryOtherKeywords: '试试其他关键词或分类',
    },

    // 分类
    category: {
      title: '分类',
      all: '全部',
      dataProcessing: '数据处理',
      apiIntegration: 'API集成',
      automation: '自动化',
      transformation: '数据转换',
      analysis: '数据分析',
      example: '示例',
    },

    // 排序
    sort: {
      title: '排序方式',
      popular: '最热门',
      recent: '最新',
      topRated: '高评分',
      name: '名称',
    },

    // 工作流卡片
    card: {
      featured: '推荐',
      downloads: '次下载',
      author: '作者',
      version: '版本',
      category: '分类',
    },

    // 详情页
    detail: {
      backToMarket: '返回市场',
      download: '下载',
      install: '安装',
      overview: '概览',
      steps: '步骤',
      ratings: '评分',
      description: '描述',
      noDescription: '暂无详细描述',
      stepsCount: '步骤数量',
      stepUnit: '个步骤',
      noSteps: '暂无步骤信息',
      agent: 'Agent',
      dependencies: '依赖',
      downloadSuccess: '下载成功',
      downloadFailed: '下载失败',
      installSuccess: '工作流安装成功！',
      installFailed: '安装失败',
      notFound: '工作流不存在',
    },

    // 评分
    rating: {
      title: '评分和评论',
      giveRating: '给这个工作流评分',
      submit: '提交评分',
      commentPlaceholder: '写下你的评论（可选）...',
      pleaseRate: '请选择评分',
      rateSuccess: '评分成功！',
      rateFailed: '评分失败',
      noRatings: '还没有评分，成为第一个评分的人吧！',
      ratingsCount: '个评分',
    },

    // 通用
    loading: '加载中...',
    loadMore: '加载更多',
    reset: '重置筛选',
    noWorkflows: '暂无工作流',
  },

  // ============================================================================
  // 工作流构建器 (Workflow Builder)
  // ============================================================================
  workflowBuilder: {
    title: '工作流构建器',
    namePlaceholder: '请输入工作流名称',
    addStep: '添加步骤',
    testButton: '测试',
    exportButton: '导出',
    importButton: '导入',

    // 状态
    status: {
      draft: '草稿',
      active: '活跃',
      deprecated: '已弃用',
    },

    // 验证
    validation: {
      nameRequired: '请输入工作流名称',
      stepsRequired: '至少需要一个步骤',
    },

    // 保存
    save: {
      created: '工作流创建成功',
      updated: '工作流更新成功',
      failed: '保存失败',
    },

    // 测试
    test: {
      title: '测试工作流',
      input: '输入数据',
      execute: '执行',
      result: '执行结果',
      success: '测试执行成功',
      failed: '测试执行失败',
    },

    // 导出导入
    export: {
      success: '导出成功',
    },
    import: {
      success: '导入成功',
      failed: '导入失败，文件格式错误',
    },

    // 画布
    canvas: {
      emptyHint: '点击"添加步骤"开始构建工作流',
      addFirstStep: '让我们创建第一个步骤！',
    },

    // 节点
    node: {
      agent: 'Agent',
      dependencies: '依赖',
      input: '输入',
      output: '输出',
      connect: '连接',
      allowFailure: '允许失败',
      timeout: '超时时间',
    },

    // 步骤编辑器
    stepEditor: {
      title: '编辑步骤',
      name: '步骤名称',
      nameRequired: '请输入步骤名称',
      namePlaceholder: '例如：数据验证',
      description: '步骤描述',
      descriptionPlaceholder: '描述这个步骤的功能...',
      agent: 'Agent',
      agentRequired: '请选择一个 Agent',
      input: '输入配置',
      inputRequired: '请配置输入',
      expression: '表达式',
      invalidJson: 'JSON 格式错误',
      dependencies: '依赖步骤',
      dependenciesPlaceholder: '选择依赖的步骤',
      advancedConfig: '高级配置',
      allowFailure: '允许失败',
      allowFailureTooltip: '如果此步骤失败，工作流仍然继续执行',
      timeout: '超时时间（毫秒）',
      timeoutTooltip: '步骤执行的最大时间',
      retries: '重试次数',
      retriesTooltip: '失败后的重试次数',
      condition: '条件执行',
      conditionTooltip: '使用 SpEL 表达式控制是否执行此步骤',
    },

    // Agent 选择器
    agentSelector: {
      title: '选择 Agent',
      searchPlaceholder: '搜索 Agent...',
      noAgents: '没有可用的 Agent',
    },

    // Agent
    agents: {
      loadFailed: '加载 Agent 列表失败',
    },

    // 步骤
    step: {
      added: '步骤已添加',
      updated: '步骤已更新',
      deleted: '步骤已删除',
      deleteConfirm: '删除步骤',
      deleteWarning: '确定要删除这个步骤吗？这将同时删除所有依赖此步骤的连接。',
    },

    // AI 生成
    ai: {
      placeholder: '描述你想要的工作流，例如：我需要一个数据处理流程，先验证数据格式，然后转换数据，最后过滤无效数据...',
      generate: 'AI 生成工作流',
      generating: '正在生成...',
      generateSuccess: '工作流生成成功！',
      generateFailed: 'AI 生成失败',
      descriptionRequired: '请输入工作流描述',
    },
  },

  // ============================================================================
  // 用户 (User)
  // ============================================================================
  user: {
    menu: {
      profile: '个人资料',
      accountSettings: '账户设置',
      logout: '退出登录',
    }
  },

  // ============================================================================
  // 错误消息 (Error Messages)
  // ============================================================================
  error: {
    networkError: '网络错误，请检查连接',
    serverError: '服务器错误，请稍后重试',
    notFound: '未找到资源',
    unauthorized: '未授权，请先登录',
    forbidden: '无权限访问',
    validationError: '数据验证失败',
    unknownError: '未知错误',
  },

  // ============================================================================
  // 成功消息 (Success Messages)
  // ============================================================================
  success: {
    saved: '保存成功',
    deleted: '删除成功',
    updated: '更新成功',
    created: '创建成功',
    uploaded: '上传成功',
  },
  // ============================================================================
  // RAG 流程可视化 (RAG Flow Visualization)
  // ============================================================================
  ragFlow: {
    // 文档处理流程 (Document Processing Flow)
    document: {
      title: '文档处理流程',
      subtitle: '实时追踪文档从上传到索引的完整过程',
    },

    // 处理阶段 (Processing Stages)
    stages: {
      upload: {
        title: '文档上传',
        desc: '上传文档到系统',
      },
      extract: {
        title: '文本提取',
        desc: '从文档中提取文本内容',
      },
      chunk: {
        title: '智能分块',
        desc: '使用算法进行智能分块',
      },
      vectorize: {
        title: '向量化',
        desc: '将文本转换为向量表示',
      },
      index: {
        title: '索引存储',
        desc: '存储到向量数据库',
      },
      completed: {
        title: '处理完成',
        desc: '文档已成功索引到系统',
      },
    },

    // 状态 (Status)
    status: {
      running: '处理中',
      completed: '已完成',
      failed: '处理失败',
      waiting: '等待中',
    },

    // 消息 (Messages)
    messages: {
      noDocument: '请选择要处理的文档',
      uploadTip: '上传文档后将自动开始处理流程',
      processingFailed: '处理失败',
      wsError: 'WebSocket连接错误',
    },

    // 操作 (Actions)
    actions: {
      viewResult: '查看结果',
      retry: '重新处理',
      delete: '删除记录',
      refresh: '刷新',
    },

    // 信息显示 (Info Display)
    info: {
      documentName: '文档名称',
      documentId: '文档ID',
      status: '状态',
      currentProgress: '当前进度',
      step: '步骤',
      elapsedTime: '已耗时',
      preview: '预览',
    },
  },
}

