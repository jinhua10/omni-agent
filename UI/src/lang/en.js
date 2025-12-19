/**
 * English Language Pack
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

export default {
  // ============================================================================
  // Common
  // ============================================================================
  common: {
    confirm: 'Confirm',
    cancel: 'Cancel',
    save: 'Save',
    delete: 'Delete',
    edit: 'Edit',
    add: 'Add',
    search: 'Search',
    filter: 'Filter',
    reset: 'Reset',
    submit: 'Submit',
    close: 'Close',
    back: 'Back',
    next: 'Next',
    prev: 'Previous',
    finish: 'Finish',
    loading: 'Loading...',
    success: 'Success',
    error: 'Error',
    warning: 'Warning',
    info: 'Info',
    upload: 'Upload',
    download: 'Download',
    refresh: 'Refresh',
    more: 'More',
    collapse: 'Collapse',
    expand: 'Expand',
    loadMore: 'Load More',
    switchToDark: 'Switch to Dark Mode',
    switchToLight: 'Switch to Light Mode',
    documentation: 'Documentation',
    about: 'About',
    clearCache: 'Clear Cache',
    clearCacheTitle: 'Clear Cache',
    clearCacheDescription: 'Please select cache items to clear:',
    clearCacheConfirm: 'Confirm Clear',
    clearCacheWarning: '⚠️ Page will refresh after clearing',
    floatingPanelConfig: 'Floating Panel Config',
    themeSettings: 'Theme Settings',
    uiThemeConfig: 'UI Theme Config',
    otherCacheData: 'Other Cache Data',
  },

  // ============================================================================
  // Theme Customizer
  // ============================================================================
  theme: {
    colorTheme: 'Color Theme',
    customizer: {
      title: 'Theme Customizer',
      primary: 'Primary Color',
      background: 'Background Color',
      surface: 'Surface Color',
      text: 'Primary Text Color',
      textSecondary: 'Secondary Text Color',
      border: 'Border Color',
      apply: 'Apply',
      reset: 'Reset',
      applySuccess: 'Theme applied successfully',
      resetSuccess: 'Reset to default theme',
      previewText: 'This is primary text',
      previewSecondary: 'This is secondary text',
      previewButton: 'Primary Button',
    },
  },

  // ============================================================================
  // UI Theme Engine
  // ============================================================================
  uiTheme: {
    switcher: {
      title: 'UI Theme Switcher',
      current: 'Current',
      apply: 'Apply',
      builtin: 'Built-in',
      custom: 'Custom',
      developing: 'In Development',
      comingSoon: 'Coming Soon',
      version: 'Version',
      author: 'Author',
      builtinThemes: 'Built-in Themes',
      customThemes: 'Custom Themes',
      management: 'Theme Management',
      importExport: 'Import/Export Themes',
      importTheme: 'Import Theme',
      exportTheme: 'Export Theme',
      export: 'Export',
      uninstall: 'Uninstall',
      confirmUninstall: 'Confirm Uninstall',
      uninstallWarning: 'Are you sure you want to uninstall this theme?',
      switchSuccess: 'Theme switched successfully',
      themeInDev: 'This theme is in development, coming soon',
      importSuccess: 'Theme imported successfully',
      importFailed: 'Failed to import theme',
      exportSuccess: 'Theme exported successfully',
      uninstallSuccess: 'Theme uninstalled successfully',
      noCustomThemes: 'No custom themes',
      importTip: 'Click the button below to import a theme',
      howToUse: 'How to Use',
      step1: 'Get theme file from theme marketplace or AI generation',
      step2: 'Check "Upload to Server" (Recommended)',
      step3: 'Click "Import Theme" button to select JSON file',
      step4: 'Apply in "Custom Themes" tab after successful import',
      step5: 'Themes uploaded to server will be permanently saved',
      aiGeneration: 'AI Theme Generation',
      aiGenerationDesc: 'Future feature: Generate unique UI themes using AI',
      uploadToServer: 'Upload to Server (Recommended)',
      serverPersistTip: 'Theme will be persisted to server static resources directory',
      uploadSuccess: 'Theme uploaded to server successfully',
      uploading: 'Uploading...',
    },
  },

  // ============================================================================
  // Navigation
  // ============================================================================
  nav: {
    home: 'Home',
    qa: 'Q&A',
    documents: 'Documents',
    roles: 'Roles',
    feedback: 'Feedback',
    collaboration: 'Collaboration',
    analytics: 'Analytics',
    wish: 'Wish List',
    aiService: 'AI Services',
    profile: 'Profile',
    settings: 'Settings',
    admin: 'Admin',
  },

  // ============================================================================
  // Document Management
  // ============================================================================
  document: {
    title: 'Document Management',

    // View mode toggle
    viewMode: {
      browser: 'Browser View',
      list: 'List View',
      card: 'Card View',
    },

    upload: 'Upload Document',
    uploadTip: 'Click or drag files to this area to upload',
    uploadHint: 'Support PDF, Word, Excel, PPT formats, max 100MB per file, batch upload supported',
    uploadSuccess: 'Upload successful',
    uploadFailed: 'Upload failed',
    batchUploadSuccess: 'Batch upload completed: {success} succeeded, {failed} failed',
    batchUploading: 'Uploading {count} files...',
    
    // Shell page specific
    shell: {
      loading: 'Loading...',
      loadingData: 'Fetching document data...',
      loadFailed: 'Failed to load',
      heroTitle: 'Document Management',
      heroSubtitle: 'Intelligent Document Processing Platform',
      stats: {
        totalDocs: 'Total Documents',
        totalDocsDesc: 'Uploaded document count',
        docs: 'docs',
        indexed: 'Indexed',
        indexedDesc: 'Searchable documents',
        unindexed: 'Unindexed',
        unindexedDesc: 'Pending documents',
        fileTypes: 'File Types',
        fileTypesDesc: 'Supported formats',
      },
      features: {
        library: 'File Library',
        libraryDesc: 'Centralized document management',
        search: 'Smart Search',
        searchDesc: 'Quick file location',
        edit: 'Online Editing',
        editDesc: 'Real-time collaboration',
        security: 'Secure Storage',
        securityDesc: 'Enterprise-grade security',
      }
    },
    upload: 'Upload Document',
    uploadTip: 'Click or drag files to this area to upload',
    uploadHint: 'Support PDF, Word, Excel, PPT formats, max file size 100MB',
    uploadSuccess: 'Upload successful',
    uploadFailed: 'Upload failed',
    uploadLimit: 'File size cannot exceed {size}MB',
    uploadFirst: 'Upload first document',
    list: 'Document List',
    total: 'Total {count} documents',
    name: 'Document Name',
    size: 'Size',
    uploadTime: 'Upload Time',
    action: 'Action',
    view: 'View',
    delete: 'Delete',
    deleteConfirm: 'Are you sure you want to delete this document?',
    deleteSuccess: 'Delete successful',
    deleteFailed: 'Delete failed',
    download: 'Download',
    downloadSuccess: 'Download successful',
    downloadFailed: 'Download failed',
    preview: 'Preview',
    detail: 'Detail',
    category: 'Category',
    tags: 'Tags',
    description: 'Description',
    searchPlaceholder: 'Search documents by name, tags...',
    noDocuments: 'No documents yet',
    noSearchResults: 'No matching documents found',
    loadFailed: 'Failed to load',
    uploading: 'Uploading...',
    selectFiles: 'Select Files',
    uploadProgress: 'Upload Progress',
    
    // Search related
    simpleSearch: 'Simple Search',
    advancedSearch: 'Advanced Search',
    search: 'Search',
    keyword: 'Keyword',
    fileType: 'File Type',
    fileSize: 'File Size',
    minSize: 'Min',
    maxSize: 'Max',
    indexStatus: 'Index Status',
    dateRange: 'Date Range',
    sortBy: 'Sort By',
    applyFilters: 'Apply Filters',
    resetFilters: 'Reset Filters',
    refresh: 'Refresh',
    refreshing: 'Refreshing...',
    loading: 'Loading...',
    indexed: 'Indexed',
    prevPage: 'Previous',
    nextPage: 'Next',
    page: 'Page',
    
    searchMode: {
      contains: 'Contains',
      exact: 'Exact Match',
      regex: 'Regular Expression',
    },
    
    indexStatus: {
      all: 'All',
      indexed: 'Indexed',
      unindexed: 'Unindexed',
    },
    
    sortBy: {
      date: 'Date',
      name: 'Name',
      size: 'Size',
    },
    
    sortOrder: {
      asc: 'Ascending',
      desc: 'Descending',
    },

    // FTP-style browser
    browse: {
      root: 'Root',
      name: 'Name',
      type: 'Type',
      size: 'Size',
      modified: 'Modified',
      actions: 'Actions',
      folder: 'Folder',
      file: 'File',
      files: 'Files',
      folders: 'Folders',
      totalSize: 'Total Size',
      upload: 'Upload File',
      uploadTitle: 'Upload Document',
      createFolder: 'New Folder',
      createFolderTitle: 'Create Folder',
      createFolderSuccess: 'Folder created successfully',
      createFolderFailed: 'Failed to create folder',
      folderNameRequired: 'Please enter folder name',
      folderNamePlaceholder: 'Enter folder name',
      download: 'Download',
      downloadStarted: 'Download started',
      downloadFailed: 'Download failed',
      delete: 'Delete',
      confirmDelete: 'Confirm Delete',
      deleteWarning: 'This action cannot be undone. Are you sure to delete',
      deleteSuccess: 'Deleted successfully',
      deleteFailed: 'Failed to delete',
      viewDetail: 'View Details',
      detailTitle: 'Document Details',
      aiChat: 'AI Interaction',
      loadFailed: 'Failed to load',
      emptyFolder: 'Folder is empty',
      searchResults: 'Search Results',
    },
  },

  // ============================================================================
  // Q&A
  // ============================================================================
  qa: {
    title: 'Intelligent Q&A',
    emptyMessage: 'Start asking questions to begin your intelligent conversation',

    // Shell Page
    shell: {
      loading: 'Loading...',
      loadingData: 'Fetching system data...',
      loadFailed: 'Load Failed',
      heroTitle: 'Intelligent Q&A',
      heroSubtitle: 'AI-Powered Intelligent Dialogue System',
      systemOnline: '✅ System Online',
      systemOffline: '⚠️',
      
      // Stats Cards
      stats: {
        knowledgeBase: 'Knowledge Base',
        knowledgeBaseDesc: 'Indexed document count',
        documentsCount: 'docs',
        indexed: 'Indexed',
        indexedDesc: 'Available for Q&A',
        indexProgress: 'Index Progress',
        indexProgressDesc: 'Knowledge base building progress',
        systemStatus: 'System Status',
        systemStatusDesc: 'Current running status',
        needsIndexing: 'Needs Indexing',
        running: 'Running Normal',
      },
    },

    // Input
    input: {
      placeholder: 'Enter your question...',
      hint: 'Ctrl+Enter to send | ↑↓ History',
      send: 'Send',
      characters: 'characters',
    },

    // Similar Questions
    similarQuestions: {
      title: 'Similar Questions',
      noResults: 'No similar questions',
      askFirst: 'Similar questions will appear after you ask',
    },

    // History
    history: {
      title: 'Chat History',
      searchPlaceholder: 'Search history...',
      noResults: 'No history records',
      today: 'Today',
      yesterday: 'Yesterday',
      daysAgo: 'days ago',
    },

    // Feedback
    feedback: {
      like: 'Like',
      dislike: 'Dislike',
      copy: 'Copy',
      copied: 'Copied',
    },

    // Error
    error: {
      failed: 'Sorry, failed to get answer. Please try again later',
      network: 'Network connection failed',
      timeout: 'Request timeout',
    },
    clearHistory: 'Clear History',
    copyAnswer: 'Copy Answer',
    copySuccess: 'Copied successfully',
    stopGeneration: 'Stop Generating',
    generationStopped: 'Generation Stopped',
    
    // Mode Toggle
    mode: {
      streaming: 'Streaming',
      nonStreaming: 'Standard',
      switchToStreaming: 'Switch to Streaming Mode (Real-time output)',
      switchToNonStreaming: 'Switch to Standard Mode (Thinking animation)',
    },
    
    // Dual-track Architecture
    dualTrack: {
      hopeAnswerLabel: '💡 HOPE Quick Answer',
      llmAnswerLabel: '🤖 LLM Detailed Answer',
      hopeBadge: 'HOPE',
      leftPanelTitle: '🤖 RAG + LLM Answer',
      rightPanelTitle: '🧠 HOPE System / Role Expert Answer',
      dualTrackOutput: 'Dual Track Output',
      leftPanel: 'Left (Traditional RAG)',
      rightPanel: 'Right (Intelligent System)',
      llmBadge: 'LLM',
      confidence: 'Confidence',
      source: 'Source',
      responseTime: 'Response Time',
      generatingDetail: 'Generating detailed answer...',
      leftDescription: 'Retrieve KB + LLM Generation',
      rightDescription: 'HOPE Self-learning + Algorithm Optimization',
      roleRightDescription: 'Role Expert Knowledge Answer',
    },

    // Knowledge Mode
    knowledgeMode: {
      label: 'Knowledge Mode',
      none: 'No RAG',
      rag: 'Use RAG',
      role: 'Role KB',
    },

    // Role
    role: {
      general: 'General',
      developer: 'Developer',
      devops: 'DevOps',
      architect: 'Architect',
      researcher: 'Researcher',
      productManager: 'Product Manager',
      dataScientist: 'Data Scientist',
      securityEngineer: 'Security Engineer',
      tester: 'Test Engineer',
    },

    // Document References
    references: {
      title: 'Referenced Documents',
      addToAnalysis: 'Add to AI Analysis',
      alreadyInAnalysis: 'Already in Analysis',
      download: 'Download Document',
      addAllToAnalysis: 'Add All to Analysis',
    },

    // Knowledge Base (keep for compatibility)
    knowledgeBase: {
      enabled: 'RAG Mode',
      disabled: 'Direct AI',
      enable: 'Enable Knowledge Base (RAG retrieval)',
      disable: 'Disable Knowledge Base (Direct AI answer)',
    },

    // Bounty System
    bounty: {
      title: 'Bounty List',
      active: 'Active Bounties',
      closed: 'Closed',
      expired: 'Expired',
      id: 'Bounty ID',
      question: 'Question',
      reward: 'Reward',
      credits: 'Credits',
      deadline: 'Deadline',
      status: 'Status',
      submit: 'Submit Answer',
      submitAnswer: 'Submit Answer',
      answer: 'Answer Content',
      sources: 'Sources',
      submitting: 'Submitting...',
      submitSuccess: 'Submitted successfully, pending review',
      submitFailed: 'Submission failed',
      noActiveBounties: 'No active bounties',
      viewDetails: 'View Details',
    },

    // Leaderboard
    leaderboard: {
      title: 'Role Contribution Leaderboard',
      rank: 'Rank',
      roleName: 'Role Name',
      totalCredits: 'Total Credits',
      answerCount: 'Answer Count',
      bountyWins: 'Bounty Wins',
      lastReward: 'Last Reward',
      noData: 'No data',
      refresh: 'Refresh',
    },
  },

  // ============================================================================
  // Role Management
  // ============================================================================
  role: {
    title: 'Role Management',
    list: 'Role List',
    total: 'Total {count} roles',
    create: 'Create Role',
    createFirst: 'Create first role',
    createSuccess: 'Created successfully',
    createFailed: 'Create failed',
    edit: 'Edit Role',
    updateSuccess: 'Updated successfully',
    updateFailed: 'Update failed',
    delete: 'Delete',
    deleteConfirm: 'Are you sure you want to delete this role?',
    deleteSuccess: 'Deleted successfully',
    deleteFailed: 'Delete failed',
    name: 'Role Name',
    namePlaceholder: 'Enter role name',
    nameRequired: 'Please enter role name',
    description: 'Description',
    descriptionPlaceholder: 'Enter role description',
    descriptionRequired: 'Please enter description',
    icon: 'Icon',
    keywords: 'Keywords',
    keywordPlaceholder: 'Enter keyword and press Enter',
    keywordHint: 'Add characteristic keywords for question matching',
    addKeyword: 'Add Keyword',
    status: 'Status',
    enabled: 'Enabled',
    disabled: 'Disabled',
    statistics: 'Statistics',
    usageCount: 'Usage Count',
    successRate: 'Success Rate',
    noRoles: 'No roles yet',
    loadFailed: 'Failed to load',
    searchPlaceholder: 'Search role name, description or keywords...',
  },

  // ============================================================================
  // Feedback & Evolution
  // ============================================================================
  feedback: {
    title: 'Feedback & Evolution',
    conflictList: 'Conflict List',
    voting: 'Voting',
    evolution: 'Evolution History',
    quality: 'Quality Monitoring',

    // Status
    all: 'All',
    pending: 'Pending',
    voting: 'Voting',
    resolved: 'Resolved',

    // Conflicts
    conceptA: 'Concept A',
    conceptB: 'Concept B',
    conceptConflict: 'Concept Conflict',
    vote: 'Vote',
    voteA: 'Vote A',
    voteB: 'Vote B',
    voteSuccess: 'Vote successful',
    whichBetter: 'Which one is better?',
    context: 'Context',

    // Status labels
    status: {
      pending: 'Pending',
      voting: 'Voting',
      resolved: 'Resolved',
    },

    // Timeline
    timeline: {
      created: 'Created',
      updated: 'Updated',
      resolved: 'Resolved',
    },
    before: 'Before',
    after: 'After',

    // Quality Monitor
    concept: 'Concept',
    conflicts: 'Conflicts',
    totalConflicts: 'Total Conflicts',
    resolvedConflicts: 'Resolved',
    pendingConflicts: 'Pending',
    avgQuality: 'Avg Quality',
    conceptQuality: 'Concept Quality',

    // Empty states
    noConflicts: 'No conflicts yet',
    noEvolution: 'No evolution history',
    loadFailed: 'Failed to load',
  },

  // ============================================================================
  // Collaboration
  // ============================================================================
  collaboration: {
    title: 'Collaboration Network',
    peers: 'Peers',
    exchange: 'Knowledge Exchange',
    topology: 'Network Topology',
    sync: 'Sync Monitor',

    // Peer Management
    addPeer: 'Add Peer',
    noPeers: 'No peers yet',
    disconnect: 'Disconnect',
    disconnectSuccess: 'Disconnected successfully',
    disconnectFailed: 'Failed to disconnect',
    syncSuccess: 'Synced successfully',
    syncFailed: 'Failed to sync',

    // Connection Management
    connectionCode: 'Connection Code',
    generateCode: 'Generate Code',
    enterCode: 'Enter Code',
    connect: 'Connect',
    connectSuccess: 'Connected successfully',
    connectFailed: 'Failed to connect',
    codeGenerated: 'Code generated',
    generateFailed: 'Failed to generate',
    codeCopied: 'Code copied',
    copyCode: 'Copy Code',
    codePlaceholder: 'Enter connection code',
    generateHint: 'Generate a connection code to share with other peers',
    enterHint: 'Enter a connection code from another peer',

    // Status
    status: {
      online: 'Online',
      offline: 'Offline',
      syncing: 'Syncing',
    },

    // Statistics
    sharedDocs: 'Shared Docs',
    lastSync: 'Last Sync',
    totalPeers: 'Total Peers',
    totalConnections: 'Connections',
    me: 'Me',

    // Exchange History
    time: 'Time',
    type: 'Type',
    peer: 'Peer',
    content: 'Content',
    noHistory: 'No exchange history',
    exchangeType: {
      send: 'Send',
      receive: 'Receive',
      sync: 'Sync',
    },
    exchangeStatus: {
      success: 'Success',
      failed: 'Failed',
    },

    // Network Topology
    noTopology: 'No topology data',

    // Sync Monitor
    totalSyncs: 'Total Syncs',
    successSyncs: 'Success',
    failedSyncs: 'Failed',
    syncRate: 'Success Rate',
    recentActivity: 'Recent Activity',
    syncStatus: {
      success: 'Success',
      failed: 'Failed',
      pending: 'Pending',
    },

    // Shell page specific
    shell: {
      lastSyncLabel: 'Last Sync',
      lastSyncDefault: 'Just now',
      syncStatusLabel: 'Sync Status',
      syncStatusDefault: 'Normal',
    },

    loadFailed: 'Failed to load',
  },

  // ============================================================================
  // Wish List
  // ============================================================================
  wish: {
    title: 'Wish List',
    submit: 'Submit Wish',
    submitTitle: 'Submit New Wish',
    submitSuccess: 'Submitted successfully',
    submitFailed: 'Failed to submit',
    vote: 'Vote',
    voted: 'Voted',
    voteUp: 'Upvote',
    voteDown: 'Downvote',
    cancelVote: 'Cancel Vote',
    voteSuccess: 'Voted successfully',
    voteFailed: 'Failed to vote',
    votes: 'votes',
    comments: 'Comments',
    viewDetail: 'View Detail',
    anonymous: 'Anonymous',

    // Time
    minutesAgo: 'minutes ago',
    hoursAgo: 'hours ago',
    daysAgo: 'days ago',

    // Total
    totalWishes: 'wishes',

    // View mode
    view: {
      grid: 'Grid',
      list: 'List',
    },

    // Search and filter
    searchPlaceholder: 'Search wish title or content...',
    filter: {
      all: 'All',
      status: 'Filter by Status',
      category: 'Filter by Category',
    },

    // Sort
    sort: {
      latest: 'Latest',
      hottest: 'Hottest',
      most_voted: 'Most Voted',
    },

    // Status
    status: {
      pending: 'Pending',
      in_progress: 'In Progress',
      completed: 'Completed',
      rejected: 'Rejected',
    },

    // Category
    category: {
      feature: 'Feature Enhancement',
      bug: 'Bug Fix',
      interface: 'UI Optimization',
      improvement: 'Improvement',
    },

    // Form
    form: {
      title: 'Title',
      titlePlaceholder: 'Enter wish title (max 50 chars)',
      titleRequired: 'Please enter a title',
      titleTooLong: 'Title cannot exceed 50 characters',

      description: 'Description',
      descriptionPlaceholder: 'Describe your wish in detail (max 500 chars)',
      descriptionRequired: 'Please enter a description',
      descriptionTooLong: 'Description cannot exceed 500 characters',

      category: 'Category',
      categoryPlaceholder: 'Select category',
      categoryRequired: 'Please select a category',

      submit: 'Submit',
      cancel: 'Cancel',

      // Tips
      tipsTitle: '💡 Tips:',
      tip1: 'Describe your wish clearly for better understanding',
      tip2: 'Check if similar wishes already exist',
      tip3: 'Wishes will be reviewed before being displayed',
    },

    // Detail page
    detail: {
      description: 'Description',
      statusHistory: 'Status History',
      comments: 'Comments',
    },

    // Ranking
    ranking: {
      title: 'Wish Ranking',
      empty: 'No ranking data',
    },

    // Comment
    comment: {
      placeholder: 'Share your thoughts...',
      replyPlaceholder: 'Reply to comment...',
      reply: 'Reply',
      replyTo: 'Reply to',
      submit: 'Submit Comment',
      submitReply: 'Submit Reply',
      submitSuccess: 'Comment submitted',
      submitFailed: 'Failed to submit comment',
      emptyWarning: 'Please enter comment content',
      empty: 'No comments yet, be the first!',
      cancel: 'Cancel',
    },

    // Empty state and loading
    empty: 'No wishes yet, be the first to submit!',
    loading: 'Loading...',
    loadFailed: 'Failed to load',
  },

  // ============================================================================
  // AI Service
  // ============================================================================
  aiService: {
    title: 'AI Service Market',
    market: 'Market',
    all: 'All Services',
    installed: 'Installed',
    available: 'Available',
    services: 'services',
    install: 'Install',
    uninstall: 'Uninstall',
    configure: 'Configure',
    usage: 'Usage',
    usages: 'usages',
    author: 'Author',
    official: 'Official',
    new: 'New',
    popular: 'Popular',

    searchPlaceholder: 'Search service name or description...',

    category: {
      all: 'All Categories',
      generation: 'Content Generation',
      analysis: 'Data Analysis',
      conversion: 'Format Conversion',
      optimization: 'Performance Optimization',
    },

    pptGenerator: 'PPT Generator',
    ppt: {
      step1: 'Input Topic',
      step2: 'Generate Outline',
      step3: 'Export PPT',
      topic: 'Presentation Topic',
      topicPlaceholder: 'Enter your PPT topic',
      outline: 'Outline Content',
      outlinePlaceholder: 'Enter outline content (optional)',
      style: 'Presentation Style',
      styleBusiness: 'Business Style',
      styleAcademic: 'Academic Style',
      styleCreative: 'Creative Style',
      generate: 'Generate PPT',
      generateSuccess: 'PPT generated successfully',
      generateFailed: 'Failed to generate PPT',
    },

    modelSwitcher: 'Model Switcher',
    localModel: 'Local Model',
    onlineModel: 'Online Model',
    model: {
      title: 'Select AI Model',
      localDesc: 'Use locally deployed AI model, fast response, data security',
      onlineDesc: 'Use cloud AI model, powerful features, continuous updates',
      fast: 'Fast',
      offline: 'Offline Available',
      powerful: 'Powerful',
      latest: 'Latest Version',
      apply: 'Apply Settings',
      switchSuccess: 'Model switched successfully',
      switchFailed: 'Failed to switch model',
    },

    config: {
      enabled: 'Enable Service',
      model: 'Select Model',
      selectModel: 'Please select a model',
      apiKey: 'API Key',
      apiKeyPlaceholder: 'Enter API key (if required)',
      saveSuccess: 'Configuration saved successfully',
      saveFailed: 'Failed to save configuration',
    },

    detail: {
      overview: 'Overview',
      configuration: 'Configuration',
      changelog: 'Changelog',
      description: 'Service Description',
      info: 'Basic Information',
      version: 'Version',
      author: 'Author',
      usageCount: 'Usage Count',
      size: 'Size',
      features: 'Features',
      noChangelog: 'No changelog available',
    },

    installSuccess: 'Installed successfully',
    installFailed: 'Failed to install',
    uninstallSuccess: 'Uninstalled successfully',
    uninstallFailed: 'Failed to uninstall',
    loading: 'Loading...',
    loadFailed: 'Failed to load',
    empty: 'No services available',
  },

  // ============================================================================
  // User Profile
  // ============================================================================
  profile: {
    title: 'Profile',
    info: 'Personal Info',
    editInfo: 'Edit Info',
    statistics: 'Statistics',
    contribution: 'Contribution',
    achievement: 'Achievement',
    settings: 'Settings',

    avatar: 'Avatar',
    nickname: 'Nickname',
    email: 'Email',
    bio: 'Bio',
    defaultName: 'No nickname set',
    noBio: 'This person is lazy and left nothing',

    nicknameRequired: 'Please enter nickname',
    emailRequired: 'Please enter email',
    emailInvalid: 'Invalid email format',
    uploadAvatar: 'Upload Avatar',

    qaCount: 'Q&A Count',
    documentCount: 'Document Count',
    feedbackCount: 'Feedback Count',
    contributionScore: 'Contribution Score',
    activeHours: 'Active Hours',
    usageTrend: 'Usage Trend',

    contributionRanking: 'Contribution Ranking',

    unlocked: 'Unlocked',
    locked: 'Locked',

    language: 'Language',
    theme: 'Theme',
    lightTheme: 'Light Theme',
    darkTheme: 'Dark Theme',
    autoTheme: 'Follow System',
    notifications: 'Notifications',

    updateSuccess: 'Information updated successfully',
    updateFailed: 'Failed to update information',
    settingsSaved: 'Settings saved',
    settingsFailed: 'Failed to save settings',
    loadFailed: 'Failed to load',
  },

  // ============================================================================
  // Admin
  // ============================================================================
  admin: {
    title: 'System Admin',
    systemConfig: 'System Config',
    modelConfig: 'Model Config',
    logViewer: 'Log Viewer',
    monitor: 'Monitor',
    healthCheck: 'Health Check',
    backup: 'Backup',

    config: {
      systemName: 'System Name',
      maxFileSize: 'Max File Size',
      enableCache: 'Enable Cache',
      saveSuccess: 'Configuration saved successfully',
      saveFailed: 'Failed to save configuration',
    },

    model: {
      llmModel: 'LLM Model',
      selectModel: 'Select Model',
      gpt35: 'GPT-3.5',
      gpt4: 'GPT-4',
      local: 'Local Model',
      vectorDB: 'Vector Database',
      saveSuccess: 'Model configuration saved successfully',
      saveFailed: 'Failed to save model configuration',
    },

    log: {
      searchPlaceholder: 'Search logs...',
      all: 'All',
      error: 'Error',
      warn: 'Warning',
      info: 'Info',
      download: 'Download Logs',
      noLogs: 'No logs found',
    },

    monitorMetrics: {
      cpu: 'CPU Usage',
      memory: 'Memory Usage',
      requests: 'Requests',
      errors: 'Errors',
    },
  },

  // ============================================================================
  // User
  // ============================================================================
  user: {
    menu: {
      profile: 'Profile',
      accountSettings: 'Account Settings',
      logout: 'Logout',
    }
  },

  // ============================================================================
  // Error Messages
  // ============================================================================
  error: {
    networkError: 'Network error, please check connection',
    serverError: 'Server error, please try again later',
    notFound: 'Resource not found',
    unauthorized: 'Unauthorized, please login first',
    forbidden: 'Access forbidden',
    validationError: 'Validation failed',
    unknownError: 'Unknown error',
  },

  // ============================================================================
  // Success Messages
  // ============================================================================
  success: {
    saved: 'Saved successfully',
    deleted: 'Deleted successfully',
    updated: 'Updated successfully',
    created: 'Created successfully',
    uploaded: 'Uploaded successfully',
  },
}

