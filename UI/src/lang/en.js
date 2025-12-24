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
      flow: 'Flow View',  // ⭐ RAG Flow Visualization
      textExtraction: 'Text Extraction',  // ⭐ Text Extraction Configuration
      chunking: 'Chunking Config',  // ⭐ Chunking Strategy Configuration
      queryExpansion: 'Query Expansion',  // ⭐ Query Expansion Configuration
      retrieval: 'Retrieval Config',  // ⭐ Retrieval Parameter Configuration
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
      // Index status
      indexStatus: 'Index Status',
      statusPending: 'Pending',
      statusIndexing: 'Indexing',
      statusDone: 'Done',
      statusFailed: 'Failed',
      // Status filter
      filterAll: 'All',
      filterIndexing: 'Indexing',
      filterDone: 'Done',
      filterFailed: 'Failed',
      // Rebuild index
      rebuildIndex: 'Rebuild Index',
      batchRebuild: 'Batch Rebuild',
      confirmRebuildIndex: 'Confirm Rebuild Index',
      rebuildIndexWarning: 'Rebuild index for {count} file(s), continue?',
      rebuildIndexStarted: 'Index rebuild started',
      rebuildIndexFailed: 'Index rebuild failed',
      noFilesSelected: 'Please select files first',
      // AI Analysis
      addToAIPanel: 'Add to AI Analysis',
      removeFromAIPanel: 'Remove from AI Analysis',
      batchAddToAI: 'Batch Add to AI',
      addToAIPanelSuccess: 'Added {count} file(s) to AI analysis panel',
      removeFromAIPanelSuccess: 'Removed {name} from AI analysis panel',
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
  // Workflow Market
  // ============================================================================
  workflowMarket: {
    title: 'Workflow Market',
    subtitle: 'Discover and share powerful workflows',

    // Search and filter
    search: {
      placeholder: 'Search workflows...',
      button: 'Search',
      noResults: 'No workflows found',
      tryOtherKeywords: 'Try other keywords or categories',
    },

    // Categories
    category: {
      title: 'Category',
      all: 'All',
      dataProcessing: 'Data Processing',
      apiIntegration: 'API Integration',
      automation: 'Automation',
      transformation: 'Transformation',
      analysis: 'Analysis',
      example: 'Example',
    },

    // Sort
    sort: {
      title: 'Sort By',
      popular: 'Popular',
      recent: 'Recent',
      topRated: 'Top Rated',
      name: 'Name',
    },

    // Workflow card
    card: {
      featured: 'Featured',
      downloads: 'downloads',
      author: 'Author',
      version: 'Version',
      category: 'Category',
    },

    // Detail page
    detail: {
      backToMarket: 'Back to Market',
      download: 'Download',
      install: 'Install',
      overview: 'Overview',
      steps: 'Steps',
      ratings: 'Ratings',
      description: 'Description',
      noDescription: 'No description available',
      stepsCount: 'Steps Count',
      stepUnit: 'steps',
      noSteps: 'No steps information',
      agent: 'Agent',
      dependencies: 'Dependencies',
      downloadSuccess: 'Download successful',
      downloadFailed: 'Download failed',
      installSuccess: 'Workflow installed successfully!',
      installFailed: 'Installation failed',
      notFound: 'Workflow not found',
    },

    // Rating
    rating: {
      title: 'Ratings & Reviews',
      giveRating: 'Rate this workflow',
      submit: 'Submit Rating',
      commentPlaceholder: 'Write your review (optional)...',
      pleaseRate: 'Please select a rating',
      rateSuccess: 'Rating submitted!',
      rateFailed: 'Failed to submit rating',
      noRatings: 'No ratings yet. Be the first to rate!',
      ratingsCount: 'ratings',
    },

    // Common
    loading: 'Loading...',
    loadMore: 'Load More',
    reset: 'Reset Filters',
    noWorkflows: 'No workflows',
  },

  // ============================================================================
  // Workflow Builder
  // ============================================================================
  workflowBuilder: {
    title: 'Workflow Builder',
    namePlaceholder: 'Enter workflow name',
    addStep: 'Add Step',
    testButton: 'Test',
    exportButton: 'Export',
    importButton: 'Import',

    // Status
    status: {
      draft: 'Draft',
      active: 'Active',
      deprecated: 'Deprecated',
    },

    // Validation
    validation: {
      nameRequired: 'Please enter workflow name',
      stepsRequired: 'At least one step is required',
    },

    // Save
    save: {
      created: 'Workflow created successfully',
      updated: 'Workflow updated successfully',
      failed: 'Save failed',
    },

    // Test
    test: {
      title: 'Test Workflow',
      input: 'Input Data',
      execute: 'Execute',
      result: 'Execution Result',
      success: 'Test executed successfully',
      failed: 'Test execution failed',
    },

    // Export & Import
    export: {
      success: 'Export successful',
    },
    import: {
      success: 'Import successful',
      failed: 'Import failed, invalid file format',
    },

    // Canvas
    canvas: {
      emptyHint: 'Click "Add Step" to start building your workflow',
      addFirstStep: "Let's create the first step!",
    },

    // Node
    node: {
      agent: 'Agent',
      dependencies: 'Dependencies',
      input: 'Input',
      output: 'Output',
      connect: 'Connect',
      allowFailure: 'Allow Failure',
      timeout: 'Timeout',
    },

    // Step Editor
    stepEditor: {
      title: 'Edit Step',
      name: 'Step Name',
      nameRequired: 'Please enter step name',
      namePlaceholder: 'e.g., Data Validation',
      description: 'Step Description',
      descriptionPlaceholder: 'Describe what this step does...',
      agent: 'Agent',
      agentRequired: 'Please select an agent',
      input: 'Input Configuration',
      inputRequired: 'Please configure input',
      expression: 'Expression',
      invalidJson: 'Invalid JSON format',
      dependencies: 'Dependent Steps',
      dependenciesPlaceholder: 'Select dependent steps',
      advancedConfig: 'Advanced Configuration',
      allowFailure: 'Allow Failure',
      allowFailureTooltip: 'If this step fails, the workflow continues',
      timeout: 'Timeout (ms)',
      timeoutTooltip: 'Maximum execution time for this step',
      retries: 'Retries',
      retriesTooltip: 'Number of retries after failure',
      condition: 'Conditional Execution',
      conditionTooltip: 'Use SpEL expression to control whether to execute this step',
    },

    // Agent Selector
    agentSelector: {
      title: 'Select Agent',
      searchPlaceholder: 'Search agents...',
      noAgents: 'No agents available',
    },

    // Agents
    agents: {
      loadFailed: 'Failed to load agent list',
    },

    // Step
    step: {
      added: 'Step added',
      updated: 'Step updated',
      deleted: 'Step deleted',
      deleteConfirm: 'Delete Step',
      deleteWarning: 'Are you sure you want to delete this step? This will also remove all connections that depend on it.',
    },

    // AI Generation
    ai: {
      placeholder: 'Describe the workflow you want, e.g.: I need a data processing pipeline that validates data format, transforms data, and filters invalid data...',
      generate: 'AI Generate Workflow',
      generating: 'Generating...',
      generateSuccess: 'Workflow generated successfully!',
      generateFailed: 'AI generation failed',
      descriptionRequired: 'Please enter workflow description',
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

  // ============================================================================
  // RAG Flow Visualization
  // ============================================================================
  ragFlow: {
    // Document Processing Flow
    document: {
      title: 'Document Processing Flow',
      subtitle: 'Real-time tracking of the complete process from upload to indexing',
    },

    // Processing Stages
    stages: {
      upload: {
        title: 'Document Upload',
        desc: 'Upload document to system',
      },
      extract: {
        title: 'Text Extraction',
        desc: 'Extract text content from document',
      },
      chunk: {
        title: 'Smart Chunking',
        desc: 'Smart chunking using algorithms',
      },
      vectorize: {
        title: 'Vectorization',
        desc: 'Convert text to vector representation',
      },
      index: {
        title: 'Index Storage',
        desc: 'Store to vector database',
      },
      completed: {
        title: 'Completed',
        desc: 'Document successfully indexed to system',
      },
    },

    // Status
    status: {
      pending: 'Pending',
      running: 'Processing',
      processing: 'Processing',
      extracting: 'Extracting',
      extracted: 'Extracted',
      chunking: 'Chunking',
      chunked: 'Chunked',
      vectorizing: 'Vectorizing',
      indexing: 'Indexing',
      completed: 'Completed',
      failed: 'Failed',
      waiting: 'Waiting',
    },

    // Messages
    messages: {
      noDocument: 'Please select a document to process',
      uploadTip: 'Processing will start automatically after uploading',
      processingFailed: 'Processing failed',
      wsError: 'WebSocket connection error',
    },

    // Actions
    actions: {
      viewResult: 'View Result',
      retry: 'Retry',
      delete: 'Delete Record',
      refresh: 'Refresh',
      processingOptions: 'Processing Options',
      configureExtraction: 'Configure Text Extraction',
      configureChunking: 'Configure Chunking Strategy',
      rebuildDocument: 'Rebuild Document',
      viewChunks: 'View Chunks',
      confirmRebuild: 'Are you sure you want to rebuild this document?',
      collapseDemo: 'Collapse Demo',
      startDemo: 'Start Demo',
    },

    // Information Display
    info: {
      documentName: 'Document Name',
      documentId: 'Document ID',
      status: 'Status',
      currentProgress: 'Current Progress',
      step: 'Step',
      elapsedTime: 'Elapsed Time',
      preview: 'Preview',
    },

    // Component Internal Text
    component: {
      title: 'Document Processing Flow',
      currentProgressLabel: 'Current Progress',
      stepCounter: 'Step {current} / {total}',
      elapsedTimeLabel: 'Elapsed: {time}s',
      clickToConfigExtract: 'Click to configure text extraction',
      clickToConfigChunk: 'Click to configure chunking strategy',
      pendingDocuments: 'Pending Documents',
      noDocuments: 'No Documents',
      noDocumentsDesc: 'Please upload documents and click refresh button, or click "View Demo" button to see the demo flow.',
      demoMode: 'Demo Mode',
      demoModeDesc: 'This is a demo flow showing the complete document processing steps.',
      viewDemoFlow: 'View Demo Flow',
      viewDemo: 'View Demo',
      refresh: 'Refresh',
      createdAt: 'Created At',
      chunkCount: 'Chunk Count',
      vectorCount: 'Vector Count',
    },
  },

  // ============================================================================
  // Chunking Strategy Configuration
  // ============================================================================
  chunkingConfig: {
    // Page Titles
    title: 'Chunking Strategy Configuration',
    subtitle: 'Interactive configuration and real-time preview of document chunking strategies',

    // Strategy List
    strategyList: {
      title: 'Available Strategies',
      selectStrategy: 'Select Strategy',
      currentStrategy: 'Current Strategy',
      noStrategies: 'No strategies available',
    },

    // Strategy Info
    strategy: {
      fixedSize: 'Fixed Size',
      semantic: 'Semantic',
      ppl: 'PPL Perplexity',
      paragraph: 'Paragraph',
      sentence_boundary: 'Sentence Boundary',
      description: {
        fixedSize: 'Split document by fixed character count, suitable for general scenarios',
        semantic: 'Intelligent splitting based on semantic similarity, maintains semantic integrity',
        ppl: 'Smart splitting using perplexity algorithm, suitable for technical documents',
        paragraph: 'Split by paragraph boundaries, maintains paragraph integrity',
        sentence_boundary: 'Split by sentence boundaries, maintains sentence integrity',
      },
    },

    // Parameters
    params: {
      title: 'Parameter Settings',
      chunkSize: 'Chunk Size',
      chunkOverlap: 'Chunk Overlap',
      minChunkSize: 'Min Chunk Size',
      maxChunkSize: 'Max Chunk Size',
      similarityThreshold: 'Similarity Threshold',
      perplexityThreshold: 'Perplexity Threshold',
      respectParagraph: 'Respect Paragraph Boundary',

      // Parameter Help
      help: {
        chunkSize: 'Target character count for each chunk',
        chunkOverlap: 'Number of overlapping characters between adjacent chunks',
        minChunkSize: 'Minimum character count for a chunk',
        maxChunkSize: 'Maximum character count for a chunk',
        similarityThreshold: 'Semantic similarity threshold (0-1), higher value means finer chunks',
        perplexityThreshold: 'Perplexity threshold, higher value means finer splitting',
        respectParagraph: 'Whether to split at paragraph boundaries',
      },
    },

    // Live Preview
    preview: {
      title: 'Live Preview',
      inputText: 'Input Text',
      inputPlaceholder: 'Type or paste text to chunk here...',
      chunkResult: 'Chunk Results',
      noChunks: 'No chunks yet',
      chunkCount: '{count} chunks total',
      chunkIndex: 'Chunk {index}',
      chunkLength: '{length} characters',
      previewButton: 'Preview Chunks',
      clearButton: 'Clear',

      // Statistics
      stats: {
        title: 'Statistics',
        totalChunks: 'Total Chunks',
        avgLength: 'Average Length',
        minLength: 'Min Length',
        maxLength: 'Max Length',
        totalChars: 'Total Characters',
      },
    },

    // Strategy Comparison
    comparison: {
      title: 'Strategy Comparison',
      addStrategy: 'Add Strategy',
      removeStrategy: 'Remove Strategy',
      compareButton: 'Start Comparison',
      clearButton: 'Clear Comparison',
      selectStrategies: 'Select strategies to compare',
      noComparison: 'Select at least 2 strategies to compare',

      // Comparison Results
      result: {
        strategy: 'Strategy',
        chunks: 'Chunks',
        avgLength: 'Avg Length',
        quality: 'Quality Score',
        speed: 'Processing Speed',
      },
    },

    // Messages
    message: {
      loadSuccess: 'Strategies loaded successfully',
      loadFailed: 'Failed to load strategies',
      previewSuccess: 'Preview generated successfully',
      previewFailed: 'Failed to generate preview',
      comparisonSuccess: 'Comparison completed',
      comparisonFailed: 'Comparison failed',
      inputRequired: 'Please input text to chunk',
      selectStrategyRequired: 'Please select a chunking strategy',
      parameterInvalid: 'Invalid parameter value',
    },

    // Action Buttons
    actions: {
      preview: 'Preview',
      apply: 'Apply',
      reset: 'Reset',
      compare: 'Compare',
      export: 'Export Config',
      import: 'Import Config',
      save: 'Save as Default',
    },
  },

  // ============================================================================
  // Text Extraction Configuration
  // ============================================================================
  textExtractionConfig: {
    title: 'Text Extraction Model Selection',
    documentTitle: 'Document Text Extraction Configuration',

    // Alerts
    alerts: {
      documentConfigTitle: 'Document Configuration',
      documentConfigDesc: 'Configuring text extraction model for document {docId}. Extraction will start immediately after selection.',
      systemConfigTitle: 'System Default Configuration',
      systemConfigDesc: 'Text extraction is the first step of RAG process. Choosing the right extraction model can improve subsequent processing accuracy. This configuration will be used as default for new documents.',
      finalTipTitle: 'Tip',
      finalTipDesc: 'After saving the configuration, newly uploaded documents will use the selected model for text extraction. Processed documents can be re-extracted in document management.',
    },

    // Labels
    labels: {
      selectModel: 'Select Extraction Model',
      defaultModel: 'Default Text Extraction Model',
      modelDescription: 'Model Description',
      mainFeatures: 'Main Features',
      applicableScenarios: 'Applicable Scenarios',
      applicableFiles: 'Applicable Files',
      processingSpeed: 'Processing Speed',
      resourceConsumption: 'Resource Consumption',
      specialAbility: 'Special Ability',
      languageSupport: 'Language Support',
    },

    // Buttons
    buttons: {
      startExtraction: 'Start Extraction',
      extractionInProgress: 'Extracting...',
      applyConfig: 'Apply Configuration',
      reset: 'Reset',
      backToFlow: 'Back to Flow View',
    },

    // Tips
    tips: {
      saveSuccess: 'Configuration saved',
      saveFailed: 'Save failed',
      extractionStarted: 'Text extraction started',
      autoExtractionStarted: 'Starting auto extraction...',
      operationFailed: 'Operation failed',
    },

    // Scenario Description Values
    scenarios: {
      standard: {
        files: 'TXT, MD, Plain text documents',
        speed: '⚡ Very fast',
        resource: '💾 Low',
      },
      visionLlm: {
        files: 'PPT, PPTX, PDF (with charts), Images',
        speed: '🐢 Slower (requires LLM inference)',
        resource: '💾 High (requires GPU)',
        ability: '🎯 Can understand charts, flowcharts, architecture diagrams',
      },
      ocr: {
        files: 'Scanned PDFs, Images',
        speed: '🚀 Fast',
        resource: '💾 Medium',
        language: '🌍 Multilingual (Chinese, English, Japanese, Korean, etc.)',
      },
    },

    // 流式模式 (Streaming Mode)
    streamingMode: {
      label: 'Streaming Output',
      batchOutput: 'Batch Output',
      streamingTip: 'Display extracted content in real-time, reducing waiting time, suitable for large documents like PPTs, PDFs',
      batchTip: 'Display all content at once after extraction, suitable for small documents like TXT, Markdown',
    },

    // 提取进度 (Extraction Progress)
    progress: {
      extracting: 'Extracting text...',
      completed: '✅ Extraction completed',
      failed: '❌ Extraction failed',
      accuracy: 'Extraction accuracy',
      characters: 'characters',
      pages: 'pages',
      batches: 'batches',
    },

    // 批次 (Batches)
    batches: {
      title: 'Extraction Results',
      batch: 'Batch',
      waiting: 'Waiting',
      processing: 'Processing',
      completed: 'Completed',
      mergeBatches: 'Merge Batches',
      mergeSuccess: 'Batches merged into complete document',
      expandAll: 'Expand All',
      collapseAll: 'Collapse All',
      allCompletedTip: 'All batches completed, you can merge to view the complete document',
    },

    // 预览/源码 (Preview/Source)
    preview: {
      title: 'Preview',
      source: 'Source',
      sourcePlaceholder: 'Extracted Markdown source code will be displayed here...',
    },

    // 导出 (Export)
    export: {
      label: 'Export',
      markdown: 'Export Markdown',
      html: 'Export HTML',
      successMarkdown: 'Exported as Markdown file',
      successHTML: 'Exported as HTML file',
      documentResult: 'Document Extraction Result',
    },

    // 自动保存 (Auto Save)
    autoSave: {
      enabled: 'Auto save enabled',
      disabled: 'Auto save disabled',
      saved: 'Saved',
      lastSaved: 'Last saved',
    },

    // 提取相关 (Extraction)
    extraction: {
      streamingStart: 'Starting streaming extraction...',
      batchStart: 'Starting extraction...',
      streamingComplete: 'Streaming extraction completed',
      batchComplete: 'Extraction completed',
      autoSaveSuccess: '💾 Auto save successful',
      autoSaveFailed: 'Auto save failed',
      loadedExtractedContent: '📄 Loaded saved extracted content',
      characters: 'characters',
    },

    // 提取开关 (Extraction Toggle)
    toggleExtraction: {
      startTextExtraction: 'Start Text Extraction',
      textExtractionInProgress: 'Text extraction in progress...',
    },
  },

  // ============================================================================
  // Query Expansion Configuration
  // ============================================================================
  queryExpansionConfig: {
    // Page Titles
    title: 'Query Expansion Configuration',
    subtitle: 'Configure and optimize query expansion strategies to improve retrieval recall',

    // Basic Config
    basicConfig: {
      title: 'Basic Configuration',
      llmExpansion: 'LLM Query Expansion',
      llmExpansionHelp: 'Use large language models to generate query variants',
      maxQueries: 'Max Expanded Queries',
      maxQueriesHelp: 'Maximum number of expanded queries per original query',
      enableCache: 'Enable Cache',
      enableCacheHelp: 'Cache query expansion results for better performance',
      parallelExecution: 'Parallel Execution',
      parallelExecutionHelp: 'Execute multiple queries in parallel for faster response',
    },

    // Strategy Weights
    strategyWeights: {
      title: 'Strategy Weights',
      synonym: 'Synonym Weight',
      synonymHelp: 'Weight for synonym-based query expansion',
      llm: 'LLM Weight',
      llmHelp: 'Weight for LLM-based query expansion',
      domain: 'Domain Weight',
      domainHelp: 'Weight for domain dictionary-based expansion',
      weightTip: 'Weight range: 0.0 - 1.0, sum should be 1.0',
    },

    // Cache Config
    cacheConfig: {
      title: 'Cache Configuration',
      cacheSize: 'Cache Size',
      cacheSizeHelp: 'Maximum number of cached query results',
      cacheTtl: 'Cache TTL',
      cacheTtlHelp: 'Time to live for cached entries (minutes)',
      clearCache: 'Clear Cache',
      clearCacheConfirm: 'Are you sure to clear all cache?',
    },

    // Parallel Config
    parallelConfig: {
      title: 'Parallel Configuration',
      threads: 'Thread Pool Size',
      threadsHelp: 'Number of threads for parallel execution',
      timeout: 'Timeout',
      timeoutHelp: 'Maximum execution time per query (seconds)',
    },

    // Dictionary
    dictionary: {
      title: 'Domain Dictionary',
      addDomain: 'Add Domain',
      domainName: 'Domain Name',
      domainTerms: 'Domain Terms',
      addTerm: 'Add Term',
      removeTerm: 'Remove Term',
      totalDomains: '{count} domains',
      totalTerms: '{count} terms',
      editDomain: 'Edit Domain',
      deleteDomain: 'Delete Domain',
      exportDictionary: 'Export Dictionary',
      importDictionary: 'Import Dictionary',
    },

    // Preview
    preview: {
      title: 'Query Expansion Preview',
      originalQuery: 'Original Query',
      inputPlaceholder: 'Enter query to expand...',
      expandedQueries: 'Expanded Queries',
      enableStrategies: 'Enable Strategies',
      synonymExpansion: 'Synonym Expansion',
      llmExpansion: 'LLM Expansion',
      domainExpansion: 'Domain Expansion',
      previewButton: 'Preview Expansion',
      clearButton: 'Clear',
      noResults: 'No expansion results',
      queryCount: '{count} queries generated',

      // Statistics
      stats: {
        title: 'Statistics',
        originalLength: 'Original Length',
        avgLength: 'Average Length',
        expansionRate: 'Expansion Rate',
        estimatedRecall: 'Estimated Recall Improvement',
      },
    },

    // Cache Stats
    cacheStats: {
      title: 'Cache Statistics',
      hitRate: 'Hit Rate',
      cacheSize: 'Current Size',
      maxSize: 'Max Capacity',
      hitCount: 'Hit Count',
      missCount: 'Miss Count',
      totalRequests: 'Total Requests',
      refreshStats: 'Refresh Stats',
    },

    // Performance
    performance: {
      title: 'Performance Monitoring',
      avgResponseTime: 'Avg Response Time',
      p95ResponseTime: 'P95 Response Time',
      p99ResponseTime: 'P99 Response Time',
      throughput: 'Throughput',
      errorRate: 'Error Rate',
    },

    // Messages
    message: {
      configLoadSuccess: 'Configuration loaded successfully',
      configLoadFailed: 'Failed to load configuration',
      configSaveSuccess: 'Configuration saved successfully',
      configSaveFailed: 'Failed to save configuration',
      previewSuccess: 'Preview generated successfully',
      previewFailed: 'Failed to generate preview',
      cacheCleared: 'Cache cleared',
      cacheClearFailed: 'Failed to clear cache',
      dictionaryUpdateSuccess: 'Dictionary updated successfully',
      dictionaryUpdateFailed: 'Failed to update dictionary',
      inputRequired: 'Please input query content',
      invalidWeight: 'Invalid weight value, should be between 0.0 - 1.0',
    },

    // Actions
    actions: {
      save: 'Save Config',
      reset: 'Reset',
      preview: 'Preview',
      apply: 'Apply',
      export: 'Export',
      import: 'Import',
      clear: 'Clear',
      refresh: 'Refresh',
    },
  },

  // ============================================================================
  // Retrieval Configuration
  // ============================================================================
  retrievalConfig: {
    title: 'Retrieval Configuration',
    subtitle: 'Configure and optimize retrieval parameters for better accuracy and efficiency',

    // Basic Config
    basicConfig: {
      title: 'Basic Configuration',
      topK: 'Top-K Results',
      topKHelp: 'Return top K most similar results',
      similarityThreshold: 'Similarity Threshold',
      similarityThresholdHelp: 'Only return results with similarity above this threshold',
      timeout: 'Timeout',
      timeoutHelp: 'Maximum wait time for retrieval (seconds)',
    },

    // Strategy
    strategy: {
      title: 'Retrieval Strategy',
      select: 'Select Strategy',
      vector: 'Vector Search',
      fulltext: 'Full-text Search',
      hybrid: 'Hybrid Search',
      description: {
        vector: 'Semantic search based on vector similarity, suitable for semantic matching',
        fulltext: 'Keyword-based full-text search, suitable for exact matching',
        hybrid: 'Combines vector and full-text search, balancing semantic and exact matching',
      },
    },

    // Hybrid Weights
    hybridWeights: {
      title: 'Hybrid Search Weights',
      vectorWeight: 'Vector Weight',
      vectorWeightHelp: 'Weight for vector search results',
      fulltextWeight: 'Full-text Weight',
      fulltextWeightHelp: 'Weight for full-text search results',
      weightTip: 'Weight range: 0.0 - 1.0, sum should be 1.0',
    },

    // Reranker
    reranker: {
      title: 'Reranker Configuration',
      enable: 'Enable Reranker',
      enableHelp: 'Use reranker model to optimize result ranking',
      model: 'Reranker Model',
      modelHelp: 'Select reranker model',
      models: {
        bgeReranker: 'BGE Reranker',
        crossEncoder: 'Cross Encoder',
        colbert: 'ColBERT',
      },
    },

    // Parallel
    parallel: {
      title: 'Parallel Configuration',
      enable: 'Enable Parallel Retrieval',
      enableHelp: 'Execute multiple retrieval sources in parallel for better speed',
    },

    // Test
    test: {
      title: 'Real-time Test',
      inputQuery: 'Input Query',
      inputPlaceholder: 'Enter query to test...',
      testButton: 'Test Retrieval',
      clearButton: 'Clear',
      noResults: 'No retrieval results',
      resultCount: '{count} results',

      // Result Display
      result: {
        documentName: 'Document Name',
        score: 'Similarity',
        source: 'Source',
        content: 'Content Summary',
        vectorSource: 'Vector',
        fulltextSource: 'Full-text',
      },

      // Statistics
      stats: {
        title: 'Retrieval Statistics',
        totalResults: 'Total Results',
        retrievalTime: 'Retrieval Time',
        vectorResults: 'Vector Results',
        fulltextResults: 'Full-text Results',
        avgScore: 'Average Score',
        minScore: 'Min Score',
        maxScore: 'Max Score',
      },
    },

    // Messages
    message: {
      configLoadSuccess: 'Configuration loaded successfully',
      configLoadFailed: 'Failed to load configuration',
      configSaveSuccess: 'Configuration saved successfully',
      configSaveFailed: 'Failed to save configuration',
      testSuccess: 'Test completed',
      testFailed: 'Test failed',
      inputRequired: 'Please input query content',
      invalidTopK: 'Invalid Top-K value',
      invalidThreshold: 'Invalid threshold, should be between 0.0 - 1.0',
    },

    // Actions
    actions: {
      save: 'Save Config',
      reset: 'Reset',
      test: 'Test',
      clear: 'Clear',
    },
  },

  // ============================================================================
  // Cache Management
  // ============================================================================
  cacheManagement: {
    title: 'Cache Management',
    subtitle: 'Multi-level cache statistics, monitoring and management',

    overview: {
      title: 'Cache Overview',
      totalHitRate: 'Total Hit Rate',
      totalSize: 'Total Cache Size',
      totalRequests: 'Total Requests',
    },

    cacheTypes: {
      query: 'Query Cache',
      embedding: 'Embedding Cache',
      retrieval: 'Retrieval Cache',
    },

    stats: {
      hitRate: 'Hit Rate',
      size: 'Cache Size',
      maxSize: 'Max Capacity',
      hitCount: 'Hit Count',
      missCount: 'Miss Count',
      evictionCount: 'Eviction Count',
      avgLoadTime: 'Avg Load Time',
      usagePercent: 'Usage',
    },

    hotkeys: {
      title: 'Hot Keys',
      key: 'Cache Key',
      hitCount: 'Hit Count',
      lastAccess: 'Last Access',
      size: 'Size',
      noData: 'No hot keys data',
    },

    trends: {
      title: 'Trends Analysis',
      hitRateTrend: 'Hit Rate Trend',
      sizeTrend: 'Size Trend',
      last24Hours: 'Last 24 Hours',
      last7Days: 'Last 7 Days',
      last30Days: 'Last 30 Days',
    },

    actions: {
      clearAll: 'Clear All',
      clearExpired: 'Clear Expired',
      clearPartial: 'Clear Selected',
      warmup: 'Warmup',
      refresh: 'Refresh',
      export: 'Export Stats',
    },

    clearConfirm: {
      title: 'Confirm Clear',
      allMessage: 'Are you sure to clear all cache? This will affect system performance.',
      expiredMessage: 'Are you sure to clear expired cache?',
      partialMessage: 'Are you sure to clear selected cache items?',
    },

    warmup: {
      title: 'Cache Warmup',
      inputKeys: 'Input Cache Keys',
      inputPlaceholder: 'One key per line, or comma separated',
      startWarmup: 'Start Warmup',
      progress: 'Warmup Progress',
      result: {
        title: 'Warmup Result',
        total: 'Total',
        success: 'Success',
        failure: 'Failure',
        duration: 'Duration',
      },
    },

    message: {
      loadSuccess: 'Loaded successfully',
      loadFailed: 'Failed to load',
      clearSuccess: 'Cleared successfully',
      clearFailed: 'Failed to clear',
      warmupSuccess: 'Warmup completed',
      warmupFailed: 'Warmup failed',
      exportSuccess: 'Exported successfully',
      exportFailed: 'Failed to export',
    },
  },

  // Query Process Visualization
  queryProcess: {
    processing: 'Processing query...',
    input: {
      title: 'Input Query',
      placeholder: 'Enter your query...',
      startButton: 'Start Query',
    },
    steps: {
      received: { title: 'Received', description: 'Query received, preparing to process' },
      expansion: { title: 'Expansion', description: 'Expanding query with synonyms and LLM' },
      embedding: { title: 'Embedding', description: 'Converting query to vector representation' },
      retrieval: { title: 'Retrieval', description: 'Retrieving relevant documents' },
      reranking: { title: 'Reranking', description: 'Optimizing results with reranker' },
      completed: { title: 'Completed', description: 'Query processing completed' },
    },
    statistics: {
      title: 'Processing Statistics',
      totalTime: 'Total Time',
      expandedQueries: 'Expanded Queries',
      retrievalResults: 'Retrieved Results',
      finalResults: 'Final Results',
      cacheHit: 'Cache Hit',
      embeddingTime: 'Embedding Time',
      retrievalTime: 'Retrieval Time',
      rerankingTime: 'Reranking Time',
    },
    timeline: {
      title: 'Processing Timeline',
      received: 'Query Received',
      expanded: 'Query Expanded',
      embedded: 'Vectorized',
      retrieved: 'Retrieved',
      reranked: 'Reranked',
      completed: 'Completed',
      vectorGenerated: 'Vector generated',
      results: 'results',
      topResults: 'Top results',
      ready: 'Ready to return',
    },
    progress: { title: 'Processing' },
  },

  // Retrieval Results Visualization
  retrievalResults: {
    query: 'Query',
    result: 'Result',
    score: 'Similarity',
    similarity: 'Similarity',
    noResults: 'No retrieval results',
    statistics: {
      totalResults: 'Total Results',
      avgScore: 'Avg Similarity',
      maxScore: 'Max Similarity',
      minScore: 'Min Similarity',
    },
    source: {
      vector: 'Vector',
      fulltext: 'Full-text',
      hybrid: 'Hybrid',
    },
    sourceDistribution: { title: 'Source Distribution' },
    scoreDistribution: { title: 'Score Distribution' },
    resultsList: { title: 'Results List' },
  },
}

