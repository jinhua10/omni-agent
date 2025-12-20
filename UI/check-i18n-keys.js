/**
 * 国际化 Key 检查工具
 * 检查组件中使用的所有 i18n key 是否都在语言文件中注册
 */

// 从 JSX 文件中提取的所有 i18n keys
const usedKeys = [
  // WorkflowBuilder.jsx
  'workflowBuilder.agents.loadFailed',
  'workflowBuilder.step.added',
  'workflowBuilder.step.deleteConfirm',
  'workflowBuilder.step.deleteWarning',
  'workflowBuilder.step.deleted',
  'workflowBuilder.step.updated',
  'workflowBuilder.validation.nameRequired',
  'workflowBuilder.validation.stepsRequired',
  'workflowBuilder.save.updated',
  'workflowBuilder.save.created',
  'workflowBuilder.save.failed',
  'workflowBuilder.test.success',
  'workflowBuilder.test.failed',
  'workflowBuilder.export.success',
  'workflowBuilder.import.success',
  'workflowBuilder.import.failed',
  'workflowBuilder.namePlaceholder',
  'workflowBuilder.status.draft',
  'workflowBuilder.status.active',
  'workflowBuilder.status.deprecated',
  'workflowBuilder.addStep',
  'workflowBuilder.testButton',
  'workflowBuilder.exportButton',
  'workflowBuilder.importButton',
  'workflowBuilder.stepEditor.title',
  'workflowBuilder.agentSelector.title',
  'workflowBuilder.test.title',
  'workflowBuilder.test.input',
  'workflowBuilder.test.execute',
  'workflowBuilder.test.result',

  // WorkflowCanvas.jsx
  'workflowBuilder.canvas.emptyHint',
  'workflowBuilder.canvas.addFirstStep',

  // WorkflowNode.jsx
  'workflowBuilder.node.allowFailure',
  'workflowBuilder.node.connect',
  'workflowBuilder.node.agent',
  'workflowBuilder.node.dependencies',
  'workflowBuilder.node.input',
  'workflowBuilder.node.output',

  // StepEditor.jsx
  'workflowBuilder.stepEditor.invalidJson',
  'workflowBuilder.stepEditor.name',
  'workflowBuilder.stepEditor.nameRequired',
  'workflowBuilder.stepEditor.namePlaceholder',
  'workflowBuilder.stepEditor.description',
  'workflowBuilder.stepEditor.descriptionPlaceholder',
  'workflowBuilder.stepEditor.agent',
  'workflowBuilder.stepEditor.agentRequired',
  'workflowBuilder.stepEditor.input',
  'workflowBuilder.stepEditor.expression',
  'workflowBuilder.stepEditor.inputRequired',
  'workflowBuilder.stepEditor.dependencies',
  'workflowBuilder.stepEditor.dependenciesPlaceholder',
  'workflowBuilder.stepEditor.advancedConfig',
  'workflowBuilder.stepEditor.allowFailure',
  'workflowBuilder.stepEditor.allowFailureTooltip',
  'workflowBuilder.stepEditor.timeout',
  'workflowBuilder.stepEditor.timeoutTooltip',
  'workflowBuilder.stepEditor.retries',
  'workflowBuilder.stepEditor.retriesTooltip',
  'workflowBuilder.stepEditor.condition',
  'workflowBuilder.stepEditor.conditionTooltip',

  // AgentSelector.jsx
  'workflowBuilder.agentSelector.searchPlaceholder',
  'workflowBuilder.agentSelector.noAgents',

  // SearchBar.jsx
  'workflowMarket.search.placeholder',
  'workflowMarket.search.button',

  // MarketBrowser.jsx
  'workflowMarket.title',
  'workflowMarket.subtitle',
  'workflowMarket.loading',
  'workflowMarket.search.noResults',
  'workflowMarket.search.tryOtherKeywords',
  'workflowMarket.loadMore',

  // FilterPanel.jsx
  'workflowMarket.category.all',
  'workflowMarket.category.dataProcessing',
  'workflowMarket.category.apiIntegration',
  'workflowMarket.category.automation',
  'workflowMarket.category.transformation',
  'workflowMarket.category.analysis',
  'workflowMarket.category.example',
  'workflowMarket.sort.popular',
  'workflowMarket.sort.recent',
  'workflowMarket.sort.topRated',
  'workflowMarket.sort.name',
  'workflowMarket.category.title',
  'workflowMarket.sort.title',
  'workflowMarket.reset',
];

// 语言文件中已注册的 keys (从 zh.js 和 en.js 提取)
const registeredKeys = {
  workflowMarket: {
    title: true,
    subtitle: true,
    search: {
      placeholder: true,
      button: true,
      noResults: true,
      tryOtherKeywords: true,
    },
    category: {
      title: true,
      all: true,
      dataProcessing: true,
      apiIntegration: true,
      automation: true,
      transformation: true,
      analysis: true,
      example: true,
    },
    sort: {
      title: true,
      popular: true,
      recent: true,
      topRated: true,
      name: true,
    },
    card: {
      featured: true,
      downloads: true,
      author: true,
      version: true,
      category: true,
    },
    detail: {
      backToMarket: true,
      download: true,
      install: true,
      overview: true,
      steps: true,
      ratings: true,
      description: true,
      noDescription: true,
      stepsCount: true,
      stepUnit: true,
      noSteps: true,
      agent: true,
      dependencies: true,
      downloadSuccess: true,
      downloadFailed: true,
      installSuccess: true,
      installFailed: true,
      notFound: true,
    },
    rating: {
      title: true,
      giveRating: true,
      submit: true,
      commentPlaceholder: true,
      pleaseRate: true,
      rateSuccess: true,
      rateFailed: true,
      noRatings: true,
      ratingsCount: true,
    },
    loading: true,
    loadMore: true,
    reset: true,
    noWorkflows: true,
  },
  workflowBuilder: {
    title: true,
    namePlaceholder: true,
    addStep: true,
    testButton: true,
    exportButton: true,
    importButton: true,
    status: {
      draft: true,
      active: true,
      deprecated: true,
    },
    validation: {
      nameRequired: true,
      stepsRequired: true,
    },
    save: {
      created: true,
      updated: true,
      failed: true,
    },
    test: {
      title: true,
      input: true,
      execute: true,
      result: true,
      success: true,
      failed: true,
    },
    export: {
      success: true,
    },
    import: {
      success: true,
      failed: true,
    },
    canvas: {
      emptyHint: true,
      addFirstStep: true,
    },
    node: {
      agent: true,
      dependencies: true,
      input: true,
      output: true,
      connect: true,
      allowFailure: true,
      timeout: true,
    },
    stepEditor: {
      title: true,
      name: true,
      nameRequired: true,
      namePlaceholder: true,
      description: true,
      descriptionPlaceholder: true,
      agent: true,
      agentRequired: true,
      input: true,
      inputRequired: true,
      expression: true,
      invalidJson: true,
      dependencies: true,
      dependenciesPlaceholder: true,
      advancedConfig: true,
      allowFailure: true,
      allowFailureTooltip: true,
      timeout: true,
      timeoutTooltip: true,
      retries: true,
      retriesTooltip: true,
      condition: true,
      conditionTooltip: true,
    },
    agentSelector: {
      title: true,
      searchPlaceholder: true,
      noAgents: true,
    },
    agents: {
      loadFailed: true,
    },
    step: {
      added: true,
      updated: true,
      deleted: true,
      deleteConfirm: true,
      deleteWarning: true,
    },
  },
};

// 检查函数
function checkKey(key) {
  const parts = key.split('.');
  let current = registeredKeys;
  
  for (const part of parts) {
    if (!current || !current[part]) {
      return false;
    }
    current = current[part];
  }
  
  return current === true;
}

// 执行检查
console.log('🔍 检查国际化 Key...\n');

const missingKeys = [];
const foundKeys = [];

usedKeys.forEach(key => {
  if (checkKey(key)) {
    foundKeys.push(key);
  } else {
    missingKeys.push(key);
  }
});

console.log(`✅ 已注册: ${foundKeys.length}/${usedKeys.length}`);
console.log(`❌ 未注册: ${missingKeys.length}/${usedKeys.length}\n`);

if (missingKeys.length > 0) {
  console.log('❌ 缺失的 Keys:');
  missingKeys.forEach(key => {
    console.log(`   - ${key}`);
  });
} else {
  console.log('🎉 所有的国际化 Key 都已注册！');
}
