# 🔄 工作流引擎系统集成方案

## 📋 当前系统架构分析

### 现有文档处理流程

```
用户上传文档（Web UI）
    ↓
保存到 data/documents/
    ↓
FileWatcherService 监听
    ↓
┌─────────────────────────────────┐
│ 文档处理流程                      │
├─────────────────────────────────┤
│ 1. DocumentProcessor 处理        │
│    - Vision LLM（PPT/PDF/Word）│
│    - 文本提取                    │
│    - 图片提取                    │
│ 2. 智能分块                      │
│    - ChunkingStrategyManager    │
│    - PPL 分块                    │
│ 3. 存储                          │
│    - 原始文档 → data/storage/documents/ │
│    - 分块 → data/storage/chunks/│
│    - 图片 → data/storage/images/│
│ 4. RAG 索引                      │
│    - Lucene 文本索引             │
│    - 向量索引（ONNX）            │
└─────────────────────────────────┘
    ↓
用户通过 AI Chat 查询
```

**关键发现**：
- ✅ 文档上传和 RAG 索引流程已完善
- ✅ 存储层清晰（documents/chunks/images）
- ⚠️ 缺少**文档分类和标注**（技术文档？源码？需求？）
- ⚠️ 缺少**工作流触发机制**
- ⚠️ 缺少**增强知识库**（专门为工作流服务）

---

## 🎯 工作流引擎集成方案

### 方案概览

```
┌─────────────────────────────────────────────────────────────┐
│                    用户交互层（Web UI）                        │
├─────────────────────────────────────────────────────────────┤
│ 1. 文档上传 UI        │ 2. 工作流触发 UI   │ 3. AI Chat UI  │
│ - 上传文件            │ - 选择工作流       │ - 普通问答      │
│ - 文档类型标注 ⭐      │ - 配置参数         │ - 工作流查询⭐  │
│ - 元数据填写          │ - 查看执行状态     │                │
└─────────────────────────────────────────────────────────────┘
    ↓                       ↓                      ↓
┌─────────────────────────────────────────────────────────────┐
│                    核心处理层                                  │
├─────────────────────────────────────────────────────────────┤
│  文档处理管道           工作流引擎            AI 服务           │
│  (现有)                (新增 ⭐)             (现有)            │
│  ↓                     ↓                     ↓                │
│  FileWatcher  →  WorkflowRouter  →  EnhancedRAG             │
│                      ↓                       ↓                │
│              WorkflowEngine            AlgorithmMarket       │
│                  ↓                                            │
│              Agent 生态                                       │
└─────────────────────────────────────────────────────────────┘
    ↓                       ↓                      ↓
┌─────────────────────────────────────────────────────────────┐
│                    数据存储层                                  │
├─────────────────────────────────────────────────────────────┤
│  基础知识库              增强知识库 ⭐          工作流状态库    │
│  data/storage/          data/workflows/       data/workflow-state/ │
│  ├── documents/         ├── knowledge-graph/  ├── executions/ │
│  ├── chunks/            ├── code-analysis/    ├── tasks/      │
│  └── images/            ├── requirements/     └── history/    │
│                         └── evaluations/                      │
└─────────────────────────────────────────────────────────────┘
```

---

## 💡 核心设计思路

### 1. **双知识库架构** ⭐

#### 基础知识库（现有）
- **目的**：通用 RAG 检索
- **存储**：`data/storage/`
- **内容**：原始文档、分块、图片
- **特点**：快速、通用、适合问答

#### 增强知识库（新增）
- **目的**：工作流专用，结构化存储
- **存储**：`data/workflows/knowledge/`
- **内容**：
  - **知识图谱**：文档间关系、模块依赖
  - **代码分析结果**：AST、调用图、依赖树
  - **需求分析结果**：功能清单、影响分析
  - **评估数据**：项目评分、对比结果
- **特点**：结构化、关联性强、支持复杂查询

**关系**：
```
基础知识库 + 增强知识库 = 完整知识体系
     ↓              ↓
   快速检索      深度分析
   简单问答      复杂任务
```

### 2. **文档上传时的文档类型标注** ⭐

**问题**：当前系统无法区分文档类型，所有文档统一处理。

**解决方案**：上传时让用户选择文档类型

#### 上传 UI 增强

```vue
<template>
  <el-upload>
    <el-button>上传文档</el-button>
  </el-upload>
  
  <!-- ⭐ 新增：文档类型选择 -->
  <el-form>
    <el-form-item label="文档类型">
      <el-select v-model="documentType">
        <el-option label="📄 通用文档" value="general" />
        <el-option label="🔧 技术文档" value="technical">
          <el-option label="API 文档" value="technical-api" />
          <el-option label="架构设计" value="technical-architecture" />
          <el-option label="故障排查" value="technical-troubleshooting" />
        </el-option>
        <el-option label="💻 源码项目" value="source-code">
          <el-option label="Java 项目" value="source-java" />
          <el-option label="Python 项目" value="source-python" />
          <el-option label="Node.js 项目" value="source-nodejs" />
        </el-option>
        <el-option label="📋 需求文档" value="requirement" />
        <el-option label="📊 业务文档" value="business" />
      </el-select>
    </el-form-item>
    
    <!-- ⭐ 可选：元数据填写 -->
    <el-form-item label="项目/模块">
      <el-input v-model="projectName" placeholder="例如：OmniAgent" />
    </el-form-item>
    
    <el-form-item label="标签">
      <el-tag v-for="tag in tags" :key="tag">{{ tag }}</el-tag>
      <el-input v-model="newTag" @keyup.enter="addTag" />
    </el-form-item>
  </el-form>
</template>
```

**后端处理**：

```java
@PostMapping("/upload")
public UploadResponse uploadDocument(
        @RequestParam("file") MultipartFile file,
        @RequestParam(value = "documentType", defaultValue = "general") String documentType,
        @RequestParam(value = "projectName", required = false) String projectName,
        @RequestParam(value = "tags", required = false) List<String> tags) {
    
    // 保存文档到监听目录
    Path watchDir = Paths.get(watchDirectory);
    
    // ⭐ 同时保存元数据
    DocumentMetadata metadata = DocumentMetadata.builder()
            .fileName(file.getOriginalFilename())
            .documentType(documentType)
            .projectName(projectName)
            .tags(tags)
            .uploadTime(System.currentTimeMillis())
            .build();
    
    metadataService.saveMetadata(file.getOriginalFilename(), metadata);
    
    // 文件照常保存到 data/documents
    file.transferTo(watchDir.resolve(file.getOriginalFilename()));
    
    return response;
}
```

### 3. **文件监听器增强：自动触发工作流** ⭐

**修改 FileWatcherService**，在文档处理完成后，根据文档类型触发相应的增强工作流。

```java
@Service
public class FileWatcherService {
    
    @Autowired
    private WorkflowEngine workflowEngine;
    
    @Autowired
    private DocumentMetadataService metadataService;
    
    private void processNewFile(Path filePath, Path relativePath) {
        String filename = filePath.getFileName().toString();
        
        // ... 现有的处理逻辑（Vision LLM、分块、RAG 索引）...
        
        // ⭐ 新增：处理完成后，根据文档类型触发增强工作流
        try {
            DocumentMetadata metadata = metadataService.getMetadata(filename);
            if (metadata != null && metadata.getDocumentType() != null) {
                triggerEnhancementWorkflow(filename, metadata);
            }
        } catch (Exception e) {
            log.warn("⚠️ 触发增强工作流失败: {}", filename, e);
        }
        
        // ... 后续处理 ...
    }
    
    /**
     * 根据文档类型触发相应的增强工作流
     */
    private void triggerEnhancementWorkflow(String filename, DocumentMetadata metadata) {
        String workflowName = null;
        
        switch (metadata.getDocumentType()) {
            case "source-java":
            case "source-python":
            case "source-nodejs":
                // 源码项目 → 自动进行代码结构分析
                workflowName = "SourceCode-StructureAnalysis";
                break;
                
            case "technical-api":
            case "technical-architecture":
                // 技术文档 → 自动提取 API 信息、架构图
                workflowName = "TechDoc-KnowledgeExtraction";
                break;
                
            case "requirement":
                // 需求文档 → 自动提取功能清单
                workflowName = "Requirement-FeatureExtraction";
                break;
                
            default:
                // 通用文档 → 只做基础 RAG
                log.debug("通用文档，不触发增强工作流: {}", filename);
                return;
        }
        
        if (workflowName != null) {
            log.info("🔄 自动触发增强工作流: workflow={}, file={}", workflowName, filename);
            
            // 异步执行工作流
            CompletableFuture.runAsync(() -> {
                try {
                    Map<String, Object> input = Map.of(
                        "fileName", filename,
                        "documentType", metadata.getDocumentType(),
                        "projectName", metadata.getProjectName(),
                        "tags", metadata.getTags()
                    );
                    
                    workflowEngine.executeAsync(workflowName, input);
                } catch (Exception e) {
                    log.error("❌ 工作流执行失败: {}", workflowName, e);
                }
            });
        }
    }
}
```

### 4. **增强工作流：构建增强知识库** ⭐

#### 示例 1: 源码结构分析工作流

```yaml
workflow:
  name: "SourceCode-StructureAnalysis"
  description: "源码项目结构分析工作流（自动触发）"
  trigger: "文档上传完成"
  
  steps:
    - id: "extract_code_files"
      name: "提取代码文件"
      agent: "CodeFileExtractor"
      input: "${workflow.input.fileName}"
      output: "代码文件列表"
    
    - id: "parse_ast"
      name: "解析 AST"
      agent: "ASTParser"
      input: "代码文件列表"
      output: "AST 树"
    
    - id: "analyze_dependencies"
      name: "分析依赖关系"
      agent: "DependencyAnalyzer"
      input: "AST 树"
      output: "依赖图"
    
    - id: "extract_apis"
      name: "提取 API 接口"
      agent: "APIExtractor"
      input: "AST 树"
      output: "API 列表"
    
    - id: "build_knowledge_graph"
      name: "构建知识图谱"
      agent: "KnowledgeGraphBuilder"
      input: ["AST 树", "依赖图", "API 列表"]
      actions:
        - 创建节点（类、方法、模块）
        - 创建关系（调用、继承、依赖）
        - 保存到图数据库
      output: "知识图谱 ID"
    
    - id: "save_to_enhanced_kb"
      name: "保存到增强知识库"
      agent: "EnhancedKBWriter"
      input: "知识图谱 ID"
      storage: "data/workflows/knowledge/code-analysis/"
      format:
        - "${projectName}/structure.json"      # 项目结构
        - "${projectName}/dependencies.json"   # 依赖关系
        - "${projectName}/apis.json"           # API 列表
        - "${projectName}/graph.json"          # 知识图谱
```

**结果**：
```
data/workflows/knowledge/code-analysis/
├── OmniAgent/
│   ├── structure.json       # 项目结构
│   ├── dependencies.json    # 依赖关系
│   ├── apis.json            # API 列表
│   └── graph.json           # 知识图谱
└── MyProject/
    └── ...
```

#### 示例 2: 需求文档特征提取工作流

```yaml
workflow:
  name: "Requirement-FeatureExtraction"
  description: "需求文档特征提取工作流（自动触发）"
  
  steps:
    - id: "parse_requirements"
      name: "解析需求"
      agent: "RequirementParser"
      input: "${workflow.input.fileName}"
      output: "结构化需求列表"
    
    - id: "extract_features"
      name: "提取功能清单"
      agent: "FeatureExtractor"
      input: "结构化需求列表"
      output: "功能清单"
    
    - id: "classify_features"
      name: "功能分类"
      agent: "FeatureClassifier"
      input: "功能清单"
      categories:
        - 核心功能
        - 辅助功能
        - 非功能需求
      output: "分类后的功能清单"
    
    - id: "save_to_enhanced_kb"
      name: "保存到增强知识库"
      agent: "EnhancedKBWriter"
      storage: "data/workflows/knowledge/requirements/"
      format:
        - "${projectName}/features.json"
        - "${projectName}/priorities.json"
```

---

## 🎨 工作流 UI 设计

### 1. **工作流管理页面**

```vue
<template>
  <div class="workflow-management">
    <!-- 工作流列表 -->
    <el-card title="📋 可用工作流">
      <el-table :data="workflows">
        <el-table-column prop="name" label="工作流名称" />
        <el-table-column prop="description" label="描述" />
        <el-table-column prop="category" label="类别">
          <template #default="{ row }">
            <el-tag>{{ row.category }}</el-tag>
          </template>
        </el-table-column>
        <el-table-column label="操作">
          <template #default="{ row }">
            <el-button @click="showWorkflowDialog(row)">
              执行
            </el-button>
            <el-button @click="viewWorkflow(row)">
              查看
            </el-button>
          </template>
        </el-table-column>
      </el-table>
    </el-card>
    
    <!-- 执行历史 -->
    <el-card title="📊 执行历史" style="margin-top: 20px;">
      <el-table :data="executions">
        <el-table-column prop="workflowName" label="工作流" />
        <el-table-column prop="startTime" label="开始时间" />
        <el-table-column prop="duration" label="耗时" />
        <el-table-column prop="status" label="状态">
          <template #default="{ row }">
            <el-tag :type="getStatusType(row.status)">
              {{ row.status }}
            </el-tag>
          </template>
        </el-table-column>
        <el-table-column label="操作">
          <template #default="{ row }">
            <el-button @click="viewExecution(row)">
              查看详情
            </el-button>
          </template>
        </el-table-column>
      </el-table>
    </el-card>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue';

const workflows = ref([
  {
    name: 'TechDoc-Diagnosis',
    description: '技术文档问题诊断',
    category: '技术文档'
  },
  {
    name: 'SourceCode-VulnerabilityAnalysis',
    description: '源码漏洞分析',
    category: '源码分析'
  },
  {
    name: 'SourceCode-BusinessValueEvaluation',
    description: '商业价值评估',
    category: '源码分析'
  },
  {
    name: 'Requirement-FeasibilityAnalysis',
    description: '需求可行性分析',
    category: '需求分析'
  }
]);

const executions = ref([]);

async function showWorkflowDialog(workflow) {
  // 打开工作流配置对话框
}

async function viewWorkflow(workflow) {
  // 查看工作流定义
}

async function viewExecution(execution) {
  // 查看执行详情
}
</script>
```

### 2. **工作流执行对话框**

```vue
<template>
  <el-dialog title="执行工作流" v-model="visible" width="600px">
    <el-form :model="form">
      <el-form-item label="工作流">
        <el-input v-model="workflow.name" disabled />
      </el-form-item>
      
      <!-- ⭐ 动态输入参数 -->
      <el-form-item 
        v-for="param in workflow.params" 
        :key="param.name"
        :label="param.label"
      >
        <!-- 文档选择 -->
        <el-select 
          v-if="param.type === 'document'"
          v-model="form[param.name]"
          placeholder="选择文档"
        >
          <el-option 
            v-for="doc in documents"
            :key="doc.fileName"
            :label="doc.fileName"
            :value="doc.fileName"
          />
        </el-select>
        
        <!-- 文本输入 -->
        <el-input 
          v-else-if="param.type === 'text'"
          v-model="form[param.name]"
          :placeholder="param.placeholder"
        />
        
        <!-- 多选框 -->
        <el-checkbox-group 
          v-else-if="param.type === 'multi-select'"
          v-model="form[param.name]"
        >
          <el-checkbox 
            v-for="option in param.options"
            :key="option.value"
            :label="option.value"
          >
            {{ option.label }}
          </el-checkbox>
        </el-checkbox-group>
      </el-form-item>
    </el-form>
    
    <template #footer>
      <el-button @click="visible = false">取消</el-button>
      <el-button type="primary" @click="executeWorkflow">
        执行
      </el-button>
    </template>
  </el-dialog>
</template>

<script setup>
import { ref } from 'vue';
import axios from 'axios';

const visible = ref(false);
const workflow = ref({});
const form = ref({});
const documents = ref([]);

async function executeWorkflow() {
  try {
    const response = await axios.post('/api/workflows/execute', {
      workflowName: workflow.value.name,
      input: form.value
    });
    
    if (response.data.success) {
      ElMessage.success('工作流已开始执行');
      visible.value = false;
      // 跳转到执行详情页面
      router.push(`/workflows/executions/${response.data.executionId}`);
    }
  } catch (error) {
    ElMessage.error('执行失败: ' + error.message);
  }
}
</script>
```

### 3. **工作流执行详情页面**

```vue
<template>
  <div class="workflow-execution-detail">
    <!-- 执行概览 -->
    <el-card>
      <h2>{{ execution.workflowName }}</h2>
      <el-descriptions :column="3">
        <el-descriptions-item label="执行ID">
          {{ execution.id }}
        </el-descriptions-item>
        <el-descriptions-item label="状态">
          <el-tag :type="getStatusType(execution.status)">
            {{ execution.status }}
          </el-tag>
        </el-descriptions-item>
        <el-descriptions-item label="开始时间">
          {{ formatTime(execution.startTime) }}
        </el-descriptions-item>
        <el-descriptions-item label="耗时">
          {{ execution.duration }}ms
        </el-descriptions-item>
      </el-descriptions>
    </el-card>
    
    <!-- ⭐ 步骤执行流程图 -->
    <el-card style="margin-top: 20px;">
      <h3>执行流程</h3>
      <div class="workflow-steps">
        <div 
          v-for="(step, index) in execution.steps"
          :key="step.id"
          class="step-node"
          :class="{ 
            'running': step.status === 'RUNNING',
            'success': step.status === 'SUCCESS',
            'failed': step.status === 'FAILED'
          }"
        >
          <div class="step-header">
            <span class="step-number">{{ index + 1 }}</span>
            <span class="step-name">{{ step.name }}</span>
            <el-icon v-if="step.status === 'RUNNING'">
              <Loading />
            </el-icon>
            <el-icon v-else-if="step.status === 'SUCCESS'">
              <Check />
            </el-icon>
            <el-icon v-else-if="step.status === 'FAILED'">
              <Close />
            </el-icon>
          </div>
          
          <div class="step-details">
            <div>Agent: {{ step.agent }}</div>
            <div>耗时: {{ step.duration }}ms</div>
            <el-button 
              text 
              @click="viewStepDetail(step)"
            >
              查看详情
            </el-button>
          </div>
          
          <!-- 连接线 -->
          <div 
            v-if="index < execution.steps.length - 1"
            class="step-connector"
          />
        </div>
      </div>
    </el-card>
    
    <!-- 执行结果 -->
    <el-card style="margin-top: 20px;" v-if="execution.result">
      <h3>执行结果</h3>
      <pre>{{ JSON.stringify(execution.result, null, 2) }}</pre>
    </el-card>
  </div>
</template>

<script setup>
import { ref, onMounted } from 'vue';
import { useRoute } from 'vue-router';
import axios from 'axios';

const route = useRoute();
const execution = ref({});

onMounted(async () => {
  const executionId = route.params.id;
  const response = await axios.get(`/api/workflows/executions/${executionId}`);
  execution.value = response.data;
  
  // 如果正在执行，轮询更新状态
  if (execution.value.status === 'RUNNING') {
    startPolling();
  }
});

function startPolling() {
  const timer = setInterval(async () => {
    const response = await axios.get(
      `/api/workflows/executions/${execution.value.id}`
    );
    execution.value = response.data;
    
    if (execution.value.status !== 'RUNNING') {
      clearInterval(timer);
    }
  }, 2000);
}
</script>

<style scoped>
.workflow-steps {
  display: flex;
  flex-direction: column;
  gap: 20px;
}

.step-node {
  border: 2px solid #e0e0e0;
  border-radius: 8px;
  padding: 16px;
  transition: all 0.3s;
}

.step-node.running {
  border-color: #409eff;
  background: #ecf5ff;
}

.step-node.success {
  border-color: #67c23a;
  background: #f0f9ff;
}

.step-node.failed {
  border-color: #f56c6c;
  background: #fef0f0;
}

.step-connector {
  width: 2px;
  height: 20px;
  background: #e0e0e0;
  margin: 0 auto;
}
</style>
```

### 4. **AI Chat 中集成工作流** ⭐

在 AI Chat 界面中，用户可以直接触发工作流：

```vue
<template>
  <div class="ai-chat">
    <!-- 聊天消息列表 -->
    <div class="messages">
      <div v-for="msg in messages" :key="msg.id" class="message">
        <div v-if="msg.type === 'user'" class="user-message">
          {{ msg.content }}
        </div>
        
        <div v-else-if="msg.type === 'assistant'" class="assistant-message">
          {{ msg.content }}
        </div>
        
        <!-- ⭐ 工作流执行结果 -->
        <div v-else-if="msg.type === 'workflow'" class="workflow-message">
          <el-card>
            <template #header>
              <div class="workflow-header">
                <span>🔄 工作流执行</span>
                <el-tag :type="getStatusType(msg.status)">
                  {{ msg.status }}
                </el-tag>
              </div>
            </template>
            
            <div>工作流: {{ msg.workflowName }}</div>
            <div>耗时: {{ msg.duration }}ms</div>
            
            <!-- 结果预览 -->
            <div v-if="msg.result" style="margin-top: 10px;">
              <el-collapse>
                <el-collapse-item title="查看详细结果">
                  <pre>{{ JSON.stringify(msg.result, null, 2) }}</pre>
                </el-collapse-item>
              </el-collapse>
            </div>
            
            <el-button 
              text 
              @click="viewWorkflowExecution(msg.executionId)"
            >
              查看完整执行过程
            </el-button>
          </el-card>
        </div>
      </div>
    </div>
    
    <!-- 输入框 -->
    <div class="input-area">
      <el-input 
        v-model="userInput"
        placeholder="输入问题，或 @workflow 触发工作流..."
        @keyup.enter="sendMessage"
      >
        <template #append>
          <el-button @click="sendMessage">发送</el-button>
        </template>
      </el-input>
      
      <!-- ⭐ 工作流快捷按钮 -->
      <div class="quick-workflows">
        <el-button 
          size="small"
          @click="triggerWorkflow('TechDoc-Diagnosis')"
        >
          🔧 问题诊断
        </el-button>
        <el-button 
          size="small"
          @click="triggerWorkflow('SourceCode-VulnerabilityAnalysis')"
        >
          🔍 漏洞分析
        </el-button>
        <el-button 
          size="small"
          @click="triggerWorkflow('Requirement-FeasibilityAnalysis')"
        >
          📋 可行性分析
        </el-button>
      </div>
    </div>
  </div>
</template>

<script setup>
import { ref } from 'vue';
import axios from 'axios';

const messages = ref([]);
const userInput = ref('');

async function sendMessage() {
  const content = userInput.value.trim();
  if (!content) return;
  
  // 添加用户消息
  messages.value.push({
    id: Date.now(),
    type: 'user',
    content
  });
  
  userInput.value = '';
  
  // ⭐ 检查是否触发工作流
  if (content.startsWith('@workflow ')) {
    await handleWorkflowCommand(content);
    return;
  }
  
  // 普通 AI 问答
  await handleNormalChat(content);
}

async function handleWorkflowCommand(content) {
  // 解析工作流命令
  // 例如: @workflow TechDoc-Diagnosis 参数1 参数2
  
  const parts = content.substring(10).split(' ');
  const workflowName = parts[0];
  const params = parts.slice(1);
  
  // 执行工作流
  const response = await axios.post('/api/workflows/execute', {
    workflowName,
    input: { userInput: params.join(' ') }
  });
  
  // 添加工作流消息
  messages.value.push({
    id: Date.now(),
    type: 'workflow',
    workflowName,
    executionId: response.data.executionId,
    status: 'RUNNING'
  });
  
  // 轮询工作流执行状态
  pollWorkflowStatus(response.data.executionId);
}

async function handleNormalChat(content) {
  // 调用 AI 服务
  const response = await axios.post('/api/ai/chat', {
    message: content
  });
  
  messages.value.push({
    id: Date.now(),
    type: 'assistant',
    content: response.data.reply
  });
}

async function triggerWorkflow(workflowName) {
  // 显示工作流配置对话框
  // ...
}
</script>
```

---

## 🔌 后端 API 设计

### 1. 工作流执行 API

```java
@RestController
@RequestMapping("/api/workflows")
public class WorkflowController {
    
    @Autowired
    private WorkflowEngine workflowEngine;
    
    @Autowired
    private WorkflowExecutionRepository executionRepository;
    
    /**
     * 执行工作流
     */
    @PostMapping("/execute")
    public Map<String, Object> executeWorkflow(@RequestBody WorkflowExecutionRequest request) {
        try {
            // 创建执行记录
            WorkflowExecution execution = WorkflowExecution.builder()
                    .id(UUID.randomUUID().toString())
                    .workflowName(request.getWorkflowName())
                    .input(request.getInput())
                    .status(ExecutionStatus.RUNNING)
                    .startTime(System.currentTimeMillis())
                    .build();
            
            executionRepository.save(execution);
            
            // 异步执行工作流
            CompletableFuture.runAsync(() -> {
                try {
                    WorkflowResult result = workflowEngine.execute(
                        request.getWorkflowName(), 
                        request.getInput()
                    );
                    
                    // 更新执行记录
                    execution.setStatus(ExecutionStatus.SUCCESS);
                    execution.setResult(result);
                    execution.setEndTime(System.currentTimeMillis());
                    executionRepository.save(execution);
                    
                } catch (Exception e) {
                    execution.setStatus(ExecutionStatus.FAILED);
                    execution.setError(e.getMessage());
                    execution.setEndTime(System.currentTimeMillis());
                    executionRepository.save(execution);
                }
            });
            
            return Map.of(
                "success", true,
                "executionId", execution.getId(),
                "message", "工作流已开始执行"
            );
            
        } catch (Exception e) {
            return Map.of(
                "success", false,
                "message", "启动工作流失败: " + e.getMessage()
            );
        }
    }
    
    /**
     * 获取工作流执行状态
     */
    @GetMapping("/executions/{executionId}")
    public WorkflowExecution getExecution(@PathVariable String executionId) {
        return executionRepository.findById(executionId)
                .orElseThrow(() -> new NotFoundException("执行记录不存在"));
    }
    
    /**
     * 获取所有工作流列表
     */
    @GetMapping("/list")
    public List<WorkflowInfo> listWorkflows() {
        return workflowRegistry.getAllWorkflows();
    }
}
```

### 2. 文档元数据 API

```java
@RestController
@RequestMapping("/api/documents/metadata")
public class DocumentMetadataController {
    
    @Autowired
    private DocumentMetadataService metadataService;
    
    /**
     * 保存文档元数据
     */
    @PostMapping
    public Map<String, Object> saveMetadata(@RequestBody DocumentMetadata metadata) {
        metadataService.saveMetadata(metadata.getFileName(), metadata);
        return Map.of("success", true);
    }
    
    /**
     * 获取文档元数据
     */
    @GetMapping("/{fileName}")
    public DocumentMetadata getMetadata(@PathVariable String fileName) {
        return metadataService.getMetadata(fileName);
    }
    
    /**
     * 按文档类型查询
     */
    @GetMapping("/by-type/{documentType}")
    public List<DocumentMetadata> getByType(@PathVariable String documentType) {
        return metadataService.findByDocumentType(documentType);
    }
}
```

---

## 📊 数据存储结构

### 1. 文档元数据存储

```json
// data/config/document-metadata.json
{
  "technical-doc.pdf": {
    "fileName": "technical-doc.pdf",
    "documentType": "technical-api",
    "projectName": "OmniAgent",
    "tags": ["API", "文档", "Spring Boot"],
    "uploadTime": 1734691234000,
    "processedTime": 1734691245000,
    "enhancementWorkflows": [
      {
        "workflowName": "TechDoc-KnowledgeExtraction",
        "executionId": "abc-123",
        "status": "SUCCESS",
        "completedTime": 1734691260000
      }
    ]
  }
}
```

### 2. 工作流执行记录

```json
// data/workflow-state/executions/abc-123.json
{
  "id": "abc-123",
  "workflowName": "TechDoc-KnowledgeExtraction",
  "input": {
    "fileName": "technical-doc.pdf",
    "documentType": "technical-api"
  },
  "status": "SUCCESS",
  "startTime": 1734691245000,
  "endTime": 1734691260000,
  "duration": 15000,
  "steps": [
    {
      "id": "extract_apis",
      "name": "提取 API",
      "agent": "APIExtractor",
      "status": "SUCCESS",
      "startTime": 1734691245000,
      "endTime": 1734691250000,
      "duration": 5000,
      "result": {
        "apiCount": 25,
        "apis": [...]
      }
    }
  ],
  "result": {
    "extractedAPIs": 25,
    "savedTo": "data/workflows/knowledge/tech-docs/OmniAgent/apis.json"
  }
}
```

### 3. 增强知识库存储

```
data/workflows/knowledge/
├── code-analysis/              # 代码分析结果
│   ├── OmniAgent/
│   │   ├── structure.json      # 项目结构
│   │   ├── dependencies.json   # 依赖关系
│   │   ├── apis.json           # API 列表
│   │   └── graph.json          # 知识图谱
│   └── MyProject/
│       └── ...
├── tech-docs/                  # 技术文档知识
│   ├── OmniAgent/
│   │   ├── apis.json           # 提取的 API
│   │   └── architecture.json   # 架构信息
│   └── ...
├── requirements/               # 需求分析结果
│   ├── ProjectA/
│   │   ├── features.json       # 功能清单
│   │   └── priorities.json     # 优先级
│   └── ...
└── evaluations/                # 评估结果
    ├── OmniAgent-eval.json     # 项目评估
    └── comparison-2025-12-20.json  # 对比结果
```

---

## 🎯 完整流程示例

### 场景：上传源码项目并进行漏洞分析

#### 1. 用户上传

```
用户打开上传页面
    ↓
选择文件: MyWebApp.zip
    ↓
选择文档类型: "源码项目 - Java"
    ↓
填写元数据:
  - 项目名: MyWebApp
  - 标签: [Web, Spring Boot, MySQL]
    ↓
点击上传
```

#### 2. 系统自动处理

```
文件保存到: data/documents/MyWebApp.zip
元数据保存到: data/config/document-metadata.json
    ↓
FileWatcherService 监听到新文件
    ↓
执行基础处理:
  - 解压 ZIP
  - 提取代码文件
  - 基础 RAG 索引
  - 保存到 data/storage/documents/MyWebApp.zip/
    ↓
触发增强工作流: "SourceCode-StructureAnalysis"
    ↓
执行工作流步骤:
  1. 提取代码文件列表 ✓
  2. 解析 AST ✓
  3. 分析依赖关系 ✓
  4. 构建知识图谱 ✓
  5. 保存到增强知识库 ✓
    ↓
保存结果:
  - data/workflows/knowledge/code-analysis/MyWebApp/
    ├── structure.json
    ├── dependencies.json
    └── graph.json
```

#### 3. 用户主动触发工作流

```
用户打开工作流管理页面
    ↓
选择工作流: "SourceCode-VulnerabilityAnalysis"
    ↓
选择文档: MyWebApp.zip
    ↓
点击执行
    ↓
跳转到执行详情页面
    ↓
实时查看执行进度:
  1. 代码结构分析 ✓
  2. 依赖漏洞扫描 ⏳ (正在执行)
  3. 代码模式分析 ⏸️ (等待)
  4. API 安全分析 ⏸️
  5. 漏洞评分 ⏸️
  6. 修复建议 ⏸️
  7. 报告生成 ⏸️
    ↓
全部完成
    ↓
查看最终报告:
  - 高危漏洞: 3 个
  - 中危漏洞: 7 个
  - 修复建议: [详细列表]
```

#### 4. AI Chat 集成使用

```
用户在 AI Chat 中输入:
"分析 MyWebApp 的安全漏洞"
    ↓
系统识别意图: 漏洞分析
    ↓
自动触发工作流: "SourceCode-VulnerabilityAnalysis"
    ↓
返回执行链接: "工作流已开始执行，点击查看详情"
    ↓
工作流完成后，在聊天中显示摘要:
"漏洞分析完成：
  - 高危漏洞: 3 个
  - 中危漏洞: 7 个
  最严重的问题是 SQL 注入风险...
  [查看完整报告]"
```

---

## 💡 总结

### 核心思路

1. **双知识库架构**：
   - 基础知识库（现有）：快速检索、通用问答
   - 增强知识库（新增）：结构化存储、深度分析

2. **三种工作流触发方式**：
   - 自动触发：文档上传后自动执行增强工作流
   - 手动触发：用户在工作流管理页面主动执行
   - AI 集成触发：在 AI Chat 中自然语言触发

3. **文档类型标注**：
   - 上传时让用户选择文档类型
   - 根据文档类型自动选择增强工作流
   - 元数据持久化，便于后续查询

4. **增量增强**：
   - 基础 RAG 照常工作（不影响现有功能）
   - 增强工作流异步执行（不阻塞用户）
   - 增强知识库独立存储（扩展性强）

### 优势

- ✅ **不破坏现有架构**：基础 RAG 照常工作
- ✅ **渐进式增强**：逐步添加增强能力
- ✅ **用户体验好**：异步处理、实时反馈
- ✅ **可扩展性强**：易于添加新工作流和新场景

### 实施路径

#### 核心功能实施

1. **Phase 1**：文档元数据支持（1周）
   - DocumentMetadata 实体
   - 上传 API 支持文档类型
   - 前端文档类型选择

2. **Phase 2**：工作流引擎核心（2周）
   - WorkflowEngine 实现
   - WorkflowRegistry 实现
   - 基础 Agent 实现
   - DAG 构建和拓扑排序

3. **Phase 3**：第一个增强工作流（1周）
   - SourceCode-StructureAnalysis 工作流
   - 相关 Agent 实现
   - FileWatcherService 集成

4. **Phase 4**：UI 页面（1周）
   - 工作流管理页面
   - 工作流执行详情页面

5. **Phase 5**：AI Chat 集成（1周）
   - 意图识别
   - 工作流触发
   - 结果展示

#### 高级功能实施 ⭐

6. **Phase 6**：工作流持久化和版本管理（1周）
   - 工作流定义 CRUD API
   - 版本管理
   - 工作流模板

7. **Phase 7**：工作流编排（WorkflowInvoker）（1周）
   - WorkflowInvokerAgent 实现
   - 支持工作流链式调用
   - 支持并行和批量执行

8. **Phase 8**：MCP 集成（2周）
   - MCP Client 实现
   - MCPAgent 实现
   - MCP Server 配置管理

9. **Phase 9**：可视化工作流编辑器（2周）
   - 拖拽式编辑器
   - 属性面板
   - 测试和发布功能

**总计：12周完成完整框架（包含 MCP 和可视化编辑器）！** 🚀

---

## 🔗 相关文档

- **[工作流框架设计](WORKFLOW_FRAMEWORK_DESIGN.md)** - 详细的工作流设计和场景分析
- **[工作流 MCP 集成](WORKFLOW_MCP_INTEGRATION.md)** ⭐ - MCP 集成方案和工作流持久化
- **[RAG 算法决策树](../worklog/RAG_ALGORITHM_DECISION_TREE.md)** - RAG 优化算法选择指南

