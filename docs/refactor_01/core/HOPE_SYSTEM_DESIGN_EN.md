# HOPE System Design Documentation

**HOPE = Hierarchical Omni-Agent Persistent Engine**  
**Chinese Name:** Hierarchical Intelligent Persistence Engine

**Created:** 2025-12-31  
**Version:** 1.0.0  
**Status:** ✅ Implemented

---

## 📋 Table of Contents

1. [System Overview](#system-overview)
2. [Three-Layer Knowledge Structure](#three-layer-knowledge-structure)
3. [Core Components](#core-components)
4. [Workflow](#workflow)
5. [Persistence Mechanism](#persistence-mechanism)
6. [Configuration](#configuration)
7. [API Reference](#api-reference)
8. [Best Practices](#best-practices)
9. [Relationship with Knowledge Network](#relationship-with-knowledge-network)

---

## 🎯 System Overview

### 1.1 What is HOPE?

HOPE (Hierarchical Omni-Agent Persistent Engine) is OmniAgent's **core knowledge management system**, implementing layered knowledge storage and intelligent retrieval mechanisms.

**Core Principles:**
- Different types of knowledge should be stored in different layers
- Frequently accessed knowledge should be retrieved with priority
- Core knowledge should be preserved long-term

### 1.2 Design Goals

| Goal | Description |
|------|-------------|
| 🎯 **Intelligent Layering** | Automatically select the most appropriate knowledge layer based on question type |
| ⚡ **Efficient Retrieval** | Prioritize high-frequency knowledge retrieval to improve response speed |
| 💾 **Persistence** | Long-term preservation of core knowledge to avoid repetitive learning |
| 🔄 **Dynamic Adjustment** | Dynamically adjust knowledge layers based on access frequency |
| 🧠 **Intelligent Learning** | Learn new knowledge from user interactions |

### 1.3 System Architecture

```
┌─────────────────────────────────────────────────────────┐
│                  HOPE Knowledge Manager                  │
│           (Core Coordinator - HOPEKnowledgeManager)      │
└─────────────────────────────────────────────────────────┘
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                   ↓                   ↓
┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│  Permanent    │  │  Ordinary     │  │ High Frequency│
│  Layer        │  │  Layer        │  │  Layer        │
│               │  │               │  │               │
│  Core         │  │  General      │  │  Hot          │
│  Knowledge    │  │  Knowledge    │  │  Knowledge    │
│  Long-term    │  │  Regular Q&A  │  │  Frequent     │
│  Stable       │  │               │  │  Access       │
└───────────────┘  └───────────────┘  └───────────────┘
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                   ↓                   ↓
┌───────────────┐  ┌───────────────┐  ┌───────────────┐
│ Question      │  │  RAG Service  │  │  Statistics   │
│ Classifier    │  │  (Semantic    │  │  Module       │
│               │  │   Search)     │  │  LayerStats   │
└───────────────┘  └───────────────┘  └───────────────┘
                            ↓
        ┌───────────────────┼───────────────────┐
        ↓                                       ↓
┌───────────────────────────┐  ┌───────────────────────┐
│   Persistence Abstraction │  │   Knowledge Registry  │
│   (HopePersistence)       │  │                       │
│                           │  │                       │
│  - InMemory (default)     │  │  - Metadata mgmt      │
│  - KnowledgeRegistry      │  │  - Config storage     │
└───────────────────────────┘  └───────────────────────┘
```

---

## 📚 Three-Layer Knowledge Structure

### 2.1 Permanent Layer

**Characteristics:**
- 📌 **Long-term Stable** - Core knowledge, rarely changes
- 🎓 **Authoritative Reliable** - Verified knowledge
- 🔒 **Manually Managed** - Usually configured by administrators

**Use Cases:**
- System usage instructions
- Core concept definitions
- FAQ (official)
- Product feature descriptions

**Example Question Types:**
```yaml
question-types:
  - id: "system-core"
    name: "System Core Features"
    layer: "permanent"
    keywords:
      - "what is"
      - "core feature"
      - "design principle"
      - "architecture"
```

### 2.2 Ordinary Layer

**Characteristics:**
- 📝 **General Knowledge** - Regular business knowledge
- 🔄 **Dynamic Updates** - Updates as content grows
- 📊 **Medium Frequency** - Moderate access frequency

**Use Cases:**
- Business process descriptions
- Feature usage guides
- Regular technical documentation
- Development documentation

**Example Question Types:**
```yaml
question-types:
  - id: "usage-guide"
    name: "Usage Guide"
    layer: "ordinary"
    keywords:
      - "how to use"
      - "how to configure"
      - "operation steps"
```

### 2.3 High Frequency Layer

**Characteristics:**
- 🔥 **Hot Knowledge** - Frequently accessed by users
- ⚡ **Quick Response** - Priority retrieval
- 🔄 **Dynamic Adjustment** - Automatically adjusts based on access frequency

**Use Cases:**
- Recently frequently asked questions
- Popular feature descriptions
- Newly released feature introductions
- User feedback issues

**Dynamic Adjustment Mechanism:**
```java
// When question access count exceeds threshold, automatically promote to high-frequency layer
if (questionStats.getAccessCount() > HIGH_FREQUENCY_THRESHOLD) {
    moveToHighFrequencyLayer(question);
}
```

---

## 🔧 Core Components

### 3.1 HOPEKnowledgeManager (Knowledge Manager)

**Location:** `top.yumbo.ai.omni.hope.starter.impl.HOPEKnowledgeManager`

**Responsibilities:**
- Coordinate three-layer knowledge structure
- Execute intelligent queries
- Maintain layer statistics
- Dynamically adjust knowledge layers

**Core Methods:**

```java
@Service
public class HOPEKnowledgeManager {
    
    /**
     * Query knowledge
     * @param question User question
     * @param maxResults Maximum results
     * @return Query result
     */
    public QueryResult query(String question, int maxResults) {
        // 1. Classify question
        String questionType = questionClassifier.classify(question);
        String suggestedLayer = questionClassifier.getSuggestedLayer(questionType);
        
        // 2. Use RAG for semantic search
        List<Document> documents = ragService.semanticSearch(question, maxResults);
        
        // 3. Update statistics
        updateLayerStats(suggestedLayer);
        
        // 4. Build result
        return buildQueryResult(question, questionType, suggestedLayer, documents);
    }
    
    /**
     * Smart query (enhanced)
     * @param question User question
     * @param context Context information
     * @return Query result
     */
    public QueryResult smartQuery(String question, String context) {
        // Smarter query with context
    }
    
    /**
     * Get layer statistics
     */
    public Map<String, LayerStats> getLayerStats() {
        return layerStatsMap;
    }
}
```

**Query Result Model:**

```java
@Data
public class QueryResult {
    private String question;              // Original question
    private String questionType;          // Question type
    private String suggestedLayer;        // Suggested knowledge layer
    private List<Document> documents;     // Retrieved documents
    private String answer;                // Answer (optional)
    private Double confidence;            // Confidence
    private Long queryTimeMs;             // Query time
    private Boolean success;              // Success status
}
```

### 3.2 QuestionClassifier (Question Classifier)

**Location:** `top.yumbo.ai.omni.hope.starter.impl.QuestionClassifier`

**Responsibilities:**
- Analyze user questions
- Determine question type
- Suggest knowledge layer to use

**Classification Mechanism:**

```java
@Component
public class QuestionClassifier {
    
    /**
     * Classify question
     * @param question User question
     * @return Question type ID
     */
    public String classify(String question) {
        // 1. Keyword matching
        for (Map.Entry<String, List<String>> entry : keywordCache.entrySet()) {
            String typeId = entry.getKey();
            List<String> keywords = entry.getValue();
            
            for (String keyword : keywords) {
                if (question.contains(keyword)) {
                    return typeId;
                }
            }
        }
        
        // 2. Pattern matching (regex)
        for (Map.Entry<String, List<Pattern>> entry : patternCache.entrySet()) {
            String typeId = entry.getKey();
            List<Pattern> patterns = entry.getValue();
            
            for (Pattern pattern : patterns) {
                if (pattern.matcher(question).find()) {
                    return typeId;
                }
            }
        }
        
        // 3. Default type
        return "general";
    }
    
    /**
     * Get suggested knowledge layer
     * @param questionType Question type
     * @return Layer name
     */
    public String getSuggestedLayer(String questionType) {
        QuestionTypeConfig config = configCache.get(questionType);
        return config != null ? config.getSuggestedLayer() : "ordinary";
    }
}
```

**Question Type Configuration:**

```java
@Data
@Builder
public class QuestionTypeConfig {
    private String id;                  // Type ID
    private String name;                // Type name
    private String suggestedLayer;      // Suggested layer
    private List<String> keywords;      // Keyword list
    private List<String> patterns;      // Regex pattern list
    private Integer priority;           // Priority
    private Boolean enabled;            // Enabled status
}
```

### 3.3 LayerStats (Layer Statistics)

**Responsibilities:**
- Record access count per layer
- Track query time
- Support performance analysis

**Statistics Information:**

```java
@Data
public class LayerStats {
    private String layerName;           // Layer name
    private long queryCount;            // Query count
    private long totalQueryTimeMs;      // Total query time
    private double avgQueryTimeMs;      // Average query time
    private long lastAccessTime;        // Last access time
    
    public void incrementQueryCount() {
        this.queryCount++;
        this.lastAccessTime = System.currentTimeMillis();
    }
    
    public void addQueryTime(long timeMs) {
        this.totalQueryTimeMs += timeMs;
        this.avgQueryTimeMs = (double) totalQueryTimeMs / queryCount;
    }
}
```

---

## 🔄 Workflow

### 4.1 Knowledge Query Flow

```
User Question
    ↓
┌─────────────────────────────────────┐
│  1. Question Classifier Analysis    │
│     - Keyword matching              │
│     - Pattern matching              │
│     - Determine question type       │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  2. Determine Knowledge Layer       │
│     - Based on question type        │
│     - Get suggested layer           │
│     - permanent/ordinary/high_freq  │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  3. RAG Semantic Search             │
│     - Vector retrieval              │
│     - Similarity ranking            │
│     - Return Top-K documents        │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  4. Update Statistics               │
│     - Record access count           │
│     - Update query time             │
│     - Dynamically adjust layers     │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  5. Build Query Result              │
│     - Calculate confidence          │
│     - Assemble answer               │
│     - Return to user                │
└─────────────────────────────────────┘
```

### 4.2 Knowledge Learning Flow

```
New Knowledge Input
    ↓
┌─────────────────────────────────────┐
│  1. Knowledge Preprocessing         │
│     - Text cleaning                 │
│     - Format standardization        │
│     - Extract metadata              │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  2. Knowledge Classification        │
│     - Determine knowledge type      │
│     - Select target layer           │
│     - Set priority                  │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  3. Store to Persistent Layer       │
│     - Save to HopePersistence       │
│     - Update index                  │
│     - Sync to RAG system            │
└─────────────────────────────────────┘
    ↓
┌─────────────────────────────────────┐
│  4. Validation and Testing          │
│     - Test query effectiveness      │
│     - Verify accuracy               │
│     - Adjust configuration          │
└─────────────────────────────────────┘
```

---

## 💾 Persistence Mechanism

### 5.1 HopePersistence Interface

**Location:** `top.yumbo.ai.omni.hope.api.persistence.HopePersistence`

**Interface Definition:**

```java
public interface HopePersistence {
    
    // ========== Question Type Management ==========
    
    /**
     * Get all question types
     */
    List<QuestionTypeConfig> getAllQuestionTypes();
    
    /**
     * Save question type
     */
    void saveQuestionType(QuestionTypeConfig config);
    
    /**
     * Delete question type
     */
    void deleteQuestionType(String typeId);
    
    // ========== Keyword Management ==========
    
    /**
     * Get keywords for specified type
     */
    List<String> getKeywords(String typeId);
    
    /**
     * Save keywords
     */
    void saveKeywords(String typeId, List<String> keywords);
    
    // ========== Pattern Management ==========
    
    /**
     * Get regex patterns for specified type
     */
    List<String> getPatterns(String typeId);
    
    /**
     * Save regex patterns
     */
    void savePatterns(String typeId, List<String> patterns);
}
```

### 5.2 Implementations

#### 5.2.1 InMemoryHopePersistence (Default)

**Location:** `top.yumbo.ai.omni.hope.starter.persistence.InMemoryHopePersistence`

**Characteristics:**
- ✅ Zero dependencies, out-of-the-box
- ✅ Suitable for development and testing
- ❌ Data lost after restart

**Implementation:**
```java
@Service
@ConditionalOnMissingBean(HopePersistence.class)
public class InMemoryHopePersistence implements HopePersistence {
    
    private final Map<String, QuestionTypeConfig> typeCache = new ConcurrentHashMap<>();
    private final Map<String, List<String>> keywordCache = new ConcurrentHashMap<>();
    private final Map<String, List<String>> patternCache = new ConcurrentHashMap<>();
    
    @PostConstruct
    public void init() {
        // Load default configuration
        loadDefaultConfiguration();
    }
    
    private void loadDefaultConfiguration() {
        // Predefined question types
        saveQuestionType(QuestionTypeConfig.builder()
            .id("system-core")
            .name("System Core Features")
            .suggestedLayer("permanent")
            .keywords(Arrays.asList("what is", "core feature", "design principle"))
            .build());
        
        // ... More default configurations
    }
}
```

#### 5.2.2 KnowledgeRegistryHopePersistence (Recommended)

**Location:** `top.yumbo.ai.omni.hope.starter.persistence.KnowledgeRegistryHopePersistence`

**Characteristics:**
- ✅ Data persistence
- ✅ Supports multiple storage backends (File/Mongo/Redis)
- ✅ Suitable for production

**Implementation:**
```java
@Service
@ConditionalOnBean(KnowledgeRegistry.class)
public class KnowledgeRegistryHopePersistence implements HopePersistence {
    
    private final KnowledgeStorageService storageService;
    
    @Override
    public List<QuestionTypeConfig> getAllQuestionTypes() {
        // Read config from knowledge registry
        String configJson = storageService.load("hope", "question-types.json");
        return parseQuestionTypes(configJson);
    }
    
    @Override
    public void saveQuestionType(QuestionTypeConfig config) {
        // Save to knowledge registry
        List<QuestionTypeConfig> types = getAllQuestionTypes();
        types.add(config);
        String json = toJson(types);
        storageService.save("hope", "question-types.json", json);
    }
}
```

---

## ⚙️ Configuration

### 6.1 Spring Boot Configuration

```yaml
# application.yml

omni:
  hope:
    # Enable HOPE system
    enabled: true
    
    # Persistence implementation type
    # Options: memory, knowledge-registry
    persistence-type: knowledge-registry
    
    # Default knowledge layer
    default-layer: ordinary
    
    # High-frequency layer threshold (access count)
    high-frequency-threshold: 100
    
    # Layer weight configuration
    layer-weights:
      permanent: 1.5      # Permanent layer weight
      ordinary: 1.0       # Ordinary layer weight
      high-frequency: 2.0 # High-frequency layer weight
    
    # Question classifier configuration
    classifier:
      # Enable cache
      enable-cache: true
      
      # Cache TTL (seconds)
      cache-ttl: 3600
```

### 6.2 Question Type Configuration (JSON)

```json
{
  "question-types": [
    {
      "id": "system-core",
      "name": "System Core Features",
      "suggestedLayer": "permanent",
      "keywords": ["what is", "core feature", "design principle", "architecture"],
      "patterns": ["^what is.*", "^.*what does.*do"],
      "priority": 100,
      "enabled": true
    },
    {
      "id": "usage-guide",
      "name": "Usage Guide",
      "suggestedLayer": "ordinary",
      "keywords": ["how to use", "how to configure", "operation steps"],
      "patterns": ["^how to.*", "^how do I.*"],
      "priority": 50,
      "enabled": true
    },
    {
      "id": "troubleshooting",
      "name": "Troubleshooting",
      "suggestedLayer": "high_frequency",
      "keywords": ["error", "exception", "failure", "not working"],
      "patterns": ["^why.*failed", "^.*error.*"],
      "priority": 80,
      "enabled": true
    }
  ]
}
```

### 6.3 Auto Configuration

**Location:** `top.yumbo.ai.omni.hope.starter.config.HopePersistenceAutoConfiguration`

```java
@Configuration
@ConditionalOnProperty(name = "omni.hope.enabled", havingValue = "true", matchIfMissing = true)
public class HopePersistenceAutoConfiguration {
    
    @Bean
    @ConditionalOnMissingBean
    public HopePersistence hopePersistence(
            @Autowired(required = false) KnowledgeRegistry knowledgeRegistry) {
        
        if (knowledgeRegistry != null) {
            log.info("✅ Using KnowledgeRegistryHopePersistence");
            return new KnowledgeRegistryHopePersistence(knowledgeRegistry);
        } else {
            log.info("✅ Using InMemoryHopePersistence (default)");
            return new InMemoryHopePersistence();
        }
    }
    
    @Bean
    public QuestionClassifier questionClassifier(HopePersistence persistence) {
        return new QuestionClassifier(persistence);
    }
    
    @Bean
    public HOPEKnowledgeManager hopeKnowledgeManager(
            QuestionClassifier classifier,
            RagService ragService) {
        return new HOPEKnowledgeManager(classifier, ragService);
    }
}
```

---

## 📖 API Reference

### 7.1 Query API

```java
@RestController
@RequestMapping("/api/hope")
public class HopeController {
    
    @Autowired
    private HOPEKnowledgeManager hopeManager;
    
    /**
     * Query knowledge
     * POST /api/hope/query
     */
    @PostMapping("/query")
    public QueryResult query(@RequestBody QueryRequest request) {
        return hopeManager.query(
            request.getQuestion(),
            request.getMaxResults()
        );
    }
    
    /**
     * Smart query (with context)
     * POST /api/hope/smart-query
     */
    @PostMapping("/smart-query")
    public QueryResult smartQuery(@RequestBody SmartQueryRequest request) {
        return hopeManager.smartQuery(
            request.getQuestion(),
            request.getContext()
        );
    }
    
    /**
     * Get layer statistics
     * GET /api/hope/stats
     */
    @GetMapping("/stats")
    public Map<String, LayerStats> getStats() {
        return hopeManager.getLayerStats();
    }
}
```

### 7.2 Admin API

```java
@RestController
@RequestMapping("/api/hope/admin")
public class HopeAdminController {
    
    @Autowired
    private QuestionClassifier classifier;
    
    @Autowired
    private HopePersistence persistence;
    
    /**
     * Add question type
     * POST /api/hope/admin/question-types
     */
    @PostMapping("/question-types")
    public void addQuestionType(@RequestBody QuestionTypeConfig config) {
        persistence.saveQuestionType(config);
        classifier.reload();  // Reload configuration
    }
    
    /**
     * Add keywords
     * POST /api/hope/admin/keywords
     */
    @PostMapping("/keywords")
    public void addKeywords(
            @RequestParam String typeId,
            @RequestBody List<String> keywords) {
        persistence.saveKeywords(typeId, keywords);
        classifier.reload();
    }
}
```

---

## 💡 Best Practices

### 8.1 Question Type Design

**Principles:**
1. **From coarse to fine** - Define major categories first, then subdivide
2. **Mutually exclusive keywords** - Avoid keyword overlap causing misclassification
3. **Clear priorities** - Set higher priorities for important question types

**Example:**
```json
{
  "question-types": [
    {
      "id": "security",
      "name": "Security Related",
      "suggestedLayer": "permanent",
      "keywords": ["security", "vulnerability", "CVE", "permission"],
      "priority": 100
    },
    {
      "id": "performance",
      "name": "Performance Optimization",
      "suggestedLayer": "ordinary",
      "keywords": ["performance", "slow", "optimization", "lag"],
      "priority": 80
    }
  ]
}
```

### 8.2 Layer Selection Strategy

| Question Type | Suggested Layer | Reason |
|--------------|----------------|--------|
| System core concepts | permanent | Long-term stable, rarely changes |
| API documentation | ordinary | May update, but moderate access frequency |
| Common errors | high_frequency | Users frequently encounter |
| New feature descriptions | high_frequency | High-frequency access in short term |

### 8.3 Performance Optimization Tips

1. **Enable cache**
```yaml
omni:
  hope:
    classifier:
      enable-cache: true
      cache-ttl: 3600
```

2. **Pre-compile regex patterns**
```java
// QuestionClassifier automatically pre-compiles during initialization
private final Map<String, List<Pattern>> patternCache = new ConcurrentHashMap<>();
```

3. **Async statistics updates**
```java
@Async
public void updateStatsAsync(String layer, long queryTime) {
    LayerStats stats = layerStatsMap.get(layer);
    stats.incrementQueryCount();
    stats.addQueryTime(queryTime);
}
```

---

## 🔗 Relationship with Knowledge Network

### 9.1 Collaborative Working Mode

```
┌─────────────────────────────────────────────────────────────┐
│                      User Query                              │
└─────────────────────────────────────────────────────────────┘
                            ↓
        ┌───────────────────┴───────────────────┐
        ↓                                       ↓
┌───────────────────┐                  ┌───────────────────┐
│   HOPE System     │                  │ Knowledge Network │
│                   │                  │                   │
│  - Question       │                  │  - Domain mgmt    │
│    classification │ ←─Collaborate──→ │  - Knowledge      │
│  - Layer          │                  │    extraction     │
│    selection      │                  │  - Cross-domain   │
│  - Stats mgmt     │                  │    query          │
└───────────────────┘                  └───────────────────┘
        ↓                                       ↓
        └───────────────────┬───────────────────┘
                            ↓
        ┌─────────────────────────────────────┐
        │         RAG Service                 │
        │      (Unified Retrieval Interface)  │
        └─────────────────────────────────────┘
```

### 9.2 Data Flow

```
1. Doc Upload → Knowledge Network → Domain Storage → RAG Index
                            ↓
2. User Query → HOPE System → Question Classification → Determine Layer
                            ↓
3. RAG Retrieval ← HOPE System ← Domain ← Knowledge Network
                            ↓
4. Return Result → Update Stats → HOPE System → User
```

### 9.3 Complementary Relationship

| System | Responsibilities | Focus |
|--------|-----------------|-------|
| **HOPE System** | Knowledge query and layered management | How to quickly find correct knowledge |
| **Knowledge Network** | Knowledge organization and association | How knowledge is stored and associated |
| **RAG System** | Vector retrieval and ranking | How to calculate similarity |

**Key Points:**
- HOPE System is **not responsible** for knowledge storage, only intelligent retrieval
- Knowledge Network is **not responsible** for query optimization, only knowledge organization
- Both collaborate through RAG Service

---

## 📊 Monitoring and Diagnostics

### 10.1 Statistics

```bash
# Get layer statistics
GET /api/hope/stats

# Response example
{
  "permanent": {
    "layerName": "permanent",
    "queryCount": 1520,
    "totalQueryTimeMs": 45600,
    "avgQueryTimeMs": 30.0,
    "lastAccessTime": 1735660800000
  },
  "ordinary": {
    "layerName": "ordinary",
    "queryCount": 8940,
    "totalQueryTimeMs": 267000,
    "avgQueryTimeMs": 29.9,
    "lastAccessTime": 1735660800000
  },
  "high_frequency": {
    "layerName": "high_frequency",
    "queryCount": 12350,
    "totalQueryTimeMs": 246000,
    "avgQueryTimeMs": 19.9,
    "lastAccessTime": 1735660800000
  }
}
```

### 10.2 Logging Configuration

```yaml
logging:
  level:
    top.yumbo.ai.omni.core.hope: DEBUG
```

**Key Logs:**
```
🎯 Question classified as: usage-guide (suggested layer: ordinary)
✅ Query completed in 25ms, found 5 documents, confidence: 0.85
📊 Layer statistics: permanent=1520, ordinary=8940, high_frequency=12350
```

---

## 🚀 Quick Start

### 11.1 Basic Usage

```java
@Service
public class MyService {
    
    @Autowired
    private HOPEKnowledgeManager hopeManager;
    
    public void example() {
        // Query knowledge
        QueryResult result = hopeManager.query("What is knowledge network?", 5);
        
        System.out.println("Question type: " + result.getQuestionType());
        System.out.println("Suggested layer: " + result.getSuggestedLayer());
        System.out.println("Confidence: " + result.getConfidence());
        System.out.println("Documents found: " + result.getDocuments().size());
    }
}
```

### 11.2 Custom Question Types

```java
@Service
public class CustomConfiguration {
    
    @Autowired
    private HopePersistence persistence;
    
    @PostConstruct
    public void init() {
        // Add custom question type
        QuestionTypeConfig config = QuestionTypeConfig.builder()
            .id("custom-type")
            .name("Custom Type")
            .suggestedLayer("ordinary")
            .keywords(Arrays.asList("custom", "special"))
            .priority(70)
            .enabled(true)
            .build();
        
        persistence.saveQuestionType(config);
    }
}
```

---

## 📚 Related Documentation

- [Knowledge Network Architecture](./KNOWLEDGE_NETWORK_ARCHITECTURE_EN.md)
- [Intelligent Q&A System Design](./INTELLIGENT_QA_SYSTEM_DESIGN_EN.md)
- [RAG Architecture Design](./KNOWLEDGE_NETWORK_AND_RAG_ARCHITECTURE_EN.md)
- [Batch 1 Analysis Report](../../analysis/BATCH_01_CORE_MODULES_ANALYSIS_EN.md)

---

## ✅ Summary

### Core Values

1. **Intelligent Layering** - Automatically select optimal knowledge layer based on question type
2. **Efficient Retrieval** - Prioritize high-frequency knowledge retrieval to improve response speed
3. **Persistence** - Long-term preservation of core knowledge to avoid repetitive learning
4. **Extensibility** - Support custom question types and persistence backends

### Design Highlights

- ✅ Unique three-layer knowledge structure design
- ✅ Flexible and configurable question classifier
- ✅ Persistence abstraction supports multiple backends
- ✅ Perfect collaboration with knowledge network

### Future Outlook

1. **Machine Learning Enhancement** - Use ML models to automatically learn question classification
2. **Dynamic Adjustment** - Automatically adjust layers based on access patterns
3. **Multi-language Support** - Support multi-language question classification
4. **Visual Management** - Provide Web UI for managing question types

---

**Documentation Version:** 1.0.0  
**Last Updated:** 2025-12-31  
**Maintainer:** OmniAgent Team

