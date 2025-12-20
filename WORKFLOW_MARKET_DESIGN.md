# 🏪 工作流市场和持久化方案

## 📋 设计目标

1. ✅ **工作流市场** - 类似 GitHub Marketplace
2. ✅ **用户创建和分享** - 发布、下载、收藏
3. ✅ **灵活持久化** - 支持多种存储后端
4. ✅ **社区互动** - 评分、评论、点赞
5. ✅ **版本管理** - 工作流版本控制

---

## 🏗️ 架构设计

### 三层架构

```
┌─────────────────────────────────────────────────────────────┐
│                    应用层                                      │
├─────────────────────────────────────────────────────────────┤
│  工作流市场 UI  │  工作流编辑器  │  我的工作流  │  市场浏览    │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    服务层 ⭐                                  │
├─────────────────────────────────────────────────────────────┤
│  WorkflowMarketService    │  WorkflowStorageService         │
│  - 发布工作流              │  - CRUD 操作                     │
│  - 搜索/浏览              │  - 版本管理                      │
│  - 下载/安装              │  - 查询                          │
│  - 评分/评论              │                                  │
└─────────────────────────────────────────────────────────────┘
                              ↓
┌─────────────────────────────────────────────────────────────┐
│                    持久化层（可插拔）⭐                        │
├─────────────────────────────────────────────────────────────┤
│  WorkflowRepository (接口)                                   │
│      ↓              ↓              ↓              ↓          │
│  FileRepository  SQLiteRepo    MongoRepo      ESRepo        │
└─────────────────────────────────────────────────────────────┘
```

---

## 📦 数据模型

### 1. MarketWorkflow（市场工作流）

```java
@Data
@Builder
public class MarketWorkflow {
    // 基本信息
    private String id;                    // 唯一ID
    private String name;                  // 工作流名称
    private String version;               // 版本号
    private String description;           // 描述
    private String category;              // 分类
    private List<String> tags;            // 标签
    
    // 作者信息
    private String authorId;              // 作者ID
    private String authorName;            // 作者名称
    
    // 工作流定义
    private Workflow workflowDefinition;  // 工作流定义
    
    // 市场信息
    private String status;                // draft/published/deprecated
    private boolean isPublic;             // 是否公开
    private String license;               // 许可证（MIT/Apache/GPL）
    
    // 统计信息
    private long downloadCount;           // 下载次数
    private long installCount;            // 安装次数
    private long favoriteCount;           // 收藏次数
    private double rating;                // 平均评分（0-5）
    private long ratingCount;             // 评分人数
    
    // 时间信息
    private Long createdAt;
    private Long updatedAt;
    private Long publishedAt;
    
    // 元数据
    private Map<String, Object> metadata;
    
    // 依赖关系
    private List<String> dependencies;    // 依赖的其他工作流
    private List<String> requiredAgents;  // 需要的 Agent
}
```

### 2. WorkflowRating（评分）

```java
@Data
@Builder
public class WorkflowRating {
    private String id;
    private String workflowId;
    private String userId;
    private String userName;
    private int rating;                   // 1-5 星
    private String comment;               // 评论
    private Long createdAt;
}
```

### 3. WorkflowInstallation（安装记录）

```java
@Data
@Builder
public class WorkflowInstallation {
    private String id;
    private String workflowId;
    private String workflowVersion;
    private String userId;
    private Long installedAt;
    private boolean enabled;              // 是否启用
}
```

---

## 🔌 持久化接口设计

### WorkflowRepository 接口

```java
package top.yumbo.ai.omni.workflow.repository;

import java.util.List;
import java.util.Optional;

/**
 * 工作流存储接口（可插拔）
 * 
 * 支持多种存储后端：File, SQLite, MongoDB, Elasticsearch
 */
public interface WorkflowRepository {
    
    // ========== 基础 CRUD ==========
    
    /**
     * 保存工作流
     */
    String save(MarketWorkflow workflow);
    
    /**
     * 更新工作流
     */
    boolean update(MarketWorkflow workflow);
    
    /**
     * 删除工作流
     */
    boolean delete(String workflowId);
    
    /**
     * 根据ID查询
     */
    Optional<MarketWorkflow> findById(String workflowId);
    
    /**
     * 根据名称和版本查询
     */
    Optional<MarketWorkflow> findByNameAndVersion(String name, String version);
    
    /**
     * 查询所有版本
     */
    List<MarketWorkflow> findAllVersions(String name);
    
    /**
     * 查询最新版本
     */
    Optional<MarketWorkflow> findLatestVersion(String name);
    
    // ========== 市场查询 ==========
    
    /**
     * 查询所有公开工作流
     */
    List<MarketWorkflow> findPublic(int page, int size);
    
    /**
     * 按分类查询
     */
    List<MarketWorkflow> findByCategory(String category, int page, int size);
    
    /**
     * 按标签查询
     */
    List<MarketWorkflow> findByTag(String tag, int page, int size);
    
    /**
     * 按作者查询
     */
    List<MarketWorkflow> findByAuthor(String authorId, int page, int size);
    
    /**
     * 搜索（名称、描述、标签）
     */
    List<MarketWorkflow> search(String keyword, int page, int size);
    
    /**
     * 热门工作流（按下载量排序）
     */
    List<MarketWorkflow> findPopular(int limit);
    
    /**
     * 最新工作流
     */
    List<MarketWorkflow> findRecent(int limit);
    
    /**
     * 高评分工作流
     */
    List<MarketWorkflow> findTopRated(int limit);
    
    // ========== 统计更新 ==========
    
    /**
     * 增加下载次数
     */
    void incrementDownloadCount(String workflowId);
    
    /**
     * 增加安装次数
     */
    void incrementInstallCount(String workflowId);
    
    /**
     * 增加收藏次数
     */
    void incrementFavoriteCount(String workflowId);
    
    /**
     * 更新评分
     */
    void updateRating(String workflowId, double rating, long ratingCount);
    
    // ========== 评分和评论 ==========
    
    /**
     * 保存评分
     */
    String saveRating(WorkflowRating rating);
    
    /**
     * 查询工作流的评分
     */
    List<WorkflowRating> findRatings(String workflowId, int page, int size);
    
    /**
     * 查询用户的评分
     */
    Optional<WorkflowRating> findUserRating(String workflowId, String userId);
    
    // ========== 安装记录 ==========
    
    /**
     * 保存安装记录
     */
    String saveInstallation(WorkflowInstallation installation);
    
    /**
     * 查询用户已安装的工作流
     */
    List<WorkflowInstallation> findUserInstallations(String userId);
    
    /**
     * 检查是否已安装
     */
    boolean isInstalled(String workflowId, String userId);
}
```

---

## 🔧 具体实现

### 1. SQLite 实现

```java
package top.yumbo.ai.omni.workflow.repository.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;

@Slf4j
public class SQLiteWorkflowRepository implements WorkflowRepository {
    
    private final JdbcTemplate jdbcTemplate;
    private final ObjectMapper objectMapper;
    
    // 表结构
    private static final String CREATE_TABLES = """
        CREATE TABLE IF NOT EXISTS market_workflows (
            id TEXT PRIMARY KEY,
            name TEXT NOT NULL,
            version TEXT NOT NULL,
            description TEXT,
            category TEXT,
            tags TEXT,
            author_id TEXT,
            author_name TEXT,
            workflow_definition TEXT,
            status TEXT,
            is_public INTEGER,
            license TEXT,
            download_count INTEGER DEFAULT 0,
            install_count INTEGER DEFAULT 0,
            favorite_count INTEGER DEFAULT 0,
            rating REAL DEFAULT 0,
            rating_count INTEGER DEFAULT 0,
            created_at INTEGER,
            updated_at INTEGER,
            published_at INTEGER,
            metadata TEXT,
            UNIQUE(name, version)
        );
        
        CREATE INDEX IF NOT EXISTS idx_category ON market_workflows(category);
        CREATE INDEX IF NOT EXISTS idx_author ON market_workflows(author_id);
        CREATE INDEX IF NOT EXISTS idx_status ON market_workflows(status);
        CREATE INDEX IF NOT EXISTS idx_public ON market_workflows(is_public);
        
        CREATE TABLE IF NOT EXISTS workflow_ratings (
            id TEXT PRIMARY KEY,
            workflow_id TEXT NOT NULL,
            user_id TEXT NOT NULL,
            user_name TEXT,
            rating INTEGER,
            comment TEXT,
            created_at INTEGER,
            FOREIGN KEY(workflow_id) REFERENCES market_workflows(id),
            UNIQUE(workflow_id, user_id)
        );
        
        CREATE TABLE IF NOT EXISTS workflow_installations (
            id TEXT PRIMARY KEY,
            workflow_id TEXT NOT NULL,
            workflow_version TEXT,
            user_id TEXT NOT NULL,
            installed_at INTEGER,
            enabled INTEGER,
            FOREIGN KEY(workflow_id) REFERENCES market_workflows(id),
            UNIQUE(workflow_id, user_id)
        );
    """;
    
    public SQLiteWorkflowRepository(JdbcTemplate jdbcTemplate, ObjectMapper objectMapper) {
        this.jdbcTemplate = jdbcTemplate;
        this.objectMapper = objectMapper;
        initDatabase();
    }
    
    private void initDatabase() {
        jdbcTemplate.execute(CREATE_TABLES);
        log.info("✅ SQLite 工作流表初始化完成");
    }
    
    @Override
    public String save(MarketWorkflow workflow) {
        String sql = """
            INSERT INTO market_workflows (
                id, name, version, description, category, tags,
                author_id, author_name, workflow_definition, status,
                is_public, license, download_count, install_count,
                favorite_count, rating, rating_count, created_at,
                updated_at, published_at, metadata
            ) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """;
        
        try {
            jdbcTemplate.update(sql,
                workflow.getId(),
                workflow.getName(),
                workflow.getVersion(),
                workflow.getDescription(),
                workflow.getCategory(),
                toJson(workflow.getTags()),
                workflow.getAuthorId(),
                workflow.getAuthorName(),
                toJson(workflow.getWorkflowDefinition()),
                workflow.getStatus(),
                workflow.isPublic() ? 1 : 0,
                workflow.getLicense(),
                workflow.getDownloadCount(),
                workflow.getInstallCount(),
                workflow.getFavoriteCount(),
                workflow.getRating(),
                workflow.getRatingCount(),
                workflow.getCreatedAt(),
                workflow.getUpdatedAt(),
                workflow.getPublishedAt(),
                toJson(workflow.getMetadata())
            );
            
            return workflow.getId();
        } catch (Exception e) {
            log.error("保存工作流失败", e);
            return null;
        }
    }
    
    @Override
    public List<MarketWorkflow> findPublic(int page, int size) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 AND status = 'published'
            ORDER BY created_at DESC
            LIMIT ? OFFSET ?
        """;
        
        return jdbcTemplate.query(sql, workflowRowMapper(), size, page * size);
    }
    
    @Override
    public List<MarketWorkflow> search(String keyword, int page, int size) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 
            AND status = 'published'
            AND (name LIKE ? OR description LIKE ? OR tags LIKE ?)
            ORDER BY download_count DESC
            LIMIT ? OFFSET ?
        """;
        
        String pattern = "%" + keyword + "%";
        return jdbcTemplate.query(sql, workflowRowMapper(), 
                pattern, pattern, pattern, size, page * size);
    }
    
    @Override
    public List<MarketWorkflow> findPopular(int limit) {
        String sql = """
            SELECT * FROM market_workflows 
            WHERE is_public = 1 AND status = 'published'
            ORDER BY download_count DESC, rating DESC
            LIMIT ?
        """;
        
        return jdbcTemplate.query(sql, workflowRowMapper(), limit);
    }
    
    private RowMapper<MarketWorkflow> workflowRowMapper() {
        return (rs, rowNum) -> {
            try {
                return MarketWorkflow.builder()
                    .id(rs.getString("id"))
                    .name(rs.getString("name"))
                    .version(rs.getString("version"))
                    .description(rs.getString("description"))
                    .category(rs.getString("category"))
                    .tags(fromJson(rs.getString("tags"), List.class))
                    .authorId(rs.getString("author_id"))
                    .authorName(rs.getString("author_name"))
                    .workflowDefinition(fromJson(rs.getString("workflow_definition"), Workflow.class))
                    .status(rs.getString("status"))
                    .isPublic(rs.getInt("is_public") == 1)
                    .license(rs.getString("license"))
                    .downloadCount(rs.getLong("download_count"))
                    .installCount(rs.getLong("install_count"))
                    .favoriteCount(rs.getLong("favorite_count"))
                    .rating(rs.getDouble("rating"))
                    .ratingCount(rs.getLong("rating_count"))
                    .createdAt(rs.getLong("created_at"))
                    .updatedAt(rs.getLong("updated_at"))
                    .publishedAt(rs.getLong("published_at"))
                    .metadata(fromJson(rs.getString("metadata"), Map.class))
                    .build();
            } catch (Exception e) {
                throw new RuntimeException("解析工作流失败", e);
            }
        };
    }
    
    private String toJson(Object obj) {
        try {
            return objectMapper.writeValueAsString(obj);
        } catch (Exception e) {
            return null;
        }
    }
    
    private <T> T fromJson(String json, Class<T> type) {
        try {
            return objectMapper.readValue(json, type);
        } catch (Exception e) {
            return null;
        }
    }
}
```

### 2. MongoDB 实现

```java
package top.yumbo.ai.omni.workflow.repository.impl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

@Slf4j
public class MongoWorkflowRepository implements WorkflowRepository {
    
    private final MongoTemplate mongoTemplate;
    private static final String COLLECTION = "market_workflows";
    
    public MongoWorkflowRepository(MongoTemplate mongoTemplate) {
        this.mongoTemplate = mongoTemplate;
    }
    
    @Override
    public String save(MarketWorkflow workflow) {
        mongoTemplate.save(workflow, COLLECTION);
        return workflow.getId();
    }
    
    @Override
    public Optional<MarketWorkflow> findById(String workflowId) {
        MarketWorkflow workflow = mongoTemplate.findById(workflowId, MarketWorkflow.class, COLLECTION);
        return Optional.ofNullable(workflow);
    }
    
    @Override
    public List<MarketWorkflow> search(String keyword, int page, int size) {
        Query query = new Query();
        query.addCriteria(
            Criteria.where("isPublic").is(true)
                .and("status").is("published")
                .orOperator(
                    Criteria.where("name").regex(keyword, "i"),
                    Criteria.where("description").regex(keyword, "i"),
                    Criteria.where("tags").in(keyword)
                )
        );
        query.skip(page * size).limit(size);
        
        return mongoTemplate.find(query, MarketWorkflow.class, COLLECTION);
    }
    
    @Override
    public List<MarketWorkflow> findPopular(int limit) {
        Query query = new Query();
        query.addCriteria(
            Criteria.where("isPublic").is(true)
                .and("status").is("published")
        );
        query.limit(limit);
        query.with(Sort.by(Sort.Direction.DESC, "downloadCount", "rating"));
        
        return mongoTemplate.find(query, MarketWorkflow.class, COLLECTION);
    }
    
    @Override
    public void incrementDownloadCount(String workflowId) {
        Query query = new Query(Criteria.where("id").is(workflowId));
        Update update = new Update().inc("downloadCount", 1);
        mongoTemplate.updateFirst(query, update, COLLECTION);
    }
}
```

### 3. Elasticsearch 实现

```java
package top.yumbo.ai.omni.workflow.repository.impl;

import co.elastic.clients.elasticsearch.ElasticsearchClient;
import co.elastic.clients.elasticsearch.core.*;
import co.elastic.clients.elasticsearch.core.search.Hit;

@Slf4j
public class ElasticsearchWorkflowRepository implements WorkflowRepository {
    
    private final ElasticsearchClient esClient;
    private static final String INDEX = "market-workflows";
    
    @Override
    public String save(MarketWorkflow workflow) {
        try {
            IndexResponse response = esClient.index(i -> i
                .index(INDEX)
                .id(workflow.getId())
                .document(workflow)
            );
            return response.id();
        } catch (Exception e) {
            log.error("保存到 ES 失败", e);
            return null;
        }
    }
    
    @Override
    public List<MarketWorkflow> search(String keyword, int page, int size) {
        try {
            SearchResponse<MarketWorkflow> response = esClient.search(s -> s
                .index(INDEX)
                .query(q -> q
                    .bool(b -> b
                        .must(m -> m.term(t -> t.field("isPublic").value(true)))
                        .must(m -> m.term(t -> t.field("status").value("published")))
                        .should(sh -> sh
                            .multiMatch(mm -> mm
                                .query(keyword)
                                .fields("name^3", "description^2", "tags")
                            )
                        )
                    )
                )
                .from(page * size)
                .size(size)
                .sort(so -> so.field(f -> f.field("downloadCount").order(SortOrder.Desc)))
                , MarketWorkflow.class
            );
            
            return response.hits().hits().stream()
                .map(Hit::source)
                .collect(Collectors.toList());
                
        } catch (Exception e) {
            log.error("ES 搜索失败", e);
            return Collections.emptyList();
        }
    }
}
```

---

## 🎨 WorkflowMarketService

```java
package top.yumbo.ai.omni.workflow.market;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class WorkflowMarketService {
    
    @Autowired
    private WorkflowRepository workflowRepository;
    
    @Autowired
    private WorkflowRegistry workflowRegistry;
    
    /**
     * 发布工作流到市场
     */
    public String publishWorkflow(Workflow workflow, String authorId, String authorName) {
        MarketWorkflow marketWorkflow = MarketWorkflow.builder()
            .id(UUID.randomUUID().toString())
            .name(workflow.getName())
            .version(workflow.getVersion())
            .description(workflow.getDescription())
            .authorId(authorId)
            .authorName(authorName)
            .workflowDefinition(workflow)
            .status("published")
            .isPublic(true)
            .license("MIT")
            .createdAt(System.currentTimeMillis())
            .updatedAt(System.currentTimeMillis())
            .publishedAt(System.currentTimeMillis())
            .build();
        
        String id = workflowRepository.save(marketWorkflow);
        log.info("✅ 工作流已发布到市场: name={}, id={}", workflow.getName(), id);
        return id;
    }
    
    /**
     * 从市场下载工作流
     */
    public Workflow downloadWorkflow(String workflowId, String userId) {
        Optional<MarketWorkflow> opt = workflowRepository.findById(workflowId);
        if (opt.isEmpty()) {
            throw new RuntimeException("工作流不存在");
        }
        
        MarketWorkflow marketWorkflow = opt.get();
        
        // 增加下载次数
        workflowRepository.incrementDownloadCount(workflowId);
        
        log.info("⬇️ 用户下载工作流: user={}, workflow={}", userId, marketWorkflow.getName());
        
        return marketWorkflow.getWorkflowDefinition();
    }
    
    /**
     * 安装工作流到本地
     */
    public boolean installWorkflow(String workflowId, String userId) {
        Workflow workflow = downloadWorkflow(workflowId, userId);
        
        // 注册到本地
        workflowRegistry.register(workflow);
        
        // 记录安装
        WorkflowInstallation installation = WorkflowInstallation.builder()
            .id(UUID.randomUUID().toString())
            .workflowId(workflowId)
            .workflowVersion(workflow.getVersion())
            .userId(userId)
            .installedAt(System.currentTimeMillis())
            .enabled(true)
            .build();
        
        workflowRepository.saveInstallation(installation);
        workflowRepository.incrementInstallCount(workflowId);
        
        log.info("✅ 工作流已安装: user={}, workflow={}", userId, workflow.getName());
        return true;
    }
    
    /**
     * 搜索工作流
     */
    public List<MarketWorkflow> searchWorkflows(String keyword, int page, int size) {
        return workflowRepository.search(keyword, page, size);
    }
    
    /**
     * 获取热门工作流
     */
    public List<MarketWorkflow> getPopularWorkflows(int limit) {
        return workflowRepository.findPopular(limit);
    }
    
    /**
     * 评分
     */
    public boolean rateWorkflow(String workflowId, String userId, String userName, 
                                int rating, String comment) {
        WorkflowRating workflowRating = WorkflowRating.builder()
            .id(UUID.randomUUID().toString())
            .workflowId(workflowId)
            .userId(userId)
            .userName(userName)
            .rating(rating)
            .comment(comment)
            .createdAt(System.currentTimeMillis())
            .build();
        
        workflowRepository.saveRating(workflowRating);
        
        // 重新计算平均分
        List<WorkflowRating> allRatings = workflowRepository.findRatings(workflowId, 0, Integer.MAX_VALUE);
        double avgRating = allRatings.stream()
            .mapToInt(WorkflowRating::getRating)
            .average()
            .orElse(0.0);
        
        workflowRepository.updateRating(workflowId, avgRating, allRatings.size());
        
        log.info("⭐ 用户评分: user={}, workflow={}, rating={}", 
                 userId, workflowId, rating);
        return true;
    }
}
```

---

## 📊 配置管理

```yaml
# application.yml
omni-agent:
  workflow:
    # 存储类型: file | sqlite | mongodb | elasticsearch
    storage-type: sqlite
    
    # SQLite 配置
    sqlite:
      db-path: ./data/workflows/workflows.db
    
    # MongoDB 配置
    mongodb:
      uri: mongodb://localhost:27017
      database: omniagent
      collection: workflows
    
    # Elasticsearch 配置
    elasticsearch:
      uris: http://localhost:9200
      index: market-workflows
    
    # 市场配置
    market:
      enabled: true
      page-size: 20
      max-file-size: 10485760  # 10MB
```

---

## 🎨 REST API

```java
@RestController
@RequestMapping("/api/workflows/market")
public class WorkflowMarketController {
    
    @Autowired
    private WorkflowMarketService marketService;
    
    /**
     * 发布工作流
     */
    @PostMapping("/publish")
    public Map<String, Object> publishWorkflow(
            @RequestBody Workflow workflow,
            @RequestHeader("X-User-Id") String userId,
            @RequestHeader("X-User-Name") String userName) {
        
        String id = marketService.publishWorkflow(workflow, userId, userName);
        return Map.of("success", true, "id", id);
    }
    
    /**
     * 搜索工作流
     */
    @GetMapping("/search")
    public List<MarketWorkflow> search(
            @RequestParam String keyword,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "20") int size) {
        
        return marketService.searchWorkflows(keyword, page, size);
    }
    
    /**
     * 热门工作流
     */
    @GetMapping("/popular")
    public List<MarketWorkflow> popular(@RequestParam(defaultValue = "10") int limit) {
        return marketService.getPopularWorkflows(limit);
    }
    
    /**
     * 下载工作流
     */
    @GetMapping("/{workflowId}/download")
    public Workflow download(
            @PathVariable String workflowId,
            @RequestHeader("X-User-Id") String userId) {
        
        return marketService.downloadWorkflow(workflowId, userId);
    }
    
    /**
     * 安装工作流
     */
    @PostMapping("/{workflowId}/install")
    public Map<String, Object> install(
            @PathVariable String workflowId,
            @RequestHeader("X-User-Id") String userId) {
        
        boolean success = marketService.installWorkflow(workflowId, userId);
        return Map.of("success", success);
    }
    
    /**
     * 评分
     */
    @PostMapping("/{workflowId}/rate")
    public Map<String, Object> rate(
            @PathVariable String workflowId,
            @RequestHeader("X-User-Id") String userId,
            @RequestHeader("X-User-Name") String userName,
            @RequestBody RatingRequest request) {
        
        boolean success = marketService.rateWorkflow(
            workflowId, userId, userName, request.getRating(), request.getComment()
        );
        return Map.of("success", success);
    }
}
```

---

## 🎉 总结

### 核心特性

1. ✅ **工作流市场** - 发布、搜索、下载、安装
2. ✅ **灵活持久化** - 支持 SQLite/MongoDB/ES/File
3. ✅ **社区互动** - 评分、评论、统计
4. ✅ **版本管理** - 多版本支持
5. ✅ **可插拔设计** - 易于扩展新的存储后端

### 实施路径

1. **Phase 1** ✅ - 基础工作流引擎（已完成）
2. **Phase 2** - 工作流市场和持久化（当前）
   - 持久化接口设计
   - SQLite 实现
   - 市场服务
   - REST API

3. **Phase 3** - 前端UI
   - 市场浏览页面
   - 工作流详情页面
   - 安装管理页面

**工作流系统将具备完整的生态能力！** 🚀

