package top.yumbo.ai.omni.rag.adapter.impl.file;

import lombok.extern.slf4j.Slf4j;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StoredField;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.*;
import org.apache.lucene.queryparser.classic.MultiFieldQueryParser;
import org.apache.lucene.queryparser.classic.ParseException;
import org.apache.lucene.search.*;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;
import top.yumbo.ai.omni.rag.model.SearchResult;
import top.yumbo.ai.omni.rag.model.IndexStatistics;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

/**
 * 基于 Lucene 的文件 RAG 实现
 * (Lucene-based File RAG Implementation)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class LuceneRAGService implements RagService {

    private final FileRAGProperties properties;
    private final String domainId;
    private Directory directory;
    private Analyzer analyzer;
    private IndexWriter indexWriter;
    private SearcherManager searcherManager;

    public LuceneRAGService(FileRAGProperties properties) {
        this(properties, "file-domain");
    }

    public LuceneRAGService(FileRAGProperties properties, String domainId) {
        this.properties = properties;
        this.domainId = domainId;
    }

    @PostConstruct
    public void init() {
        try {
            log.info("初始化 Lucene RAG 服务，索引路径: {}", properties.getIndexPath());

            // 创建索引目录
            Path indexPath = Paths.get(properties.getIndexPath());
            if (!Files.exists(indexPath)) {
                Files.createDirectories(indexPath);
            }

            // 清理可能残留的锁文件（处理异常退出情况）
            Path lockFile = indexPath.resolve("write.lock");
            if (Files.exists(lockFile)) {
                try {
                    Files.delete(lockFile);
                    log.warn("检测到旧的索引锁文件，已自动清理: {}", lockFile);
                } catch (IOException e) {
                    log.warn("无法删除锁文件: {}, 将尝试继续初始化", lockFile);
                }
            }

            // 初始化 Lucene 组件
            this.directory = FSDirectory.open(indexPath);
            this.analyzer = new StandardAnalyzer();

            // 配置 IndexWriter
            IndexWriterConfig config = new IndexWriterConfig(analyzer);
            config.setOpenMode(IndexWriterConfig.OpenMode.CREATE_OR_APPEND);
            config.setRAMBufferSizeMB(properties.getRamBufferSizeMb());

            this.indexWriter = new IndexWriter(directory, config);
            this.indexWriter.commit();

            // 初始化 SearcherManager
            this.searcherManager = new SearcherManager(directory, null);

            log.info("Lucene RAG 服务初始化完成，文档总数: {}", indexWriter.getDocStats().numDocs);
        } catch (IOException e) {
            log.error("初始化 Lucene RAG 服务失败", e);
            throw new RuntimeException("初始化 Lucene RAG 服务失败: " + e.getMessage(), e);
        }
    }

    @PreDestroy
    public void destroy() {
        log.info("关闭 Lucene RAG 服务");
        try {
            if (searcherManager != null) {
                searcherManager.close();
            }
            if (indexWriter != null) {
                indexWriter.close();
            }
            if (directory != null) {
                directory.close();
            }
            if (analyzer != null) {
                analyzer.close();
            }
        } catch (IOException e) {
            log.error("关闭 Lucene 资源失败", e);
        }
    }

    // ========== 文档索引 ==========

    /**
     * 索引单个文档（内部方法）
     */
    public String indexDocument(Document document) {
        try {
            // 生成 ID（如果没有）
            if (document.getId() == null || document.getId().isEmpty()) {
                document.setId(UUID.randomUUID().toString());
            }

            // 设置索引时间
            document.setIndexedAt(System.currentTimeMillis());

            // 创建 Lucene Document
            org.apache.lucene.document.Document luceneDoc = convertToLuceneDocument(document);

            // 删除旧文档（如果存在）
            indexWriter.deleteDocuments(new Term("id", document.getId()));

            // 添加新文档
            indexWriter.addDocument(luceneDoc);
            indexWriter.commit();

            // 刷新 SearcherManager
            searcherManager.maybeRefresh();

            log.debug("文档索引成功: {}", document.getId());
            return document.getId();

        } catch (IOException e) {
            log.error("索引文档失败: {}", document.getId(), e);
            throw new RuntimeException("索引文档失败", e);
        }
    }

    /**
     * 批量索引文档（内部方法）
     */
    public List<String> indexDocuments(List<Document> documents) {
        List<String> indexedIds = new ArrayList<>();
        try {
            for (Document document : documents) {
                if (document.getId() == null || document.getId().isEmpty()) {
                    document.setId(UUID.randomUUID().toString());
                }
                document.setIndexedAt(System.currentTimeMillis());

                org.apache.lucene.document.Document luceneDoc = convertToLuceneDocument(document);
                indexWriter.deleteDocuments(new Term("id", document.getId()));
                indexWriter.addDocument(luceneDoc);

                indexedIds.add(document.getId());
            }

            indexWriter.commit();
            searcherManager.maybeRefresh();

            log.info("批量索引完成，共 {} 个文档", indexedIds.size());
            return indexedIds;

        } catch (IOException e) {
            log.error("批量索引文档失败", e);
            throw new RuntimeException("批量索引文档失败", e);
        }
    }

    /**
     * 更新文档（内部方法）
     */
    public boolean updateDocument(Document document) {
        try {
            if (document.getId() == null || document.getId().isEmpty()) {
                log.error("更新文档失败：文档ID为空");
                return false;
            }

            document.setUpdatedAt(System.currentTimeMillis());
            document.setIndexedAt(System.currentTimeMillis());

            org.apache.lucene.document.Document luceneDoc = convertToLuceneDocument(document);
            indexWriter.updateDocument(new Term("id", document.getId()), luceneDoc);
            indexWriter.commit();
            searcherManager.maybeRefresh();

            log.debug("文档更新成功: {}", document.getId());
            return true;

        } catch (IOException e) {
            log.error("更新文档失败: {}", document.getId(), e);
            return false;
        }
    }

    /**
     * 删除文档（内部方法）
     */
    public boolean deleteDocument(String documentId) {
        try {
            indexWriter.deleteDocuments(new Term("id", documentId));
            indexWriter.commit();
            searcherManager.maybeRefresh();

            log.debug("文档删除成功: {}", documentId);
            return true;

        } catch (IOException e) {
            log.error("删除文档失败: {}", documentId, e);
            return false;
        }
    }

    @Override
    public void delete(String id) {
        deleteDocument(id);
    }

    @Override
    public void clearAll() {
        try {
            indexWriter.deleteAll();
            indexWriter.commit();
            searcherManager.maybeRefresh();

            log.info("所有索引已清空");

        } catch (IOException e) {
            log.error("清空索引失败", e);
            throw new RuntimeException("清空索引失败", e);
        }
    }

    // ========== 文本搜索 ==========

    /**
     * 内部文本搜索方法
     */
    public List<SearchResult> searchByText(String text, int topK) {
        try {
            IndexSearcher searcher = searcherManager.acquire();
            try {
                // 使用 MultiFieldQueryParser 支持多字段搜索
                String[] fields = {"title", "content", "summary", "tags"};
                MultiFieldQueryParser parser = new MultiFieldQueryParser(fields, analyzer);
                org.apache.lucene.search.Query luceneQuery = parser.parse(text);

                // 执行搜索
                TopDocs topDocs = searcher.search(luceneQuery, topK);

                // 转换结果
                List<SearchResult> results = new ArrayList<>();
                int rank = 1;
                for (ScoreDoc scoreDoc : topDocs.scoreDocs) {
                    org.apache.lucene.document.Document luceneDoc = searcher.storedFields().document(scoreDoc.doc);
                    Document document = convertFromLuceneDocument(luceneDoc);

                    SearchResult result = SearchResult.builder()
                            .document(document)
                            .score((double) scoreDoc.score)
                            .rank(rank++)
                            .matchedField("文本匹配")
                            .build();

                    results.add(result);
                }

                log.debug("文本搜索完成，查询: {}, 结果数: {}", text, results.size());
                return results;

            } finally {
                searcherManager.release(searcher);
            }

        } catch (IOException | ParseException e) {
            log.error("文本搜索失败: {}", text, e);
            return Collections.emptyList();
        }
    }

    // ========== 向量搜索 ==========

    /**
     * 内部向量搜索方法（使用 float[] 数组）
     */
    public List<SearchResult> vectorSearchInternal(float[] embedding, int topK) {
        // Lucene 9.x 支持向量搜索，但需要额外配置
        // 这里提供基础实现，实际使用时需要根据需求扩展
        log.warn("Lucene File RAG 暂不支持原生向量搜索，请使用语义搜索或混合搜索");
        return Collections.emptyList();
    }

    /**
     * 内部向量搜索方法（使用 float[] 数组和过滤器）
     */
    public List<SearchResult> vectorSearchInternal(float[] embedding, int topK, Map<String, Object> filters) {
        log.warn("Lucene File RAG 暂不支持原生向量搜索，请使用语义搜索或混合搜索");
        return Collections.emptyList();
    }

    @Override
    public List<Document> vectorSearch(Vector vector, int maxResults) {
        // 实现向量搜索，返回 Document 列表
        log.warn("Lucene File RAG 暂不支持原生向量搜索");
        return Collections.emptyList();
    }

    // ========== 混合检索 ==========

    /**
     * 内部混合搜索方法
     */
    public List<SearchResult> hybridSearchInternal(String text, float[] embedding,
                                          float textWeight, float vectorWeight, int topK) {
        // 混合搜索：先执行文本搜索
        List<SearchResult> textResults = searchByText(text, topK * 2);

        // 如果没有向量，直接返回文本结果
        if (embedding == null || embedding.length == 0) {
            return textResults.stream().limit(topK).collect(Collectors.toList());
        }

        // 重新计算分数（文本权重 + 向量权重）
        for (SearchResult result : textResults) {
            Double scoreValue = result.getScore();
            double score = scoreValue != null ? scoreValue : 0.0;
            double combinedScore = score * textWeight;
            result.setScore(combinedScore);
        }

        // 排序并返回 topK
        return textResults.stream()
                .sorted(Comparator.comparing(SearchResult::getScore).reversed())
                .limit(topK)
                .collect(Collectors.toList());
    }

    // ========== 语义搜索 ==========

    @Override
    public List<Document> semanticSearch(String text, int maxResults) {
        // 语义搜索需要集成 AI Embedding 服务
        // 这里先使用文本搜索作为降级方案
        log.warn("语义搜索需要 AI Embedding 服务支持，当前使用文本搜索");
        List<SearchResult> searchResults = searchByText(text, maxResults);
        return searchResults.stream()
                .map(SearchResult::getDocument)
                .collect(Collectors.toList());
    }

    // ========== 向量化 ==========

    @Override
    public Vector embed(String text) {
        // 需要集成 AI Embedding 服务
        log.warn("向量化需要 AI Embedding 服务支持");
        return null;
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        // 需要集成 AI Embedding 服务
        log.warn("批量向量化需要 AI Embedding 服务支持");
        return Collections.emptyList();
    }

    // ========== 文档索引接口实现 ==========

    @Override
    public void index(String id, Vector vector, Map<String, Object> metadata) {
        // 创建文档对象并索引
        Document.DocumentBuilder builder = Document.builder()
                .id(id);

        if (metadata != null) {
            if (metadata.containsKey("title")) {
                builder.title((String) metadata.get("title"));
            }
            if (metadata.containsKey("content")) {
                builder.content((String) metadata.get("content"));
            }
            if (metadata.containsKey("source")) {
                builder.source((String) metadata.get("source"));
            }
        }

        Document document = builder.build();
        indexDocument(document);
    }

    @Override
    public void batchIndex(List<Document> documents) {
        indexDocuments(documents);
    }

    // ========== 域管理 ==========

    @Override
    public String getDomainId() {
        return domainId;
    }

    // ========== 文档管理 ==========

    @Override
    public Optional<Document> getDocument(String documentId) {
        try {
            IndexSearcher searcher = searcherManager.acquire();
            try {
                org.apache.lucene.search.Query query = new TermQuery(new Term("id", documentId));
                TopDocs topDocs = searcher.search(query, 1);

                if (topDocs.totalHits.value > 0) {
                    org.apache.lucene.document.Document luceneDoc = searcher.storedFields().document(topDocs.scoreDocs[0].doc);
                    Document document = convertFromLuceneDocument(luceneDoc);
                    return Optional.of(document);
                }

                return Optional.empty();

            } finally {
                searcherManager.release(searcher);
            }

        } catch (IOException e) {
            log.error("获取文档失败: {}", documentId, e);
            return Optional.empty();
        }
    }

    @Override
    public boolean documentExists(String documentId) {
        return getDocument(documentId).isPresent();
    }

    @Override
    public long getDocumentCount() {
        return indexWriter.getDocStats().numDocs;
    }

    @Override
    public List<Document> getAllDocuments(int offset, int limit) {
        try {
            IndexSearcher searcher = searcherManager.acquire();
            try {
                List<Document> documents = new ArrayList<>();
                IndexReader reader = searcher.getIndexReader();

                // 使用numDocs()获取未删除的文档数，而不是maxDoc()
                int numDocs = reader.numDocs();
                int maxDoc = reader.maxDoc();

                log.debug("获取所有文档: offset={}, limit={}, numDocs={}, maxDoc={}",
                        offset, limit, numDocs, maxDoc);

                // 获取存活文档的位集合（liveDocs）
                org.apache.lucene.util.Bits liveDocs = org.apache.lucene.index.MultiBits.getLiveDocs(reader);

                // 跳过offset个有效文档，然后读取limit个
                int skipped = 0;
                int collected = 0;

                for (int i = 0; i < maxDoc && collected < limit; i++) {
                    // 检查文档是否存在（未被删除）
                    // 如果liveDocs为null，说明没有删除操作，所有文档都存在
                    // 如果liveDocs不为null，需要检查位是否为true（true表示文档存在）
                    if (liveDocs != null && !liveDocs.get(i)) {
                        continue; // 跳过已删除的文档
                    }

                    try {
                        org.apache.lucene.document.Document luceneDoc = searcher.storedFields().document(i);
                        if (luceneDoc != null) {
                            // 跳过前offset个文档
                            if (skipped < offset) {
                                skipped++;
                                continue;
                            }

                            // 收集文档
                            Document document = convertFromLuceneDocument(luceneDoc);
                            documents.add(document);
                            collected++;
                        }
                    } catch (Exception e) {
                        // 文档读取失败，跳过
                        log.debug("跳过文档: docId={}", i);
                    }
                }

                log.debug("获取文档完成: 返回 {} 个文档", documents.size());
                return documents;

            } finally {
                searcherManager.release(searcher);
            }

        } catch (IOException e) {
            log.error("获取所有文档失败", e);
            return Collections.emptyList();
        }
    }

    // ========== 统计与健康 ==========

    @Override
    public IndexStatistics getStatistics() {
        try {
            long totalDocs = indexWriter.getDocStats().numDocs;
            String[] files = directory.listAll();
            long indexSize = files.length;

            return IndexStatistics.builder()
                    .totalDocuments(totalDocs)
                    .indexSize(indexSize)
                    .indexType("Lucene-File")
                    .vectorSearchEnabled(false)
                    .healthy(true)
                    .timestamp(System.currentTimeMillis())
                    .domainId(domainId)
                    .build();

        } catch (IOException e) {
            log.error("获取统计信息失败", e);
            return IndexStatistics.builder()
                    .indexType("Lucene-File")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
                    .domainId(domainId)
                    .build();
        }
    }

    @Override
    public boolean isHealthy() {
        try {
            return indexWriter != null && directory != null && searcherManager != null;
        } catch (Exception e) {
            log.error("健康检查失败", e);
            return false;
        }
    }

    @Override
    public void rebuildIndex() {
        log.info("重建索引开始");
        try {
            // 获取所有文档
            List<Document> allDocuments = new ArrayList<>();
            IndexSearcher searcher = searcherManager.acquire();
            try {
                IndexReader reader = searcher.getIndexReader();
                for (int i = 0; i < reader.maxDoc(); i++) {
                    org.apache.lucene.document.Document luceneDoc = searcher.storedFields().document(i);
                    allDocuments.add(convertFromLuceneDocument(luceneDoc));
                }
            } finally {
                searcherManager.release(searcher);
            }

            // 清空索引
            clearAll();

            // 重新索引
            indexDocuments(allDocuments);

            log.info("重建索引完成，共 {} 个文档", allDocuments.size());

        } catch (IOException e) {
            log.error("重建索引失败", e);
            throw new RuntimeException("重建索引失败", e);
        }
    }

    // ========== 辅助方法 ==========

    /**
     * 转换为 Lucene Document
     */
    private org.apache.lucene.document.Document convertToLuceneDocument(Document document) {
        org.apache.lucene.document.Document luceneDoc = new org.apache.lucene.document.Document();

        // ID（不分词，用于精确查询）
        luceneDoc.add(new StringField("id", document.getId(), Field.Store.YES));

        // 标题（分词，存储）
        if (document.getTitle() != null) {
            luceneDoc.add(new TextField("title", document.getTitle(), Field.Store.YES));
        }

        // 内容（分词，存储）
        if (document.getContent() != null) {
            luceneDoc.add(new TextField("content", document.getContent(), Field.Store.YES));
        }

        // 摘要（分词，存储）
        if (document.getSummary() != null) {
            luceneDoc.add(new TextField("summary", document.getSummary(), Field.Store.YES));
        }

        // 来源
        if (document.getSource() != null) {
            luceneDoc.add(new StringField("source", document.getSource(), Field.Store.YES));
        }

        // 类型
        if (document.getType() != null) {
            luceneDoc.add(new StringField("type", document.getType(), Field.Store.YES));
        }

        // 作者
        if (document.getAuthor() != null) {
            luceneDoc.add(new StringField("author", document.getAuthor(), Field.Store.YES));
        }

        // 标签
        if (document.getTags() != null && !document.getTags().isEmpty()) {
            String tagsStr = String.join(",", document.getTags());
            luceneDoc.add(new TextField("tags", tagsStr, Field.Store.YES));
        }

        // 时间戳
        if (document.getCreatedAt() != null) {
            luceneDoc.add(new StoredField("createdAt", document.getCreatedAt()));
        }
        if (document.getUpdatedAt() != null) {
            luceneDoc.add(new StoredField("updatedAt", document.getUpdatedAt()));
        }
        if (document.getIndexedAt() != null) {
            luceneDoc.add(new StoredField("indexedAt", document.getIndexedAt()));
        }

        return luceneDoc;
    }

    /**
     * 从 Lucene Document 转换
     */
    private Document convertFromLuceneDocument(org.apache.lucene.document.Document luceneDoc) {
        Document.DocumentBuilder builder = Document.builder();

        builder.id(luceneDoc.get("id"));
        builder.title(luceneDoc.get("title"));
        builder.content(luceneDoc.get("content"));
        builder.summary(luceneDoc.get("summary"));
        builder.source(luceneDoc.get("source"));
        builder.type(luceneDoc.get("type"));
        builder.author(luceneDoc.get("author"));

        // 标签
        String tagsStr = luceneDoc.get("tags");
        if (tagsStr != null && !tagsStr.isEmpty()) {
            builder.tags(Arrays.asList(tagsStr.split(",")));
        }

        // 时间戳
        IndexableField createdAtField = luceneDoc.getField("createdAt");
        if (createdAtField != null) {
            builder.createdAt(createdAtField.numericValue().longValue());
        }

        IndexableField updatedAtField = luceneDoc.getField("updatedAt");
        if (updatedAtField != null) {
            builder.updatedAt(updatedAtField.numericValue().longValue());
        }

        IndexableField indexedAtField = luceneDoc.getField("indexedAt");
        if (indexedAtField != null) {
            builder.indexedAt(indexedAtField.numericValue().longValue());
        }

        return builder.build();
    }
}
