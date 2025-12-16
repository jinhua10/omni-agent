package top.yumbo.ai.rag.file;

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
import top.yumbo.ai.rag.api.RAGService;
import top.yumbo.ai.rag.api.model.Document;
import top.yumbo.ai.rag.api.model.IndexStatistics;
import top.yumbo.ai.rag.api.model.Query;
import top.yumbo.ai.rag.api.model.SearchResult;

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
public class LuceneRAGService implements RAGService {

    private final FileRAGProperties properties;
    private Directory directory;
    private Analyzer analyzer;
    private IndexWriter indexWriter;
    private IndexReader indexReader;
    private SearcherManager searcherManager;

    public LuceneRAGService(FileRAGProperties properties) {
        this.properties = properties;
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

    @Override
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

    @Override
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

    @Override
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

    @Override
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

    @Override
    public List<SearchResult> search(Query query) {
        switch (query.getMode()) {
            case TEXT:
                return searchByText(query.getText(), query.getTopK());
            case VECTOR:
                return query.getEmbedding() != null ?
                        vectorSearch(query.getEmbedding(), query.getTopK()) :
                        Collections.emptyList();
            case HYBRID:
                return hybridSearch(query);
            case SEMANTIC:
                return semanticSearch(query.getText(), query.getTopK());
            default:
                return searchByText(query.getText(), query.getTopK());
        }
    }

    @Override
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
                            .score(scoreDoc.score)
                            .textScore(scoreDoc.score)
                            .rank(rank++)
                            .reason("文本匹配")
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

    @Override
    public List<SearchResult> vectorSearch(float[] embedding, int topK) {
        // Lucene 9.x 支持向量搜索，但需要额外配置
        // 这里提供基础实现，实际使用时需要根据需求扩展
        log.warn("Lucene File RAG 暂不支持原生向量搜索，请使用语义搜索或混合搜索");
        return Collections.emptyList();
    }

    @Override
    public List<SearchResult> vectorSearch(float[] embedding, int topK, Map<String, Object> filters) {
        log.warn("Lucene File RAG 暂不支持原生向量搜索，请使用语义搜索或混合搜索");
        return Collections.emptyList();
    }

    // ========== 混合检索 ==========

    @Override
    public List<SearchResult> hybridSearch(Query query) {
        // 混合搜索：先执行文本搜索，然后根据需要结合向量搜索
        List<SearchResult> textResults = searchByText(query.getText(), query.getTopK() * 2);

        // 如果没有向量，直接返回文本结果
        if (query.getEmbedding() == null || query.getEmbedding().length == 0) {
            return textResults.stream().limit(query.getTopK()).collect(Collectors.toList());
        }

        // 重新计算分数（文本权重 + 向量权重）
        for (SearchResult result : textResults) {
            float textScore = result.getTextScore() != null ? result.getTextScore() : 0;
            float combinedScore = textScore * query.getTextWeight();
            result.setScore(combinedScore);
        }

        // 排序并返回 topK
        return textResults.stream()
                .sorted(Comparator.comparing(SearchResult::getScore).reversed())
                .limit(query.getTopK())
                .collect(Collectors.toList());
    }

    @Override
    public List<SearchResult> hybridSearch(String text, float[] embedding,
                                          float textWeight, float vectorWeight, int topK) {
        Query query = Query.builder()
                .text(text)
                .embedding(embedding)
                .topK(topK)
                .mode(Query.SearchMode.HYBRID)
                .textWeight(textWeight)
                .vectorWeight(vectorWeight)
                .build();
        return hybridSearch(query);
    }

    // ========== 语义搜索 ==========

    @Override
    public List<SearchResult> semanticSearch(String text, int topK) {
        // 语义搜索需要集成 AI Embedding 服务
        // 这里先使用文本搜索作为降级方案
        log.warn("语义搜索需要 AI Embedding 服务支持，当前使用文本搜索");
        return searchByText(text, topK);
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
                    .lastIndexedAt(System.currentTimeMillis())
                    .timestamp(System.currentTimeMillis())
                    .build();

        } catch (IOException e) {
            log.error("获取统计信息失败", e);
            return IndexStatistics.builder()
                    .indexType("Lucene-File")
                    .healthy(false)
                    .timestamp(System.currentTimeMillis())
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
