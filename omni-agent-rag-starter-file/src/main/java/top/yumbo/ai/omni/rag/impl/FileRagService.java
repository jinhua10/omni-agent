package top.yumbo.ai.omni.rag.impl;

import lombok.extern.slf4j.Slf4j;
import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.document.Field;
import org.apache.lucene.document.StringField;
import org.apache.lucene.document.TextField;
import org.apache.lucene.index.*;
import org.apache.lucene.search.*;
import org.apache.lucene.store.Directory;
import org.apache.lucene.store.FSDirectory;
import top.yumbo.ai.omni.ai.api.EmbeddingService;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.IndexStatistics;
import top.yumbo.ai.omni.rag.model.Vector;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.*;

/**
 * 基于 Lucene 的文件 RAG 实现
 * (Lucene-based File RAG Implementation)
 *
 * 支持多域架构，每个实例绑定一个域ID
 *
 * @author OmniAgent Team
 * @since 2.0.0
 */
@Slf4j
public class FileRagService implements RagService {

    private final String domainId;
    private final String indexPath;
    private final EmbeddingService embeddingService; // ⭐ AI Embedding 服务

    private Directory directory;
    private Analyzer analyzer;
    private IndexWriter indexWriter;
    private IndexReader indexReader;
    private SearcherManager searcherManager;

    /**
     * 构造函数
     *
     * @param domainId 域ID（支持多域架构）
     * @param indexPath 索引路径
     * @param embeddingService AI Embedding 服务（可选）
     */
    public FileRagService(String domainId, String indexPath, EmbeddingService embeddingService) {
        this.domainId = domainId;
        this.indexPath = indexPath;
        this.embeddingService = embeddingService;
        log.info("创建 FileRagService: domainId={}, indexPath={}, embedding={}",
                domainId, indexPath, embeddingService != null ? "enabled" : "disabled");
    }

    @PostConstruct
    public void init() {
        try {
            // 创建索引目录
            directory = FSDirectory.open(Paths.get(indexPath));
            analyzer = new StandardAnalyzer();

            // 配置索引
            IndexWriterConfig config = new IndexWriterConfig(analyzer);
            config.setOpenMode(IndexWriterConfig.OpenMode.CREATE_OR_APPEND);

            // 创建索引写入器
            indexWriter = new IndexWriter(directory, config);
            indexWriter.commit();

            // 创建索引读取器和搜索管理器
            indexReader = DirectoryReader.open(directory);
            searcherManager = new SearcherManager(directory, null);

            log.info("✅ FileRagService 初始化成功: domainId={}", domainId);
        } catch (IOException e) {
            log.error("初始化 FileRagService 失败", e);
            throw new RuntimeException("初始化 FileRagService 失败", e);
        }
    }

    @PreDestroy
    public void destroy() {
        try {
            if (searcherManager != null) searcherManager.close();
            if (indexReader != null) indexReader.close();
            if (indexWriter != null) indexWriter.close();
            if (analyzer != null) analyzer.close();
            if (directory != null) directory.close();
            log.info("FileRagService 已关闭: domainId={}", domainId);
        } catch (IOException e) {
            log.error("关闭 FileRagService 失败", e);
        }
    }

    // ========== 核心检索 ==========

    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        log.debug("语义搜索: domainId={}, query={}, maxResults={}", domainId, query, maxResults);

        if (embeddingService != null) {
            // 使用 AI Embedding 进行真正的语义搜索
            try {
                float[] queryEmbedding = embeddingService.embed(query);
                return vectorSearchInternal(queryEmbedding, maxResults);
            } catch (Exception e) {
                log.warn("语义搜索失败，降级到文本搜索: {}", e.getMessage());
                return textSearch(query, maxResults);
            }
        } else {
            // 降级到文本搜索
            log.debug("EmbeddingService 未配置，使用文本搜索");
            return textSearch(query, maxResults);
        }
    }

    @Override
    public List<Document> vectorSearch(Vector vector, int maxResults) {
        log.debug("向量搜索: domainId={}, dimension={}, maxResults={}",
                domainId, vector.getDimension(), maxResults);

        if (vector == null || vector.getData() == null || vector.getData().length == 0) {
            log.warn("向量为空，返回空结果");
            return Collections.emptyList();
        }

        return vectorSearchInternal(vector.getData(), maxResults);
    }

    /**
     * 内部向量搜索实现
     */
    private List<Document> vectorSearchInternal(float[] queryVector, int maxResults) {
        // TODO: Lucene 9.x 支持向量搜索（KNN），需要在索引时存储向量
        // 当前返回文本搜索结果作为降级方案
        log.warn("向量搜索功能待实现，使用文本搜索降级");
        return getAllDocuments(0, maxResults);
    }

    /**
     * 文本搜索（内部方法）
     */
    private List<Document> textSearch(String queryText, int maxResults) {
        try {
            IndexSearcher searcher = searcherManager.acquire();
            try {
                // 构建查询
                Query query = new org.apache.lucene.queryparser.classic.QueryParser("content", analyzer)
                        .parse(queryText);

                // 执行搜索
                TopDocs topDocs = searcher.search(query, maxResults);

                // 转换结果
                List<Document> results = new ArrayList<>();
                for (ScoreDoc scoreDoc : topDocs.scoreDocs) {
                    org.apache.lucene.document.Document luceneDoc =
                            searcher.storedFields().document(scoreDoc.doc);
                    Document doc = convertFromLuceneDocument(luceneDoc);
                    doc.setScore((double) scoreDoc.score);
                    results.add(doc);
                }

                return results;
            } finally {
                searcherManager.release(searcher);
            }
        } catch (Exception e) {
            log.error("文本搜索失败: query={}", queryText, e);
            return Collections.emptyList();
        }
    }

    // ========== 向量化 ==========

    @Override
    public Vector embed(String text) {
        if (embeddingService != null) {
            try {
                float[] embedding = embeddingService.embed(text);
                return Vector.of(embedding);
            } catch (Exception e) {
                log.error("向量化失败: {}", e.getMessage(), e);
                return Vector.of(new float[0]);
            }
        } else {
            log.warn("EmbeddingService 未配置，无法进行向量化");
            return Vector.of(new float[0]);
        }
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        if (embeddingService != null) {
            try {
                List<float[]> embeddings = embeddingService.embedBatch(texts);
                return embeddings.stream()
                        .map(Vector::of)
                        .toList();
            } catch (Exception e) {
                log.error("批量向量化失败: {}", e.getMessage(), e);
                return texts.stream()
                        .map(t -> Vector.of(new float[0]))
                        .toList();
            }
        } else {
            log.warn("EmbeddingService 未配置，无法进行批量向量化");
            return texts.stream()
                    .map(t -> Vector.of(new float[0]))
                    .toList();
        }
    }

    // ========== 文档索引 ==========

    @Override
    public void index(String id, Vector vector, Map<String, Object> metadata) {
        try {
            org.apache.lucene.document.Document luceneDoc = new org.apache.lucene.document.Document();

            // 添加文档ID
            luceneDoc.add(new StringField("id", id, Field.Store.YES));

            // 添加元数据
            if (metadata != null) {
                for (Map.Entry<String, Object> entry : metadata.entrySet()) {
                    String value = String.valueOf(entry.getValue());
                    if ("content".equals(entry.getKey()) || "title".equals(entry.getKey())) {
                        luceneDoc.add(new TextField(entry.getKey(), value, Field.Store.YES));
                    } else {
                        luceneDoc.add(new StringField(entry.getKey(), value, Field.Store.YES));
                    }
                }
            }

            // 索引文档
            indexWriter.updateDocument(new Term("id", id), luceneDoc);
            indexWriter.commit();

            // 刷新搜索器
            searcherManager.maybeRefresh();

            log.debug("文档已索引: domainId={}, id={}", domainId, id);
        } catch (IOException e) {
            log.error("索引文档失败: id={}", id, e);
            throw new RuntimeException("索引文档失败", e);
        }
    }

    @Override
    public void batchIndex(List<Document> documents) {
        log.info("批量索引文档: domainId={}, count={}", domainId, documents.size());

        for (Document doc : documents) {
            try {
                org.apache.lucene.document.Document luceneDoc = convertToLuceneDocument(doc);
                indexWriter.updateDocument(new Term("id", doc.getId()), luceneDoc);
            } catch (Exception e) {
                log.error("索引文档失败: id={}", doc.getId(), e);
            }
        }

        try {
            indexWriter.commit();
            searcherManager.maybeRefresh();
            log.info("✅ 批量索引完成: domainId={}, count={}", domainId, documents.size());
        } catch (IOException e) {
            log.error("提交索引失败", e);
            throw new RuntimeException("提交索引失败", e);
        }
    }

    @Override
    public void delete(String id) {
        try {
            indexWriter.deleteDocuments(new Term("id", id));
            indexWriter.commit();
            searcherManager.maybeRefresh();
            log.debug("文档已删除: domainId={}, id={}", domainId, id);
        } catch (IOException e) {
            log.error("删除文档失败: id={}", id, e);
            throw new RuntimeException("删除文档失败", e);
        }
    }

    @Override
    public void clearAll() {
        try {
            indexWriter.deleteAll();
            indexWriter.commit();
            searcherManager.maybeRefresh();
            log.info("已清空所有文档: domainId={}", domainId);
        } catch (IOException e) {
            log.error("清空文档失败", e);
            throw new RuntimeException("清空文档失败", e);
        }
    }

    // ========== 域管理 ==========

    @Override
    public String getDomainId() {
        return this.domainId;
    }

    // ========== 文档管理 ==========

    @Override
    public Optional<Document> getDocument(String documentId) {
        try {
            IndexSearcher searcher = searcherManager.acquire();
            try {
                Query query = new TermQuery(new Term("id", documentId));
                TopDocs topDocs = searcher.search(query, 1);

                if (topDocs.totalHits.value > 0) {
                    org.apache.lucene.document.Document luceneDoc =
                            searcher.storedFields().document(topDocs.scoreDocs[0].doc);
                    return Optional.of(convertFromLuceneDocument(luceneDoc));
                }
                return Optional.empty();
            } finally {
                searcherManager.release(searcher);
            }
        } catch (IOException e) {
            log.error("获取文档失败: id={}", documentId, e);
            return Optional.empty();
        }
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
                Query query = new MatchAllDocsQuery();
                TopDocs topDocs = searcher.search(query, offset + limit);

                List<Document> results = new ArrayList<>();
                int start = Math.min(offset, topDocs.scoreDocs.length);
                int end = Math.min(offset + limit, topDocs.scoreDocs.length);

                for (int i = start; i < end; i++) {
                    org.apache.lucene.document.Document luceneDoc =
                            searcher.storedFields().document(topDocs.scoreDocs[i].doc);
                    results.add(convertFromLuceneDocument(luceneDoc));
                }

                return results;
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
        return IndexStatistics.builder()
                .domainId(domainId)
                .totalDocuments(getDocumentCount())
                .indexType("Lucene")
                .vectorSearchEnabled(false)
                .healthy(isHealthy())
                .timestamp(System.currentTimeMillis())
                .build();
    }

    @Override
    public boolean isHealthy() {
        try {
            return indexWriter != null && indexWriter.isOpen();
        } catch (Exception e) {
            return false;
        }
    }

    @Override
    public void rebuildIndex() {
        log.warn("rebuildIndex() 暂未实现");
        // TODO: 实现索引重建逻辑
    }

    // ========== 辅助方法 ==========

    /**
     * 转换为 Lucene Document
     */
    private org.apache.lucene.document.Document convertToLuceneDocument(Document document) {
        org.apache.lucene.document.Document luceneDoc = new org.apache.lucene.document.Document();

        // 必需字段
        luceneDoc.add(new StringField("id", document.getId(), Field.Store.YES));

        if (document.getContent() != null) {
            luceneDoc.add(new TextField("content", document.getContent(), Field.Store.YES));
        }

        if (document.getTitle() != null) {
            luceneDoc.add(new TextField("title", document.getTitle(), Field.Store.YES));
        }

        // 可选字段
        if (document.getSummary() != null) {
            luceneDoc.add(new TextField("summary", document.getSummary(), Field.Store.YES));
        }

        if (document.getSource() != null) {
            luceneDoc.add(new StringField("source", document.getSource(), Field.Store.YES));
        }

        if (document.getType() != null) {
            luceneDoc.add(new StringField("type", document.getType(), Field.Store.YES));
        }

        if (document.getAuthor() != null) {
            luceneDoc.add(new StringField("author", document.getAuthor(), Field.Store.YES));
        }

        // 元数据
        if (document.getMetadata() != null) {
            for (Map.Entry<String, Object> entry : document.getMetadata().entrySet()) {
                String key = "meta_" + entry.getKey();
                String value = String.valueOf(entry.getValue());
                luceneDoc.add(new StringField(key, value, Field.Store.YES));
            }
        }

        return luceneDoc;
    }

    /**
     * 从 Lucene Document 转换
     */
    private Document convertFromLuceneDocument(org.apache.lucene.document.Document luceneDoc) {
        Map<String, Object> metadata = new HashMap<>();

        // 提取元数据
        for (IndexableField field : luceneDoc.getFields()) {
            String name = field.name();
            if (name.startsWith("meta_")) {
                metadata.put(name.substring(5), field.stringValue());
            }
        }

        return Document.builder()
                .id(luceneDoc.get("id"))
                .content(luceneDoc.get("content"))
                .title(luceneDoc.get("title"))
                .summary(luceneDoc.get("summary"))
                .source(luceneDoc.get("source"))
                .type(luceneDoc.get("type"))
                .author(luceneDoc.get("author"))
                .metadata(metadata)
                .build();
    }
}

