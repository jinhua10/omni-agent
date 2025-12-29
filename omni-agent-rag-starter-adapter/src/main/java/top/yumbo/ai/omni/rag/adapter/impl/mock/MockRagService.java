package top.yumbo.ai.omni.rag.adapter.impl.mock;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.Vector;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Mock RAG 服务实现
 *
 * <p>用于测试和开发的简单 RAG 实现</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class MockRagService implements RagService {

    private final String domainId;
    private final Map<String, Document> documents = new ConcurrentHashMap<>();
    private final Random random = new Random();

    public MockRagService(String domainId) {
        this.domainId = domainId;
        log.info("创建 Mock RAG 服务: {}", domainId);
    }

    @Override
    public List<Document> semanticSearch(String query, int maxResults) {
        log.info("Mock 语义搜索: query={}, maxResults={}", query, maxResults);

        // 返回模拟文档
        List<Document> results = new ArrayList<>();
        for (int i = 0; i < Math.min(maxResults, 5); i++) {
            Document doc = Document.builder()
                    .id("mock-doc-" + i)
                    .title("模拟文档 " + (i + 1) + " (查询: " + query + ")")
                    .content("这是一个模拟的文档内容。\n" +
                            "查询关键词: " + query + "\n" +
                            "文档编号: " + (i + 1) + "\n" +
                            "来自域: " + domainId)
                    .summary("文档摘要 " + (i + 1))
                    .score(0.95 - i * 0.1)
                    .metadata(Map.of(
                            "domainId", domainId,
                            "index", i,
                            "type", "mock"
                    ))
                    .build();
            results.add(doc);
        }

        return results;
    }

    @Override
    public List<Document> vectorSearch(Vector vector, int maxResults) {
        log.info("Mock 向量搜索: dimension={}, maxResults={}",
                vector.getDimension(), maxResults);

        // 返回随机文档
        return semanticSearch("vector-search", maxResults);
    }

    @Override
    public Vector embed(String text) {
        log.debug("Mock 文本向量化: length={}", text.length());

        // 返回随机向量
        float[] data = new float[768];
        for (int i = 0; i < data.length; i++) {
            data[i] = random.nextFloat();
        }

        return Vector.of(data);
    }

    @Override
    public List<Vector> batchEmbed(List<String> texts) {
        log.info("Mock 批量向量化: count={}", texts.size());

        return texts.stream()
                .map(this::embed)
                .toList();
    }

    @Override
    public void index(String id, Vector vector, Map<String, Object> metadata) {
        log.info("Mock 索引文档: id={}, dimension={}", id, vector.getDimension());

        Document doc = Document.builder()
                .id(id)
                .title((String) metadata.getOrDefault("title", "Untitled"))
                .content((String) metadata.getOrDefault("content", ""))
                .metadata(metadata)
                .build();

        documents.put(id, doc);
    }

    @Override
    public void batchIndex(List<Document> docs) {
        log.info("Mock 批量索引: count={}", docs.size());

        for (Document doc : docs) {
            Vector vector = embed(doc.getContent());
            index(doc.getId(), vector, doc.getMetadata());
        }
    }

    @Override
    public void delete(String id) {
        log.info("Mock 删除文档: id={}", id);
        documents.remove(id);
    }

    @Override
    public String getDomainId() {
        return domainId;
    }
}

