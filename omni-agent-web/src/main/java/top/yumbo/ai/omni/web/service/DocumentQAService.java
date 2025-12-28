package top.yumbo.ai.omni.web.service;

import lombok.Data;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.ai.api.AIService;
import top.yumbo.ai.omni.ai.api.model.ChatMessage;
import top.yumbo.ai.omni.storage.api.DocumentStorageService;
import top.yumbo.ai.omni.rag.RagService;
import top.yumbo.ai.omni.rag.model.Document;
import top.yumbo.ai.omni.rag.model.SearchResult;

import java.util.List;

/**
 * 完整文档AI问答服务
 * (Document Q&A Service)
 *
 * <p>对完整文档进行AI问答，基于已有分块</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
@RequiredArgsConstructor
public class DocumentQAService {

    private final DocumentStorageService storageService;
    private final RagService ragService;
    private final AIService aiService;

    /**
     * 对文档进行问答
     *
     * @param documentId 文档ID
     * @param question 问题
     * @return 问答报告
     */
    public DocumentQAReport queryDocument(String documentId, String question) {
        DocumentQAReport report = new DocumentQAReport();
        report.setDocumentId(documentId);
        report.setQuestion(question);

        try {
            log.info("开始文档问答: documentId={}, question={}", documentId, question);

            // 1. 获取文档的所有分块
            List<Chunk> chunks = storageService.getChunksByDocument(documentId);
            if (chunks.isEmpty()) {
                report.setSuccess(false);
                report.setErrorMessage("文档不存在或没有分块");
                return report;
            }
            log.debug("获取文档分块: chunks={}", chunks.size());

            // 2. 使用RAG检索（假设文档已被索引）
            List<Document> documents = ragService.semanticSearch(question, 5);
            List<SearchResult> results = documents.stream()
                    .map(SearchResult::fromDocument)
                    .toList();

            // 过滤出属于指定文档的结果
            List<SearchResult> docResults = results.stream()
                .filter(r -> documentId.equals(r.getDocument().getSource()))
                .toList();

            log.debug("检索完成: total={}, filtered={}", results.size(), docResults.size());

            // 如果没有找到相关结果，使用文档的前几个分块
            if (docResults.isEmpty()) {
                log.debug("未找到相关结果，使用前5个分块");
                docResults = chunks.stream()
                    .limit(5)
                    .map(chunk -> {
                        Document doc = Document.builder()
                            .id(chunk.getId())
                            .content(chunk.getContent())
                            .source(documentId)
                            .build();
                        return SearchResult.builder()
                            .document(doc)
                            .score(1.0)
                            .build();
                    })
                    .toList();
            }

            // 3. 构建上下文
            StringBuilder context = new StringBuilder();
            context.append("基于以下文档内容回答问题：\n\n");

            for (int i = 0; i < docResults.size(); i++) {
                SearchResult result = docResults.get(i);
                context.append(String.format("[段落%d] ", i + 1));
                context.append(result.getDocument().getContent());
                context.append("\n\n");
            }

            context.append("问题：").append(question);

            // 4. AI生成答案
            String answer = aiService.chat(context.toString());
            log.debug("答案生成完成");

            // 5. 构建报告
            report.setSuccess(true);
            report.setAnswer(answer);
            report.setChunkCount(chunks.size());
            report.setReferenceCount(docResults.size());
            report.setReferences(docResults);

            log.info("文档问答完成: documentId={}", documentId);

        } catch (Exception e) {
            log.error("文档问答失败: documentId={}", documentId, e);
            report.setSuccess(false);
            report.setErrorMessage(e.getMessage());
        }

        return report;
    }

    /**
     * 流式文档问答
     *
     * @param documentId 文档ID
     * @param question 问题
     * @return 流式响应
     */
    public reactor.core.publisher.Flux<String> queryDocumentStream(String documentId, String question) {
        try {
            log.info("开始流式文档问答: documentId={}, question={}", documentId, question);

            // 1. 获取文档分块
            List<Chunk> chunks = storageService.getChunksByDocument(documentId);
            if (chunks.isEmpty()) {
                return reactor.core.publisher.Flux.just(
                    "data: [ERROR] 文档不存在或没有分块\n\n"
                );
            }

            // 2. 检索相关分块
            List<Document> documents = ragService.semanticSearch(question, 5);
            List<SearchResult> results = documents.stream()
                    .map(SearchResult::fromDocument)
                    .toList();

            // 过滤出属于指定文档的结果
            List<SearchResult> docResults = results.stream()
                .filter(r -> documentId.equals(r.getDocument().getSource()))
                .toList();

            // 如果没有相关结果，使用前5个分块
            if (docResults.isEmpty()) {
                docResults = chunks.stream()
                    .limit(5)
                    .map(chunk -> {
                        Document doc = Document.builder()
                            .id(chunk.getId())
                            .content(chunk.getContent())
                            .source(documentId)
                            .build();
                        return SearchResult.builder()
                            .document(doc)
                            .score(1.0)
                            .build();
                    })
                    .toList();
            }

            // 3. 构建上下文
            StringBuilder context = new StringBuilder();
            context.append("基于以下文档内容回答问题：\n\n");

            for (int i = 0; i < docResults.size(); i++) {
                SearchResult result = docResults.get(i);
                context.append(String.format("[段落%d] ", i + 1));
                context.append(result.getDocument().getContent());
                context.append("\n\n");
            }

            context.append("问题：").append(question);

            // 4. 流式生成答案
            List<ChatMessage> messages = List.of(
                ChatMessage.builder()
                    .role("user")
                    .content(context.toString())
                    .build()
            );

            return aiService.chatFlux(messages)
                .map(token -> "data: " + token + "\n\n")
                .onErrorResume(e -> {
                    log.error("流式文档问答失败", e);
                    return reactor.core.publisher.Flux.just(
                        "data: [ERROR] " + e.getMessage() + "\n\n"
                    );
                });

        } catch (Exception e) {
            log.error("流式文档问答初始化失败", e);
            return reactor.core.publisher.Flux.just(
                "data: [ERROR] " + e.getMessage() + "\n\n"
            );
        }
    }

    /**
     * 文档问答报告
     */
    @Data
    public static class DocumentQAReport {
        private boolean success;
        private String documentId;
        private String question;
        private String answer;
        private int chunkCount;
        private int referenceCount;
        private List<SearchResult> references;
        private String errorMessage;
    }
}





