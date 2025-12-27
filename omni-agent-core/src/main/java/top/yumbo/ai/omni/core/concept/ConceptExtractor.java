package top.yumbo.ai.omni.core.concept;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 * 概念提取器 - 从文本中提取知识概念
 * (Concept Extractor - Extract knowledge concepts from text)
 *
 * <p>
 * 核心功能：
 * - 技术术语提取
 * - 操作关键词识别
 * - 概念权重计算
 * - 概念关系发现
 * </p>
 *
 * <p>
 * 提取策略：
 * 1. 基于规则：识别常见模式（如：大写开头的技术名词）
 * 2. 基于词典：匹配预定义的技术术语库
 * 3. 基于统计：TF-IDF 计算重要性
 * 4. 基于上下文：分析词语共现关系
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class ConceptExtractor {

    /**
     * 技术术语模式（简化版）
     */
    private static final Pattern TECH_TERM_PATTERN = Pattern.compile(
            "\\b([A-Z][a-zA-Z0-9]*(?:\\s+[A-Z][a-zA-Z0-9]*)*)\\b"
    );

    /**
     * 预定义的技术术语词典（简化版）
     */
    private static final Set<String> TECH_TERMS = Set.of(
            // Java 相关
            "Java", "Spring", "Spring Boot", "Spring MVC", "Spring Cloud",
            "Maven", "Gradle", "JPA", "Hibernate", "MyBatis",
            "Servlet", "Tomcat", "Jetty", "JDBC", "JUnit",

            // Web 相关
            "React", "Vue", "Angular", "JavaScript", "TypeScript",
            "HTML", "CSS", "HTTP", "REST", "GraphQL",
            "WebSocket", "Ajax", "JSON", "XML",

            // 数据库相关
            "MySQL", "PostgreSQL", "MongoDB", "Redis", "Elasticsearch",
            "SQL", "NoSQL", "Database", "Index", "Transaction",

            // 架构相关
            "Microservice", "Docker", "Kubernetes", "DevOps", "CI/CD",
            "Load Balancer", "Cache", "Message Queue", "API Gateway",

            // 设计模式
            "Singleton", "Factory", "Observer", "Strategy", "MVC",
            "Dependency Injection", "AOP", "IoC"
    );

    /**
     * 操作关键词
     */
    private static final Set<String> OPERATION_KEYWORDS = Set.of(
            "配置", "部署", "安装", "优化", "调试", "测试", "运行",
            "启动", "停止", "重启", "更新", "升级", "迁移",
            "设置", "管理", "监控", "日志", "备份"
    );

    /**
     * 从文本中提取概念
     *
     * @param text       输入文本
     * @param documentId 文档ID
     * @return 提取的概念列表
     */
    public List<KnowledgeConcept> extractConcepts(String text, String documentId) {
        log.debug("开始提取概念: docId={}, textLength={}", documentId, text.length());

        List<KnowledgeConcept> concepts = new ArrayList<>();

        // 1. 提取技术术语
        List<KnowledgeConcept> techConcepts = extractTechTerms(text, documentId);
        concepts.addAll(techConcepts);

        // 2. 提取操作概念
        List<KnowledgeConcept> operationConcepts = extractOperations(text, documentId);
        concepts.addAll(operationConcepts);

        // 3. 计算概念重要性
        calculateImportance(concepts, text);

        // 4. 发现概念关系（基于共现）
        discoverRelations(concepts, text);

        log.info("概念提取完成: docId={}, 提取了 {} 个概念", documentId, concepts.size());

        return concepts;
    }

    /**
     * 提取技术术语
     */
    private List<KnowledgeConcept> extractTechTerms(String text, String documentId) {
        List<KnowledgeConcept> concepts = new ArrayList<>();
        Set<String> foundTerms = new HashSet<>();

        // 方法1：使用正则表达式匹配
        Matcher matcher = TECH_TERM_PATTERN.matcher(text);
        while (matcher.find()) {
            String term = matcher.group(1);
            if (TECH_TERMS.contains(term) && !foundTerms.contains(term)) {
                foundTerms.add(term);
            }
        }

        // 方法2：直接匹配词典
        for (String techTerm : TECH_TERMS) {
            if (text.contains(techTerm) && !foundTerms.contains(techTerm)) {
                foundTerms.add(techTerm);
            }
        }

        // 创建概念对象
        for (String term : foundTerms) {
            KnowledgeConcept concept = KnowledgeConcept.builder()
                    .conceptId(UUID.randomUUID().toString())
                    .name(term)
                    .type(KnowledgeConcept.ConceptType.TECHNOLOGY)
                    .description("技术术语: " + term)
                    .keywords(List.of(term.toLowerCase(), term))
                    .importance(0.0) // 后续计算
                    .documentIds(List.of(documentId))
                    .properties(new HashMap<>())
                    .relations(new ArrayList<>())
                    .createdAt(System.currentTimeMillis())
                    .updatedAt(System.currentTimeMillis())
                    .build();

            concepts.add(concept);
        }

        return concepts;
    }

    /**
     * 提取操作概念
     */
    private List<KnowledgeConcept> extractOperations(String text, String documentId) {
        List<KnowledgeConcept> concepts = new ArrayList<>();
        Set<String> foundOperations = new HashSet<>();

        for (String operation : OPERATION_KEYWORDS) {
            if (text.contains(operation) && !foundOperations.contains(operation)) {
                foundOperations.add(operation);

                KnowledgeConcept concept = KnowledgeConcept.builder()
                        .conceptId(UUID.randomUUID().toString())
                        .name(operation)
                        .type(KnowledgeConcept.ConceptType.OPERATION)
                        .description("操作概念: " + operation)
                        .keywords(List.of(operation))
                        .importance(0.0) // 后续计算
                        .documentIds(List.of(documentId))
                        .properties(new HashMap<>())
                        .relations(new ArrayList<>())
                        .createdAt(System.currentTimeMillis())
                        .updatedAt(System.currentTimeMillis())
                        .build();

                concepts.add(concept);
            }
        }

        return concepts;
    }

    /**
     * 计算概念重要性（基于词频和位置）
     */
    private void calculateImportance(List<KnowledgeConcept> concepts, String text) {
        String lowerText = text.toLowerCase();
        int textLength = text.length();

        for (KnowledgeConcept concept : concepts) {
            String term = concept.getName().toLowerCase();

            // 计算词频 (TF)
            int count = countOccurrences(lowerText, term);
            double tf = (double) count / textLength;

            // 计算位置权重（越靠前权重越高）
            int firstPosition = lowerText.indexOf(term);
            double positionWeight = firstPosition == -1 ? 0.5 :
                    1.0 - ((double) firstPosition / textLength);

            // 综合计算重要性
            double importance = (tf * 10000 + positionWeight) / 2.0;
            importance = Math.min(importance, 1.0); // 归一化到 [0, 1]

            concept.setImportance(importance);
        }

        // 按重要性排序
        concepts.sort((c1, c2) -> Double.compare(c2.getImportance(), c1.getImportance()));
    }

    /**
     * 发现概念之间的关系（基于共现）
     */
    private void discoverRelations(List<KnowledgeConcept> concepts, String text) {
        // 简化实现：检查概念是否在同一个句子中出现
        String[] sentences = text.split("[。！？\\.]");

        for (int i = 0; i < concepts.size(); i++) {
            KnowledgeConcept concept1 = concepts.get(i);

            for (int j = i + 1; j < concepts.size(); j++) {
                KnowledgeConcept concept2 = concepts.get(j);

                // 检查两个概念是否共现
                int cooccurrenceCount = 0;
                for (String sentence : sentences) {
                    if (sentence.contains(concept1.getName()) &&
                            sentence.contains(concept2.getName())) {
                        cooccurrenceCount++;
                    }
                }

                if (cooccurrenceCount > 0) {
                    // 创建关系
                    double strength = Math.min((double) cooccurrenceCount / sentences.length, 1.0);

                    KnowledgeConcept.ConceptRelation relation = KnowledgeConcept.ConceptRelation.builder()
                            .type(KnowledgeConcept.RelationType.RELATED_TO)
                            .targetConceptId(concept2.getConceptId())
                            .strength(strength)
                            .description(String.format("%s 与 %s 相关（共现 %d 次）",
                                    concept1.getName(), concept2.getName(), cooccurrenceCount))
                            .build();

                    concept1.getRelations().add(relation);
                }
            }
        }
    }

    /**
     * 计算子串出现次数
     */
    private int countOccurrences(String text, String substring) {
        int count = 0;
        int index = 0;
        while ((index = text.indexOf(substring, index)) != -1) {
            count++;
            index += substring.length();
        }
        return count;
    }

    /**
     * 批量提取概念（从多个文档）
     *
     * @param documents 文档 Map (documentId -> content)
     * @return 合并后的概念列表
     */
    public List<KnowledgeConcept> extractConceptsFromDocuments(Map<String, String> documents) {
        log.info("开始批量提取概念: {} 个文档", documents.size());

        Map<String, KnowledgeConcept> conceptMap = new HashMap<>();

        for (Map.Entry<String, String> entry : documents.entrySet()) {
            String docId = entry.getKey();
            String content = entry.getValue();

            List<KnowledgeConcept> concepts = extractConcepts(content, docId);

            // 合并相同名称的概念
            for (KnowledgeConcept concept : concepts) {
                String key = concept.getName() + "_" + concept.getType();

                if (conceptMap.containsKey(key)) {
                    // 更新已有概念
                    KnowledgeConcept existing = conceptMap.get(key);
                    existing.getDocumentIds().add(docId);
                    existing.setImportance(Math.max(existing.getImportance(), concept.getImportance()));
                    existing.setUpdatedAt(System.currentTimeMillis());
                } else {
                    // 添加新概念
                    conceptMap.put(key, concept);
                }
            }
        }

        List<KnowledgeConcept> allConcepts = new ArrayList<>(conceptMap.values());
        allConcepts.sort((c1, c2) -> Double.compare(c2.getImportance(), c1.getImportance()));

        log.info("批量概念提取完成: 总共 {} 个唯一概念", allConcepts.size());

        return allConcepts;
    }
}


