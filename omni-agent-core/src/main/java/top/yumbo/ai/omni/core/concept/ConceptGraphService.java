package top.yumbo.ai.omni.core.concept;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.stream.Collectors;

/**
 * 概念图服务 - 构建和管理概念关系图
 * (Concept Graph Service - Build and manage concept relationship graph)
 *
 * <p>
 * 核心功能：
 * - 概念图构建
 * - 概念查询和搜索
 * - 路径发现（两个概念之间的关系路径）
 * - 概念推理（基于关系图的推理）
 * - 相似概念推荐
 * </p>
 *
 * <p>
 * 使用场景：
 * - 双轨系统右轨的智能推理
 * - 问题分解和答案综合
 * - 知识关联分析
 * </p>
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
@Slf4j
@Service
public class ConceptGraphService {

    /**
     * 概念存储 (conceptId -> concept)
     */
    private final Map<String, KnowledgeConcept> conceptStore = new HashMap<>();

    /**
     * 概念名称索引 (name -> concept)
     */
    private final Map<String, KnowledgeConcept> nameIndex = new HashMap<>();

    /**
     * 类型索引 (type -> List<concept>)
     */
    private final Map<KnowledgeConcept.ConceptType, List<KnowledgeConcept>> typeIndex = new HashMap<>();

    /**
     * 添加概念到图中
     *
     * @param concept 概念对象
     */
    public void addConcept(KnowledgeConcept concept) {
        conceptStore.put(concept.getConceptId(), concept);
        nameIndex.put(concept.getName(), concept);

        typeIndex.computeIfAbsent(concept.getType(), k -> new ArrayList<>()).add(concept);

        log.debug("添加概念: {} ({})", concept.getName(), concept.getType());
    }

    /**
     * 批量添加概念
     *
     * @param concepts 概念列表
     */
    public void addConcepts(List<KnowledgeConcept> concepts) {
        log.info("批量添加概念: {} 个", concepts.size());
        concepts.forEach(this::addConcept);
    }

    /**
     * 根据ID获取概念
     *
     * @param conceptId 概念ID
     * @return 概念对象，不存在则返回null
     */
    public KnowledgeConcept getConceptById(String conceptId) {
        return conceptStore.get(conceptId);
    }

    /**
     * 根据名称获取概念
     *
     * @param name 概念名称
     * @return 概念对象，不存在则返回null
     */
    public KnowledgeConcept getConceptByName(String name) {
        return nameIndex.get(name);
    }

    /**
     * 根据类型获取概念列表
     *
     * @param type 概念类型
     * @return 概念列表
     */
    public List<KnowledgeConcept> getConceptsByType(KnowledgeConcept.ConceptType type) {
        return typeIndex.getOrDefault(type, Collections.emptyList());
    }

    /**
     * 搜索概念（基于名称或关键词）
     *
     * @param keyword 搜索关键词
     * @param topK 返回前K个结果
     * @return 匹配的概念列表
     */
    public List<KnowledgeConcept> searchConcepts(String keyword, int topK) {
        String lowerKeyword = keyword.toLowerCase();

        return conceptStore.values().stream()
                .filter(concept ->
                        concept.getName().toLowerCase().contains(lowerKeyword) ||
                        concept.getKeywords().stream()
                                .anyMatch(k -> k.toLowerCase().contains(lowerKeyword))
                )
                .sorted((c1, c2) -> Double.compare(c2.getImportance(), c1.getImportance()))
                .limit(topK)
                .collect(Collectors.toList());
    }

    /**
     * 找出两个概念之间的关系路径（BFS）
     *
     * @param fromConceptId 起始概念ID
     * @param toConceptId 目标概念ID
     * @param maxDepth 最大搜索深度
     * @return 关系路径列表（概念ID序列）
     */
    public List<List<String>> findPaths(String fromConceptId, String toConceptId, int maxDepth) {
        log.debug("查找概念路径: {} -> {}, maxDepth={}", fromConceptId, toConceptId, maxDepth);

        List<List<String>> paths = new ArrayList<>();
        Queue<PathNode> queue = new LinkedList<>();
        Set<String> visited = new HashSet<>();

        queue.offer(new PathNode(fromConceptId, new ArrayList<>(List.of(fromConceptId)), 0));

        while (!queue.isEmpty()) {
            PathNode current = queue.poll();

            if (current.depth > maxDepth) {
                continue;
            }

            if (current.conceptId.equals(toConceptId)) {
                paths.add(current.path);
                continue;
            }

            if (visited.contains(current.conceptId)) {
                continue;
            }
            visited.add(current.conceptId);

            KnowledgeConcept concept = conceptStore.get(current.conceptId);
            if (concept == null || concept.getRelations() == null) {
                continue;
            }

            for (KnowledgeConcept.ConceptRelation relation : concept.getRelations()) {
                String nextId = relation.getTargetConceptId();
                if (!visited.contains(nextId)) {
                    List<String> newPath = new ArrayList<>(current.path);
                    newPath.add(nextId);
                    queue.offer(new PathNode(nextId, newPath, current.depth + 1));
                }
            }
        }

        log.debug("找到 {} 条路径", paths.size());
        return paths;
    }

    /**
     * 概念推理 - 基于关系推导新知识
     *
     * @param conceptName 概念名称
     * @return 推理结果（相关概念和推理路径）
     */
    public ConceptInferenceResult infer(String conceptName) {
        log.info("开始概念推理: {}", conceptName);

        KnowledgeConcept concept = nameIndex.get(conceptName);
        if (concept == null) {
            log.warn("概念不存在: {}", conceptName);
            return ConceptInferenceResult.builder()
                    .conceptName(conceptName)
                    .found(false)
                    .relatedConcepts(Collections.emptyList())
                    .inferredKnowledge(Collections.emptyList())
                    .build();
        }

        // 1. 收集直接相关的概念
        List<KnowledgeConcept> directlyRelated = new ArrayList<>();
        for (KnowledgeConcept.ConceptRelation relation : concept.getRelations()) {
            KnowledgeConcept relatedConcept = conceptStore.get(relation.getTargetConceptId());
            if (relatedConcept != null) {
                directlyRelated.add(relatedConcept);
            }
        }

        // 2. 推理规则应用（简化版）
        List<String> inferences = new ArrayList<>();

        for (KnowledgeConcept.ConceptRelation relation : concept.getRelations()) {
            KnowledgeConcept related = conceptStore.get(relation.getTargetConceptId());
            if (related == null) continue;

            switch (relation.getType()) {
                case DEPENDS_ON:
                    inferences.add(String.format(
                            "%s 依赖于 %s，因此使用 %s 时需要先配置 %s",
                            concept.getName(), related.getName(),
                            concept.getName(), related.getName()
                    ));
                    break;

                case USED_FOR:
                    inferences.add(String.format(
                            "%s 可用于 %s",
                            concept.getName(), related.getName()
                    ));
                    break;

                case SOLVES:
                    inferences.add(String.format(
                            "%s 可以解决 %s 问题",
                            concept.getName(), related.getName()
                    ));
                    break;

                case PREREQUISITE_OF:
                    inferences.add(String.format(
                            "%s 是 %s 的前置条件",
                            concept.getName(), related.getName()
                    ));
                    break;

                default:
                    inferences.add(String.format(
                            "%s 与 %s 相关",
                            concept.getName(), related.getName()
                    ));
            }
        }

        log.info("推理完成: 发现 {} 个相关概念，生成 {} 条推理",
                directlyRelated.size(), inferences.size());

        return ConceptInferenceResult.builder()
                .conceptName(conceptName)
                .found(true)
                .concept(concept)
                .relatedConcepts(directlyRelated)
                .inferredKnowledge(inferences)
                .build();
    }

    /**
     * 获取概念图统计信息
     *
     * @return 统计信息 Map
     */
    public Map<String, Object> getStatistics() {
        Map<String, Object> stats = new HashMap<>();
        stats.put("totalConcepts", conceptStore.size());
        stats.put("conceptsByType", typeIndex.entrySet().stream()
                .collect(Collectors.toMap(
                        e -> e.getKey().name(),
                        e -> e.getValue().size()
                )));

        int totalRelations = conceptStore.values().stream()
                .mapToInt(c -> c.getRelations() != null ? c.getRelations().size() : 0)
                .sum();
        stats.put("totalRelations", totalRelations);

        return stats;
    }

    /**
     * 清空概念图
     */
    public void clear() {
        conceptStore.clear();
        nameIndex.clear();
        typeIndex.clear();
        log.info("概念图已清空");
    }

    /**
     * 路径节点（用于BFS搜索）
     */
    private static class PathNode {
        String conceptId;
        List<String> path;
        int depth;

        PathNode(String conceptId, List<String> path, int depth) {
            this.conceptId = conceptId;
            this.path = path;
            this.depth = depth;
        }
    }

    /**
     * 概念推理结果
     */
    @lombok.Data
    @lombok.Builder
    @lombok.NoArgsConstructor
    @lombok.AllArgsConstructor
    public static class ConceptInferenceResult {
        /** 查询的概念名称 */
        private String conceptName;

        /** 是否找到概念 */
        private boolean found;

        /** 概念对象 */
        private KnowledgeConcept concept;

        /** 相关概念列表 */
        private List<KnowledgeConcept> relatedConcepts;

        /** 推理出的知识（文本描述） */
        private List<String> inferredKnowledge;
    }
}


