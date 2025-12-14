package top.yumbo.ai.omni.core.evolution;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 进化服务 (Evolution Service)
 *
 * 管理概念的演化和版本控制
 * (Manages concept evolution and version control)
 *
 * 核心功能 (Core Features):
 * - 版本创建 (Version creation)
 * - 版本查询 (Version query)
 * - 演化历史 (Evolution history)
 * - 版本比较 (Version comparison)
 *
 * @author OmniAgent Team
 * @since 2025-12-15
 */
@Slf4j
@Service
public class EvolutionService {

    /**
     * 版本存储 (Version storage)
     * Key: versionId, Value: ConceptVersion
     */
    private final Map<String, ConceptVersion> versionStorage = new ConcurrentHashMap<>();

    /**
     * 概念版本索引 (Concept version index)
     * Key: conceptId, Value: List of version IDs
     */
    private final Map<String, List<String>> conceptVersionIndex = new ConcurrentHashMap<>();

    /**
     * 当前版本索引 (Current version index)
     * Key: conceptId, Value: current version ID
     */
    private final Map<String, String> currentVersionIndex = new ConcurrentHashMap<>();

    /**
     * 创建新版本 (Create new version)
     *
     * @param conceptId 概念ID (Concept ID)
     * @param content 概念内容 (Concept content)
     * @param changeDescription 变更描述 (Change description)
     * @param changeType 变更类型 (Change type)
     * @param creator 创建者 (Creator)
     * @return 新版本对象 (New version object)
     */
    public ConceptVersion createVersion(String conceptId, String content,
                                       String changeDescription,
                                       ConceptVersion.ChangeType changeType,
                                       String creator) {
        // 获取当前版本号 (Get current version number)
        int nextVersion = getNextVersionNumber(conceptId);

        // 获取父版本ID (Get parent version ID)
        String parentVersionId = currentVersionIndex.get(conceptId);

        // 创建新版本 (Create new version)
        ConceptVersion version = ConceptVersion.builder()
                .versionId(UUID.randomUUID().toString())
                .conceptId(conceptId)
                .version(nextVersion)
                .content(content)
                .changeDescription(changeDescription)
                .changeType(changeType)
                .createTime(new Date())
                .creator(creator)
                .parentVersionId(parentVersionId)
                .current(true)
                .build();

        // 如果存在父版本，将其设为非当前版本 (If parent exists, set it as non-current)
        if (parentVersionId != null) {
            ConceptVersion parentVersion = versionStorage.get(parentVersionId);
            if (parentVersion != null) {
                parentVersion.setCurrent(false);
            }
        }

        // 存储版本 (Store version)
        versionStorage.put(version.getVersionId(), version);

        // 更新索引 (Update index)
        conceptVersionIndex.computeIfAbsent(conceptId, k -> new ArrayList<>())
                .add(version.getVersionId());
        currentVersionIndex.put(conceptId, version.getVersionId());

        log.info("New concept version created: conceptId={}, version={}, changeType={}",
                conceptId, nextVersion, changeType);

        return version;
    }

    /**
     * 获取概念的当前版本 (Get current version of concept)
     *
     * @param conceptId 概念ID (Concept ID)
     * @return 当前版本对象 (Current version object)
     */
    public ConceptVersion getCurrentVersion(String conceptId) {
        String currentVersionId = currentVersionIndex.get(conceptId);
        if (currentVersionId == null) {
            return null;
        }
        return versionStorage.get(currentVersionId);
    }

    /**
     * 获取概念的所有版本 (Get all versions of concept)
     *
     * @param conceptId 概念ID (Concept ID)
     * @return 版本列表，按时间倒序 (List of versions, sorted by time descending)
     */
    public List<ConceptVersion> getVersionHistory(String conceptId) {
        List<String> versionIds = conceptVersionIndex.get(conceptId);
        if (versionIds == null || versionIds.isEmpty()) {
            return Collections.emptyList();
        }

        return versionIds.stream()
                .map(versionStorage::get)
                .filter(Objects::nonNull)
                .sorted(Comparator.comparing(ConceptVersion::getCreateTime).reversed())
                .collect(Collectors.toList());
    }

    /**
     * 获取特定版本 (Get specific version)
     *
     * @param versionId 版本ID (Version ID)
     * @return 版本对象 (Version object)
     */
    public ConceptVersion getVersion(String versionId) {
        return versionStorage.get(versionId);
    }

    /**
     * 获取特定版本号的版本 (Get version by version number)
     *
     * @param conceptId 概念ID (Concept ID)
     * @param versionNumber 版本号 (Version number)
     * @return 版本对象 (Version object)
     */
    public ConceptVersion getVersionByNumber(String conceptId, int versionNumber) {
        return getVersionHistory(conceptId).stream()
                .filter(v -> v.getVersion() == versionNumber)
                .findFirst()
                .orElse(null);
    }

    /**
     * 比较两个版本 (Compare two versions)
     *
     * @param versionId1 版本1 ID (Version 1 ID)
     * @param versionId2 版本2 ID (Version 2 ID)
     * @return 差异描述 Map (Difference description map)
     */
    public Map<String, Object> compareVersions(String versionId1, String versionId2) {
        ConceptVersion v1 = versionStorage.get(versionId1);
        ConceptVersion v2 = versionStorage.get(versionId2);

        if (v1 == null || v2 == null) {
            throw new IllegalArgumentException("Version not found");
        }

        Map<String, Object> diff = new HashMap<>();
        diff.put("version1", v1.getVersion());
        diff.put("version2", v2.getVersion());
        diff.put("versionDiff", v2.getVersion() - v1.getVersion());
        diff.put("content1", v1.getContent());
        diff.put("content2", v2.getContent());
        diff.put("contentChanged", !Objects.equals(v1.getContent(), v2.getContent()));
        diff.put("timeDiff", v2.getCreateTime().getTime() - v1.getCreateTime().getTime());

        return diff;
    }

    /**
     * 获取演化统计 (Get evolution statistics)
     *
     * @return 统计信息 Map (Statistics map)
     */
    public Map<String, Object> getStatistics() {
        Map<String, Object> stats = new HashMap<>();

        stats.put("totalVersions", versionStorage.size());
        stats.put("totalConcepts", conceptVersionIndex.size());

        // 按变更类型统计 (Statistics by change type)
        Map<ConceptVersion.ChangeType, Long> typeStats = versionStorage.values().stream()
                .collect(Collectors.groupingBy(
                        ConceptVersion::getChangeType,
                        Collectors.counting()
                ));
        stats.put("byChangeType", typeStats);

        // 平均版本数 (Average versions per concept)
        double avgVersions = conceptVersionIndex.values().stream()
                .mapToInt(List::size)
                .average()
                .orElse(0.0);
        stats.put("averageVersionsPerConcept", avgVersions);

        return stats;
    }

    /**
     * 清除概念的所有版本 (Clear all versions of concept)
     *
     * @param conceptId 概念ID (Concept ID)
     */
    public void clearConcept(String conceptId) {
        List<String> versionIds = conceptVersionIndex.remove(conceptId);
        if (versionIds != null) {
            versionIds.forEach(versionStorage::remove);
            currentVersionIndex.remove(conceptId);
            log.info("Concept versions cleared: conceptId={}, count={}", conceptId, versionIds.size());
        }
    }

    /**
     * 清除所有版本 (Clear all versions)
     */
    public void clearAll() {
        versionStorage.clear();
        conceptVersionIndex.clear();
        currentVersionIndex.clear();
        log.info("All evolution data cleared");
    }

    /**
     * 获取下一个版本号 (Get next version number)
     *
     * @param conceptId 概念ID (Concept ID)
     * @return 下一个版本号 (Next version number)
     */
    private int getNextVersionNumber(String conceptId) {
        List<String> versionIds = conceptVersionIndex.get(conceptId);
        if (versionIds == null || versionIds.isEmpty()) {
            return 1;
        }

        return versionIds.stream()
                .map(versionStorage::get)
                .filter(Objects::nonNull)
                .mapToInt(ConceptVersion::getVersion)
                .max()
                .orElse(0) + 1;
    }
}

