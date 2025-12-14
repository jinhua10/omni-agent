package top.yumbo.ai.omni.core.evolution;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.DisplayName;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * EvolutionService 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@DisplayName("演化服务 测试")
class EvolutionServiceTest {

    private EvolutionService evolutionService;

    @BeforeEach
    void setUp() {
        evolutionService = new EvolutionService();
        // 确保每个测试都从干净状态开始
        evolutionService.clearAll();
    }

    @Test
    @DisplayName("创建第一个版本")
    void testCreateFirstVersion() {
        // Given
        String conceptId = "concept-001";
        String content = "Java是一种编程语言";
        String changeDescription = "初始版本";
        ConceptVersion.ChangeType changeType = ConceptVersion.ChangeType.CREATE;
        String creator = "user-001";

        // When
        ConceptVersion version = evolutionService.createVersion(
            conceptId, content, changeDescription, changeType, creator);

        // Then
        assertNotNull(version);
        assertNotNull(version.getVersionId());
        assertEquals(conceptId, version.getConceptId());
        assertEquals(1, version.getVersion());
        assertEquals(content, version.getContent());
        assertEquals(changeDescription, version.getChangeDescription());
        assertEquals(changeType, version.getChangeType());
        assertEquals(creator, version.getCreator());
        assertTrue(version.isCurrent());
        assertNull(version.getParentVersionId());
        assertNotNull(version.getCreateTime());
    }

    @Test
    @DisplayName("创建第二个版本")
    void testCreateSecondVersion() {
        // Given
        String conceptId = "concept-002";
        evolutionService.createVersion(
            conceptId, "内容v1", "创建", ConceptVersion.ChangeType.CREATE, "user-001");

        // When
        ConceptVersion v2 = evolutionService.createVersion(
            conceptId, "内容v2", "更新", ConceptVersion.ChangeType.UPDATE, "user-002");

        // Then
        assertNotNull(v2);
        assertEquals(2, v2.getVersion());
        assertTrue(v2.isCurrent());
        assertNotNull(v2.getParentVersionId());
    }

    @Test
    @DisplayName("获取当前版本")
    void testGetCurrentVersion() {
        // Given
        String conceptId = "concept-003";
        evolutionService.createVersion(
            conceptId, "v1", "创建", ConceptVersion.ChangeType.CREATE, "user");
        ConceptVersion v2 = evolutionService.createVersion(
            conceptId, "v2", "更新", ConceptVersion.ChangeType.UPDATE, "user");

        // When
        ConceptVersion current = evolutionService.getCurrentVersion(conceptId);

        // Then
        assertNotNull(current);
        assertEquals(v2.getVersionId(), current.getVersionId());
        assertEquals(2, current.getVersion());
        assertTrue(current.isCurrent());
    }

    @Test
    @DisplayName("获取不存在概念的当前版本返回null")
    void testGetCurrentVersionForNonExistentConcept() {
        // When
        ConceptVersion version = evolutionService.getCurrentVersion("non-existent");

        // Then
        assertNull(version);
    }

    @Test
    @DisplayName("获取版本历史")
    void testGetVersionHistory() throws InterruptedException {
        // Given - 使用完全唯一的ID并添加小延迟确保独立性
        Thread.sleep(10);
        String conceptId = "concept-hist-" + System.nanoTime() + "-" + Thread.currentThread().getId();

        ConceptVersion v1 = evolutionService.createVersion(conceptId, "v1", "创建", ConceptVersion.ChangeType.CREATE, "user");
        assertNotNull(v1, "Version 1 should be created");
        Thread.sleep(1);

        ConceptVersion v2 = evolutionService.createVersion(conceptId, "v2", "更新", ConceptVersion.ChangeType.UPDATE, "user");
        assertNotNull(v2, "Version 2 should be created");
        Thread.sleep(1);

        ConceptVersion v3 = evolutionService.createVersion(conceptId, "v3", "合并", ConceptVersion.ChangeType.MERGE, "user");
        assertNotNull(v3, "Version 3 should be created");

        // When
        List<ConceptVersion> history = evolutionService.getVersionHistory(conceptId);

        // Then
        assertNotNull(history);
        assertEquals(3, history.size(), "Should have exactly 3 versions, but got: " + history.size());

        if (history.size() >= 3) {
            // 验证按时间倒序排列
            assertEquals(3, history.get(0).getVersion());
            assertEquals(2, history.get(1).getVersion());
            assertEquals(1, history.get(2).getVersion());
        }
    }

    @Test
    @DisplayName("获取空历史")
    void testGetEmptyVersionHistory() {
        // When
        List<ConceptVersion> history = evolutionService.getVersionHistory("non-existent");

        // Then
        assertNotNull(history);
        assertTrue(history.isEmpty());
    }

    @Test
    @DisplayName("通过ID获取版本")
    void testGetVersionById() {
        // Given
        String conceptId = "concept-005";
        ConceptVersion created = evolutionService.createVersion(
            conceptId, "content", "desc", ConceptVersion.ChangeType.CREATE, "user");

        // When
        ConceptVersion retrieved = evolutionService.getVersion(created.getVersionId());

        // Then
        assertNotNull(retrieved);
        assertEquals(created.getVersionId(), retrieved.getVersionId());
        assertEquals(created.getContent(), retrieved.getContent());
    }

    @Test
    @DisplayName("通过版本号获取版本")
    void testGetVersionByNumber() {
        // Given
        String conceptId = "concept-006";
        evolutionService.createVersion(conceptId, "v1", "创建", ConceptVersion.ChangeType.CREATE, "user");
        ConceptVersion v2 = evolutionService.createVersion(
            conceptId, "v2", "更新", ConceptVersion.ChangeType.UPDATE, "user");

        // When
        ConceptVersion retrieved = evolutionService.getVersionByNumber(conceptId, 2);

        // Then
        assertNotNull(retrieved);
        assertEquals(v2.getVersionId(), retrieved.getVersionId());
        assertEquals(2, retrieved.getVersion());
    }

    @Test
    @DisplayName("通过不存在的版本号返回null")
    void testGetNonExistentVersionByNumber() {
        // Given
        String conceptId = "concept-007";
        evolutionService.createVersion(conceptId, "v1", "创建", ConceptVersion.ChangeType.CREATE, "user");

        // When
        ConceptVersion version = evolutionService.getVersionByNumber(conceptId, 999);

        // Then
        assertNull(version);
    }

    @Test
    @DisplayName("比较两个版本")
    void testCompareVersions() {
        // Given
        String conceptId = "concept-008";
        ConceptVersion v1 = evolutionService.createVersion(
            conceptId, "内容v1", "创建", ConceptVersion.ChangeType.CREATE, "user1");
        ConceptVersion v2 = evolutionService.createVersion(
            conceptId, "内容v2", "更新", ConceptVersion.ChangeType.UPDATE, "user2");

        // When
        Map<String, Object> diff = evolutionService.compareVersions(
            v1.getVersionId(), v2.getVersionId());

        // Then
        assertNotNull(diff);
        assertTrue(diff.containsKey("version1"));
        assertTrue(diff.containsKey("version2"));
        assertTrue(diff.containsKey("contentChanged"));
    }

    @Test
    @DisplayName("获取演化统计")
    void testGetStatistics() {
        // Given
        evolutionService.createVersion("c1", "content", "desc", ConceptVersion.ChangeType.CREATE, "user");
        evolutionService.createVersion("c1", "content2", "desc", ConceptVersion.ChangeType.UPDATE, "user");
        evolutionService.createVersion("c2", "content", "desc", ConceptVersion.ChangeType.CREATE, "user");

        // When
        Map<String, Object> stats = evolutionService.getStatistics();

        // Then
        assertNotNull(stats);
        assertEquals(3, ((Number) stats.get("totalVersions")).intValue());
        assertEquals(2, ((Number) stats.get("totalConcepts")).intValue());
    }

    @Test
    @DisplayName("清除概念的所有版本")
    void testClearConcept() {
        // Given
        String conceptId = "concept-009";
        evolutionService.createVersion(conceptId, "v1", "创建", ConceptVersion.ChangeType.CREATE, "user");
        evolutionService.createVersion(conceptId, "v2", "更新", ConceptVersion.ChangeType.UPDATE, "user");

        // When
        evolutionService.clearConcept(conceptId);

        // Then
        assertNull(evolutionService.getCurrentVersion(conceptId));
        assertTrue(evolutionService.getVersionHistory(conceptId).isEmpty());
    }

    @Test
    @DisplayName("清除所有版本")
    void testClearAll() {
        // Given
        evolutionService.createVersion("c1", "content", "desc", ConceptVersion.ChangeType.CREATE, "user");
        evolutionService.createVersion("c2", "content", "desc", ConceptVersion.ChangeType.CREATE, "user");

        // When
        evolutionService.clearAll();

        // Then
        Map<String, Object> stats = evolutionService.getStatistics();
        assertEquals(0, ((Number) stats.get("totalVersions")).intValue());
    }

    @Test
    @DisplayName("版本号自动递增")
    void testVersionNumberAutoIncrement() {
        // Given
        String conceptId = "concept-010";

        // When
        ConceptVersion v1 = evolutionService.createVersion(
            conceptId, "v1", "创建", ConceptVersion.ChangeType.CREATE, "user");
        ConceptVersion v2 = evolutionService.createVersion(
            conceptId, "v2", "更新", ConceptVersion.ChangeType.UPDATE, "user");
        ConceptVersion v3 = evolutionService.createVersion(
            conceptId, "v3", "合并", ConceptVersion.ChangeType.MERGE, "user");

        // Then
        assertEquals(1, v1.getVersion());
        assertEquals(2, v2.getVersion());
        assertEquals(3, v3.getVersion());
    }

    @Test
    @DisplayName("父版本设置为非当前")
    void testParentVersionSetToNonCurrent() {
        // Given
        String conceptId = "concept-011";
        ConceptVersion v1 = evolutionService.createVersion(
            conceptId, "v1", "创建", ConceptVersion.ChangeType.CREATE, "user");

        // When
        evolutionService.createVersion(
            conceptId, "v2", "更新", ConceptVersion.ChangeType.UPDATE, "user");

        // Then
        ConceptVersion retrievedV1 = evolutionService.getVersion(v1.getVersionId());
        assertFalse(retrievedV1.isCurrent());
    }

    @Test
    @DisplayName("不同变更类型")
    void testDifferentChangeTypes() {
        // Given
        String conceptId = "concept-012";

        // When
        ConceptVersion create = evolutionService.createVersion(
            conceptId, "v1", "创建", ConceptVersion.ChangeType.CREATE, "user");
        ConceptVersion update = evolutionService.createVersion(
            conceptId, "v2", "更新", ConceptVersion.ChangeType.UPDATE, "user");
        ConceptVersion fix = evolutionService.createVersion(
            conceptId, "v3", "合并", ConceptVersion.ChangeType.MERGE, "user");
        ConceptVersion enhance = evolutionService.createVersion(
            conceptId, "v4", "拆分", ConceptVersion.ChangeType.SPLIT, "user");

        // Then
        assertEquals(ConceptVersion.ChangeType.CREATE, create.getChangeType());
        assertEquals(ConceptVersion.ChangeType.UPDATE, update.getChangeType());
        assertEquals(ConceptVersion.ChangeType.MERGE, fix.getChangeType());
        assertEquals(ConceptVersion.ChangeType.SPLIT, enhance.getChangeType());
    }

    @Test
    @DisplayName("多个概念独立演化")
    void testMultipleConceptsIndependentEvolution() {
        // Given & When
        evolutionService.createVersion("c1", "c1-v1", "创建", ConceptVersion.ChangeType.CREATE, "user");
        evolutionService.createVersion("c1", "c1-v2", "更新", ConceptVersion.ChangeType.UPDATE, "user");
        evolutionService.createVersion("c2", "c2-v1", "创建", ConceptVersion.ChangeType.CREATE, "user");

        // Then
        assertEquals(2, evolutionService.getVersionHistory("c1").size());
        assertEquals(1, evolutionService.getVersionHistory("c2").size());
        assertEquals(2, evolutionService.getCurrentVersion("c1").getVersion());
        assertEquals(1, evolutionService.getCurrentVersion("c2").getVersion());
    }
}

