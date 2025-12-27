package top.yumbo.ai.omni.knowledge.registry.file;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;

import java.nio.file.Path;
import java.util.List;
import java.util.Optional;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.*;

/**
 * FileKnowledgeRegistry 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
class FileKnowledgeRegistryTest {

    @TempDir
    Path tempDir;

    private KnowledgeRegistry registry;

    @BeforeEach
    void setUp() {
        registry = new FileKnowledgeRegistry(tempDir.toString(), true);
    }

    @AfterEach
    void tearDown() {
        // Cleanup is handled by @TempDir
    }

    @Test
    void testSaveAndFindDomain() {
        // Given
        KnowledgeDomain domain = createTestDomain();

        // When
        String savedId = registry.saveDomain(domain);
        Optional<KnowledgeDomain> found = registry.findDomainById(savedId);

        // Then
        assertNotNull(savedId);
        assertTrue(found.isPresent());
        assertEquals(domain.getDomainName(), found.get().getDomainName());
        assertEquals(domain.getDomainType(), found.get().getDomainType());
    }

    @Test
    void testFindDomainById_NotFound() {
        // When
        Optional<KnowledgeDomain> found = registry.findDomainById("non-existent-id");

        // Then
        assertFalse(found.isPresent());
    }

    @Test
    void testFindAllDomains() {
        // Given
        registry.saveDomain(createTestDomain("Domain 1", DomainType.DOCUMENT));
        registry.saveDomain(createTestDomain("Domain 2", DomainType.SOURCE_CODE));
        registry.saveDomain(createTestDomain("Domain 3", DomainType.DOCUMENT));

        // When
        List<KnowledgeDomain> domains = registry.findAllDomains();

        // Then
        assertEquals(3, domains.size());
    }

    @Test
    void testFindDomainsByType() {
        // Given
        registry.saveDomain(createTestDomain("Domain 1", DomainType.DOCUMENT));
        registry.saveDomain(createTestDomain("Domain 2", DomainType.SOURCE_CODE));
        registry.saveDomain(createTestDomain("Domain 3", DomainType.DOCUMENT));

        // When
        List<KnowledgeDomain> docDomains = registry.findDomainsByType(DomainType.DOCUMENT);
        List<KnowledgeDomain> codeDomains = registry.findDomainsByType(DomainType.SOURCE_CODE);

        // Then
        assertEquals(2, docDomains.size());
        assertEquals(1, codeDomains.size());
    }

    @Test
    void testFindDomainsByStatus() {
        // Given
        KnowledgeDomain activeDomain = createTestDomain();
        activeDomain.setStatus(DomainStatus.ACTIVE);
        registry.saveDomain(activeDomain);

        KnowledgeDomain inactiveDomain = createTestDomain();
        inactiveDomain.setStatus(DomainStatus.INACTIVE);
        registry.saveDomain(inactiveDomain);

        // When
        List<KnowledgeDomain> activeDomains = registry.findDomainsByStatus(DomainStatus.ACTIVE);
        List<KnowledgeDomain> inactiveDomains = registry.findDomainsByStatus(DomainStatus.INACTIVE);

        // Then
        assertEquals(1, activeDomains.size());
        assertEquals(1, inactiveDomains.size());
    }

    @Test
    void testUpdateDomain() {
        // Given
        KnowledgeDomain domain = createTestDomain();
        String domainId = registry.saveDomain(domain);

        // When
        domain.setDescription("Updated description");
        boolean updated = registry.updateDomain(domain);
        Optional<KnowledgeDomain> found = registry.findDomainById(domainId);

        // Then
        assertTrue(updated);
        assertTrue(found.isPresent());
        assertEquals("Updated description", found.get().getDescription());
    }

    @Test
    void testUpdateDomain_NotFound() {
        // Given
        KnowledgeDomain domain = createTestDomain();

        // When
        boolean updated = registry.updateDomain(domain);

        // Then
        assertFalse(updated);
    }

    @Test
    void testDeleteDomain() {
        // Given
        KnowledgeDomain domain = createTestDomain();
        String domainId = registry.saveDomain(domain);

        // When
        boolean deleted = registry.deleteDomain(domainId);
        Optional<KnowledgeDomain> found = registry.findDomainById(domainId);

        // Then
        assertTrue(deleted);
        assertFalse(found.isPresent());
    }

    @Test
    void testDomainExists() {
        // Given
        KnowledgeDomain domain = createTestDomain();
        String domainId = registry.saveDomain(domain);

        // Then
        assertTrue(registry.domainExists(domainId));
        assertFalse(registry.domainExists("non-existent-id"));
    }

    @Test
    void testCountDomains() {
        // Given
        registry.saveDomain(createTestDomain());
        registry.saveDomain(createTestDomain());
        registry.saveDomain(createTestDomain());

        // When
        long count = registry.countDomains();

        // Then
        assertEquals(3, count);
    }

    @Test
    void testCountDomainsByType() {
        // Given
        registry.saveDomain(createTestDomain("Domain 1", DomainType.DOCUMENT));
        registry.saveDomain(createTestDomain("Domain 2", DomainType.DOCUMENT));
        registry.saveDomain(createTestDomain("Domain 3", DomainType.SOURCE_CODE));

        // When
        long docCount = registry.countDomainsByType(DomainType.DOCUMENT);
        long codeCount = registry.countDomainsByType(DomainType.SOURCE_CODE);

        // Then
        assertEquals(2, docCount);
        assertEquals(1, codeCount);
    }

    // ========== 辅助方法 ==========

    private KnowledgeDomain createTestDomain() {
        return createTestDomain("Test Domain", DomainType.DOCUMENT);
    }

    private KnowledgeDomain createTestDomain(String name, DomainType type) {
        return KnowledgeDomain.builder()
                .domainId(UUID.randomUUID().toString())
                .domainName(name)
                .domainType(type)
                .description("Test description")
                .storagePath("/test/storage")
                .ragIndexPath("/test/rag-index")
                .status(DomainStatus.ACTIVE)
                .build();
    }
}

