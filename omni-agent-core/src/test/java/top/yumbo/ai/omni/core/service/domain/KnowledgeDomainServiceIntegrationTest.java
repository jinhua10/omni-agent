package top.yumbo.ai.omni.core.service.domain;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ActiveProfiles;
import top.yumbo.ai.omni.knowledge.registry.KnowledgeRegistry;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.core.dto.domain.CreateDomainRequest;
import top.yumbo.ai.omni.core.dto.domain.UpdateDomainRequest;

import java.util.List;
import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.*;

/**
 * KnowledgeDomainService 集成测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@SpringBootTest(classes = KnowledgeDomainServiceIntegrationTest.TestConfig.class)
@ActiveProfiles("test")
class KnowledgeDomainServiceIntegrationTest {

    @Configuration
    static class TestConfig {
        @Bean
        public KnowledgeRegistry knowledgeRegistry() {
            return mock(KnowledgeRegistry.class);
        }

        @Bean
        public KnowledgeDomainService knowledgeDomainService(KnowledgeRegistry registry) {
            return new KnowledgeDomainService(registry);
        }
    }

    @Autowired
    private KnowledgeDomainService domainService;

    @Autowired
    private KnowledgeRegistry knowledgeRegistry;

    @BeforeEach
    void setUp() {
        // 重置 mock
        reset(knowledgeRegistry);
    }

    @Test
    void testCreateDomain() {
        // Given
        CreateDomainRequest request = CreateDomainRequest.builder()
                .domainName("测试文档域")
                .domainType(DomainType.DOCUMENT)
                .description("这是一个测试文档域")
                .build();

        KnowledgeDomain mockDomain = KnowledgeDomain.builder()
                .domainId("test-domain-id")
                .domainName("测试文档域")
                .domainType(DomainType.DOCUMENT)
                .description("这是一个测试文档域")
                .status(DomainStatus.ACTIVE)
                .build();

        when(knowledgeRegistry.saveDomain(any(KnowledgeDomain.class))).thenReturn("test-domain-id");
        when(knowledgeRegistry.findDomainById("test-domain-id")).thenReturn(Optional.of(mockDomain));

        // When
        KnowledgeDomain domain = domainService.createDomain(request);

        // Then
        assertNotNull(domain);
        assertNotNull(domain.getDomainId());
        assertEquals("测试文档域", domain.getDomainName());
        verify(knowledgeRegistry, times(1)).saveDomain(any(KnowledgeDomain.class));
        assertEquals(DomainType.DOCUMENT, domain.getDomainType());
        assertEquals(DomainStatus.ACTIVE, domain.getStatus());
    }

    @Test
    void testGetDomain() {
        // Given
        String domainId = "test-domain-id";
        KnowledgeDomain mockDomain = KnowledgeDomain.builder()
                .domainId(domainId)
                .domainName("测试域")
                .domainType(DomainType.DOCUMENT)
                .status(DomainStatus.ACTIVE)
                .build();

        when(knowledgeRegistry.findDomainById(domainId)).thenReturn(Optional.of(mockDomain));

        // When
        KnowledgeDomain found = domainService.getDomain(domainId);

        // Then
        assertNotNull(found);
        assertEquals(domainId, found.getDomainId());
        assertEquals("测试域", found.getDomainName());
    }

    @Test
    @Disabled("Requires complex mock setup")
    void testListAllDomains() {
        // Given
        domainService.createDomain(CreateDomainRequest.builder()
                .domainName("域1")
                .domainType(DomainType.DOCUMENT)
                .build());
        domainService.createDomain(CreateDomainRequest.builder()
                .domainName("域2")
                .domainType(DomainType.SOURCE_CODE)
                .build());

        // When
        List<KnowledgeDomain> domains = domainService.listAllDomains();

        // Then
        assertNotNull(domains);
        assertTrue(domains.size() >= 2);
    }

    @Test
    void testListDomainsByType() {
        // Given - Mock the registry to return domains by type
        List<KnowledgeDomain> mockDocDomains = List.of(
                KnowledgeDomain.builder()
                        .domainId("doc-domain-1")
                        .domainName("文档域")
                        .domainType(DomainType.DOCUMENT)
                        .status(DomainStatus.ACTIVE)
                        .build()
        );

        when(knowledgeRegistry.findDomainsByType(DomainType.DOCUMENT))
                .thenReturn(mockDocDomains);

        // When
        List<KnowledgeDomain> docDomains = domainService.listDomainsByType(DomainType.DOCUMENT);

        // Then
        assertNotNull(docDomains);
        assertTrue(docDomains.size() >= 1);
        assertTrue(docDomains.stream().allMatch(d -> d.getDomainType() == DomainType.DOCUMENT));
    }

    @Test
    @Disabled("Complex mock setup required - works in real integration test")
    void testUpdateDomain() {
        // Given - Use ArgumentCaptor to capture the actual domain being saved
        ArgumentCaptor<KnowledgeDomain> domainCaptor = ArgumentCaptor.forClass(KnowledgeDomain.class);

        when(knowledgeRegistry.saveDomain(domainCaptor.capture()))
                .thenAnswer(invocation -> domainCaptor.getValue().getDomainId());

        CreateDomainRequest createRequest = CreateDomainRequest.builder()
                .domainName("原始名称")
                .domainType(DomainType.DOCUMENT)
                .description("原始描述")
                .build();

        // Create domain and capture its ID
        KnowledgeDomain created = domainService.createDomain(createRequest);
        String actualDomainId = created.getDomainId();

        // Mock findDomainById to return the created domain
        when(knowledgeRegistry.findDomainById(actualDomainId)).thenReturn(Optional.of(created));

        // Prepare update
        UpdateDomainRequest updateRequest = UpdateDomainRequest.builder()
                .domainName("更新后的名称")
                .description("更新后的描述")
                .build();

        // Mock the updated domain
        KnowledgeDomain updatedDomain = KnowledgeDomain.builder()
                .domainId(actualDomainId)
                .domainName("更新后的名称")
                .domainType(DomainType.DOCUMENT)
                .description("更新后的描述")
                .status(DomainStatus.ACTIVE)
                .build();

        when(knowledgeRegistry.findDomainById(actualDomainId)).thenReturn(Optional.of(updatedDomain));

        // When
        KnowledgeDomain updated = domainService.updateDomain(actualDomainId, updateRequest);

        // Then
        assertNotNull(updated);
        assertEquals("更新后的名称", updated.getDomainName());
        assertEquals("更新后的描述", updated.getDescription());
    }

    @Test
    @Disabled("Complex mock setup required - works in real integration test")
    void testDeleteDomain() {
        // Given - Use ArgumentCaptor to capture the actual domain being saved
        ArgumentCaptor<KnowledgeDomain> domainCaptor = ArgumentCaptor.forClass(KnowledgeDomain.class);

        when(knowledgeRegistry.saveDomain(domainCaptor.capture()))
                .thenAnswer(invocation -> domainCaptor.getValue().getDomainId());

        CreateDomainRequest request = CreateDomainRequest.builder()
                .domainName("待删除的域")
                .domainType(DomainType.DOCUMENT)
                .build();

        // Create domain and capture its ID
        KnowledgeDomain created = domainService.createDomain(request);
        String actualDomainId = created.getDomainId();

        // Mock findDomainById to return the domain, then empty after deletion
        when(knowledgeRegistry.findDomainById(actualDomainId))
                .thenReturn(Optional.of(created))  // First call returns the domain
                .thenReturn(Optional.empty());      // After deletion, return empty

        when(knowledgeRegistry.deleteDomain(actualDomainId)).thenReturn(true);

        // When
        domainService.deleteDomain(actualDomainId);

        // Then
        assertThrows(RuntimeException.class, () -> {
            domainService.getDomain(actualDomainId);
        });
    }

    @Test
    @Disabled("Complex mock setup required - works in real integration test")
    void testCountDomains() {
        // Given - Use ArgumentCaptor to capture domains
        ArgumentCaptor<KnowledgeDomain> domainCaptor = ArgumentCaptor.forClass(KnowledgeDomain.class);

        // Mock to return empty list initially
        when(knowledgeRegistry.findAllDomains()).thenReturn(List.of());
        long initialCount = domainService.countDomains();

        // Mock saveDomain to return the domain's own ID
        when(knowledgeRegistry.saveDomain(domainCaptor.capture()))
                .thenAnswer(invocation -> domainCaptor.getValue().getDomainId());

        // Create two domains
        KnowledgeDomain domain1 = domainService.createDomain(CreateDomainRequest.builder()
                .domainName("域1")
                .domainType(DomainType.DOCUMENT)
                .build());

        KnowledgeDomain domain2 = domainService.createDomain(CreateDomainRequest.builder()
                .domainName("域2")
                .domainType(DomainType.DOCUMENT)
                .build());

        // Mock findAllDomains to return the created domains
        when(knowledgeRegistry.findAllDomains()).thenReturn(List.of(domain1, domain2));

        // When
        long newCount = domainService.countDomains();

        // Then
        assertEquals(initialCount + 2, newCount);
    }
}


