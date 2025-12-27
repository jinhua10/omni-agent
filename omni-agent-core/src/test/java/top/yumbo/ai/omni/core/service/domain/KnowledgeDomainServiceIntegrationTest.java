package top.yumbo.ai.omni.core.service.domain;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import top.yumbo.ai.omni.knowledge.registry.model.DomainStatus;
import top.yumbo.ai.omni.knowledge.registry.model.DomainType;
import top.yumbo.ai.omni.knowledge.registry.model.KnowledgeDomain;
import top.yumbo.ai.omni.core.dto.domain.CreateDomainRequest;
import top.yumbo.ai.omni.core.dto.domain.UpdateDomainRequest;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * KnowledgeDomainService 集成测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@SpringBootTest
@ActiveProfiles("test")
class KnowledgeDomainServiceIntegrationTest {

    @Autowired
    private KnowledgeDomainService domainService;

    @BeforeEach
    void setUp() {
        // 清理测试数据（如果需要）
    }

    @Test
    void testCreateDomain() {
        // Given
        CreateDomainRequest request = CreateDomainRequest.builder()
                .domainName("测试文档域")
                .domainType(DomainType.DOCUMENT)
                .description("这是一个测试文档域")
                .build();

        // When
        KnowledgeDomain domain = domainService.createDomain(request);

        // Then
        assertNotNull(domain);
        assertNotNull(domain.getDomainId());
        assertEquals("测试文档域", domain.getDomainName());
        assertEquals(DomainType.DOCUMENT, domain.getDomainType());
        assertEquals(DomainStatus.ACTIVE, domain.getStatus());
    }

    @Test
    void testGetDomain() {
        // Given
        CreateDomainRequest request = CreateDomainRequest.builder()
                .domainName("测试域")
                .domainType(DomainType.DOCUMENT)
                .build();
        KnowledgeDomain created = domainService.createDomain(request);

        // When
        KnowledgeDomain found = domainService.getDomain(created.getDomainId());

        // Then
        assertNotNull(found);
        assertEquals(created.getDomainId(), found.getDomainId());
        assertEquals(created.getDomainName(), found.getDomainName());
    }

    @Test
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
        // Given
        domainService.createDomain(CreateDomainRequest.builder()
                .domainName("文档域")
                .domainType(DomainType.DOCUMENT)
                .build());
        domainService.createDomain(CreateDomainRequest.builder()
                .domainName("源码域")
                .domainType(DomainType.SOURCE_CODE)
                .build());

        // When
        List<KnowledgeDomain> docDomains = domainService.listDomainsByType(DomainType.DOCUMENT);

        // Then
        assertNotNull(docDomains);
        assertTrue(docDomains.size() >= 1);
        assertTrue(docDomains.stream().allMatch(d -> d.getDomainType() == DomainType.DOCUMENT));
    }

    @Test
    void testUpdateDomain() {
        // Given
        CreateDomainRequest createRequest = CreateDomainRequest.builder()
                .domainName("原始名称")
                .domainType(DomainType.DOCUMENT)
                .description("原始描述")
                .build();
        KnowledgeDomain created = domainService.createDomain(createRequest);

        UpdateDomainRequest updateRequest = UpdateDomainRequest.builder()
                .domainName("更新后的名称")
                .description("更新后的描述")
                .build();

        // When
        KnowledgeDomain updated = domainService.updateDomain(created.getDomainId(), updateRequest);

        // Then
        assertNotNull(updated);
        assertEquals("更新后的名称", updated.getDomainName());
        assertEquals("更新后的描述", updated.getDescription());
    }

    @Test
    void testDeleteDomain() {
        // Given
        CreateDomainRequest request = CreateDomainRequest.builder()
                .domainName("待删除的域")
                .domainType(DomainType.DOCUMENT)
                .build();
        KnowledgeDomain created = domainService.createDomain(request);

        // When
        domainService.deleteDomain(created.getDomainId());

        // Then
        assertThrows(RuntimeException.class, () -> {
            domainService.getDomain(created.getDomainId());
        });
    }

    @Test
    void testCountDomains() {
        // Given
        long initialCount = domainService.countDomains();

        domainService.createDomain(CreateDomainRequest.builder()
                .domainName("域1")
                .domainType(DomainType.DOCUMENT)
                .build());
        domainService.createDomain(CreateDomainRequest.builder()
                .domainName("域2")
                .domainType(DomainType.DOCUMENT)
                .build());

        // When
        long newCount = domainService.countDomains();

        // Then
        assertEquals(initialCount + 2, newCount);
    }
}


