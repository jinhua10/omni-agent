package top.yumbo.ai.p2p.core;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import top.yumbo.ai.omni.p2p.api.P2PConnection;
import top.yumbo.ai.omni.p2p.api.P2PEndpointDiscovery;

import java.util.Optional;

import static org.junit.jupiter.api.Assertions.*;

/**
 * DefaultP2PEndpointDiscovery 单元测试
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
class DefaultP2PEndpointDiscoveryTest {

    private DefaultP2PEndpointDiscovery discovery;

    @BeforeEach
    void setUp() {
        discovery = new DefaultP2PEndpointDiscovery();
    }

    @Test
    void testGenerateConnectionCode() {
        // When
        String code = discovery.generateConnectionCode("node1", 1440); // 24 hours

        // Then
        assertNotNull(code);
        assertEquals(8, code.length());
        assertTrue(code.matches("[A-Z0-9]{8}"));
    }

    @Test
    void testGenerateConnectionCode_UniqueForDifferentNodes() {
        // When
        String code1 = discovery.generateConnectionCode("node1", 1440);
        String code2 = discovery.generateConnectionCode("node2", 1440);

        // Then
        assertNotEquals(code1, code2);
    }

    @Test
    void testRegisterEndpoint() {
        // Given
        P2PConnection.EndpointInfo endpoint = new P2PConnection.EndpointInfo(
            "node1", "memory"
        );
        String connectionCode = discovery.generateConnectionCode("node1", 1440);

        // When
        var registration = discovery.registerEndpoint(endpoint, connectionCode);

        // Then
        assertNotNull(registration);
        assertEquals("node1", registration.getEndpointId());
        assertEquals(connectionCode, registration.getConnectionCode());
    }

    @Test
    void testFindEndpoint_Found() {
        // Given
        P2PConnection.EndpointInfo endpoint = new P2PConnection.EndpointInfo("node1", "memory");
        String code = discovery.generateConnectionCode("node1", 1440);
        discovery.registerEndpoint(endpoint, code);

        // When
        Optional<P2PEndpointDiscovery.DiscoveredEndpoint> found = discovery.findEndpoint("node1");

        // Then
        assertTrue(found.isPresent());
        assertEquals("node1", found.get().getEndpointInfo().getEndpointId());
    }

    @Test
    void testFindEndpoint_NotFound() {
        // When
        Optional<P2PEndpointDiscovery.DiscoveredEndpoint> found = discovery.findEndpoint("unknown");

        // Then
        assertFalse(found.isPresent());
    }

    @Test
    void testValidateConnectionCode() {
        // Given
        P2PConnection.EndpointInfo endpoint = new P2PConnection.EndpointInfo("node1", "memory");
        String code = discovery.generateConnectionCode("node1", 1440);
        discovery.registerEndpoint(endpoint, code);

        // When
        boolean isValid = discovery.validateConnectionCode("node1", code);

        // Then
        assertTrue(isValid);
    }

    @Test
    void testValidateConnectionCode_Invalid() {
        // When
        boolean isValid = discovery.validateConnectionCode("node1", "INVALID");

        // Then
        assertFalse(isValid);
    }

    @Test
    void testScanEndpoints() {
        // Given
        P2PConnection.EndpointInfo endpoint1 = new P2PConnection.EndpointInfo("node1", "memory");
        String code1 = discovery.generateConnectionCode("node1", 1440);
        discovery.registerEndpoint(endpoint1, code1);

        // When
        var endpoints = discovery.scanEndpoints(null);

        // Then
        assertNotNull(endpoints);
        assertFalse(endpoints.isEmpty());
    }

    @Test
    void testMultipleEndpointRegistration() {
        // Given
        P2PConnection.EndpointInfo endpoint1 = new P2PConnection.EndpointInfo("node1", "memory");
        P2PConnection.EndpointInfo endpoint2 = new P2PConnection.EndpointInfo("node2", "h2");

        String code1 = discovery.generateConnectionCode("node1", 1440);
        String code2 = discovery.generateConnectionCode("node2", 1440);

        // When
        var reg1 = discovery.registerEndpoint(endpoint1, code1);
        var reg2 = discovery.registerEndpoint(endpoint2, code2);

        // Then
        assertNotNull(reg1);
        assertNotNull(reg2);
    }

    @Test
    void testConnectionCodeGeneration_Format() {
        // When
        String code = discovery.generateConnectionCode("test", 1440);

        // Then
        assertNotNull(code);
        assertTrue(code.matches("[A-Z0-9]{8}"), "Code should be 8 alphanumeric characters");
    }
}


