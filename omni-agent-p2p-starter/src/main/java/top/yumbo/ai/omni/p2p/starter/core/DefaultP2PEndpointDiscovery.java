package top.yumbo.ai.omni.p2p.starter.core;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import top.yumbo.ai.omni.p2p.api.P2PConnection;
import top.yumbo.ai.omni.p2p.api.P2PEndpointDiscovery;

import java.security.SecureRandom;
import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * P2P端点发现服务实现
 * (P2P Endpoint Discovery Service Implementation)
 *
 * <p>支持端点注册、发现和连接码验证</p>
 * <p>Supports endpoint registration, discovery, and connection code validation</p>
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Component
public class DefaultP2PEndpointDiscovery implements P2PEndpointDiscovery {

    /**
     * 端点注册表
     * Key: endpointId
     * Value: 注册信息
     */
    private final Map<String, RegisteredEndpoint> endpointRegistry = new ConcurrentHashMap<>();

    /**
     * 连接码存储
     * Key: endpointId
     * Value: ConnectionCodeEntry
     */
    private final Map<String, ConnectionCodeEntry> connectionCodes = new ConcurrentHashMap<>();

    /**
     * 安全随机数生成器
     */
    private final SecureRandom secureRandom = new SecureRandom();

    @Override
    public EndpointRegistration registerEndpoint(P2PConnection.EndpointInfo endpoint, String connectionCode) {
        log.info("注册端点到网络: {} ({})", endpoint.getEndpointId(), endpoint.getStorageType());

        // 验证连接码
        if (connectionCode == null || connectionCode.length() < 6) {
            throw new IllegalArgumentException("连接码必须至少6位: Connection code must be at least 6 characters");
        }

        // 生成注册信息
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime expiresAt = now.plusHours(24); // 默认24小时有效期

        // 创建注册条目
        RegisteredEndpoint registered = new RegisteredEndpoint(
                endpoint,
                EndpointStatus.ONLINE,
                now,
                expiresAt,
                new HashMap<>()
        );

        endpointRegistry.put(endpoint.getEndpointId(), registered);

        // 存储连接码（加密存储）
        String hashedCode = hashConnectionCode(connectionCode);
        ConnectionCodeEntry codeEntry = new ConnectionCodeEntry(
                hashedCode,
                endpoint.getEndpointId(),
                now,
                expiresAt
        );
        connectionCodes.put(endpoint.getEndpointId(), codeEntry);

        log.info("✓ 端点注册成功: {} (有效期至: {})", endpoint.getEndpointId(), expiresAt);

        return new EndpointRegistration(
                endpoint.getEndpointId(),
                connectionCode,
                now,
                expiresAt,
                "local-registry"
        );
    }

    @Override
    public boolean unregisterEndpoint(String endpointId) {
        log.info("注销端点: {}", endpointId);
        
        RegisteredEndpoint removed = endpointRegistry.remove(endpointId);
        connectionCodes.remove(endpointId);
        
        boolean success = removed != null;
        if (success) {
            log.info("✓ 端点注销成功: {}", endpointId);
        } else {
            log.warn("端点不存在: {}", endpointId);
        }
        
        return success;
    }

    @Override
    public List<DiscoveredEndpoint> scanEndpoints(EndpointFilter filter) {
        log.info("扫描网络端点 (过滤器: {})", filter != null ? "已应用" : "无");

        // 清理过期端点
        cleanupExpiredEndpoints();

        // 获取所有在线端点
        return endpointRegistry.values().stream()
                .filter(reg -> reg.status == EndpointStatus.ONLINE)
                .filter(reg -> matchesFilter(reg, filter))
                .map(reg -> new DiscoveredEndpoint(
                        reg.endpointInfo,
                        reg.status,
                        reg.registeredAt,
                        reg.capabilities,
                        connectionCodes.containsKey(reg.endpointInfo.getEndpointId())
                ))
                .collect(Collectors.toList());
    }

    @Override
    public Optional<DiscoveredEndpoint> findEndpoint(String endpointId) {
        log.debug("查找端点: {}", endpointId);

        RegisteredEndpoint registered = endpointRegistry.get(endpointId);
        if (registered == null) {
            return Optional.empty();
        }

        // 检查是否过期
        if (LocalDateTime.now().isAfter(registered.expiresAt)) {
            log.warn("端点已过期: {}", endpointId);
            endpointRegistry.remove(endpointId);
            return Optional.empty();
        }

        DiscoveredEndpoint discovered = new DiscoveredEndpoint(
                registered.endpointInfo,
                registered.status,
                registered.registeredAt,
                registered.capabilities,
                connectionCodes.containsKey(endpointId)
        );

        return Optional.of(discovered);
    }

    @Override
    public boolean validateConnectionCode(String endpointId, String connectionCode) {
        log.debug("验证连接码: {}", endpointId);

        ConnectionCodeEntry entry = connectionCodes.get(endpointId);
        if (entry == null) {
            log.warn("端点没有连接码: {}", endpointId);
            return false;
        }

        // 检查是否过期
        if (LocalDateTime.now().isAfter(entry.expiresAt)) {
            log.warn("连接码已过期: {}", endpointId);
            connectionCodes.remove(endpointId);
            return false;
        }

        // 验证连接码
        String hashedInput = hashConnectionCode(connectionCode);
        boolean valid = entry.hashedCode.equals(hashedInput);

        if (valid) {
            log.info("✓ 连接码验证成功: {}", endpointId);
        } else {
            log.warn("✗ 连接码验证失败: {}", endpointId);
        }

        return valid;
    }

    @Override
    public String generateConnectionCode(String endpointId, int validityMinutes) {
        log.info("生成连接码: {} (有效期: {}分钟)", endpointId, validityMinutes);

        // 生成8位随机连接码
        String code = generateRandomCode(8);

        // 存储连接码
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime expiresAt = now.plusMinutes(validityMinutes);

        String hashedCode = hashConnectionCode(code);
        ConnectionCodeEntry entry = new ConnectionCodeEntry(
                hashedCode,
                endpointId,
                now,
                expiresAt
        );

        connectionCodes.put(endpointId, entry);

        log.info("✓ 连接码生成成功: {} (有效期至: {})", endpointId, expiresAt);

        return code;
    }

    @Override
    public Optional<DiscoveredEndpoint> findEndpointByIp(String ipAddress, int port) {
        log.info("通过 IP 查找端点: {}:{}", ipAddress, port);

        // 清理过期端点
        cleanupExpiredEndpoints();

        // 在注册表中查找匹配 IP 和端口的端点
        Optional<RegisteredEndpoint> found = endpointRegistry.values().stream()
                .filter(reg -> ipAddress.equals(reg.endpointInfo.getHost()))
                .filter(reg -> port == reg.endpointInfo.getPort())
                .filter(reg -> reg.status == EndpointStatus.ONLINE)
                .findFirst();

        if (found.isEmpty()) {
            log.warn("未找到端点: {}:{}", ipAddress, port);
            return Optional.empty();
        }

        RegisteredEndpoint registered = found.get();
        DiscoveredEndpoint discovered = new DiscoveredEndpoint(
                registered.endpointInfo,
                registered.status,
                registered.registeredAt,
                registered.capabilities,
                connectionCodes.containsKey(registered.endpointInfo.getEndpointId())
        );

        log.info("✓ 找到端点: {} ({}:{})", registered.endpointInfo.getEndpointId(), ipAddress, port);
        return Optional.of(discovered);
    }

    @Override
    public boolean validateRemoteConnectionCode(String ipAddress, int port, String connectionCode) {
        log.info("验证远程端点连接码: {}:{}", ipAddress, port);

        // 首先通过 IP 查找端点
        Optional<DiscoveredEndpoint> endpoint = findEndpointByIp(ipAddress, port);
        if (endpoint.isEmpty()) {
            log.warn("端点不存在: {}:{}", ipAddress, port);
            return false;
        }

        String endpointId = endpoint.get().getEndpointInfo().getEndpointId();

        // 验证连接码
        return validateConnectionCode(endpointId, connectionCode);
    }

    /**
     * 清理过期端点
     */
    private void cleanupExpiredEndpoints() {
        LocalDateTime now = LocalDateTime.now();
        
        // 清理过期的端点注册
        endpointRegistry.entrySet().removeIf(entry -> 
            now.isAfter(entry.getValue().expiresAt)
        );

        // 清理过期的连接码
        connectionCodes.entrySet().removeIf(entry ->
            now.isAfter(entry.getValue().expiresAt)
        );
    }

    /**
     * 检查端点是否匹配过滤条件
     */
    private boolean matchesFilter(RegisteredEndpoint registered, EndpointFilter filter) {
        if (filter == null) {
            return true;
        }

        // 存储类型过滤
        if (filter.getStorageType() != null && 
            !filter.getStorageType().equalsIgnoreCase(registered.endpointInfo.getStorageType())) {
            return false;
        }

        // 网络段过滤（简化实现）
        if (filter.getNetworkSegment() != null && registered.endpointInfo.getHost() != null) {
            if (!registered.endpointInfo.getHost().startsWith(filter.getNetworkSegment().split("/")[0].substring(0, 10))) {
                return false;
            }
        }

        // 认证要求过滤
        if (filter.getRequiresAuth() != null) {
            boolean hasAuth = connectionCodes.containsKey(registered.endpointInfo.getEndpointId());
            if (filter.getRequiresAuth() != hasAuth) {
                return false;
            }
        }

        return true;
    }

    /**
     * 生成随机连接码
     */
    private String generateRandomCode(int length) {
        String chars = "ABCDEFGHJKLMNPQRSTUVWXYZ23456789"; // 排除易混淆字符
        StringBuilder code = new StringBuilder();
        
        for (int i = 0; i < length; i++) {
            int index = secureRandom.nextInt(chars.length());
            code.append(chars.charAt(index));
        }
        
        return code.toString();
    }

    /**
     * 哈希连接码（简化实现，生产环境应使用BCrypt或Argon2）
     */
    private String hashConnectionCode(String code) {
        return Integer.toHexString(code.hashCode());
    }

    /**
     * 注册端点内部类
     */
    private static class RegisteredEndpoint {
        final P2PConnection.EndpointInfo endpointInfo;
        final EndpointStatus status;
        final LocalDateTime registeredAt;
        final LocalDateTime expiresAt;
        final Map<String, Object> capabilities;

        RegisteredEndpoint(P2PConnection.EndpointInfo endpointInfo,
                          EndpointStatus status,
                          LocalDateTime registeredAt,
                          LocalDateTime expiresAt,
                          Map<String, Object> capabilities) {
            this.endpointInfo = endpointInfo;
            this.status = status;
            this.registeredAt = registeredAt;
            this.expiresAt = expiresAt;
            this.capabilities = capabilities;
        }
    }

    /**
     * 连接码条目
     */
    private static class ConnectionCodeEntry {
        final String hashedCode;
        final String endpointId;
        final LocalDateTime createdAt;
        final LocalDateTime expiresAt;

        ConnectionCodeEntry(String hashedCode, String endpointId,
                           LocalDateTime createdAt, LocalDateTime expiresAt) {
            this.hashedCode = hashedCode;
            this.endpointId = endpointId;
            this.createdAt = createdAt;
            this.expiresAt = expiresAt;
        }
    }
}


