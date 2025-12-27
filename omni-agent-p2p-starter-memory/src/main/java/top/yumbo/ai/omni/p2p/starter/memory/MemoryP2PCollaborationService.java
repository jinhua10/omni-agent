package top.yumbo.ai.omni.p2p.starter.memory;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.p2p.api.P2PCollaborationService;
import top.yumbo.ai.omni.p2p.api.model.ConnectionCode;
import top.yumbo.ai.omni.p2p.api.model.PeerConnection;
import top.yumbo.ai.omni.p2p.api.model.SharedKnowledge;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 内存P2P协作服务实现
 * (Memory P2P Collaboration Service Implementation)
 *
 * 基于内存的简单实现，适合开发和测试
 * (Memory-based simple implementation, suitable for development and testing)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class MemoryP2PCollaborationService implements P2PCollaborationService {

    // 连接码存储
    private final Map<String, ConnectionCode> connectionCodes = new ConcurrentHashMap<>();

    // 连接存储
    private final Map<String, PeerConnection> connections = new ConcurrentHashMap<>();

    // 用户连接索引
    private final Map<String, Set<String>> userConnections = new ConcurrentHashMap<>();

    // 共享知识存储
    private final Map<String, List<SharedKnowledge>> sharedKnowledge = new ConcurrentHashMap<>();

    @Override
    public ConnectionCode generateConnectionCode(String userId, String userName, int validMinutes) {
        String code = generateRandomCode();

        ConnectionCode connectionCode = ConnectionCode.builder()
                .code(code)
                .initiatorId(userId)
                .initiatorName(userName)
                .createTime(LocalDateTime.now())
                .expiryTime(LocalDateTime.now().plusMinutes(validMinutes))
                .used(false)
                .publicKey("mock-public-key-" + userId)
                .build();

        connectionCodes.put(code, connectionCode);

        log.info("Generated connection code: {} for user: {}", code, userId);

        // 自动清理过期码
        scheduleCleanup(code, validMinutes);

        return connectionCode;
    }

    @Override
    public PeerConnection connectWithCode(String code, String userId, String userName) {
        ConnectionCode connectionCode = connectionCodes.get(code);

        if (connectionCode == null) {
            throw new IllegalArgumentException("Connection code not found");
        }

        if (connectionCode.isUsed()) {
            throw new IllegalArgumentException("Connection code already used");
        }

        if (LocalDateTime.now().isAfter(connectionCode.getExpiryTime())) {
            connectionCodes.remove(code);
            throw new IllegalArgumentException("Connection code expired");
        }

        // 标记为已使用
        connectionCode.setUsed(true);

        // 创建连接
        String connectionId = UUID.randomUUID().toString();
        PeerConnection connection = PeerConnection.builder()
                .connectionId(connectionId)
                .localUserId(userId)
                .remoteUserId(connectionCode.getInitiatorId())
                .remoteUserName(connectionCode.getInitiatorName())
                .status(PeerConnection.ConnectionStatus.ESTABLISHED)
                .establishedTime(LocalDateTime.now())
                .lastActiveTime(LocalDateTime.now())
                .encryptionKey("mock-encryption-key")
                .build();

        connections.put(connectionId, connection);

        // 更新索引
        userConnections.computeIfAbsent(userId, k -> ConcurrentHashMap.newKeySet())
                .add(connectionId);
        userConnections.computeIfAbsent(connectionCode.getInitiatorId(), k -> ConcurrentHashMap.newKeySet())
                .add(connectionId);

        log.info("Established connection: {} between {} and {}",
                connectionId, userId, connectionCode.getInitiatorId());

        return connection;
    }

    @Override
    public boolean disconnect(String connectionId) {
        PeerConnection connection = connections.remove(connectionId);
        if (connection == null) {
            return false;
        }

        // 从索引中移除
        userConnections.getOrDefault(connection.getLocalUserId(), Collections.emptySet())
                .remove(connectionId);
        userConnections.getOrDefault(connection.getRemoteUserId(), Collections.emptySet())
                .remove(connectionId);

        // 清理共享知识
        sharedKnowledge.remove(connectionId);

        log.info("Disconnected connection: {}", connectionId);

        return true;
    }

    @Override
    public List<PeerConnection> getConnections(String userId) {
        Set<String> connectionIds = userConnections.getOrDefault(userId, Collections.emptySet());
        return connectionIds.stream()
                .map(connections::get)
                .filter(Objects::nonNull)
                .filter(c -> c.getStatus() == PeerConnection.ConnectionStatus.ESTABLISHED)
                .collect(Collectors.toList());
    }

    @Override
    public Optional<PeerConnection> getConnection(String connectionId) {
        return Optional.ofNullable(connections.get(connectionId));
    }

    @Override
    public SharedKnowledge shareKnowledge(String connectionId, SharedKnowledge knowledge) {
        if (!connections.containsKey(connectionId)) {
            throw new IllegalArgumentException("Connection not found");
        }

        SharedKnowledge shared = SharedKnowledge.builder()
                .knowledgeId(UUID.randomUUID().toString())
                .sourceUserId(knowledge.getSourceUserId())
                .sourceUserName(knowledge.getSourceUserName())
                .encryptedContent(knowledge.getEncryptedContent())
                .knowledgeType(knowledge.getKnowledgeType())
                .createTime(LocalDateTime.now())
                .qualityScore(knowledge.getQualityScore())
                .verified(false)
                .tags(knowledge.getTags())
                .build();

        sharedKnowledge.computeIfAbsent(connectionId, k -> new ArrayList<>())
                .add(shared);

        log.info("Shared knowledge: {} on connection: {}", shared.getKnowledgeId(), connectionId);

        return shared;
    }

    @Override
    public List<SharedKnowledge> receiveKnowledge(String connectionId) {
        return new ArrayList<>(sharedKnowledge.getOrDefault(connectionId, Collections.emptyList()));
    }

    @Override
    public boolean verifyKnowledge(String knowledgeId, double qualityScore) {
        for (List<SharedKnowledge> list : sharedKnowledge.values()) {
            for (SharedKnowledge k : list) {
                if (k.getKnowledgeId().equals(knowledgeId)) {
                    k.setQualityScore(qualityScore);
                    k.setVerified(true);
                    log.info("Verified knowledge: {} with score: {}", knowledgeId, qualityScore);
                    return true;
                }
            }
        }
        return false;
    }

    @Override
    public Map<String, Object> getSharingStatistics(String userId) {
        Map<String, Object> stats = new HashMap<>();
        stats.put("connections_total", userConnections.getOrDefault(userId, Collections.emptySet()).size());
        stats.put("knowledge_shared", calculateKnowledgeShared(userId));
        stats.put("knowledge_received", calculateKnowledgeReceived(userId));
        return stats;
    }

    @Override
    public String encrypt(String content, String connectionId) {
        // 简单的Base64编码模拟加密
        return Base64.getEncoder().encodeToString(content.getBytes());
    }

    @Override
    public String decrypt(String encryptedContent, String connectionId) {
        // 简单的Base64解码模拟解密
        return new String(Base64.getDecoder().decode(encryptedContent));
    }

    // Helper methods

    private String generateRandomCode() {
        Random random = new Random();
        String letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        StringBuilder code = new StringBuilder();

        for (int i = 0; i < 3; i++) {
            code.append(letters.charAt(random.nextInt(letters.length())));
        }
        code.append("-");
        for (int i = 0; i < 3; i++) {
            code.append(letters.charAt(random.nextInt(letters.length())));
        }
        code.append("-");
        for (int i = 0; i < 3; i++) {
            code.append(random.nextInt(10));
        }

        return code.toString();
    }

    private void scheduleCleanup(String code, int validMinutes) {
        new Thread(() -> {
            try {
                Thread.sleep((validMinutes + 1) * 60 * 1000);
                connectionCodes.remove(code);
                log.info("Cleaned up expired connection code: {}", code);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }).start();
    }

    private long calculateKnowledgeShared(String userId) {
        return sharedKnowledge.values().stream()
                .flatMap(List::stream)
                .filter(k -> k.getSourceUserId().equals(userId))
                .count();
    }

    private long calculateKnowledgeReceived(String userId) {
        Set<String> userConnectionIds = userConnections.getOrDefault(userId, Collections.emptySet());
        return userConnectionIds.stream()
                .flatMap(connId -> sharedKnowledge.getOrDefault(connId, Collections.emptyList()).stream())
                .filter(k -> !k.getSourceUserId().equals(userId))
                .count();
    }
}

