package top.yumbo.ai.omni.core.p2p;

import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import top.yumbo.ai.p2p.api.P2PCollaborationService;
import top.yumbo.ai.p2p.api.model.ConnectionCode;
import top.yumbo.ai.p2p.api.model.PeerConnection;
import top.yumbo.ai.p2p.api.model.SharedKnowledge;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * P2P协作管理器
 * (P2P Collaboration Manager)
 *
 * 管理P2P连接和知识共享
 * (Manages P2P connections and knowledge sharing)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class P2PCollaborationManager implements P2PCollaborationService {

    @Autowired
    private ConnectionCodeGenerator codeGenerator;

    @Autowired
    private P2PEncryptionHandler encryptionHandler;

    /**
     * 连接存储
     * Key: connectionId, Value: PeerConnection
     */
    private final Map<String, PeerConnection> connections = new ConcurrentHashMap<>();

    /**
     * 用户连接索引
     * Key: userId, Value: Set of connectionIds
     */
    private final Map<String, Set<String>> userConnections = new ConcurrentHashMap<>();

    /**
     * 共享知识存储
     * Key: connectionId, Value: List of SharedKnowledge
     */
    private final Map<String, List<SharedKnowledge>> sharedKnowledge = new ConcurrentHashMap<>();

    /**
     * 统计数据
     */
    private final Map<String, Map<String, Long>> statistics = new ConcurrentHashMap<>();

    @Override
    public ConnectionCode generateConnectionCode(String userId, String userName, int validMinutes) {
        // 生成RSA密钥对
        String publicKey = encryptionHandler.generateKeyPair(userId);

        // 生成连接码
        String code = codeGenerator.generate(userId, userName, publicKey);

        // 构建返回对象
        return ConnectionCode.builder()
                .code(code)
                .initiatorId(userId)
                .initiatorName(userName)
                .createTime(LocalDateTime.now())
                .expiryTime(LocalDateTime.now().plusMinutes(validMinutes))
                .used(false)
                .publicKey(publicKey)
                .build();
    }

    @Override
    public PeerConnection connectWithCode(String code, String userId, String userName) {
        // 验证连接码
        ConnectionCodeGenerator.CodeInfo codeInfo = codeGenerator.validate(code);
        if (codeInfo == null) {
            throw new IllegalArgumentException("Invalid or expired connection code");
        }

        // 标记为已使用
        codeGenerator.markUsed(code);

        // 生成连接ID
        String connectionId = UUID.randomUUID().toString();

        // 生成本地用户的密钥对
        String localPublicKey = encryptionHandler.generateKeyPair(userId);

        // 生成会话密钥
        String sessionKey = encryptionHandler.generateSessionKey(connectionId);

        // 使用对方公钥加密会话密钥
        String encryptedSessionKey = encryptionHandler.encryptSessionKey(
            sessionKey, codeInfo.publicKey
        );

        // 存储会话密钥
        encryptionHandler.storeSessionKey(connectionId, sessionKey);

        // 创建连接
        PeerConnection connection = PeerConnection.builder()
                .connectionId(connectionId)
                .localUserId(userId)
                .remoteUserId(codeInfo.userId)
                .remoteUserName(codeInfo.userName)
                .status(PeerConnection.ConnectionStatus.ESTABLISHED)
                .establishedTime(LocalDateTime.now())
                .lastActiveTime(LocalDateTime.now())
                .encryptionKey(encryptedSessionKey)
                .build();

        // 存储连接
        connections.put(connectionId, connection);
        userConnections.computeIfAbsent(userId, k -> ConcurrentHashMap.newKeySet())
                .add(connectionId);
        userConnections.computeIfAbsent(codeInfo.userId, k -> ConcurrentHashMap.newKeySet())
                .add(connectionId);

        log.info("Established P2P connection: {} between {} and {}",
                connectionId, userId, codeInfo.userId);

        // 更新统计
        updateStatistics(userId, "connections_established", 1);

        return connection;
    }

    @Override
    public boolean disconnect(String connectionId) {
        PeerConnection connection = connections.get(connectionId);
        if (connection == null) {
            return false;
        }

        // 更新状态
        connection.setStatus(PeerConnection.ConnectionStatus.DISCONNECTED);

        // 清理加密密钥
        encryptionHandler.clearConnectionKey(connectionId);

        // 从索引中移除
        userConnections.getOrDefault(connection.getLocalUserId(), Collections.emptySet())
                .remove(connectionId);
        userConnections.getOrDefault(connection.getRemoteUserId(), Collections.emptySet())
                .remove(connectionId);

        // 移除连接
        connections.remove(connectionId);

        // 清理共享知识
        sharedKnowledge.remove(connectionId);

        log.info("Disconnected P2P connection: {}", connectionId);

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
        PeerConnection connection = connections.get(connectionId);
        if (connection == null) {
            throw new IllegalArgumentException("Connection not found: " + connectionId);
        }

        // 加密知识内容
        String encryptedContent = encryptionHandler.encrypt(
            knowledge.getEncryptedContent(), connectionId
        );

        // 更新知识
        SharedKnowledge encrypted = SharedKnowledge.builder()
                .knowledgeId(UUID.randomUUID().toString())
                .sourceUserId(knowledge.getSourceUserId())
                .sourceUserName(knowledge.getSourceUserName())
                .encryptedContent(encryptedContent)
                .knowledgeType(knowledge.getKnowledgeType())
                .createTime(LocalDateTime.now())
                .qualityScore(knowledge.getQualityScore())
                .verified(false)
                .tags(knowledge.getTags())
                .build();

        // 存储
        sharedKnowledge.computeIfAbsent(connectionId, k -> new ArrayList<>())
                .add(encrypted);

        // 更新统计
        updateStatistics(knowledge.getSourceUserId(), "knowledge_shared", 1);

        log.info("Shared knowledge: {} on connection: {}", encrypted.getKnowledgeId(), connectionId);

        return encrypted;
    }

    @Override
    public List<SharedKnowledge> receiveKnowledge(String connectionId) {
        List<SharedKnowledge> knowledgeList = sharedKnowledge.getOrDefault(
            connectionId, Collections.emptyList()
        );

        // 解密知识
        return knowledgeList.stream()
                .map(k -> {
                    try {
                        String decrypted = encryptionHandler.decrypt(
                            k.getEncryptedContent(), connectionId
                        );
                        return SharedKnowledge.builder()
                                .knowledgeId(k.getKnowledgeId())
                                .sourceUserId(k.getSourceUserId())
                                .sourceUserName(k.getSourceUserName())
                                .encryptedContent(decrypted)
                                .knowledgeType(k.getKnowledgeType())
                                .createTime(k.getCreateTime())
                                .qualityScore(k.getQualityScore())
                                .verified(k.isVerified())
                                .tags(k.getTags())
                                .build();
                    } catch (Exception e) {
                        log.error("Failed to decrypt knowledge: {}", k.getKnowledgeId(), e);
                        return null;
                    }
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    @Override
    public boolean verifyKnowledge(String knowledgeId, double qualityScore) {
        // 在所有连接的知识中查找
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
        Map<String, Long> userStats = statistics.getOrDefault(userId, new HashMap<>());

        Map<String, Object> stats = new HashMap<>();
        stats.put("connections_total", userConnections.getOrDefault(userId, Collections.emptySet()).size());
        stats.put("connections_established", userStats.getOrDefault("connections_established", 0L));
        stats.put("knowledge_shared", userStats.getOrDefault("knowledge_shared", 0L));
        stats.put("knowledge_received", userStats.getOrDefault("knowledge_received", 0L));

        return stats;
    }

    @Override
    public String encrypt(String content, String connectionId) {
        return encryptionHandler.encrypt(content, connectionId);
    }

    @Override
    public String decrypt(String encryptedContent, String connectionId) {
        return encryptionHandler.decrypt(encryptedContent, connectionId);
    }

    /**
     * 更新统计
     */
    private void updateStatistics(String userId, String metric, long increment) {
        statistics.computeIfAbsent(userId, k -> new ConcurrentHashMap<>())
                .merge(metric, increment, Long::sum);
    }
}

