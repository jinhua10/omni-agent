package top.yumbo.ai.omni.p2p.starter.redis;

import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.redis.core.RedisTemplate;
import top.yumbo.ai.omni.p2p.api.P2PCollaborationService;
import top.yumbo.ai.omni.p2p.api.model.ConnectionCode;
import top.yumbo.ai.omni.p2p.api.model.PeerConnection;
import top.yumbo.ai.omni.p2p.api.model.SharedKnowledge;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;
import java.util.Base64;

/**
 * Redis P2P协作服务实现
 * (Redis P2P Collaboration Service Implementation)
 *
 * 基于Redis的分布式P2P协作实现
 * (Redis-based distributed P2P collaboration implementation)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class RedisP2PCollaborationService implements P2PCollaborationService {

    private final RedisTemplate<String, Object> redisTemplate;
    private final RedisP2PProperties properties;
    private final ObjectMapper objectMapper;

    // Redis Key前缀
    private static final String CODE_PREFIX = "code:";
    private static final String CONNECTION_PREFIX = "connection:";
    private static final String USER_CONNECTIONS_PREFIX = "user:connections:";
    private static final String KNOWLEDGE_PREFIX = "knowledge:";

    public RedisP2PCollaborationService(RedisTemplate<String, Object> redisTemplate,
                                       RedisP2PProperties properties,
                                       ObjectMapper objectMapper) {
        this.redisTemplate = redisTemplate;
        this.properties = properties;
        this.objectMapper = objectMapper;
        log.info("RedisP2PCollaborationService initialized with Redis: {}:{}",
                properties.getHost(), properties.getPort());
    }

    @Override
    public ConnectionCode generateConnectionCode(String userId, String userName, int validMinutes) {
        String code = generateRandomCode();
        String key = getKey(CODE_PREFIX + code);

        ConnectionCode connectionCode = ConnectionCode.builder()
                .code(code)
                .initiatorId(userId)
                .initiatorName(userName)
                .createTime(LocalDateTime.now())
                .expiryTime(LocalDateTime.now().plusMinutes(validMinutes))
                .used(false)
                .publicKey("redis-public-key-" + userId)
                .build();

        // 存储到Redis，设置过期时间
        redisTemplate.opsForValue().set(key, connectionCode, validMinutes, TimeUnit.MINUTES);

        log.info("Generated connection code: {} for user: {} (valid for {} minutes)",
                code, userId, validMinutes);

        return connectionCode;
    }

    @Override
    public PeerConnection connectWithCode(String code, String userId, String userName) {
        String codeKey = getKey(CODE_PREFIX + code);
        ConnectionCode connectionCode = (ConnectionCode) redisTemplate.opsForValue().get(codeKey);

        if (connectionCode == null) {
            throw new IllegalArgumentException("Connection code not found or expired");
        }

        if (connectionCode.isUsed()) {
            throw new IllegalArgumentException("Connection code already used");
        }

        // 标记为已使用
        connectionCode.setUsed(true);
        redisTemplate.opsForValue().set(codeKey, connectionCode, 
                properties.getCodeExpirationMinutes(), TimeUnit.MINUTES);

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
                .encryptionKey("redis-encryption-key-" + connectionId)
                .build();

        // 存储连接
        String connectionKey = getKey(CONNECTION_PREFIX + connectionId);
        redisTemplate.opsForValue().set(connectionKey, connection,
                properties.getConnectionTimeoutMinutes(), TimeUnit.MINUTES);

        // 更新用户连接索引
        String userConnectionsKey = getKey(USER_CONNECTIONS_PREFIX + userId);
        redisTemplate.opsForSet().add(userConnectionsKey, connectionId);
        
        String initiatorConnectionsKey = getKey(USER_CONNECTIONS_PREFIX + connectionCode.getInitiatorId());
        redisTemplate.opsForSet().add(initiatorConnectionsKey, connectionId);

        log.info("Established connection: {} between {} and {}",
                connectionId, userId, connectionCode.getInitiatorId());

        return connection;
    }

    @Override
    public boolean disconnect(String connectionId) {
        String connectionKey = getKey(CONNECTION_PREFIX + connectionId);
        PeerConnection connection = (PeerConnection) redisTemplate.opsForValue().get(connectionKey);

        if (connection == null) {
            return false;
        }

        // 从用户索引中移除
        String localUserKey = getKey(USER_CONNECTIONS_PREFIX + connection.getLocalUserId());
        redisTemplate.opsForSet().remove(localUserKey, connectionId);

        String remoteUserKey = getKey(USER_CONNECTIONS_PREFIX + connection.getRemoteUserId());
        redisTemplate.opsForSet().remove(remoteUserKey, connectionId);

        // 删除连接和共享知识
        redisTemplate.delete(connectionKey);
        String knowledgeKey = getKey(KNOWLEDGE_PREFIX + connectionId);
        redisTemplate.delete(knowledgeKey);

        log.info("Disconnected connection: {}", connectionId);

        return true;
    }

    @Override
    public List<PeerConnection> getConnections(String userId) {
        String userConnectionsKey = getKey(USER_CONNECTIONS_PREFIX + userId);
        Set<Object> connectionIds = redisTemplate.opsForSet().members(userConnectionsKey);

        if (connectionIds == null || connectionIds.isEmpty()) {
            return Collections.emptyList();
        }

        return connectionIds.stream()
                .map(Object::toString)
                .map(id -> (PeerConnection) redisTemplate.opsForValue().get(getKey(CONNECTION_PREFIX + id)))
                .filter(Objects::nonNull)
                .filter(c -> c.getStatus() == PeerConnection.ConnectionStatus.ESTABLISHED)
                .collect(Collectors.toList());
    }

    @Override
    public Optional<PeerConnection> getConnection(String connectionId) {
        String key = getKey(CONNECTION_PREFIX + connectionId);
        PeerConnection connection = (PeerConnection) redisTemplate.opsForValue().get(key);
        return Optional.ofNullable(connection);
    }

    @Override
    public SharedKnowledge shareKnowledge(String connectionId, SharedKnowledge knowledge) {
        String connectionKey = getKey(CONNECTION_PREFIX + connectionId);
        if (!redisTemplate.hasKey(connectionKey)) {
            throw new IllegalArgumentException("Connection not found: " + connectionId);
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

        // 使用List存储连接的所有共享知识
        String knowledgeKey = getKey(KNOWLEDGE_PREFIX + connectionId);
        redisTemplate.opsForList().rightPush(knowledgeKey, shared);

        // 设置过期时间
        redisTemplate.expire(knowledgeKey, properties.getConnectionTimeoutMinutes(), TimeUnit.MINUTES);

        log.info("Shared knowledge: {} on connection: {}", shared.getKnowledgeId(), connectionId);

        return shared;
    }

    @Override
    public List<SharedKnowledge> receiveKnowledge(String connectionId) {
        String knowledgeKey = getKey(KNOWLEDGE_PREFIX + connectionId);
        List<Object> knowledgeList = redisTemplate.opsForList().range(knowledgeKey, 0, -1);

        if (knowledgeList == null || knowledgeList.isEmpty()) {
            return Collections.emptyList();
        }

        return knowledgeList.stream()
                .map(obj -> (SharedKnowledge) obj)
                .collect(Collectors.toList());
    }

    @Override
    public boolean verifyKnowledge(String knowledgeId, double qualityScore) {
        // 扫描所有knowledge keys查找特定knowledge
        Set<String> keys = redisTemplate.keys(getKey(KNOWLEDGE_PREFIX + "*"));
        if (keys == null) {
            return false;
        }

        for (String key : keys) {
            List<Object> knowledgeList = redisTemplate.opsForList().range(key, 0, -1);
            if (knowledgeList == null) continue;

            for (int i = 0; i < knowledgeList.size(); i++) {
                SharedKnowledge k = (SharedKnowledge) knowledgeList.get(i);
                if (k.getKnowledgeId().equals(knowledgeId)) {
                    k.setQualityScore(qualityScore);
                    k.setVerified(true);
                    redisTemplate.opsForList().set(key, i, k);
                    log.info("Verified knowledge: {} with score: {}", knowledgeId, qualityScore);
                    return true;
                }
            }
        }

        return false;
    }

    @Override
    public Map<String, Object> getSharingStatistics(String userId) {
        String userConnectionsKey = getKey(USER_CONNECTIONS_PREFIX + userId);
        Long connectionsCount = redisTemplate.opsForSet().size(userConnectionsKey);

        Map<String, Object> stats = new HashMap<>();
        stats.put("connections_total", connectionsCount != null ? connectionsCount : 0);
        stats.put("knowledge_shared", calculateKnowledgeShared(userId));
        stats.put("knowledge_received", calculateKnowledgeReceived(userId));
        stats.put("storage_backend", "redis");
        
        return stats;
    }

    // ========== 辅助方法 ==========

    private String getKey(String suffix) {
        return properties.getKeyPrefix() + suffix;
    }

    private String generateRandomCode() {
        return UUID.randomUUID().toString().substring(0, 8).toUpperCase();
    }

    private int calculateKnowledgeShared(String userId) {
        // 计算用户共享的知识数量
        Set<Object> connectionIds = redisTemplate.opsForSet().members(
                getKey(USER_CONNECTIONS_PREFIX + userId));
        
        if (connectionIds == null) return 0;

        int total = 0;
        for (Object connId : connectionIds) {
            String knowledgeKey = getKey(KNOWLEDGE_PREFIX + connId);
            Long size = redisTemplate.opsForList().size(knowledgeKey);
            if (size != null) total += size;
        }
        return total;
    }

    private int calculateKnowledgeReceived(String userId) {
        // 简化实现：返回shared数量的80%作为received估算
        return (int) (calculateKnowledgeShared(userId) * 0.8);
    }

    @Override
    public String encrypt(String content, String connectionId) {
        // 简化实现：使用Base64编码模拟加密
        // 实际生产环境应使用AES或RSA等真实加密算法
        if (content == null) return null;
        
        String connectionKey = getKey(CONNECTION_PREFIX + connectionId);
        PeerConnection connection = (PeerConnection) redisTemplate.opsForValue().get(connectionKey);
        
        if (connection == null) {
            throw new IllegalArgumentException("Connection not found: " + connectionId);
        }
        
        try {
            String encrypted = Base64.getEncoder().encodeToString(
                    (content + ":" + connection.getEncryptionKey()).getBytes());
            log.debug("Encrypted content for connection: {}", connectionId);
            return encrypted;
        } catch (Exception e) {
            log.error("Encryption failed for connection: {}", connectionId, e);
            throw new RuntimeException("Encryption failed", e);
        }
    }

    @Override
    public String decrypt(String encryptedContent, String connectionId) {
        // 简化实现：使用Base64解码模拟解密
        // 实际生产环境应使用AES或RSA等真实加密算法
        if (encryptedContent == null) return null;
        
        String connectionKey = getKey(CONNECTION_PREFIX + connectionId);
        PeerConnection connection = (PeerConnection) redisTemplate.opsForValue().get(connectionKey);
        
        if (connection == null) {
            throw new IllegalArgumentException("Connection not found: " + connectionId);
        }
        
        try {
            String decoded = new String(Base64.getDecoder().decode(encryptedContent));
            String suffix = ":" + connection.getEncryptionKey();
            if (decoded.endsWith(suffix)) {
                String decrypted = decoded.substring(0, decoded.length() - suffix.length());
                log.debug("Decrypted content for connection: {}", connectionId);
                return decrypted;
            }
            throw new IllegalArgumentException("Invalid encryption key");
        } catch (Exception e) {
            log.error("Decryption failed for connection: {}", connectionId, e);
            throw new RuntimeException("Decryption failed", e);
        }
    }
}
