package top.yumbo.ai.omni.p2p.starter.elasticsearch;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.query.Criteria;
import org.springframework.data.elasticsearch.core.query.CriteriaQuery;
import org.springframework.data.elasticsearch.core.query.Query;
import top.yumbo.ai.omni.p2p.api.P2PCollaborationService;
import top.yumbo.ai.omni.p2p.api.model.ConnectionCode;
import top.yumbo.ai.omni.p2p.api.model.PeerConnection;
import top.yumbo.ai.omni.p2p.api.model.SharedKnowledge;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public class ElasticsearchP2PCollaborationService implements P2PCollaborationService {

    private final ElasticsearchOperations elasticsearchOperations;
    private final ElasticsearchP2PProperties properties;

    public ElasticsearchP2PCollaborationService(ElasticsearchOperations elasticsearchOperations,
                                               ElasticsearchP2PProperties properties) {
        this.elasticsearchOperations = elasticsearchOperations;
        this.properties = properties;
        log.info("ElasticsearchP2PCollaborationService initialized");
    }

    @Override
    public ConnectionCode generateConnectionCode(String userId, String userName, int validMinutes) {
        String code = UUID.randomUUID().toString().substring(0, 8).toUpperCase();
        ConnectionCode connectionCode = ConnectionCode.builder()
                .code(code)
                .initiatorId(userId)
                .initiatorName(userName)
                .createTime(LocalDateTime.now())
                .expiryTime(LocalDateTime.now().plusMinutes(validMinutes))
                .used(false)
                .publicKey("es-public-key-" + userId)
                .build();
        
        elasticsearchOperations.save(connectionCode);
        log.info("Generated connection code: {}", code);
        return connectionCode;
    }

    @Override
    public PeerConnection connectWithCode(String code, String userId, String userName) {
        Query query = new CriteriaQuery(
            Criteria.where("code").is(code)
                .and("used").is(false)
        );
        
        var results = elasticsearchOperations.search(query, ConnectionCode.class);
        if (results.isEmpty()) {
            throw new IllegalArgumentException("Connection code not found or expired");
        }
        
        ConnectionCode connectionCode = results.getSearchHit(0).getContent();
        connectionCode.setUsed(true);
        elasticsearchOperations.save(connectionCode);
        
        String connectionId = UUID.randomUUID().toString();
        PeerConnection connection = PeerConnection.builder()
                .connectionId(connectionId)
                .localUserId(userId)
                .remoteUserId(connectionCode.getInitiatorId())
                .remoteUserName(connectionCode.getInitiatorName())
                .status(PeerConnection.ConnectionStatus.ESTABLISHED)
                .establishedTime(LocalDateTime.now())
                .lastActiveTime(LocalDateTime.now())
                .encryptionKey("es-encryption-key-" + connectionId)
                .build();
        
        elasticsearchOperations.save(connection);
        log.info("Established connection: {}", connectionId);
        return connection;
    }

    @Override
    public boolean disconnect(String connectionId) {
        Query query = new CriteriaQuery(Criteria.where("connectionId").is(connectionId));
        elasticsearchOperations.delete(query, PeerConnection.class);
        log.info("Disconnected: {}", connectionId);
        return true;
    }

    @Override
    public List<PeerConnection> getConnections(String userId) {
        Query query = new CriteriaQuery(
            Criteria.where("localUserId").is(userId)
                .or("remoteUserId").is(userId)
        );
        return elasticsearchOperations.search(query, PeerConnection.class)
                .stream().map(hit -> hit.getContent()).collect(Collectors.toList());
    }

    @Override
    public Optional<PeerConnection> getConnection(String connectionId) {
        Query query = new CriteriaQuery(Criteria.where("connectionId").is(connectionId));
        var results = elasticsearchOperations.search(query, PeerConnection.class);
        return results.isEmpty() ? Optional.empty() : Optional.of(results.getSearchHit(0).getContent());
    }

    @Override
    public SharedKnowledge shareKnowledge(String connectionId, SharedKnowledge knowledge) {
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
        
        elasticsearchOperations.save(shared);
        log.info("Shared knowledge: {}", shared.getKnowledgeId());
        return shared;
    }

    @Override
    public List<SharedKnowledge> receiveKnowledge(String connectionId) {
        Query query = Query.findAll();
        return elasticsearchOperations.search(query, SharedKnowledge.class)
                .stream().map(hit -> hit.getContent()).collect(Collectors.toList());
    }

    @Override
    public boolean verifyKnowledge(String knowledgeId, double qualityScore) {
        Query query = new CriteriaQuery(Criteria.where("knowledgeId").is(knowledgeId));
        var results = elasticsearchOperations.search(query, SharedKnowledge.class);
        if (!results.isEmpty()) {
            SharedKnowledge knowledge = results.getSearchHit(0).getContent();
            knowledge.setQualityScore(qualityScore);
            knowledge.setVerified(true);
            elasticsearchOperations.save(knowledge);
            return true;
        }
        return false;
    }

    @Override
    public Map<String, Object> getSharingStatistics(String userId) {
        Map<String, Object> stats = new HashMap<>();
        stats.put("connections_total", getConnections(userId).size());
        stats.put("storage_backend", "elasticsearch");
        return stats;
    }

    @Override
    public String encrypt(String content, String connectionId) {
        if (content == null) return null;
        return Base64.getEncoder().encodeToString((content + ":" + connectionId).getBytes());
    }

    @Override
    public String decrypt(String encryptedContent, String connectionId) {
        if (encryptedContent == null) return null;
        String decoded = new String(Base64.getDecoder().decode(encryptedContent));
        String suffix = ":" + connectionId;
        return decoded.endsWith(suffix) ? decoded.substring(0, decoded.length() - suffix.length()) : decoded;
    }
}
