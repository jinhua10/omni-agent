package top.yumbo.ai.omni.voting.starter.impl.mongodb;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;
import top.yumbo.ai.omni.voting.api.VotingService;
import top.yumbo.ai.omni.voting.api.model.Vote;
import top.yumbo.ai.omni.voting.api.model.VotingResult;
import top.yumbo.ai.omni.voting.api.model.VotingSession;

import java.time.LocalDateTime;
import java.util.*;

/**
 * MongoDB投票服务实现
 * (MongoDB Voting Service Implementation)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class MongoVotingService implements VotingService {

    private final MongoTemplate mongoTemplate;
    private final MongoVotingProperties properties;

    public MongoVotingService(MongoTemplate mongoTemplate, MongoVotingProperties properties) {
        this.mongoTemplate = mongoTemplate;
        this.properties = properties;
        log.info("MongoVotingService initialized with MongoDB");
    }

    @Override
    public VotingSession createSession(String topic, String description, String initiatorId,
                                      int durationMinutes, int minParticipants, double approvalThreshold) {
        String sessionId = UUID.randomUUID().toString();

        VotingSession session = VotingSession.builder()
                .sessionId(sessionId)
                .topic(topic)
                .description(description)
                .initiatorId(initiatorId)
                .createTime(LocalDateTime.now())
                .deadline(LocalDateTime.now().plusMinutes(durationMinutes))
                .status(VotingSession.VotingStatus.OPEN)
                .minParticipants(minParticipants)
                .approvalThreshold(approvalThreshold)
                .votes(new ArrayList<>())
                .build();

        mongoTemplate.save(session, properties.getSessionCollectionName());

        log.info("Created voting session: {} for topic: {}", sessionId, topic);

        return session;
    }

    @Override
    public Optional<VotingSession> getSession(String sessionId) {
        Query query = new Query(Criteria.where("sessionId").is(sessionId));
        VotingSession session = mongoTemplate.findOne(query, VotingSession.class,
                properties.getSessionCollectionName());
        return Optional.ofNullable(session);
    }

    @Override
    public List<VotingSession> getActiveSessions() {
        Query query = new Query(Criteria.where("status").is(VotingSession.VotingStatus.OPEN));
        return mongoTemplate.find(query, VotingSession.class, properties.getSessionCollectionName());
    }

    @Override
    public boolean closeSession(String sessionId) {
        Query query = new Query(Criteria.where("sessionId").is(sessionId));
        VotingSession session = mongoTemplate.findOne(query, VotingSession.class,
                properties.getSessionCollectionName());

        if (session == null) return false;

        VotingResult result = arbitrate(sessionId);

        VotingSession.VotingStatus newStatus = VotingSession.VotingStatus.CLOSED;
        if (result.getFinalDecision() == VotingResult.Decision.APPROVED) {
            newStatus = VotingSession.VotingStatus.APPROVED;
        } else if (result.getFinalDecision() == VotingResult.Decision.REJECTED) {
            newStatus = VotingSession.VotingStatus.REJECTED;
        }

        Update update = new Update()
                .set("status", newStatus)
                .set("result", result);

        mongoTemplate.updateFirst(query, update, VotingSession.class,
                properties.getSessionCollectionName());

        log.info("Closed session: {} - {}", sessionId, result.getFinalDecision());
        return true;
    }

    @Override
    public Vote castVote(Vote vote) {
        String voteId = UUID.randomUUID().toString();
        vote.setVoteId(voteId);
        vote.setVoteTime(LocalDateTime.now());

        if (vote.getWeight() == 0) {
            vote.setWeight(vote.getVoterType().getDefaultWeight());
        }

        mongoTemplate.save(vote, properties.getVoteCollectionName());

        log.info("Vote cast: {} - {}", voteId, vote.getChoice());
        return vote;
    }

    @Override
    public List<Vote> getVotes(String sessionId) {
        Query query = new Query(Criteria.where("sessionId").is(sessionId));
        return mongoTemplate.find(query, Vote.class, properties.getVoteCollectionName());
    }

    @Override
    public boolean revokeVote(String voteId) {
        Query query = new Query(Criteria.where("voteId").is(voteId));
        var result = mongoTemplate.remove(query, Vote.class, properties.getVoteCollectionName());

        boolean success = result.getDeletedCount() > 0;
        if (success) {
            log.info("Vote revoked: {}", voteId);
        }
        return success;
    }

    @Override
    public VotingResult calculateResult(String sessionId) {
        List<Vote> voteList = getVotes(sessionId);

        int approveCount = 0, rejectCount = 0, abstainCount = 0;
        double approveScore = 0, rejectScore = 0, abstainScore = 0;

        for (Vote vote : voteList) {
            switch (vote.getChoice()) {
                case APPROVE -> {
                    approveCount++;
                    approveScore += vote.getWeight();
                }
                case REJECT -> {
                    rejectCount++;
                    rejectScore += vote.getWeight();
                }
                case ABSTAIN -> {
                    abstainCount++;
                    abstainScore += vote.getWeight();
                }
            }
        }

        double totalScore = approveScore + rejectScore;
        double approvalRate = totalScore > 0 ? (approveScore / totalScore) * 100 : 0;

        return VotingResult.builder()
                .sessionId(sessionId)
                .approveCount(approveCount)
                .rejectCount(rejectCount)
                .abstainCount(abstainCount)
                .approveScore(approveScore)
                .rejectScore(rejectScore)
                .abstainScore(abstainScore)
                .totalParticipants(voteList.size())
                .approvalRate(approvalRate)
                .build();
    }

    @Override
    public VotingResult arbitrate(String sessionId) {
        Query query = new Query(Criteria.where("sessionId").is(sessionId));
        VotingSession session = mongoTemplate.findOne(query, VotingSession.class,
                properties.getSessionCollectionName());

        if (session == null) {
            throw new IllegalArgumentException("Session not found: " + sessionId);
        }

        VotingResult result = calculateResult(sessionId);

        if (result.getTotalParticipants() < session.getMinParticipants()) {
            result.setFinalDecision(VotingResult.Decision.PENDING);
            result.setReason("Insufficient participants");
        } else if (result.getApprovalRate() >= session.getApprovalThreshold() * 100) {
            result.setFinalDecision(VotingResult.Decision.APPROVED);
            result.setReason(String.format("Approved: %.1f%%", result.getApprovalRate()));
        } else {
            result.setFinalDecision(VotingResult.Decision.REJECTED);
            result.setReason(String.format("Rejected: %.1f%%", result.getApprovalRate()));
        }

        log.info("Arbitrated session: {} - {} ({})",
                sessionId, result.getFinalDecision(), result.getReason());

        return result;
    }

    @Override
    public Map<String, Object> getStatistics() {
        long totalSessions = mongoTemplate.count(new Query(), VotingSession.class,
                properties.getSessionCollectionName());
        long activeSessions = mongoTemplate.count(
                new Query(Criteria.where("status").is(VotingSession.VotingStatus.OPEN)),
                VotingSession.class, properties.getSessionCollectionName());
        long totalVotes = mongoTemplate.count(new Query(), Vote.class,
                properties.getVoteCollectionName());

        Map<String, Object> stats = new HashMap<>();
        stats.put("totalSessions", totalSessions);
        stats.put("activeSessions", activeSessions);
        stats.put("totalVotes", totalVotes);
        stats.put("storage_backend", "mongodb");

        return stats;
    }
}
