package top.yumbo.ai.omni.voting.starter.elasticsearch;

import lombok.extern.slf4j.Slf4j;
import org.springframework.data.elasticsearch.core.ElasticsearchOperations;
import org.springframework.data.elasticsearch.core.query.Criteria;
import org.springframework.data.elasticsearch.core.query.CriteriaQuery;
import org.springframework.data.elasticsearch.core.query.Query;
import top.yumbo.ai.omni.voting.api.VotingService;
import top.yumbo.ai.omni.voting.api.model.Vote;
import top.yumbo.ai.omni.voting.api.model.VotingResult;
import top.yumbo.ai.omni.voting.api.model.VotingSession;

import java.time.LocalDateTime;
import java.util.*;
import java.util.stream.Collectors;

@Slf4j
public class ElasticsearchVotingService implements VotingService {

    private final ElasticsearchOperations elasticsearchOperations;
    private final ElasticsearchVotingProperties properties;

    public ElasticsearchVotingService(ElasticsearchOperations elasticsearchOperations,
                                     ElasticsearchVotingProperties properties) {
        this.elasticsearchOperations = elasticsearchOperations;
        this.properties = properties;
        log.info("ElasticsearchVotingService initialized");
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

        elasticsearchOperations.save(session);
        log.info("Created voting session: {} for topic: {}", sessionId, topic);

        return session;
    }

    @Override
    public Optional<VotingSession> getSession(String sessionId) {
        Query query = new CriteriaQuery(Criteria.where("sessionId").is(sessionId));
        var results = elasticsearchOperations.search(query, VotingSession.class);
        return results.isEmpty() ? Optional.empty() : Optional.of(results.getSearchHit(0).getContent());
    }

    @Override
    public List<VotingSession> getActiveSessions() {
        Query query = new CriteriaQuery(Criteria.where("status").is(VotingSession.VotingStatus.OPEN));
        return elasticsearchOperations.search(query, VotingSession.class)
                .stream().map(hit -> hit.getContent()).collect(Collectors.toList());
    }

    @Override
    public boolean closeSession(String sessionId) {
        Query query = new CriteriaQuery(Criteria.where("sessionId").is(sessionId));
        var results = elasticsearchOperations.search(query, VotingSession.class);
        
        if (results.isEmpty()) return false;

        VotingSession session = results.getSearchHit(0).getContent();
        VotingResult result = arbitrate(sessionId);

        if (result.getFinalDecision() == VotingResult.Decision.APPROVED) {
            session.setStatus(VotingSession.VotingStatus.APPROVED);
        } else if (result.getFinalDecision() == VotingResult.Decision.REJECTED) {
            session.setStatus(VotingSession.VotingStatus.REJECTED);
        } else {
            session.setStatus(VotingSession.VotingStatus.CLOSED);
        }

        session.setResult(result);
        elasticsearchOperations.save(session);

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

        elasticsearchOperations.save(vote);
        log.info("Vote cast: {} - {}", voteId, vote.getChoice());
        return vote;
    }

    @Override
    public List<Vote> getVotes(String sessionId) {
        Query query = new CriteriaQuery(Criteria.where("sessionId").is(sessionId));
        return elasticsearchOperations.search(query, Vote.class)
                .stream().map(hit -> hit.getContent()).collect(Collectors.toList());
    }

    @Override
    public boolean revokeVote(String voteId) {
        Query query = new CriteriaQuery(Criteria.where("voteId").is(voteId));
        elasticsearchOperations.delete(query, Vote.class);
        log.info("Vote revoked: {}", voteId);
        return true;
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
        Query query = new CriteriaQuery(Criteria.where("sessionId").is(sessionId));
        var results = elasticsearchOperations.search(query, VotingSession.class);

        if (results.isEmpty()) {
            throw new IllegalArgumentException("Session not found: " + sessionId);
        }

        VotingSession session = results.getSearchHit(0).getContent();
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
        Query allSessionsQuery = Query.findAll();
        long totalSessions = elasticsearchOperations.count(allSessionsQuery, VotingSession.class);
        
        Query activeQuery = new CriteriaQuery(Criteria.where("status").is(VotingSession.VotingStatus.OPEN));
        long activeSessions = elasticsearchOperations.count(activeQuery, VotingSession.class);
        
        long totalVotes = elasticsearchOperations.count(Query.findAll(), Vote.class);

        Map<String, Object> stats = new HashMap<>();
        stats.put("totalSessions", totalSessions);
        stats.put("activeSessions", activeSessions);
        stats.put("totalVotes", totalVotes);
        stats.put("storage_backend", "elasticsearch");

        return stats;
    }
}
