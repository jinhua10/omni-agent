package top.yumbo.ai.omni.voting.starter.impl.memory;

import lombok.extern.slf4j.Slf4j;
import top.yumbo.ai.omni.voting.api.VotingService;
import top.yumbo.ai.omni.voting.api.model.Vote;
import top.yumbo.ai.omni.voting.api.model.VotingResult;
import top.yumbo.ai.omni.voting.api.model.VotingSession;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 内存投票服务实现
 * (Memory Voting Service Implementation)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
public class MemoryVotingService implements VotingService {

    private final Map<String, VotingSession> sessions = new ConcurrentHashMap<>();
    private final Map<String, Vote> votes = new ConcurrentHashMap<>();
    private final Map<String, Set<String>> sessionVotes = new ConcurrentHashMap<>();

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

        sessions.put(sessionId, session);
        log.info("Created voting session: {}", sessionId);

        return session;
    }

    @Override
    public Optional<VotingSession> getSession(String sessionId) {
        return Optional.ofNullable(sessions.get(sessionId));
    }

    @Override
    public List<VotingSession> getActiveSessions() {
        return sessions.values().stream()
                .filter(s -> s.getStatus() == VotingSession.VotingStatus.OPEN)
                .collect(Collectors.toList());
    }

    @Override
    public boolean closeSession(String sessionId) {
        VotingSession session = sessions.get(sessionId);
        if (session == null) return false;

        session.setStatus(VotingSession.VotingStatus.CLOSED);
        VotingResult result = arbitrate(sessionId);
        session.setResult(result);

        if (result.getFinalDecision() == VotingResult.Decision.APPROVED) {
            session.setStatus(VotingSession.VotingStatus.APPROVED);
        } else if (result.getFinalDecision() == VotingResult.Decision.REJECTED) {
            session.setStatus(VotingSession.VotingStatus.REJECTED);
        }

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

        votes.put(voteId, vote);
        sessionVotes.computeIfAbsent(vote.getSessionId(), k -> ConcurrentHashMap.newKeySet())
                .add(voteId);

        log.info("Vote cast: {} - {}", voteId, vote.getChoice());
        return vote;
    }

    @Override
    public List<Vote> getVotes(String sessionId) {
        return sessionVotes.getOrDefault(sessionId, Collections.emptySet()).stream()
                .map(votes::get)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    @Override
    public boolean revokeVote(String voteId) {
        Vote vote = votes.remove(voteId);
        if (vote != null) {
            sessionVotes.getOrDefault(vote.getSessionId(), Collections.emptySet())
                    .remove(voteId);
            return true;
        }
        return false;
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
        VotingSession session = sessions.get(sessionId);
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

        return result;
    }

    @Override
    public Map<String, Object> getStatistics() {
        Map<String, Object> stats = new HashMap<>();
        stats.put("totalSessions", sessions.size());
        stats.put("activeSessions", getActiveSessions().size());
        stats.put("totalVotes", votes.size());
        return stats;
    }
}

