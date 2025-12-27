package top.yumbo.ai.omni.core.voting;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import top.yumbo.ai.omni.voting.api.VotingService;
import top.yumbo.ai.omni.voting.api.model.Vote;
import top.yumbo.ai.omni.voting.api.model.VotingResult;
import top.yumbo.ai.omni.voting.api.model.VotingSession;

import java.time.LocalDateTime;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * 投票仲裁器
 * (Voting Arbiter)
 *
 * 管理投票会话和仲裁决策
 * (Manages voting sessions and arbitration decisions)
 *
 * @author OmniAgent Team
 * @since 1.0.0
 */
@Slf4j
@Service
public class VotingArbiter implements VotingService {

    /**
     * 投票会话存储
     */
    private final Map<String, VotingSession> sessions = new ConcurrentHashMap<>();

    /**
     * 投票存储
     */
    private final Map<String, Vote> votes = new ConcurrentHashMap<>();

    /**
     * 会话投票索引
     */
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

        log.info("Created voting session: {} - {}", sessionId, topic);

        // 自动关闭会话
        scheduleAutoClose(sessionId, durationMinutes);

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
                .filter(s -> LocalDateTime.now().isBefore(s.getDeadline()))
                .collect(Collectors.toList());
    }

    @Override
    public boolean closeSession(String sessionId) {
        VotingSession session = sessions.get(sessionId);
        if (session == null) {
            return false;
        }

        session.setStatus(VotingSession.VotingStatus.CLOSED);

        // 计算最终结果
        VotingResult result = calculateResult(sessionId);
        session.setResult(result);

        // 根据结果更新状态
        if (result.getFinalDecision() == VotingResult.Decision.APPROVED) {
            session.setStatus(VotingSession.VotingStatus.APPROVED);
        } else if (result.getFinalDecision() == VotingResult.Decision.REJECTED) {
            session.setStatus(VotingSession.VotingStatus.REJECTED);
        }

        log.info("Closed voting session: {} - Result: {}", sessionId, result.getFinalDecision());

        return true;
    }

    @Override
    public Vote castVote(Vote vote) {
        VotingSession session = sessions.get(vote.getSessionId());
        if (session == null) {
            throw new IllegalArgumentException("Voting session not found");
        }

        if (session.getStatus() != VotingSession.VotingStatus.OPEN) {
            throw new IllegalStateException("Voting session is not open");
        }

        if (LocalDateTime.now().isAfter(session.getDeadline())) {
            session.setStatus(VotingSession.VotingStatus.EXPIRED);
            throw new IllegalStateException("Voting session has expired");
        }

        // 设置投票权重
        if (vote.getWeight() == 0) {
            vote.setWeight(vote.getVoterType().getDefaultWeight());
        }

        // 生成投票ID
        String voteId = UUID.randomUUID().toString();
        vote.setVoteId(voteId);
        vote.setVoteTime(LocalDateTime.now());

        // 存储投票
        votes.put(voteId, vote);
        sessionVotes.computeIfAbsent(vote.getSessionId(), k -> ConcurrentHashMap.newKeySet())
                .add(voteId);

        log.info("Vote cast: {} - {} by {} ({})",
                voteId, vote.getChoice(), vote.getVoterId(), vote.getVoterType());

        return vote;
    }

    @Override
    public List<Vote> getVotes(String sessionId) {
        Set<String> voteIds = sessionVotes.getOrDefault(sessionId, Collections.emptySet());
        return voteIds.stream()
                .map(votes::get)
                .filter(Objects::nonNull)
                .collect(Collectors.toList());
    }

    @Override
    public boolean revokeVote(String voteId) {
        Vote vote = votes.remove(voteId);
        if (vote == null) {
            return false;
        }

        sessionVotes.getOrDefault(vote.getSessionId(), Collections.emptySet())
                .remove(voteId);

        log.info("Vote revoked: {}", voteId);
        return true;
    }

    @Override
    public VotingResult calculateResult(String sessionId) {
        List<Vote> sessionVoteList = getVotes(sessionId);

        int approveCount = 0;
        int rejectCount = 0;
        int abstainCount = 0;

        double approveScore = 0;
        double rejectScore = 0;
        double abstainScore = 0;

        for (Vote vote : sessionVoteList) {
            switch (vote.getChoice()) {
                case APPROVE:
                    approveCount++;
                    approveScore += vote.getWeight();
                    break;
                case REJECT:
                    rejectCount++;
                    rejectScore += vote.getWeight();
                    break;
                case ABSTAIN:
                    abstainCount++;
                    abstainScore += vote.getWeight();
                    break;
            }
        }

        int totalParticipants = sessionVoteList.size();
        double totalScore = approveScore + rejectScore;
        double approvalRate = totalScore > 0 ? (approveScore / totalScore) * 100 : 0;

        VotingResult result = VotingResult.builder()
                .sessionId(sessionId)
                .approveCount(approveCount)
                .rejectCount(rejectCount)
                .abstainCount(abstainCount)
                .approveScore(approveScore)
                .rejectScore(rejectScore)
                .abstainScore(abstainScore)
                .totalParticipants(totalParticipants)
                .approvalRate(approvalRate)
                .build();

        return result;
    }

    @Override
    public VotingResult arbitrate(String sessionId) {
        VotingSession session = sessions.get(sessionId);
        if (session == null) {
            throw new IllegalArgumentException("Voting session not found");
        }

        VotingResult result = calculateResult(sessionId);

        // 判断是否达到最小参与人数
        if (result.getTotalParticipants() < session.getMinParticipants()) {
            result.setFinalDecision(VotingResult.Decision.PENDING);
            result.setReason("Insufficient participants");
            return result;
        }

        // 判断是否达到通过阈值
        if (result.getApprovalRate() >= session.getApprovalThreshold() * 100) {
            result.setFinalDecision(VotingResult.Decision.APPROVED);
            result.setReason(String.format("Approved with %.1f%% approval rate", result.getApprovalRate()));
        } else {
            result.setFinalDecision(VotingResult.Decision.REJECTED);
            result.setReason(String.format("Rejected with %.1f%% approval rate (threshold: %.1f%%)",
                    result.getApprovalRate(), session.getApprovalThreshold() * 100));
        }

        log.info("Arbitration result for session {}: {} - {}",
                sessionId, result.getFinalDecision(), result.getReason());

        return result;
    }

    @Override
    public Map<String, Object> getStatistics() {
        Map<String, Object> stats = new HashMap<>();
        stats.put("totalSessions", sessions.size());
        stats.put("activeSessions", getActiveSessions().size());
        stats.put("totalVotes", votes.size());

        long approvedSessions = sessions.values().stream()
                .filter(s -> s.getStatus() == VotingSession.VotingStatus.APPROVED)
                .count();
        long rejectedSessions = sessions.values().stream()
                .filter(s -> s.getStatus() == VotingSession.VotingStatus.REJECTED)
                .count();

        stats.put("approvedSessions", approvedSessions);
        stats.put("rejectedSessions", rejectedSessions);

        return stats;
    }

    /**
     * 定时自动关闭会话
     */
    private void scheduleAutoClose(String sessionId, int durationMinutes) {
        new Thread(() -> {
            try {
                Thread.sleep((durationMinutes + 1) * 60 * 1000);
                VotingSession session = sessions.get(sessionId);
                if (session != null && session.getStatus() == VotingSession.VotingStatus.OPEN) {
                    closeSession(sessionId);
                    log.info("Auto-closed expired voting session: {}", sessionId);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }).start();
    }
}


